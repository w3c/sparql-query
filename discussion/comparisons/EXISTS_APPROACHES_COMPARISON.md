# Comparison of EXISTS Implementation Approaches: ONCE, OVERALL, and CONSTRAIN

## Overview

This document compares three different approaches for implementing SPARQL EXISTS with DEEP INJECTION semantics:

1. **ONCE (One-at-a-time)** - Current implementation in `exists.lisp` and `exists-once.lisp`
2. **OVERALL** - Proposed variant that evaluates EXISTS pattern once for all solutions
3. **CONSTRAIN-based** - Alternative using isolated subquery execution similar to `constrain.lisp`

## The Three Approaches

### 1. ONCE (One-at-a-time) Approach

**Semantics (from document):**
```
expr(μ, D, G) for EXISTS:
  Let E be the algebraic query expression for the pattern
  Then expr(μ, D, G) is:
    "true" if eval(D(G), E, μ) is nonempty
    "false" otherwise
```

**Implementation (operator-level, no pattern rewrite):**
- For each solution μ in the filter context:
  1. Construct the unary multiset Ω' = {μ} (O(V))
  2. Evaluate the EXISTS pattern by modifying only the leaf operators (BGP/Path/ToMultiset) to compute `Join(Ω, Ω')` internally
  3. Return true if any results, false otherwise

**Correlation with Original Context:**
- Each solution is processed individually
- EXISTS evaluation happens per solution
- Results are immediately boolean (true/false) per solution
- No separate result correlation step needed - evaluation happens in context of each solution

**Code Pattern (conceptual):**
```lisp
;; For each solution:
(let ((solution-mapping (extract-from-page base-page base-index)))
  (let ((exists-result (eval-with-unary-context exists-pattern solution-mapping)))
    (if exists-result 'true 'false)))
```

---

### 2. OVERALL Approach

**Semantics (from document):**
```
Filter(F, Ω, D, G) = {μ in Ω | expr(μ, D, G, Ω) has effective boolean value true}

expr(μ, D, G, Ω_ctx) for EXISTS:
  Let E be the algebraic query expression for the pattern
  Let Ω = eval(D(G), E, Ω_ctx)  -- evaluated ONCE for ALL solutions
  Then expr(μ, D, G, Ω_ctx) is:
    "true" if there exists μ' in Ω such that μ and μ' are compatible
    "false" otherwise
```

**Key Insight:**
- The EXISTS pattern is evaluated **once** with the entire solution set Ω_ctx
- Then for each individual solution μ, we check compatibility with results in Ω
- This is the optimization: pattern evaluated once, compatibility checks done per solution

**Correlation with Original Context:**
- Step 1: Evaluate EXISTS pattern with all solutions from filter context
  - Pattern sees all solution mappings: `eval(D(G), E, Ω_ctx)`
  - Results in multiset Ω of solution mappings
- Step 2: For each original solution μ:
  - Check if μ is compatible with any solution in Ω
  - This is: `Join({μ}, Ω) ≠ ∅`
  - Compatibility means: same variable bindings agree on shared variables

**Implementation Pattern:**
```lisp
;; Step 1: Evaluate EXISTS pattern once with all solutions
(let ((all-solutions (get-filter-context-solutions)))  ; Ω_ctx
  (let ((exists-results (evaluate-exists-pattern all-solutions))))  ; Ω

;; Step 2: Correlate for each solution
(loop for solution in filter-context-solutions
  do (if (compatible-with-any? solution exists-results)
         (keep solution)
         (discard solution)))
```

**Example:**
```
Query: SELECT ?s WHERE { ?s :p ?o FILTER EXISTS { ?s :q ?v } }

Filter context: { {?s=1, ?o=A}, {?s=2, ?o=B}, {?s=1, ?o=C} }

OVERALL approach:
1. Evaluate EXISTS pattern with Ω_ctx = { {?s=1, ?o=A}, {?s=2, ?o=B}, {?s=1, ?o=C} }
   - Pattern: { ?s :q ?v }
   - Results Ω: { {?s=1, ?v=X}, {?s=2, ?v=Y} }
   
2. Correlate each solution:
   - {?s=1, ?o=A} → compatible with {?s=1, ?v=X} → KEEP
   - {?s=2, ?o=B} → compatible with {?s=2, ?v=Y} → KEEP  
   - {?s=1, ?o=C} → compatible with {?s=1, ?v=X} → KEEP
   
ONCE approach would evaluate the pattern 3 times:
- Once with {?s=1, ?o=A} → finds {?s=1, ?v=X} → true
- Once with {?s=2, ?o=B} → finds {?s=2, ?v=Y} → true
- Once with {?s=1, ?o=C} → finds {?s=1, ?v=X} → true
```

**Advantages:**
- Pattern evaluated once instead of N times
- Can leverage join optimization for compatibility checking
- More efficient for large solution sets

**Disadvantages:**
- More complex correlation logic
- Must handle compatibility checking (variable overlap)
- Requires storing all EXISTS results until correlation complete

---

### 3. CONSTRAIN-based Approach

**Semantics (derived from EXTEND):**
- Rewrites the secondary (dependent) pattern to embed correlation on shared variables (e.g., via VALUES/bindings at the leaves), so it can be executed as an isolated subquery.
- Evaluates the rewritten subquery once (or once per segment when grouping/sharding applies), then joins its results with the current context using a standard join operator.
- Falls back to dynamic SIP-style per-solution execution only when shared variables are partially unbound and cannot be statically injected.

**Implementation Pattern (conceptual):**

```lisp
;; Compile-time rewrite
(let* ((shared (intersect-dimensions base-vars subquery-vars))
       (rewritten (inject-correlation-into-subquery subquery shared)))
  ;; Execute rewritten subquery as an isolated pattern (once or per segment)
  (let ((Ω_sub (evaluate rewritten)))
    ;; Correlate by join on shared variables
    (join base Ω_sub :on shared)))
```

**Correlation with Original Context:**
- Achieved by a standard join between the base solutions and the (rewritten) subquery results on the shared variables.
- In segment mode, the subquery is evaluated per segment of shared keys (not per individual solution), then joined.
- Only in the fallback mode (unbound overlap) does it evaluate per solution with dynamic bindings.

**Key Differences from ONCE:**
- Prefers rewrite-and-join over per-solution execution; typically evaluates the dependent pattern once (or per segment) rather than N times.
- Uses join-based correlation of result mappings instead of immediate boolean checks.

---

## Comparison Table

| Aspect | ONCE (operator-level) | OVERALL | CONSTRAIN |
|--------|------------------------|---------|-----------|
| **Pattern Evaluation** | N times (once per solution) | 1 time (all solutions) | 1 time (rewrite/segment) or N times (fallback) |
| **Result Correlation** | Immediate boolean per solution | Compatibility check per solution | Join on shared variables |
| **Complexity** | O(N × (V + Q)) | O(Q + N × C) where C = compatibility check | O(Q + J) in rewrite/segment mode; O(N × (V + Q + J)) in fallback |
| **Space** | O(P + V) | O(P + \|Ω\|) where |\Ω\| = EXISTS result size | O(P + \|Ω_sub\|) rewrite/segment; O(P + V) fallback |
| **Optimization Potential** | Similar to dynamic binding | High - pattern evaluated once | High when rewrite/segment applies |

### Complexity Breakdown

**ONCE (operator-level):**
- Per solution: Extract (O(V)) + Pattern eval (O(Q))
- Total: O(N × (V + Q))
- Note: A rewriting-based ONCE (injecting VALUES by transforming the pattern) would add O(N × P), but this is optional and not required by the DEEP INJECTION definition.

**OVERALL:**
- Pattern evaluation: O(Q) - done once
- Compatibility checks: O(N × C) where C depends on result size
  - If |Ω| is small: C = O(|Ω|) - linear search
  - If |Ω| is large: C = O(log |Ω|) - can use hash join or index
- Total: O(Q + N × C)
- Key insight: If Q is expensive and N is large, OVERALL is better

**CONSTRAIN:**
- Similar to ONCE but with explicit join correlation
- Per solution: Extract (O(V)) + Pattern eval (O(Q)) + Join (O(J))
- Total: O(N × (V + Q + J))
- J depends on EXISTS result size and join algorithm

---

## Correlation Mechanisms

### ONCE: Direct Evaluation
```
Solution μ → Inject into pattern → Evaluate → Boolean result
No separate correlation - evaluation happens in solution context
```

### OVERALL: Compatibility Checking
```
Step 1: All solutions → Pattern evaluation → Result set Ω
Step 2: For each μ: Check Join({μ}, Ω) ≠ ∅
Correlation via set compatibility/compatibility matching
```

### CONSTRAIN: Join-based Correlation
```
Solution μ → Dynamic bindings → Subquery execution → Result mappings
Then: Join(base solutions, EXISTS results) on shared variables
Correlation via explicit join operator
```

---

## Use Case Analysis

### Scenario 1: Simple EXISTS, Many Solutions

**Query:** `SELECT ?s WHERE { ?s :p ?o FILTER EXISTS { ?s :q ?v } }`
- N = 10,000 solutions
- Pattern complexity: Low (simple BGP)
- EXISTS results: Typically sparse (few matches)

**ONCE:** 10,000 pattern evaluations
**OVERALL:** 1 pattern evaluation + 10,000 compatibility checks
**Winner:** OVERALL (pattern evaluation is the expensive part)

### Scenario 2: Complex EXISTS, Few Solutions

**Query:** `SELECT ?s WHERE { ?s :p ?o FILTER EXISTS { [complex nested pattern] } }`
- N = 10 solutions  
- Pattern complexity: High (many joins, filters)
- EXISTS results: Potentially large

**ONCE:** 10 expensive pattern evaluations
**OVERALL:** 1 expensive evaluation + 10 cheap compatibility checks
**Winner:** OVERALL (pattern is expensive, few solutions to check)

### Scenario 3: EXISTS with Variable Projection

**Query:** `SELECT ?s WHERE { ?s :p ?o FILTER EXISTS { SELECT ?s (COUNT(?x) as ?cnt) WHERE {...} } }`
- EXISTS returns aggregated results
- Need to correlate aggregated values

**ONCE:** Works but must evaluate per solution
**OVERALL:** Can evaluate once, but compatibility more complex (aggregated values)
**CONSTRAIN:** Better suited - explicit join can handle aggregated correlation
**Winner:** CONSTRAIN or OVERALL depending on aggregation complexity

### Scenario 4: EXISTS in OPTIONAL Context

**Query:** `SELECT ?s WHERE { ?s :p ?o OPTIONAL { ?s :q ?v FILTER EXISTS {...} } }`

**ONCE:** Natural fit - evaluates in context of optional binding
**OVERALL:** More complex - must handle optional context in compatibility
**CONSTRAIN:** Good fit - handles optional via leftjoin semantics
**Winner:** ONCE or CONSTRAIN

---

## Implementation Considerations

### OVERALL Implementation Challenges

1. **Solution Set Collection:**
   - Must collect all solutions from filter context before EXISTS evaluation
   - May require buffering if filter is part of streaming evaluation

2. **Compatibility Checking:**
   - Must efficiently check: "Is solution μ compatible with any in Ω?"
   - Options:
     - Hash-based: O(1) per check, O(|Ω|) space
     - Linear search: O(|Ω|) per check, O(1) space
     - Indexed: O(log |Ω|) per check, O(|Ω|) space

3. **Variable Scoping:**
   - Must handle variable overlap correctly
   - Compatibility: μ and μ' agree on all shared variables

4. **Early Termination:**
   - For EXISTS, only need to know if compatible solution exists
   - Can optimize compatibility check to stop at first match

### CONSTRAIN Implementation Benefits

1. **Reuses Existing Infrastructure:**
   - Leverages constrain.lisp mechanisms
   - Generator/channel system already in place
   - Dynamic binding system already working

2. **Explicit Correlation:**
   - Join operator handles correlation automatically
   - No manual compatibility checking needed
   - Leverages existing join optimizations

3. **Flexible:**
   - Can be adapted for EXISTS by:
     - Treating EXISTS pattern as constrained subquery
     - Converting boolean result to solution mapping format
     - Or keeping solution mappings and checking non-empty

---

## Recommendation

### For Standard EXISTS (boolean result):

1. **Few solutions (N < 100):** Use **ONCE** - simpler, overhead negligible
2. **Many solutions (N > 1000):** Use **OVERALL** - pattern evaluation dominates
3. **Complex patterns:** Use **OVERALL** - amortize expensive pattern evaluation

### For EXISTS with Result Correlation:

1. **Simple correlation:** Use **OVERALL** with compatibility checking
2. **Complex correlation:** Use **CONSTRAIN** - leverages join infrastructure

### Hybrid Approach:

Implement both ONCE and OVERALL, choose based on:
- Solution count estimate
- Pattern complexity estimate
- Available optimization hints

```lisp
(defun choose-exists-strategy (solution-count pattern-complexity)
  (if (and (> solution-count 100)
           (> pattern-complexity 10))
      'overall
      'once))
```

---

## Conclusion

The three approaches represent different trade-offs:

- **ONCE**: Simple, straightforward, evaluates pattern per solution
- **OVERALL**: Optimizes pattern evaluation but requires correlation logic
- **CONSTRAIN**: Leverages existing infrastructure, explicit correlation via joins

**OVERALL** offers the most significant optimization opportunity for the common case of many solutions with moderately complex EXISTS patterns, where pattern evaluation dominates execution time.

