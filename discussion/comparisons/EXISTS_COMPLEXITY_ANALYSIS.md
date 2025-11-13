# EXISTS Implementation Complexity Analysis

## Overview

This document analyzes the **space and time complexity** differences between four EXISTS implementation approaches for a given EXISTS group expression evaluated over N solutions.

**Notation:**
- `N` = number of solutions for which EXISTS is evaluated
- `V` = number of referenced dimensions (variables to inject)
- `P` = size of the EXISTS pattern expression (nodes in the pattern tree)
- `Q` = time to execute the EXISTS subquery pattern (query execution time)
- `J` = cost of the correlating join between base solutions and dependent results (algorithm-dependent; e.g., hash join vs nested loop; a function of input sizes)
- `C` = cost of compatibility check between a solution and EXISTS result set (algorithm-dependent; typically O(1) with hash, O(|Ω|) with linear search, or O(log |Ω|) with index)
- `|Ω|` = size of EXISTS pattern result set in OVERALL approach (number of solution mappings produced by evaluating pattern with all filter context solutions)
- `|Ω_sub|` = size of the dependent subquery result set in the CONSTRAIN rewrite/segment mode (number of solution mappings produced by the rewritten subquery prior to correlation)

## Complexity Comparison

### Dynamic Binding Approach (`exists.lisp`)

#### Time Complexity per Solution
For each solution where EXISTS is evaluated:

1. **Binding Update**: O(V)
   - Updates dynamic bindings in-place for each referenced variable
   - Code: `(setf (first dynamic-values) ,variable)` for each variable
   - Linear in number of variables to inject

2. **Query Execution**: O(Q)
   - Pattern structure is unchanged, compiled once
   - Query execution time depends on pattern complexity and data
   - Same for both approaches

**Total per solution**: O(V + Q)

**For N solutions** (worst case, assuming EXISTS is evaluated for all):
- If early termination possible: O(V + Q) - stops at first match
- Without early termination: O(N × (V + Q))

#### Space Complexity

1. **Task Creation**: O(1) amortized per EXISTS context
   - Surrogate task created once per EXISTS expression context
   - Task structure: O(1) constant space overhead
   - Dynamic bindings structure extended once: O(V) additional space

2. **Pattern Storage**: O(P)
   - Pattern stored once, unchanged
   - No per-solution pattern modification

3. **Per-solution overhead**: O(1)
   - Only in-place updates to existing bindings structure
   - No new pattern structures created

**Total space**: O(P + V) - pattern plus bindings structure

---

### "One at a Time" Approach (DEEP INJECTION: ONCE)

#### Time Complexity per Solution

For each solution where EXISTS is evaluated:

1. **Unary multiset construction**: O(V)
   - Build the singleton multiset Ω' = {μ} from the current solution mapping

2. **Operator-level injection (no pattern rewrite)**: included in Q
   - Per the DEEP INJECTION document, evaluation is abstracted as `eval(D(G), A, μ)` and only BGP (and Property Path, ToMultiset) change to perform `Join(Ω, Ω')`
   - This join is accounted for inside the normal operator evaluation cost (Q)

3. **Query Execution**: O(Q)
   - Evaluate the EXISTS pattern with the injected μ via the operator-level join

**Total per solution**: O(V + Q)

**For N solutions** (worst case):
- If early termination possible: O(V + Q) - stops at first match
- Without early termination: O(N × (V + Q))

#### Space Complexity

1. **Task Creation**: O(1) per solution
   - New task cloned for each solution
   - But tasks may be garbage collected after use

2. **VALUES/Unary multiset Creation**: O(V)
   - Creates VALUES binding structure: `(spocq.a:|bindings| (,solution-values) ,referenced-dimensions)`
   - One per solution

**Total space per solution evaluation**: O(P + V)

**For N solutions simultaneously in memory**: O(N × (P + V))
**With garbage collection (evaluations are sequential)**: O(P + V) - previous modified patterns can be garbage collected

---

### CONSTRAIN-based Approach (Rewrite/Segment preferred; SIP fallback)

Two execution modes, chosen based on shared-variable analysis and modifiers:

1) Rewrite/Segment (preferred):
- Rewrite dependent pattern to embed correlation on shared variables (e.g., VALUES at leaves) so it can run as an isolated subquery once (or per key segment).
- Join the subquery results with the base on the shared variables.

Time (rewrite/segment): O(Q + J) (or Σ O(Q_seg + J_seg) across segments)

Space (rewrite/segment): O(P + |Ω_sub|)

2) SIP fallback (only when static injection is not possible):
- Evaluate per solution with dynamic bindings, then join.

Time (fallback): O(N × (V + Q + J))

Space (fallback): O(P + V)

---

### OVERALL Approach (DEEP INJECTION: OVERALL)

**Semantics:**
- Evaluates the EXISTS pattern **once** with all solutions from the filter context (Ω_ctx) as input
- Results in multiset Ω of solution mappings from the pattern evaluation
- Then for each original solution μ, checks compatibility with results in Ω
- Solution μ passes the filter if there exists μ' in Ω such that μ and μ' are compatible (agree on shared variables)

#### Time Complexity

1. **Solution Collection**: O(N)
   - Collect all solutions from filter context into Ω_ctx
   - May require buffering if filter is part of streaming evaluation

2. **Pattern Evaluation**: O(Q)
   - Evaluate EXISTS pattern once with Ω_ctx: `eval(D(G), E, Ω_ctx)`
   - Produces result multiset Ω
   - This is done **once**, not N times

3. **Compatibility Checking**: O(N × C)
   - For each of N solutions, check if compatible with any in Ω
   - C depends on compatibility check algorithm:
     - Hash-based: O(1) per check (build hash index from Ω first: O(|Ω|))
     - Linear search: O(|Ω|) per check
     - Indexed: O(log |Ω|) per check (requires index construction: O(|Ω| log |Ω|))

**Total time**: O(N + Q + N × C)

If hash-based compatibility (typical): O(N + Q + |Ω| + N) = O(Q + |Ω| + N)

#### Space Complexity

1. **Pattern Storage**: O(P)
   - Pattern stored once, unchanged

2. **Filter Context Solutions**: O(N)
   - Must buffer all solutions from filter context to form Ω_ctx
   - Space for N solutions

3. **EXISTS Results**: O(|Ω|)
   - Store result multiset Ω from pattern evaluation
   - Size depends on pattern selectivity and data

4. **Compatibility Check Structures**: O(|Ω|)
   - Hash index or other structures for efficient compatibility checking
   - One-time construction cost

**Total space**: O(P + N + |Ω|)

**Key Insight**: OVERALL amortizes the expensive pattern evaluation (Q) over all solutions, paying instead for compatibility checking (N × C) which is typically cheaper when |Ω| is moderate.

---

## Complexity Differences Summary

### Time Complexity

| Approach | Per Solution | N Solutions (worst case) | N Solutions (with early termination) |
|----------|--------------|-------------------------|-------------------------------------|
| **Dynamic Binding** | O(V + Q) | O(N × (V + Q)) | O(V + Q) |
| **One at a Time (operator-level)** | O(V + Q) | O(N × (V + Q)) | O(V + Q) |
| **OVERALL** | — | O(N + Q + N × C) | O(N + Q + N × C) |
| **CONSTRAIN (rewrite/segment)** | — | O(Q + J) | — |
| **CONSTRAIN (fallback)** | O(V + Q + J) | O(N × (V + Q + J)) | O(V + Q + J) |

**Notes:**
- OVERALL: C is compatibility check cost; typically O(1) with hash, O(|Ω|) with linear search, or O(log |Ω|) with index. With hash: O(Q + |Ω| + N)
- Early termination not applicable to OVERALL (pattern evaluated once before compatibility checks)

Note: If the ONCE approach were implemented via pattern rewriting (e.g., injecting a VALUES clause by transforming the pattern tree), it would add an extra O(P) per-solution overhead. The DEEP INJECTION document does not require rewriting; it requires only operator-level changes to BGP/Path/ToMultiset.

### Space Complexity

| Approach | Single Solution | N Solutions Simultaneous |
|----------|-----------------|--------------------------|
| **Dynamic Binding** | O(P + V) | O(P + V) |
| **One at a Time (operator-level)** | O(P + V) | O(P + V) |
| **OVERALL** | O(P + N + \|Ω\|) | O(P + N + \|Ω\|) |
| **CONSTRAIN (rewrite/segment)** | O(P + \|Ω_sub\|) | O(P + \|Ω_sub\|) |
| **CONSTRAIN (fallback)** | O(P + V) | O(P + V) |

**Notes:**
- OVERALL: Must buffer N filter context solutions and |Ω| EXISTS results simultaneously
- CONSTRAIN (rewrite/segment): Space for subquery results depends on result size |Ω_sub|

**Key Difference**: 
- Dynamic binding: Pattern stored once, reused for all solutions
- One at a time: New pattern structure created per solution (but can be garbage collected)

---

## Why the Differences Exist

### 1. Operator-level Join vs Pattern Rewriting

- Per the DEEP INJECTION document, ONCE is realized by abstracting `eval(D(G), A, μ)` and modifying BGP/Path/ToMultiset to compute `Join(Ω, {μ})`. This does not require pattern tree modification; the extra work is just constructing {μ} and performing the join within operator evaluation (counted in Q).

- An implementation alternative is to inject a VALUES clause by transforming the pattern expression (e.g., `add-sparql-bindings`). That variant incurs an additional O(P) per solution to walk and rewrite the pattern tree. This is an implementation choice, not a requirement of the ONCE semantics.

### 2. Space Considerations

- Operator-level ONCE: pattern stored once (O(P)); per-solution unary multiset is O(V); no per-solution pattern copies are required.
- Pattern-rewriting ONCE (optional implementation): creates a new pattern tree per solution (adds O(P) transient space per solution); with sequential evaluation and GC, peak space typically remains O(P + V).

### 3. Rewrite/Segment vs SIP Fallback in CONSTRAIN

- If shared variables can be fully injected, CONSTRAIN behaves like a precomputed dependent subquery plus a join (O(Q + J)).
- If some shared variables remain unbound in ways that prevent static injection, CONSTRAIN must fall back to SIP/dynamic mode (O(N × (V + Q + J))).

### 4. OVERALL: One Pattern Evaluation vs N Compatibility Checks

- OVERALL evaluates the pattern once with all solutions (Ω_ctx), producing result set Ω
- Then performs N compatibility checks instead of N pattern evaluations
- Trade-off: Pattern evaluation (Q) done once, but must:
  - Buffer all filter context solutions (O(N) space)
  - Store EXISTS results (O(|Ω|) space)
  - Perform compatibility checking (N × C time)
- When Q is expensive and C is cheap (e.g., hash-based), OVERALL is superior to ONCE

### 5. Task Cloning Frequency

**Dynamic Binding:**
- Surrogate task created once per EXISTS expression context (at macro expansion / runtime setup)
- Task is reused, bindings updated in-place per solution

**One at a Time:**
- Task cloned per solution evaluation (in `process-unary-exists-once-complex`)
- Each clone has a different `sse-expression` (modified pattern)
- Clones can be garbage collected after use, but creation overhead exists

**OVERALL:**
- Task created once, pattern evaluated once with all solutions
- No per-solution task cloning needed
- Compatibility checking done outside task execution

---

## Practical Impact

### When Pattern Size (P) is Small

If the EXISTS pattern is simple (e.g., just a BGP with a few triples):
- `P` is small (maybe 5-10 nodes)
- O(P) overhead is negligible
- Both approaches perform similarly

### When Pattern Size (P) is Large

If the EXISTS pattern is complex (nested joins, filters, unions, etc.):
- With operator-level ONCE, there is no O(P) per-solution overhead; performance matches dynamic binding in big-O terms.
- If a pattern-rewriting implementation is used, O(P) per solution can become significant; in that case dynamic binding or operator-level ONCE is preferable.

### When Correlation Can Be Pre-Injected (CONSTRAIN)

- If correlation can be injected statically, CONSTRAIN avoids per-solution execution and reduces cost to one subquery evaluation plus a join. When Q dominates and |Ω_sub| is moderate, this is often preferable.

### When Pattern Evaluation Dominates (OVERALL)

- OVERALL is most beneficial when:
  - Pattern evaluation (Q) is expensive relative to compatibility checking (C)
  - Many solutions (N large)
  - EXISTS result set (|Ω|) is moderate (not too large for compatibility checking)
- With hash-based compatibility checking: O(Q + |Ω| + N) vs ONCE's O(N × (V + Q))
- Break-even point: When Q is expensive enough that evaluating once plus N cheap checks beats N expensive evaluations

### Memory Usage

**Dynamic Binding:**
- More memory efficient
- One pattern stored, reused
- Constant space overhead per EXISTS context

**One at a Time:**
- Higher memory usage if many solutions evaluated simultaneously
- Each solution creates new pattern structure
- However, with sequential evaluation and GC, practical difference may be small

---

## Example Scenarios

### Scenario 1: Simple BGP EXISTS

Pattern: `EXISTS { ?s :p ?o }`
- V = 1 (one variable, ?s)
- P ≈ 3 (BGP node + triple node + maybe join wrapper)
- N = 1000 solutions

**Dynamic Binding:**
- Time: O(1000 × (1 + Q)) ≈ 1000 × Q (if Q dominates)
- Space: O(3 + 1) = O(1) constant

**One at a Time (operator-level):**
- Time: O(1000 × (1 + Q)) ≈ 1000 × Q (same big-O as dynamic binding)
- Space: O(3 + 1) = O(1) constant

**CONSTRAIN (rewrite/segment):**
- Time: O(Q + J)
- Space: O(P + |Ω_sub|)

**OVERALL:**
- Time: O(N + Q + N × C) ≈ O(Q + N) with hash-based compatibility (assuming |Ω| << N)
- Space: O(P + N + |Ω|)

**Verdict**: Difference is small for simple patterns. OVERALL becomes advantageous when Q is large relative to N.

### Scenario 2: Complex EXISTS with Nested Operators

Pattern: `EXISTS {
  { ?s :p1 ?o1 }
  UNION
  { { ?s :p2 ?o2 }
    FILTER (?o2 > 10)
    OPTIONAL { ?s :p3 ?o3 }
  }
}`
- V = 1 (?s)
- P ≈ 15-20 nodes (union, join, filter, optional, bgp nodes, triple nodes)
- N = 10,000 solutions

**Dynamic Binding:**
- Time: O(10000 × (1 + Q))
- Space: O(20 + 1) = O(21) constant

**One at a Time (operator-level):**
- Time: O(10000 × (1 + Q))
- Space: O(21) constant

**CONSTRAIN (rewrite/segment):**
- Time: O(Q + J)
- Space: O(21 + |Ω_sub|)

**OVERALL:**
- Time: O(10000 + Q + 10000 × C) ≈ O(Q + 10000) with hash-based compatibility (assuming |Ω| < 10000)
- Space: O(21 + 10000 + |Ω|) = O(10000 + |Ω|)

**Verdict**: For complex patterns with many solutions, OVERALL is often the most efficient, evaluating the expensive pattern once rather than 10,000 times. CONSTRAIN can also be efficient if rewrite/segment mode applies.

---

## Conclusion

### Complexity Comparison

**Time Complexity:**
- **Dynamic Binding**: O(V + Q) per solution → O(N × (V + Q)) total
- **One at a Time (operator-level)**: O(V + Q) per solution → O(N × (V + Q)) total
- **OVERALL**: O(N + Q + N × C) total, typically O(Q + |Ω| + N) with hash-based compatibility
- **CONSTRAIN (rewrite/segment)**: O(Q + J) total (or per segment)
- **CONSTRAIN (fallback)**: O(N × (V + Q + J)) total

**Space Complexity:**
- **Dynamic Binding**: O(P + V) constant
- **One at a Time**: O(P + V) constant (pattern reused, only unary multiset per solution)
- **OVERALL**: O(P + N + |Ω|) - must buffer filter context and EXISTS results
- **CONSTRAIN (rewrite/segment)**: O(P + |Ω_sub|)
- **CONSTRAIN (fallback)**: O(P + V)

### When Differences Matter

1. **Large patterns (P >> V)**: No significant difference between operator-level ONCE and dynamic binding
2. **Many solutions (N large)**: 
   - If pattern evaluation (Q) is expensive: **OVERALL** is superior (O(Q + N) vs O(N × Q))
   - If pattern evaluation is cheap: Operator-level ONCE and dynamic binding are similar (O(N × (V + Q)))
3. **CONSTRAIN**: Prefer rewrite/segment when static injection is possible (O(Q + J)); fall back to SIP only when required (O(N × (V + Q + J)))
4. **Pattern-rewriting ONCE**: Avoid unless necessary; it introduces O(P) overhead not required by the spec
5. **Memory-constrained**: 
   - OVERALL requires most space (O(P + N + |Ω|)) - must buffer all solutions and results
   - Operator-level ONCE and dynamic binding are most space-efficient (O(P + V))
   - CONSTRAIN rewrite/segment uses space proportional to subquery result size (O(P + |Ω_sub|))

### Key Takeaway

Per the DEEP INJECTION document:
- **ONCE** does not require pattern rewriting; it only requires operator-level joins with the unary multiset {μ}. Implemented that way, **ONCE and dynamic binding have the same big-O time and space complexity** (O(V + Q) per solution, O(P + V) space).
- **OVERALL** evaluates the pattern once with all solutions (O(Q)) and then performs compatibility checking (O(N × C), typically O(N) with hash). This is superior when pattern evaluation dominates: O(Q + N) vs O(N × Q) for ONCE.
- **CONSTRAIN**, when correlation can be injected statically, evaluates the dependent subquery once (or per segment) and joins (O(Q + J)); it only falls back to per-solution SIP (O(N × (V + Q + J))) when static injection is not possible.

**Summary**: Choose approach based on:
- **Few solutions, simple patterns**: ONCE or Dynamic Binding (similar performance)
- **Many solutions, expensive patterns**: OVERALL (amortizes pattern evaluation)
- **When static correlation injection possible**: CONSTRAIN rewrite/segment (single subquery + join)
- **Memory constraints**: Prefer ONCE/Dynamic Binding (lowest space overhead)

