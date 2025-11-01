# EXISTS Implementation Complexity Analysis

## Overview

This document analyzes the **space and time complexity** differences between the two EXISTS implementation approaches for a given EXISTS group expression evaluated over N solutions.

**Notation:**
- `N` = number of solutions for which EXISTS is evaluated
- `V` = number of referenced dimensions (variables to inject)
- `P` = size of the EXISTS pattern expression (nodes in the pattern tree)
- `Q` = time to execute the EXISTS subquery pattern (query execution time)

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

## Complexity Differences Summary

### Time Complexity

| Approach | Per Solution | N Solutions (worst case) | N Solutions (with early termination) |
|----------|--------------|-------------------------|-------------------------------------|
| **Dynamic Binding** | O(V + Q) | O(N × (V + Q)) | O(V + Q) |
| **One at a Time (operator-level)** | O(V + Q) | O(N × (V + Q)) | O(V + Q) |

Note: If the ONCE approach were implemented via pattern rewriting (e.g., injecting a VALUES clause by transforming the pattern tree), it would add an extra O(P) per-solution overhead. The DEEP INJECTION document does not require rewriting; it requires only operator-level changes to BGP/Path/ToMultiset.

### Space Complexity

| Approach | Single Solution | N Solutions Simultaneous |
|----------|-----------------|--------------------------|
| **Dynamic Binding** | O(P + V) | O(P + V) |
| **One at a Time** | O(P + V) | O(N × (P + V)) |

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

### 3. Task Cloning Frequency

**Dynamic Binding:**
- Surrogate task created once per EXISTS expression context (at macro expansion / runtime setup)
- Task is reused, bindings updated in-place per solution

**One at a Time:**
- Task cloned per solution evaluation (in `process-unary-exists-once-complex`)
- Each clone has a different `sse-expression` (modified pattern)
- Clones can be garbage collected after use, but creation overhead exists

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

**Verdict**: Difference is small for simple patterns.

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

**Verdict**: Significant difference. Dynamic binding saves ~200,000 tree operations.

---

## Conclusion

### Complexity Comparison

**Time Complexity:**
- **Dynamic Binding**: O(V + Q) per solution
- **One at a Time (operator-level)**: O(V + Q) per solution
- **Note**: A pattern-rewriting variant of ONCE would add O(P) per solution, but this is not required by the DEEP INJECTION definition.

**Space Complexity:**
- **Dynamic Binding**: O(P + V) constant
- **One at a Time**: O(P + V) per solution (but can be GC'd)

### When Differences Matter

1. **Large patterns (P >> V)**: Dynamic binding significantly faster
2. **Many solutions (N large)**: Operator-level ONCE and dynamic binding behave similarly in big-O; OVERALL can still be superior when Q dominates.
3. **Pattern-rewriting ONCE**: Avoid unless necessary; it introduces O(P) overhead not required by the spec.
4. **Memory-constrained**: Operator-level ONCE and dynamic binding are comparable; rewriting may increase transient space.

### Key Takeaway

Per the DEEP INJECTION document, the ONCE approach does not require pattern rewriting; it only requires operator-level joins with the unary multiset {μ}. Implemented that way, **ONCE and dynamic binding have the same big-O time and space complexity** (O(V + Q) per solution, O(P + V) space). Any O(P) overhead arises only from an optional implementation that rewrites the pattern to inject VALUES.

