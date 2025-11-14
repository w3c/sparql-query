# Comparison of EXISTS Implementation Approaches

## Overview

This document analyzes two approaches for implementing the SPARQL EXISTS operator with DEEP INJECTION semantics:

1. **Dynamic Binding Approach** (`exists.lisp`) - Current implementation
2. **"One at a Time" Explicit Injection Approach** (`exists-once.lisp`) - Alternative implementation

## Logical Equivalence Analysis

### Are they logically equivalent?

**YES** - Both approaches are logically equivalent in terms of their final semantics. Both correctly implement DEEP INJECTION where the outer solution mapping is injected into the EXISTS subquery evaluation.

However, they differ significantly in their **implementation mechanism**:

### Key Differences

#### 1. Mechanism for Solution Injection

**Dynamic Binding Approach (`exists.lisp`):**
- Uses the runtime's dynamic binding mechanism
- Creates a surrogate task with extended dynamic bindings
- Operators access bindings implicitly via `query-binding-value`
- Solution bindings are stored in the task's dynamic bindings structure
- Pattern evaluation happens in a context where bindings are globally available

**"One at a Time" Approach (`exists-once.lisp`):**
- Explicitly extracts solution mapping from current context (base-page/base-index)
- Creates a VALUES binding representation of that single solution
- Injects VALUES binding into pattern using `add-sparql-bindings` (which joins at BGP level)
- Pattern is explicitly rewritten to include the VALUES binding
- Each solution is processed individually

#### 2. Evaluation Model

**Dynamic Binding Approach:**
- Implicit evaluation model
- Bindings flow through runtime environment
- All operators that need bindings access them via dynamic lookup
- Pattern structure remains unchanged

**"One at a Time" Approach:**
- Explicit evaluation model matching the document's `eval(D(G), A, μ_ctx)` signature
- Solution mapping flows as an explicit parameter through evaluation
- Pattern structure is modified to include VALUES binding
- Follows the document's definition: `Join(Ω_BGP, {μ_ctx})` at the BGP level

#### 3. Pattern Modification

**Dynamic Binding Approach:**
- No pattern modification needed
- Pattern compiled once, executed with different dynamic binding contexts

**"One at a Time" Approach:**
- Pattern modified per solution using `add-sparql-bindings`
- Creates: `Join(VALUES({μ_ctx}), Pattern)` for each solution
- Pattern structure changes at runtime per solution

#### 4. Code Structure

**Dynamic Binding Approach:**
- Separate handling for simple BGP (uses SIP optimization)
- Complex patterns use `dynamic-exists` with surrogate task
- Leverages existing dynamic binding infrastructure

**"One at a Time" Approach:**
- Similar separation but with explicit VALUES injection
- Simple BGP: solution copied to BGP base page (implicit join)
- Complex patterns: VALUES binding created and joined via `add-sparql-bindings`
- More explicit about the injection mechanism

## Implementation Details Comparison

### Simple BGP Case

**Dynamic Binding (`exists.lisp`):**
```lisp
;; Solution passed directly to BGP pattern function
;; BGP accesses variables via query-binding-value from dynamic bindings
(funcall pattern-function #'exists-continuation #'exists-dequeue)
```

**"One at a Time" (`exists-once.lisp`):**
```lisp
;; Solution extracted and copied to BGP base page
;; Represents explicit Join(Ω_BGP, {μ_ctx})
(loop for i-from from (array-row-major-index base-page base-index 0)
      for i-to from 0 below base-page-width
      do (setf (row-major-aref bgp-base-page i-to) 
               (row-major-aref base-page i-from)))
(funcall pattern-function #'exists-continuation #'exists-dequeue)
```

### Complex Pattern Case

**Dynamic Binding (`exists.lisp`):**
```lisp
;; Create surrogate task with extended dynamic bindings
(setf (query-dynamic-bindings cloned-task)
      (cons (append dynamic-bindings-dimensions (first bindings))
            (append (make-list ...) (rest bindings))))
;; Pattern evaluated in context with dynamic bindings available
(query-run-in-thread task exists-expression)
```

**"One at a Time" (`exists-once.lisp`):**
```lisp
;; Extract solution values for referenced dimensions
(loop for var in referenced-dimensions
      for var-pos = (position var base-dimensions)
      do (setf solution-value (extract from base-page)))
;; Create VALUES binding and inject into pattern
(let ((values-binding `(spocq.a:|bindings| (,solution-values) ,referenced-dimensions)))
  (let ((injected-expression (add-sparql-bindings values-binding exists-expression)))
    (query-run-in-thread surrogate-task injected-expression)))
```

## Alignment with Document Definition

The document "Defining_the_DEEP_INJECTION_approach_for_EXISTS.md" defines:

```
eval(D(G), BGP, μ_ctx) = Join(Ω, {μ_ctx})
```

Where:
- `Ω` is the multiset from evaluating BGP
- `μ_ctx` is the solution mapping to inject
- `Join` combines the results

### How Each Approach Achieves This

**Dynamic Binding:**
- Achieves the join implicitly: BGP evaluation accesses `μ_ctx` via dynamic bindings
- The join happens semantically through variable resolution
- Equivalent to: BGP is evaluated with `μ_ctx` already in scope

**"One at a Time":**
- Achieves the join explicitly: Creates `VALUES({μ_ctx})` and joins with pattern
- The join is structural: `Join(VALUES({μ_ctx}), Pattern)`
- Directly matches the document's definition

## Practical Implications

### Performance
- **Dynamic Binding**: May be faster due to implicit access, no pattern rewriting
- **"One at a Time"**: Requires pattern rewriting per solution, but more explicit control

### Debugging
- **Dynamic Binding**: Harder to trace binding flow, relies on runtime mechanisms
- **"One at a Time"**: Easier to inspect - VALUES binding is explicit in pattern structure

### Correctness
- Both approaches are correct and semantically equivalent
- Both properly implement DEEP INJECTION semantics
- The choice depends on architectural preferences and requirements

## Conclusion

The two approaches are **logically equivalent** but **significantly different** in implementation:

- **Dynamic Binding**: Implicit, runtime-based solution injection
- **"One at a Time"**: Explicit, structural solution injection matching the document's formal definition

The `exists-once.lisp` implementation provides an alternative that more closely follows the document's mathematical formulation, explicitly passing solution mappings and joining them at the BGP level, while `exists.lisp` leverages the runtime's dynamic binding mechanism for a more implicit approach.

Both are valid implementations of DEEP INJECTION semantics for the EXISTS operator.

