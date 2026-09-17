# EXISTS Execution Modes as DEEP INJECTION Variants — Formal Semantics

**Date:** 2026-09-17

A lateral-join evaluator for SPARQL `EXISTS` (implemented in the Dydra SPOCQ engine) decides, per occurrence,
among three execution modes — **Substitute**, **Lateralize**, **Uncorrelated** — and proves each equal
to the reference correlated semantics. This note casts those modes in the notation of the W3C SPARQL
Working Group's *Defining the DEEP INJECTION approach for EXISTS*, so the correspondence is explicit:
**Substitute** is the note's **ONCE** family, **Lateralize** is its **OVERALL** family specialised to the
produced join keys, and **Uncorrelated** is the shared degenerate case. The classification is a
**with-PROJECTION** deep injection — the correlated (issue-156) reading of scope — because the shared
variable set is bounded by projection, so a hidden sub-`SELECT` variable does not correlate.

Companion: `exists-lateral-join-plan.md` (design), `exists-lateral-join-summary.md` (curated summary).

## Notation

- `μ`, `μ'` — solution mappings; `μ₀` the empty mapping (`dom(μ₀) = ∅`); `Ω` a multiset of mappings.
- `μ ~ μ'` — *compatible*: `∀x ∈ dom(μ) ∩ dom(μ'), μ(x) = μ'(x)`.
- `μ|_X` — restriction of `μ` to `X`; `π_X(Ω)` — projection of the multiset onto `X`.
- `Ω ⋉ Ω' = { μ∈Ω : ∃μ'∈Ω', μ ~ μ' }` (semi-join); `Ω ▷ Ω' = { μ∈Ω : ¬∃μ'∈Ω', μ ~ μ' }` (anti-join).
- `eval(D(G), E, μ_ctx)` — ONCE deep injection (each BGP ↦ `Join(Ω_BGP, {μ_ctx})`).
- `eval(D(G), E, Ω_ctx)` — OVERALL deep injection (each BGP ↦ `Join(Ω_BGP, Ω_ctx)`).
- `eval(D(G), E, {μ₀})` — uncorrelated evaluation (`Join(Ω_BGP, {μ₀}) = Ω_BGP`).
- For a pattern `P` with algebraic translation `E`: `proj(P)` its projected (exposed) variables,
  `match(P)` the variables it binds by matching, `free(P)` its free variables.
- `B` — the base (containing) field; `Ω_ctx = eval(D(G), B, {μ₀})` its solutions.

## Definition C — correspondence classification (with PROJECTION)

Given `B` and `EXISTS P`:

```
S  = vars(B) ∩ (proj(P) ∪ free(P))     -- shared variables
S⁺ = vars(B) ∩ (proj(P) ∩ match(P))    -- join keys (produced and exposed)
S⁻ = S \ S⁺                            -- substitution parameters (free-only)
```

Side conditions on the join keys:

- `total(S⁺, B)` iff every `s ∈ S⁺` is bound in **every** `μ ∈ Ω_ctx` (the base never leaves a key
  unbound — e.g. no `OPTIONAL`-only key).
- `safe(P)` iff `P` carries no existence-altering modifier whose group key fails to cover `S⁺`
  (aggregation / `GROUP BY` / `ORDER…LIMIT` / `OFFSET`).

The "with PROJECTION" qualifier is precisely the use of `proj`/`match`: a variable a sub-`SELECT` binds
but does not project (`∈ match(inner) \ proj`) is not in `S`, so it neither injects nor joins — the
correlated reading of `EXISTS` scope. `freevars(P)` alone would be too narrow (it closes a sub-`SELECT`
scope, `free(inner) \ proj(inner)`), missing a projected inner variable that does correspond.

## Definition S — SUBSTITUTE  (≡ ONCE, with PROJECTION)

Applies when `S⁻ ≠ ∅`, or `¬total(S⁺, B)`, or `¬safe(P)`. For each candidate `μ ∈ Ω_ctx`:

```
Ω_μ          = eval(D(G), E, μ|_S)
exprS(μ,D,G) = 'true'^^xsd:boolean   if Ω_μ ≠ ∅
             = 'false'^^xsd:boolean   if Ω_μ = ∅
```

This is the note's ONCE definition, with the injected context restricted to the corresponding
variables `μ|_S`. Operationally: the per-row sub-select (Galindo-Legaria APPLY).

## Definition L — LATERALIZE  (≡ OVERALL, projected to `S⁺`)

Applies when `S⁻ = ∅`, `S⁺ ≠ ∅`, `total(S⁺, B)`, and `safe(P)`. Evaluate `P` once, decorrelated, and
semi-join:

```
Ω_P          = eval(D(G), E, {μ₀})        -- one evaluation, no per-row context
K            = distinct( π_{S⁺}(Ω_P) )    -- the key relation
exprL(μ,D,G) = 'true'   if ∃ μ'∈Ω_P with μ ~ μ'     ( equivalently  μ|_{S⁺} ∈ K )
             = 'false'  otherwise
```

Equivalently in the OVERALL form: `Ω = eval(D(G), E, Ω_ctx)`, then
`exprL(μ) = 'true' iff ∃ μ'∈Ω with μ ~ μ'`. The two coincide because, with `S⁻ = ∅`, compatibility `~`
reduces to agreement on `S⁺`, and injecting `Ω_ctx` only prunes `Ω_P` to keys some candidate already
carries — so it changes no per-`μ` verdict. The injection is the optimisation; the semi-join is the
observable.

## Definition U — UNCORRELATED  (degenerate ONCE = degenerate OVERALL)

Applies when `S = ∅`:

```
Ω_P          = eval(D(G), E, {μ₀})
exprU(μ,D,G) = 'true'  if Ω_P ≠ ∅
             = 'false' if Ω_P = ∅          -- constant in μ
```

Since `dom(μ|_S) = ∅ = dom(μ₀)`, we have `eval(D(G), E, μ|_S) = eval(D(G), E, {μ₀})`, so U is the common
specialisation of S and L at `S = ∅` (an `ASK` evaluated once for the whole field).

## Mode selection

```
mode(B, P) = LATERALIZE    if S⁺ ≠ ∅ ∧ S⁻ = ∅ ∧ total(S⁺, B) ∧ safe(P)
           = UNCORRELATED  if S = ∅
           = SUBSTITUTE    otherwise
```

## Equivalence to the reference semantics

Let `exprONCE(μ) = [ eval(D(G), E, μ) ≠ ∅ ]` be the reference (ONCE-per-candidate) `EXISTS` value.

- **Proposition S.** `exprS ≡ exprONCE`. The variables of `μ` that `eval` can inject into `E` are exactly
  `dom(μ) ∩ (proj(P) ∪ free(P)) = S`, so `eval(D(G), E, μ|_S) = eval(D(G), E, μ)`.

- **Proposition L.** If `S⁻ = ∅` and `total(S⁺, B)` then `exprL ≡ exprONCE`.
  *Lemma:* deep-injecting `μ` joins each BGP with `{μ}` on the shared produced variables `S⁺`; with
  `S⁻ = ∅` there is no free variable of `P` bound only through a `FILTER`/expression to constrain, and
  with `total` every `s ∈ S⁺` is bound in `μ`, hence
  `eval(D(G), E, μ) ≠ ∅ ⇔ ∃ μ'∈eval(D(G), E, {μ₀}) with μ'|_{S⁺} = μ|_{S⁺}`.
  Both side conditions are tight: dropping `S⁻ = ∅` breaks it (an inner `FILTER(?b > ?a)` on a free `?a`
  constrains the injected evaluation but not the global one); dropping `total` breaks it (an unbound
  `S⁺` key ranges freely under injection but matches nothing under a join).

- **Proposition U.** `exprU ≡ exprONCE` when `S = ∅`, since `μ|_∅ = μ₀` injects nothing.

Hence `mode(B, P)` always computes the reference `EXISTS` value; it only chooses the ONCE (Substitute),
OVERALL-projected (Lateralize), or degenerate (Uncorrelated) *realisation* by which is sound and cheapest.

## Boolean host (orthogonal to the mode)

`expr(μ)` above is the `EXISTS` truth value for one candidate; the surrounding boolean context wraps it,
and this choice is independent of the mode.

- `FILTER EXISTS P` — keep `μ` iff `expr(μ)`; for LATERALIZE this is the semi-join `Ω_ctx ⋉ K`.
- `FILTER NOT EXISTS P` — keep `μ` iff `¬expr(μ)`; for LATERALIZE the anti-join `Ω_ctx ▷ K`.
- `BIND(EXISTS P AS ?b)` or a compound (`&&` / `||` / `!`) — for LATERALIZE,
  `extend( Ω_ctx ⟕ (K extend ?f = true), ?b, bound(?f) )`, evaluating the original boolean over the
  fresh flag(s) `?f`. SUBSTITUTE and UNCORRELATED use `exprS` / `exprU(μ)` directly in the host.

## Relation to the W3C note

The fetched sections of *Defining the DEEP INJECTION approach for EXISTS* formalise the ONCE and OVERALL
evaluation families **without PROJECTION**. The modes here are the **with-PROJECTION** refinement: `S` is
bounded by `proj`/`match`, so a hidden sub-`SELECT` variable does not correlate. Under that refinement,
**SUBSTITUTE = ONCE**, **LATERALIZE = OVERALL** specialised to the produced keys `S⁺` (which is why it is
a sound optimisation, not a different semantics), and **UNCORRELATED** is their common `S = ∅`
degeneracy.
