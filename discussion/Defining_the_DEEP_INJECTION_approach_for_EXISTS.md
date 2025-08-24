This document introduces an approach to extend the definitions in the SPARQL spec in order to capture the so-called [DEEP INJECTION variantions](https://lists.w3.org/Archives/Public/public-rdf-star-wg/2025Aug/0012.html) of possible evaluation semantics for EXISTS. The approach is first described for the ONCE variant of DEEP INJECTION and, thereafter, for the OVERALL variant.

Informally, the idea of DEEP INJECTION is to take the solution mappings for which an EXISTS expression in a FILTER or a BIND has to be evaluated and inject them into every group graph pattern inside the pattern of the EXISTS expression.

Then, the idea of the definition approach in this document is to pass around these (to-be-injected) solution mappings as an additional argument to the [eval](https://www.w3.org/TR/sparql12-query/#defn_eval) function.
 

# DEEP INJECTION and ONCE, without PROJECTION
In the ONCE variant, the EXISTS pattern is evaluated once per solution mapping to be injected.
To define this variant we may extend the signature of the [eval](https://www.w3.org/TR/sparql12-query/#defn_eval) function by adding such a solution mapping as another argument.

That is, we define _eval_(_D_(_G_), _A_, _μ_<sub>ctx</sub>) as the evaluation of an [algebraic query expression](https://www.w3.org/TR/sparql12-query/#defn_AlgebraicQueryExpression) _A_ with respect to a dataset _D_ having [active graph](https://www.w3.org/TR/sparql12-query/#defn_ActiveGraph) _G_ in correlation with solution mapping _μ_<sub>ctx</sub>.

The default for _μ_<sub>ctx</sub> would be the empty solution mapping _μ_<sub>0</sub> (for which _dom_(_μ_<sub>0</sub>) = ∅). The only case in which _μ_<sub>ctx</sub> may be some other solution mapping is when evaluating the graph pattern within an EXIST expression.

Given the extension of the signature of the eval function, it is necessary to adapt all the definitions in [Section 18.6.2 Evaluation Semantics](https://www.w3.org/TR/sparql12-query/#sparqlAlgebraEval).
However, the only cases for which we actually need to change something (other than extending the function signature in each definition) are the ones for basic graph patterns (BGPs), for Property Path Patterns, and for the [ToMultiset](https://www.w3.org/TR/sparql12-query/#defn_absToMultiset) operator;
that is, in all other definitions of [Section 18.6.2 Evaluation Semantics](https://www.w3.org/TR/sparql12-query/#sparqlAlgebraEval), _μ_<sub>ctx</sub> is ignored. As an example, the [definition of the evaluation of Filter](https://www.w3.org/TR/sparql12-query/#defn_evalFilter) would look as follows.

---------------
> #### Definition: Evaluation of Filter (Semantics: DEEP INJECTION and ONCE, without PROJECTION)
> eval( _D_(_G_), [Filter](https://www.w3.org/TR/sparql12-query/#defn_absFilter)(_F_, _P_), _μ_<sub>ctx</sub> ) = [Filter](https://www.w3.org/TR/sparql12-query/#defn_algFilter)( _F_, eval(_D_(_G_), _P_, _μ_<sub>ctx</sub>), _D_, _G_ )
---------------

Notice that the only change in the definition above (compared to the [current version](https://www.w3.org/TR/sparql12-query/#defn_evalFilter)) is the addition of _μ_<sub>ctx</sub> as an additional argument of the eval function.

As mentioned, the only two definitions of [Section 18.6.2 Evaluation Semantics](https://www.w3.org/TR/sparql12-query/#sparqlAlgebraEval) in which an actual change is required are [the one for BGPs](https://www.w3.org/TR/sparql12-query/#defn_evalBasicGraphPattern), [the one for Property Path Patterns](https://www.w3.org/TR/sparql12-query/#defn_evalPropertyPathPattern), and [the one for ToMultiset](https://www.w3.org/TR/sparql12-query/#defn_evalToMultiSet) (see below for an explanation why it is really only these two).
The changed version for BGPs would look as follows, and the ones for Property Path Patterns and for ToMultiset need to be changed in the same way (and are not shown here).

---------------
> #### Definition: Evaluation of a Basic Graph Pattern (Semantics: DEEP INJECTION and ONCE, without PROJECTION)
> _eval_( _D_(_G_), _BGP_, _μ_<sub>ctx</sub> ) = [Join](https://www.w3.org/TR/sparql12-query/#defn_algJoin)( _Ω_, _Ω'_ )
> 
> where _Ω_ is the multiset of solution mappings obtained by evaluating _BGP_ over _G_ (as defined in Section [18.4.1 SPARQL Basic Graph Pattern Matching](https://www.w3.org/TR/sparql12-query/#BGPsparql)) and _Ω'_ is the multiset consisting of exactly _μ_<sub>ctx</sub> with multiplicity 1.
---------------

Notice, if _μ_<sub>ctx</sub> is the empty solution mapping _μ_<sub>0</sub>, then it holds that [Join](https://www.w3.org/TR/sparql12-query/#defn_algJoin)( _Ω_, _Ω'_ ) = _Ω_ and, thus, _eval_( _D_(_G_), _BGP_, _μ_<sub>0</sub> ) is equivalent to [the current](https://www.w3.org/TR/sparql12-query/#defn_evalBasicGraphPattern) eval( _D_(_G_), _BGP_ ).

---------------
#### Why is a similar change not needed for the other cases?
For every group graph pattern within the pattern of an EXISTS expression, the solution mapping needs to be injected at the first operator obtained from translating the group graph pattern into an [algebraic query expression](https://www.w3.org/TR/sparql12-query/#defn_AlgebraicQueryExpression). Consequently, we need to discuss every possible form of an [algebraic query expression](https://www.w3.org/TR/sparql12-query/#defn_AlgebraicQueryExpression) that may be produced by translating a [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern) as per the translation algorithm in [Section 18.3.2.6 Translate Graph Patterns](https://www.w3.org/TR/sparql12-query/#sparqlTranslateGraphPatterns). To do so, let's look at every production of the SPARQL grammar that is covered by the [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern) production:

* **[SubSelect](https://www.w3.org/TR/sparql12-query/#rSubSelect) -**
  If the pattern of the EXISTS expression matches the [SubSelect](https://www.w3.org/TR/sparql12-query/#rSubSelect) production, then it is translated into an algebraic query expression of the form [ToMultiset](https://www.w3.org/TR/sparql12-query/#defn_absToMultiset)(_A_),
  where _A_ is the algebraic query expression obtained by translating the SELECT query represented by the [SubSelect](https://www.w3.org/TR/sparql12-query/#rSubSelect).
  As mentioned above, the [definition of the evaluation of ToMultiset](https://www.w3.org/TR/sparql12-query/#defn_evalToMultiSet) is one of the cases in which we need to apply the actual change (as illustrated above for BGPs).

* **[TriplesBlock](https://www.w3.org/TR/sparql12-query/#rTriplesBlock) -**
  If the pattern of the EXISTS expression matches the [TriplesBlock](https://www.w3.org/TR/sparql12-query/#rTriplesBlock) production, then it is translated into an algebraic query expression that is either
  * a BGP,
  * of the form [Path](https://www.w3.org/TR/sparql12-query/#defn_absPath)(_X_, _ppe_, _Y_), or
  * of the form [Join](https://www.w3.org/TR/sparql12-query/#defn_absJoin)(_A_<sub>1</sub>, _A_<sub>2</sub>) such that both _A_<sub>1</sub> and _A_<sub>2</sub> are also of one of these three forms.

  As mentioned above, the definitions of the [evaluation of BGPs](https://www.w3.org/TR/sparql12-query/#defn_evalBasicGraphPattern) and of the [evaluation of Property Path Patterns](https://www.w3.org/TR/sparql12-query/#defn_evalPropertyPathPattern) are cases in which we need to apply the actual change (as illustrated above for BGPs).
  For the Join case, we can assume by induction that the solution mapping is injected during the evaluation of both _A_<sub>1</sub> and _A_<sub>2</sub>;
  moreover, since the algebra operator [Join](https://www.w3.org/TR/sparql12-query/#defn_algJoin) does not drop any variables when merging input solution mappings, the injected solution mapping is still fully covered by every solution mapping that comes out of the Join.

* **[GroupOrUnionGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupOrUnionGraphPattern) -**
  [GroupOrUnionGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupOrUnionGraphPattern) is translated into an algebraic query expression of the form [Union](https://www.w3.org/TR/sparql12-query/#defn_absUnion)(_A_<sub>1</sub>, _A_<sub>2</sub>),
  where both _A_<sub>1</sub> and _A_<sub>2</sub> is an algebraic query expression obtained by translating a [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern).
  Therefore, we can assume by induction that the solution mapping is injected during the evaluation of both _A_<sub>1</sub> and _A_<sub>2</sub>.
  Since the algebra operator [Union](https://www.w3.org/TR/sparql12-query/#defn_algUnion) does not drop any variables when passing on input solution mappings, the injected solution mapping is still fully covered by every solution mapping that comes out of the Union.

* **[OptionalGraphPattern](https://www.w3.org/TR/sparql12-query/#rOptionalGraphPattern) -**
  [OptionalGraphPattern](https://www.w3.org/TR/sparql12-query/#rOptionalGraphPattern) is translated into an algebraic query expression of the form [LeftJoin](https://www.w3.org/TR/sparql12-query/#defn_absLeftJoin)(_A_<sub>1</sub>, _A_<sub>2</sub>, _F_),
  where _A_<sub>1</sub> is either an empty BGP (if the OptionalGraphPattern is the first graph pattern within the [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern)) or an algebraic query expression obtained by translating a string that matches one of the productions in this list here (i.e., any production covered by the [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern) production),
  and _A_<sub>2</sub> is an algebraic query expression obtained by translating a [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern).
  By induction, we can assume that the solution mapping is injected during the evaluation of both _A_<sub>1</sub> and _A_<sub>2</sub>. 
  Therefore, the expression _F_ is applied to solution mappings that fully cover the injected solution mapping,
  and the injected solution mapping is also still fully covered by every solution mapping that comes out of the LeftJoin.

* **[MinusGraphPattern](https://www.w3.org/TR/sparql12-query/#rMinusGraphPattern) -**
  The argument for this case is very similar to the previous one for OptionalGraphPattern.

* **[GraphGraphPattern](https://www.w3.org/TR/sparql12-query/#rGraphGraphPattern) -**
  [GraphGraphPattern](https://www.w3.org/TR/sparql12-query/#rGraphGraphPattern) is translated into an algebraic query expression of the form [Graph](https://www.w3.org/TR/sparql12-query/#defn_absGraph)(_x_, _A_),
  where _x_ is an IRI or a variable and
  _A_ is an algebraic query expression obtained by translating a [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern).
  By induction, we can assume that the solution mapping is injected during the evaluation of _A_.
  Then, for the case that _x_ is an IRI, the [evaluation of Graph](https://www.w3.org/TR/sparql12-query/#defn_evalGraph) does not change anything in any of the solution mappings obtained by the evaluation of _A_.
  For the case that _x_ is a variable, the [evaluation of Graph](https://www.w3.org/TR/sparql12-query/#defn_evalGraph) uses the algebra operators [Join](https://www.w3.org/TR/sparql12-query/#defn_algJoin) and [Union](https://www.w3.org/TR/sparql12-query/#defn_algUnion). Since none of these two operators drops any variables when merging or passing on input solution mappings, the injected solution mapping is still fully covered by every solution mapping that comes out of evaluating the Graph expression.

* **[ServiceGraphPattern](https://www.w3.org/TR/sparql12-query/#rServiceGraphPattern) -**
  TODO: This case also requires an actual change within [the definition of eval for a Service Pattern](https://www.w3.org/TR/sparql12-federated-query/#defn_evalService), which is not yet integrated in the document above. That change needs to be different from the change for BGPs and the likes, because the eval function on the SPARQL endpoint doesn't have access to the solution mapping for deep injection.
  Therefore, the idea for this definition would be to rewrite the graph pattern to be sent to the endpoint by adding a VALUES clause with the solution mapping into every group graph pattern recursively.

* **[Filter](https://www.w3.org/TR/sparql12-query/#rFilter) -**
  [Filter](https://www.w3.org/TR/sparql12-query/#rFilter) is translated into an algebraic query expression of the form [Filter](https://www.w3.org/TR/sparql12-query/#defn_absFilter)(_X_, _A_),
  where _X_ is an expression and
  _A_ is either an empty BGP (if the [Filter](https://www.w3.org/TR/sparql12-query/#rFilter) is the first element that matches within the [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern))
  or an algebraic query expression obtained by translating a string that matches one of the productions in this list here (i.e., any production covered by the [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern) production).
  By induction, we can assume that the solution mapping is injected during the evaluation of _A_.
  Therefore, the expression _X_ is applied to solution mappings that fully cover the injected solution mapping,
  and the injected solution mapping is also still fully covered by every solution mapping that comes out of the Filter.

* **[Bind](https://www.w3.org/TR/sparql12-query/#rBind) -**
  [Bind](https://www.w3.org/TR/sparql12-query/#rBind) is translated into an algebraic query expression of the form [Extend](https://www.w3.org/TR/sparql12-query/#defn_absExtend)(_A_, _var_, _expr_),
  where _A_ is either an empty BGP (if the [Bind](https://www.w3.org/TR/sparql12-query/#rBind) is the first element that matches within the [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern))
  or an algebraic query expression obtained by translating a string that matches one of the productions in this list here (i.e., any production covered by the [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern) production).
  By induction, we can assume that the solution mapping is injected during the evaluation of _A_.
  Therefore, the expression _expr_ is evaluated with respect to solution mappings that fully cover the injected solution mapping,
  and if the scoping rules for variables guarantee that the injected solution mapping does not have a binding for variable _var_ (!!!), then the injected solution mapping is also still fully covered by every solution mapping that comes out of the Bind.

* **[InlineData](https://www.w3.org/TR/sparql12-query/#rInlineData) -**
  TODO: This case still needs another change which is not yet integrated in the document above. Currently, the translation of [InlineData](https://www.w3.org/TR/sparql12-query/#rInlineData) in [Section 18.3.2.6 Translate Graph Patterns](https://www.w3.org/TR/sparql12-query/#sparqlTranslateGraphPatterns) simply outputs a multiset of solution mappings (which is a valid form of an [algebraic query expression](https://www.w3.org/TR/sparql12-query/#defn_AlgebraicQueryExpression)!).
  My proposal is to change this part of the translation and translate [InlineData](https://www.w3.org/TR/sparql12-query/#rInlineData) into an algebraic expression of the form [ToMultiset](https://www.w3.org/TR/sparql12-query/#defn_absToMultiset)(Ψ) where Ψ is the sequence (!!) of solution mappings represented by the VALUES clause.
  Then, since the [evaluation of ToMultiset](https://www.w3.org/TR/sparql12-query/#defn_evalToMultiSet) is one of the definitions for which we apply the actual change (as demonstrated for the BGPs case above), this change would now cover the case of [InlineData](https://www.w3.org/TR/sparql12-query/#rInlineData) as well (in addition to the [SubSelect](https://www.w3.org/TR/sparql12-query/#rSubSelect) case discussed at the beginning of this list).

---------------

Given these changes to the eval function, we may now define the evaluation of expressions that contain EXISTS. That is, we define _expr_(_μ_, _D_, _G_) for an expression  _expr_ that matches the [ExistsFunc](https://www.w3.org/TR/sparql12-query/#rExistsFunc) production of the SPARQL grammar, which means that _expr_ is of the form `EXISTS pattern` where `pattern` matches the [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern) production.

---------------
> #### _expr_(_μ_, _D_, _G_) for EXISTS (Semantics: DEEP INJECTION and ONCE, without PROJECTION)
> Assume that _expr_ is of the form `EXISTS pattern` where `pattern` matches the [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern) production.
>
> Let _E_ be the [algebraic query expression](https://www.w3.org/TR/sparql12-query/#defn_AlgebraicQueryExpression) obtained by translating `pattern` (as per [Section 18.3 Translation to the Algebraic Syntax](https://www.w3.org/TR/sparql12-query/#translation)).
> <br/>(Notice, this translation can be done only once, before the execution phase!)
>
> Then, expr(_μ_, _D_, _G_) is:
> * "true"^^xsd:boolean if _eval_( _D_(_G_), _E_, _μ_ ) is nonempty.
> * "false"^^xsd:boolean if _eval_( _D_(_G_), _E_, _μ_ ) is empty.
---------------

Notice that the definition passes the solution mapping _μ_ into the evaluation of the pattern inside EXISTS.


# DEEP INJECTION and OVERALL, without PROJECTION
To define DEEP INJECTION in combination with OVERALL, the definitions need to pass around a set of solution mappings _Ω_<sub>ctx</sub> rather than a single solution mapping _μ_<sub>ctx</sub>. Hence, the signature of the [eval](https://www.w3.org/TR/sparql12-query/#defn_eval) function changes to _eval_(_D_(_G_), _A_, _Ω_<sub>ctx</sub>).

The default for _Ω_<sub>ctx</sub> would be the singleton set _Ω_<sub>0</sub> that contains (only) the empty solution mapping _μ_<sub>0</sub>.

Like for ONCE, also for OVERALL, the extension of the signature of the eval function means that we have to adapt all definitions in [Section 18.6.2 Evaluation Semantics](https://www.w3.org/TR/sparql12-query/#sparqlAlgebraEval). Also in this case, the only ones for which actual changes are needed are [the one for BGPs](https://www.w3.org/TR/sparql12-query/#defn_evalBasicGraphPattern), [the one for Property Path Patterns](https://www.w3.org/TR/sparql12-query/#defn_evalPropertyPathPattern), and [the one for ToMultiset](https://www.w3.org/TR/sparql12-query/#defn_evalToMultiSet). The changed version for BGPs would look as follows, and the ones for Property Path Patterns and for ToMultiset need to be changed in the same way (and are not shown here).

---------------
> #### Definition: Evaluation of a Basic Graph Pattern (Semantics: DEEP INJECTION and OVERALL, without PROJECTION)
> eval( _D_(_G_), _BGP_, _Ω_<sub>ctx</sub> ) = [Join](https://www.w3.org/TR/sparql12-query/#defn_algJoin)( _Ω_, _Ω_<sub>ctx</sub> )
>
> where _Ω_ is the multiset of solution mappings obtained by evaluating _BGP_ over _G_ (as defined in Section [18.4.1 SPARQL Basic Graph Pattern Matching](https://www.w3.org/TR/sparql12-query/#BGPsparql)).
---------------

Notice, if _Ω_<sub>ctx</sub> is the singleton set _Ω_<sub>0</sub> containing the empty solution mapping _μ_<sub>0</sub>, then it holds that [Join](https://www.w3.org/TR/sparql12-query/#defn_algJoin)(_Ω_, _Ω_<sub>ctx</sub>) = _Ω_ and, thus, eval( _D_(_G_), _BGP_, _Ω_<sub>0</sub> ) is equivalent to [the current](https://www.w3.org/TR/sparql12-query/#defn_evalBasicGraphPattern) eval( _D_(_G_), _BGP_ ).

Now, let's define the OVERALL version of the evaluation of expressions that contain EXISTS.
For the ONCE variant, we did not need to change the notation _expr_(_μ_, _D_, _G_), which denotes the result of evaluating expression _expr_ with respect to solution mapping _μ_ and dataset _D_ with active graph _G_.
For OVERALL, however, we additionally need to consider _Ω_<sub>ctx</sub> when evaluating expressions and, thus, the notation for the result of evaluating an expression _expr_ needs to be extended to be: _expr_(_μ_, _D_, _G_, _Ω_<sub>ctx</sub>).

Before defining _expr_(_μ_, _D_, _G_, _Ω_<sub>ctx</sub>) for the case in which _expr_ is an EXISTS expression, let me emphasize that this extension means that we also have to adapt the definition of every [algebra operator](https://www.w3.org/TR/sparql12-query/#sparqlAlgebra) that involves evaluating expressions, which is the case for [Filter](https://www.w3.org/TR/sparql12-query/#defn_algFilter), [Diff](https://www.w3.org/TR/sparql12-query/#defn_algDiff), and [Extend](https://www.w3.org/TR/sparql12-query/#defn_algExtend). As an example, the definition of [Filter](https://www.w3.org/TR/sparql12-query/#defn_algFilter) would become:

---------------
> [Filter](https://www.w3.org/TR/sparql12-query/#defn_algFilter)( _F_, _Ω_, _D_, _G_ ) = { _μ_ in _Ω_ | _expr_(_μ_, _D_, _G_, _Ω_) is an expression that has an effective boolean value of true }
---------------

The difference to the [current definition of Filter](https://www.w3.org/TR/sparql12-query/#defn_algFilter) is that the given multiset _Ω_ of solution mappings is passed as _Ω_<sub>ctx</sub> into _expr_(_μ_, _D_, _G_, _Ω_<sub>ctx</sub>).

Now, let's see how _expr_(_μ_, _D_, _G_, _Ω_<sub>ctx</sub>) needs to be defined for an expression _expr_ of the form `EXISTS pattern`.

---------------
> #### _expr_(_μ_, _D_, _G_, _Ω_<sub>ctx</sub>) for EXISTS (Semantics: DEEP INJECTION and OVERALL, without PROJECTION)
> Assume that _expr_ is of the form `EXISTS pattern` where `pattern` matches the [GroupGraphPattern](https://www.w3.org/TR/sparql12-query/#rGroupGraphPattern) production.
>
> Let _E_ be the [algebraic query expression](https://www.w3.org/TR/sparql12-query/#defn_AlgebraicQueryExpression) obtained by translating `pattern` (as per [Section 18.3 Translation to the Algebraic Syntax](https://www.w3.org/TR/sparql12-query/#translation)).
> <br/>(Notice, this translation can be done only once, before the execution phase!)
>
> Let _Ω_ = _eval_(_D_(_G_), _E_, _Ω_<sub>ctx</sub>).
> <br/>(Notice that _Ω_ is independent of _μ_ and, thus, can be produced only once during the evaluation of a Filter with an expression that contains _expr_!!)
>
> Then, expr(_μ_, _D_, _G_, _Ω_<sub>ctx</sub>) is:
> * "true"^^xsd:boolean if there exists a solution mapping _μ'_ in _Ω_ such that _μ_ and _μ'_ are compatible.
> * "false"^^xsd:boolean otherwise.
---------------

Notice that an equivalent conditions for "true"^^xsd:boolean in the previous definition is to require that the [Join](https://www.w3.org/TR/sparql12-query/#defn_algJoin) between _Ω_ and the singleton set _Ω'_ = {_μ_} is nonempty.
