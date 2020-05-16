module CoreSyntax = UntypedSyntax

val of_computation : CoreSyntax.computation -> JsSyntax.js_term

val of_abstraction : ?_match:CoreTypes.Variable.t -> CoreSyntax.abstraction -> JsSyntax.variable * JsSyntax.js_term

val of_abstraction_generic: ?_match:CoreTypes.Variable.t -> CoreSyntax.abstraction -> JsSyntax.variable * JsSyntax.js_term list * JsSyntax.js_term