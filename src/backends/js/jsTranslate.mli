module CoreSyntax = UntypedSyntax

val of_computation : CoreSyntax.computation -> JsSyntax.js_term

val of_abstraction : CoreSyntax.abstraction -> JsSyntax.variable * JsSyntax.js_term

val of_abstraction_top : CoreSyntax.abstraction -> JsSyntax.js_term
