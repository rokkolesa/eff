module CoreSyntax = UntypedSyntax

val of_computation : CoreSyntax.computation -> JsSyntax.js_term

val of_pattern : CoreSyntax.pattern -> JsSyntax.pattern_shape
