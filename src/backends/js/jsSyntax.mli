type variable = CoreTypes.Variable.t

type effect = CoreTypes.Effect.t

type label = CoreTypes.Label.t

type field = CoreTypes.Field.t

(** Patterns *)
type pattern_shape =
  | PArbitrary
  | PTuple of pattern_shape list
  | PRecord of (field, pattern_shape) Assoc.t
  | PVariant of label * pattern_shape option
  | PConst of Const.t
  
type projection =
  | Int of int
  | Field of field
  | VariantProj

type js_term =
  | Var of variable
  | Const of Const.t
  (* | Null *)
  | Projection of variable * projection list
  (* a LIST of terms - equivalent to list in JS, also handles tuples *)
  | List of js_term list
  (* OBJECT representation - multiple fields, each with it's own name (string) and the actual term.. can also have a name.. also encapsulates VARIANT and RECORD (records have no names) *)
  | Record of (field * js_term) list
  | Variant of label *  js_term option
  (* LAMBDA is very similar to eff's lambda - takes a variable and a computation *)
  | Lambda of abstraction
  (* EFFECT and HANDLER are new constructs, must be created entirely from scratch *)
  | Effect of effect
  | Handler of handler

  (* LET is uniform across JS and it does not differentiate between 'let' and 'let rec' *)
  | Let of variable * js_term
  | Bind of js_term * abstraction
  (* MATCH is acually expresssion * (pattern * term) list.. but this differentiation is already done in core.. no need for that here *)
  | Match of js_term * variable * (pattern_shape * abstraction) list
  (* RETURN is a construct known only in JS - it is implicit in Eff.. perhaps it originates in 'Value' *)
  | Value of js_term
  (* APPLY is function application.. it is very similar in JS to the one in Eff *)
  | Apply of js_term * js_term
  (* IF is a condition followed by a positive and negative term *)
  (* | If of js_term * js_term * js_term *)
  | Handle of js_term * js_term
  | Sequence of js_term list
  | Comment of string

and handler = 
  { effect_clauses: (effect * abstraction2) list
  ; value_clause: abstraction
  ; finally_clause: abstraction }

and abstraction = variable * js_term

and abstraction2 = variable * variable * js_term

type cmd =
  | Term of js_term
  (* | DefEffect of effect * (ty * ty) *)
  | TopLet of (variable * js_term) list
  | External of (variable * string)
  (* | TyDef of (label * (CoreTypes.TyParam.t list * tydef)) list *)

val print_cmd : cmd -> Format.formatter -> unit
