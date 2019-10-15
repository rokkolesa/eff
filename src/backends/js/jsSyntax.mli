type variable = CoreTypes.Variable.t

type effect = CoreTypes.Effect.t

type label = CoreTypes.Label.t

type field = CoreTypes.Field.t

(** Patterns *)
type pattern_shape =
  | PVar of variable
  | PTuple of pattern_shape list
  | PRecord of (field, pattern_shape) Assoc.t
  | PVariant of label * pattern_shape option
  | PConst of Const.t
  | PNonbinding
  
type projection =
  | Int of int
  | Field of field

type js_term =
  | Var of variable
  | Const of Const.t
  | Projection of js_term * projection list
  (* a LIST of terms - equivalent to list in JS, also handles tuples *)
  | List of js_term list
  (* OBJECT representation - multiple fields, each with it's own name (string) and the actual term.. can also have a name.. also encapsulates VARIANT and RECORD (records have no names) *)
  | Obj of label option * (field * js_term) list
  (* LAMBDA is very similar to eff's lambda - takes a variable and a computation *)
  | Lambda of abstraction
  (* EFFECT and HANDLER are new constructs, must be created entirely from scratch *)
  | Effect of effect
  | Handler of handler

  (* LET is uniform across JS and it does not differentiate between 'let' and 'let rec' *)
  | Let of variable * js_term
  | Bind of js_term * abstraction
  (* MATCH is acually expresssion * (pattern * term) list.. but this differentiation is already done in core.. no need for that here *)
  | Match of js_term * match_case list
  (* RETURN is a construct known only in JS - it is implicit in Eff.. perhaps it originates in 'Value' *)
  | Return of js_term
  (* APPLY is function application.. it is very similar in JS to the one in Eff *)
  | Apply of js_term * js_term
  (* IF is a condition followed by a positive and negative term *)
  | If of js_term * js_term * js_term
  | Handle of js_term * js_term
  | Sequence of js_term list
  | Comment of string

and match_case =
  | ValueClause of abstraction
  | EffectClause of effect * abstraction2

and handler = 
  { effect_clauses: (effect * abstraction2) list
  ; value_clause: abstraction
  ; finally_clause: abstraction }

and abstraction = pattern_shape * js_term

and abstraction2 = pattern_shape * pattern_shape * js_term

type cmd =
  | Term of js_term
  (* | DefEffect of effect * (ty * ty)
  | TopLet of (pattern * term) list
  | TopLetRec of (variable * abstraction) list
  | External of (variable * Type.ty * string)
  | TyDef of (label * (CoreTypes.TyParam.t list * tydef)) list *)

val print_cmd : cmd -> Format.formatter -> unit
