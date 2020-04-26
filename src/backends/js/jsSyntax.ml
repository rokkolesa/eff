(** Syntax of the core language. *)
open CoreUtils
module CoreSyntax = UntypedSyntax

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
  (* A PROJECTION for a variable - projections denote the correct field path to take in the 'match' object *)
  | Projection of variable * projection list
  (* a LIST of terms - equivalent to list in JS, also handles tuples *)
  | List of js_term list
  (* RECORD representation - multiple fields, each with it's own name (string) and the actual term.. *)
  | Record of (field * js_term) list
  | Variant of label * js_term option
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
  (* RETURN is a construct known only in JS - it is implicit in Eff.. is always constructed as the last statement in a block *)
  | Value of js_term
  (* APPLY is function application.. it is very similar in JS to the one in Eff *)
  | Apply of js_term * js_term
  | Handle of js_term * js_term
  (* SEQUENCE denotes a sequence of terms *)
  | Sequence of js_term list
  (* COMMENTs are used for translating the terms which do not translate well to JS e.g. type checking *)
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

let print = Format.fprintf

let rec print_term t ppf = match t with
  | Var v -> print_variable v ppf
  | Const c -> Const.print c ppf
  | Projection (m, ps) -> print ppf "%t%t" (print_variable m) (Print.sequence "" print_projection ps) 
  | List ts -> print ppf "[%t]" (Print.sequence ", " print_term ts)
  | Record f_t_list -> print ppf "{%t}" (Print.sequence ", " print_record_term f_t_list)
  | Variant (lbl, t) -> print_variant lbl t ppf
  | Lambda (v, t) -> print ppf "function (%t) {%t}" (print_variable v) (print_term t)
  | Effect e -> print ppf "new Call (%t)" (print_effect e)
  | Handler h -> print ppf "Handler TODO..."
  | Let (v, t) -> print ppf "const %t = %t" (print_variable v) (print_term t)
  | Bind (t, (m, c)) -> print ppf "bind (%t, %t => {%t})" (print_term t) (print_variable m) (print_term c)
  | Match (t, x, ps_abs_list) -> print ppf "Match TODO..."
  | Value t -> print ppf "new Value (%t)" (print_term t)
  | Apply (t1, t2) -> print ppf "%t (%t)" (print_term t1) (print_term t2)
  | Handle (t1, t2) -> print ppf "eval (%t, %t)" (print_term t1) (print_term t2)
  | Sequence ts -> Print.sequence "; " print_term ts ppf
  | Comment s -> print ppf "/* %s */" s

  and print_variable v = CoreTypes.Variable.print ~safe:true v

  and print_field f = CoreTypes.Field.print ~safe:true f

  and print_effect e = CoreTypes.Effect.print ~safe:true e
  
  and print_record_term (f, t) ppf = print ppf "%t: %t" (print_field f) (print_term t)

  and print_projection p ppf = match p with
  | Int i -> print ppf "[%d]" i
  | Field f -> print ppf ".%t" (print_field f)
  | VariantProj -> print ppf ".arg"

  and print_variant lbl t_opt ppf = match t_opt with
  | None -> print ppf "{'name': %t, 'arg': null}" (CoreTypes.Label.print ~safe:true lbl)
  | Some t -> print ppf "{'name': %t, 'arg': %t}" (CoreTypes.Label.print ~safe:true lbl) (print_term t)

  and print_external name symbol_name translation ppf =
  match translation with
  | JsExternal.Unknown ->
      print ppf "throw \"Unknown external symbol %s.\"@."
        symbol_name
  | JsExternal.Exists t ->
      print ppf "const %t = %s;@." (print_variable name) t


let print_cmd cmd ppf = match cmd with
  | Term t -> print ppf "%t;@." (print_term t)
  | TopLet lst -> print ppf "%s@." "Top let TODO..."
  | External (x, f) -> (
    match Assoc.lookup f JsExternal.values with
    | None -> Error.runtime "Unknown external symbol %s." f
    | Some (JsExternal.Unknown as unknown) ->
        Print.warning
          ( "External symbol %s cannot be compiled. It has been replaced "
          ^^ "with [throw \"Unknown external symbol %s.\"]." )
          f f ;
        print_external x f unknown ppf
    | Some (JsExternal.Exists s as known) ->
        print_external x f known ppf )
