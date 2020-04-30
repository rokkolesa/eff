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
  | Thunk of js_term
  (* EFFECT and HANDLER are new constructs, must be created entirely from scratch *)
  | Effect of effect
  | Handler of handler

  (* LET is uniform across JS and it does not differentiate between 'let' and 'let rec' *)
  | Let of variable * js_term
  | Bind of js_term * abstraction
  (* MATCH is acually expresssion * (pattern * term) list.. but this differentiation is already done in core.. no need for that here *)
  | Match of js_term * variable * (pattern_shape * abstraction) list
  (* RETURN is a construct known only in JS - it is implicit in Eff.. is always constructed as the last statement in a block *)
  | Return of js_term
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
  | TopLet of js_term list
  | External of (variable * string)

let print = Format.fprintf

let rec print_term term ppf = match term with
  | Var v -> print_variable v ppf
  | Const c -> Const.print c ppf
  | Projection (m, ps) -> print ppf "%t%t" (print_variable m) (Print.sequence "" print_projection ps) 
  | List ts -> print ppf "[%t]" (Print.sequence ", " print_term ts)
  | Record f_t_list -> print ppf "{%t}" (Print.sequence ", " print_record_term f_t_list)
  | Variant (lbl, t) -> print_variant lbl t ppf
  | Lambda a -> print ppf "%t" (print_abstraction a)
  | Thunk t -> print ppf "%t" (print_thunk t)
  | Effect e -> print ppf "(args => new Call ('%t', args))" (print_effect e)
  | Handler {effect_clauses; value_clause; finally_clause} -> print ppf "new Handler(%t, %t, %t);" (print_handler_clauses effect_clauses) (print_abstraction value_clause) (print_abstraction finally_clause) 
  | Let (v, t) -> print ppf "var %t = %t;" (print_variable v) (print_term t)
  | Bind (t, a) -> print ppf "bind (%t, %t)" (print_term t) (print_abstraction a)
  | Match (t, x, ps_abs_list) -> print ppf "Match TODO..."
  | Return t -> print ppf "return %t;" (print_term t)
  | Apply (t1, t2) -> print ppf "%t (%t)" (print_term t1) (print_term t2)
  | Handle (t1, t2) -> print ppf "eval (%t, %t)" (print_term t1) (print_term t2)
  | Sequence ts -> Print.sequence "; " print_term ts ppf
  | Comment s -> print ppf "/* %s */" s

  and print_variable v = CoreTypes.Variable.print ~safe:true v

  and print_field f = CoreTypes.Field.print ~safe:true f

  and print_effect e = CoreTypes.Effect.print e
  
  and print_record_term (f, t) ppf = print ppf "%t: %t" (print_field f) (print_term t)

  and print_thunk t ppf = match t with
    | Sequence _ -> print ppf "(() => {%t})()" (print_term t)
    | _ -> print ppf "(() => {return %t;})()" (print_term t)

  and print_abstraction (v, t) ppf = match t with
    | Sequence _ -> print ppf "%t => {%t}" (print_variable v) (print_term t)
    | _ -> print ppf "%t => {return %t;}" (print_variable v) (print_term t)

  and print_abstraction2 (v1, v2, t) ppf = match t with 
    | Sequence _ -> print ppf "(%t, %t) => {%t}" (print_variable v1) (print_variable v2) (print_term t)
    | _ -> print ppf "(%t, %t) => {return %t;}" (print_variable v1) (print_variable v2) (print_term t)

  and print_handler_clauses hcs ppf = 
    let print_handler_clause (effect, abs2) ppf = print ppf "new HandlerClause('%t', %t)" (print_effect effect) (print_abstraction2 abs2) in
    print ppf "[%t]" (Print.sequence ", " print_handler_clause hcs);

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
  | Term t -> print ppf "console.log(eval(%t, _js_tophandler));@." @@ print_term t
  | TopLet ts -> print ppf "%t@." @@ print_term (Sequence ts)
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
