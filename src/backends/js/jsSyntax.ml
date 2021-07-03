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
  | Match of (pattern_shape * abstraction) list
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
  | TopLet of (variable * js_term list * js_term) list
  | TopLetRec of js_term list
  | External of (variable * string)

let print = Format.fprintf

let symbol_print desc n ppf = 
  let replaced = String.map (fun c -> if c == '\'' then '_' else c) desc in
  match replaced.[0] with
  | 'a' .. 'z' | '_' -> print ppf "%s_%d" replaced n
  | 'A' .. 'Z' -> print ppf "%s" replaced 
  | '$' -> print ppf "_var_%d" n
  | _ -> print ppf "_var_%d /* %s */" n desc

let rec indented_sequence sep pp vs ppf = match vs with
  | [] -> ()
  | [v] -> pp v ppf
  | v :: vs -> print ppf "@[<v 4>%t@]%s@,@[<v 4>%t@]" (pp v) sep (indented_sequence sep pp vs)

let rec print_term term ppf = match term with
  | Var v -> print ppf "%t" @@ print_variable v
  | Const c -> print ppf "%t" @@ print_constant c
  | Projection (m, ps) -> print ppf "%t" @@ print_projection_list m ps
  | List l -> print ppf "%t" @@ print_list l
  | Record r -> print ppf "%t" @@ print_record r
  | Variant (lbl, t) -> print ppf "%t" @@ print_variant lbl t
  | Lambda a -> print ppf "%t" @@ print_abstraction a
  | Thunk t -> print ppf "%t" @@ print_thunk t
  | Effect e -> print ppf "%t" @@ print_effect_call e
  | Handler h -> print ppf "%t" @@ print_handler h
  | Let (v, t) -> print ppf "%t" @@ print_let v t
  | Bind (t, a) -> print ppf "%t" @@ print_bind t a 
  | Match ms -> print ppf "%t" @@ print_match ms
  | Return t -> print ppf "%t" @@ print_return t
  | Apply (t1, t2) -> print ppf "%t" @@ print_apply t1 t2
  | Handle (t1, t2) -> print ppf "%t" @@ print_handle t1 t2
  | Sequence s -> print ppf "%t" @@ print_sequence s
  | Comment c -> print ppf "%t" @@ print_comment c

  and print_constant c = Const.print c

  and print_variable v = CoreTypes.Variable.fold symbol_print v

  and print_field f = CoreTypes.Field.fold symbol_print f

  and print_label lbl = CoreTypes.Label.fold symbol_print lbl

  and print_effect_label e = CoreTypes.Effect.fold symbol_print e

  and print_effect_call e ppf = print ppf "(args => new Call('%t', args))" @@ print_effect_label e

  and print_let v t ppf = print ppf "@[<v 4>const %t = %t@]" (print_variable v) (print_term t)

  and print_list l ppf = match l with
    | [] -> print ppf "[]"
    | _ -> print ppf "[@,%t@;<0 -4>]" @@ indented_sequence "," print_term l

  and print_record r ppf = print ppf "{@,%t@;<0 -4>}" @@ indented_sequence "," print_record_term r

  and print_record_term (f, t) ppf = print ppf "%t: %t" (print_field f) (print_term t)

  and print_variant lbl t_opt ppf = match t_opt with
    | None -> print ppf "{ 'name': '%t' }" (print_label lbl)
    | Some t -> print ppf "{@,'name': '%t',@,@[<v 4>'arg': %t@]@;<0 -4>}" (print_label lbl) (print_term t)

  and print_thunk t ppf = match t with
    | Sequence _ -> print ppf "(() => {@,@[<v 4>%t@]@;<0 -4>})()" @@ print_term t
    | _ -> print ppf "(() => %t@;<0 -4>)()" @@ print_term t

  and print_bind t a ppf = print ppf "bind(@,@[<v 4>%t@],@,@[<v 4>%t@]@;<0 -4>)" (print_term t) (print_abstraction a)

  and print_abstraction (v, t) ppf = match t with
    | Sequence _ -> print ppf "%t => {@,@[<v 4>%t@]@;<0 -4>}" (print_variable v) (print_term t)
    | _ -> print ppf "%t => %t" (print_variable v) (print_term t)

  and print_abstraction2 (v1, v2, t) ppf = match t with
    | Sequence _ -> print ppf "(%t, %t) => {@,@[<v 4>%t@]@;<0 -4>}" (print_variable v1) (print_variable v2) (print_term t)
    | _ -> print ppf "(%t, %t) => %t" (print_variable v1) (print_variable v2) (print_term t)

  and print_apply t1 t2 ppf = print ppf "%t(%t)" (print_term t1) (print_term t2)
  
  and print_handle t1 t2 ppf = print ppf "eval(@,@[<v 4>%t@],@,@[<v 4>%t@]@;<0 -4>)" (print_term t1) (print_term t2)
  
  and print_handler {effect_clauses; value_clause; finally_clause} ppf = print ppf "@[<v 4>new Handler(@,@[<v 4>%t@],@,@[<v 4>%t@],@,@[<v 4>%t@]@;<0 -4>)@]" (print_handler_clauses effect_clauses) (print_abstraction value_clause) (print_abstraction finally_clause)

  and print_handler_clauses hcs ppf = 
    let print_handler_clause (effect, abs2) ppf = print ppf "@[<v 4>new HandlerClause(@,'%t',@,@[<v 4>%t@]@;<0 -4>)@]" (print_effect_label effect) (print_abstraction2 abs2) in
    print ppf "[@,%t@;<0 -4>]" @@ Print.sequence "," print_handler_clause hcs

  and print_match ms ppf = print ppf "%t" @@ Print.sequence "" print_match_clause ms

  and print_match_clause (ps, (x, t)) ppf = match t with
    | Sequence _ -> print ppf "@[<v 4>if (%t.satisfies(%t)) {@,@[<v 4>%t@]@;<0 -4>}@]" (print_pattern_shape ps) (print_variable x) (print_term t)
    | _ -> print ppf "@[<v 4>if (%t.satisfies(%t)) {@,@[<v 4>return %t@]@;<0 -4>}@]" (print_pattern_shape ps) (print_variable x) (print_term t)

  and print_pattern_shape p ppf = match p with
    | PArbitrary -> print ppf "new ArbitraryPattern()"
    | PConst c -> print ppf "new ConstantPattern(%t)" (print_constant c)
    | PTuple ps -> print ppf "new TuplePattern([@,%t@;<0 -4>])" (indented_sequence "," print_pattern_shape ps)
    | PRecord pa -> print ppf "new RecordPattern({@,%t@;<0 -4>})" (indented_sequence "," print_record_pattern_shape @@ Assoc.to_list pa)
    | PVariant (lbl, ps) ->
      (
        match ps with
          | Some shp -> print ppf "new VariantPattern('%t', [@,@[<v 4>%t@]@;<0 -4>])" (print_label lbl) (print_pattern_shape shp)
          | None -> print ppf "new VariantPattern('%t')" (print_label lbl)
      )
  and print_record_pattern_shape (f, ps) ppf = print ppf "%t: %t" (print_field f) (print_pattern_shape ps)

  and print_projection_list m ps ppf = print ppf "@[<h>%t%t@]" (print_variable m) (Print.sequence "" print_projection ps)

  and print_projection b ppf = match b with
    | Int i -> print ppf "[%d]" i
    | Field f -> print ppf ".%t" @@ print_field f
    | VariantProj -> print ppf ".arg"

  and print_sequence s ppf = print ppf "@[<v>%t@]" @@ Print.sequence ";" print_term s

  and print_return r ppf = print ppf "@[<v 4>return %t@];" @@ print_term r

  and print_comment c ppf = print ppf "/* %s */" c

let rec print_cmd cmd ppf = match cmd with
  | Term t -> print ppf "@[<v 4>console.dir(@,@[<v 4>top_eval(@,@[<v 4>%t@]@;<0 -4>),@]@,{ depth: null }@]@;<0 -4>);@." @@ print_term t
  | TopLet ts -> print ppf "%t@." (Print.sequence ";" print_top_let ts)
  | TopLetRec ts -> print ppf "@[<v 4>%t@]@." @@ print_term (Sequence ts)
  | External (x, f) -> (
    match Assoc.lookup f JsExternal.values with
    | None -> Error.runtime "Unknown external symbol %s." f
    | Some (JsExternal.Unknown as unknown) ->
        Print.warning
          ( "External symbol %s cannot be compiled. It has been replaced "
          ^^ "with [throw \"Unknown external symbol %s.\"]." )
          f f ;
        print_external x f unknown ppf
    | Some (JsExternal.Exists _ as known) ->
        print_external x f known ppf )

  and print_external name symbol_name translation ppf = match translation with
  | JsExternal.Unknown ->
      print ppf "throw \"Unknown external symbol %s.\"@."
        symbol_name
  | JsExternal.Exists t ->
      print ppf "const %t = %s;@." (print_variable name) t
    
  and print_top_let (v, ts, t) ppf = 
    print ppf "@[<v 4>const %t = top_eval(@,@[<v 4>%t@]@;<0 -4>);@;<0 -4>@[<v 4>%t@];@]@."
      (print_variable v)
      (print_term t)
      (print_term (Sequence ts))