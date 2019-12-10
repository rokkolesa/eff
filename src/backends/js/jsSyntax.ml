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
  | Match of js_term * (pattern_shape * abstraction) list
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
  (* | DefEffect of effect * (ty * ty) *)
  | TopLet of (variable * js_term) list
  (* | External of (variable * Type.ty * string) *)
  (* | TyDef of (label * (CoreTypes.TyParam.t list * tydef)) list *)

let print = Format.fprintf

let print_sequence (type a) =
  (* This trick is needed to make it strongly polymorphic.
      Thanks Jane Street Tech Blog. *)
  let rec sequence sep (pp : a -> Format.formatter -> unit) vs ppf =
    match vs with
    | [] -> ()
    | [v] -> pp v ppf
    | v :: vs -> print ppf ("%t" ^^ sep ^^ "%t") (pp v) (sequence sep pp vs)
  in
  sequence

let rec print_term t ppf = match t with
  | Var v -> print_variable v ppf
  | Const c -> Const.print c ppf
  | Projection (m, ps) -> print ppf "%t%t" (print_variable m) (print_sequence "" print_projection ps) 
  | List ts -> print ppf "[%t]" (print_sequence ", " print_term ts)
  | Record f_t_list -> print ppf "Record TODO..."
  | Variant (lbl, t) -> print_variant lbl t ppf
  | Lambda (v, t) -> print ppf "function (%t) {%t}" (print_variable v) (print_term t)
  | Effect e -> CoreTypes.Effect.print e ppf
  | Handler h -> print ppf "Handler TODO..."
  | Let (v, t) -> print ppf "const %t = %t" (print_variable v) (print_term t)
  | Bind (t, (m, c)) -> print ppf "Bind TODO..."
  | Match (t, ps_abs_list) -> print ppf "Match TODO..."
  | Return t -> print ppf "return %t" (print_term t)
  | Apply (t1, t2) -> print ppf "%t (%t)" (print_term t1) (print_term t2)
  | Handle (t1, t2) -> print ppf "handle (%t, %t)" (print_term t1) (print_term t2)
  | Sequence ts -> print_sequence "; " print_term ts ppf
  | Comment s -> print ppf "// %s" s

and print_variable v ppf = CoreTypes.Variable.print v ppf

and print_projection p ppf = match p with
  | Int i -> print ppf "[%d]" i
  | Field f -> print ppf ".%t" (CoreTypes.Field.print f)
  (* TODO is this correct? AFAIK variants are only necessary when choosing the correct branch... *)
  | VariantProj -> ()

and print_variant lbl t_opt ppf = match t_opt with
  | None -> print ppf "{'name': %t, 'args': []}" (CoreTypes.Label.print lbl)
  (* TODO is this correct? *)
  | Some t -> print ppf "{'name': %t, 'args': %t}" (CoreTypes.Label.print lbl) (print_term t)

let print_cmd cmd ppf = match cmd with
  | Term t -> print ppf "%t;@." (print_term t)
  | TopLet lst -> print ppf "%s@." "Top let TODO..."

