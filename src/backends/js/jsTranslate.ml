open CoreUtils
open JsSyntax
module CoreSyntax = UntypedSyntax

(* ------------------------------------------------------------------------ *)
(* Translations *)

let abstraction_is_id (p, c) =
  (* Used to remove trivial finally clauses from handlers. *)
  match p with
  | PVar v -> CoreTypes.Variable.fold (fun desc _ -> desc = "$id_par") v
  | _ -> false

(** Conversion functions. *)
let rec of_expression {it; at} =
  match it with
  | CoreSyntax.Var v -> Var v
  | CoreSyntax.Const const -> Const const
  | CoreSyntax.Annotated (e, ty) -> of_expression e
  | CoreSyntax.Tuple es -> List (List.map of_expression es)
  | CoreSyntax.Record assoc -> Record (Assoc.to_list (Assoc.map of_expression assoc))
  | CoreSyntax.Variant (lbl, e_opt) -> 
  (
      match e_opt with
      | None -> JsSyntax.Variant (lbl, None)
      | Some e -> JsSyntax.Variant (lbl, Some (of_expression e))
  )
  | CoreSyntax.Lambda abs -> Lambda (of_abstraction abs)
  | CoreSyntax.Effect eff -> Effect eff
  | CoreSyntax.Handler {effect_clauses; value_clause; finally_clause} ->
      let eff_clauses = List.map (fun (eff, abs2) -> (eff, of_abstraction2 abs2)) @@ Assoc.to_list effect_clauses in
      Handler {effect_clauses=eff_clauses; value_clause=of_abstraction value_clause; finally_clause=of_abstraction finally_clause}


and of_computation {it; at} = 
  match it with
  | CoreSyntax.Value e -> Return (of_expression e)
  | CoreSyntax.Let (p_c_lst, c) ->
      let folder abs t = Bind (t, of_abstraction abs) in
      List.fold_right folder p_c_lst (of_computation c)
  | CoreSyntax.LetRec (var_abs_lst, c) ->
      (* let converter (var, abs) = let (_, jsterm) = of_abstraction abs in Let (var, jsterm) in
      let sequential_lets = List.map converter var_abs_lst in
      Sequence (sequential_lets @ [of_computation c]) *)
      (* TODO this, or should we create a sequence.. I think this is the way to go.. *)
      let folder (var, abs) t = Bind (Let (var, t), of_abstraction abs) in
      List.fold_right folder var_abs_lst (of_computation c)
  | CoreSyntax.Match (e, abs_lst) ->
      let converter ((p, c) as abs) = (shape_of p, of_abstraction abs) in
      Match (of_expression e, List.map converter abs_lst)
  | CoreSyntax.Apply (e1, e2) -> Apply (of_expression e1, of_expression e2)
  | CoreSyntax.Check c -> Comment "Check is not supported"
  | CoreSyntax.Handle (e, c) -> Handle(of_expression e, of_computation c)

and of_abstraction (p, c) = 
  let bindings = bindings p in 
  let _match = CoreTypes.Variable.fresh "match" in
  let to_js_term (var, pr_list) = Projection (var, pr_list) in
  let terms = List.map to_js_term bindings in
  (_match, Sequence (terms @ [of_computation c]))

(* TODO do we really not need the _match variable?? *)
and of_abstraction2 (p1, p2, c) = 
  let bindings1 = bindings p1 in 
  let bindings2 = bindings p2 in 
  let to_js_term (var, pr_list) = Projection (var, pr_list) in
  let terms1 = List.map to_js_term bindings1 in
  let terms2 = List.map to_js_term bindings2 in
  Sequence (terms1 @ terms2 @ [of_computation c])

  (* 2 of_patterns.. one to create shape and one to create bindings *)
  (* let shp = shape_of p
  (shp, of_abstraction c)
  match (of_Expr e, map (fun (p, c) -> to zgoraj) vse_veje)  *)

and shape_of {it; at} =
  match it with
  | CoreSyntax.PNonbinding -> PNonbinding
  (* constant is not bound to anything.. it's here only for choosing the correct branch *)
  | CoreSyntax.PConst const -> PConst const
  | CoreSyntax.PVar var -> PVar var
  | CoreSyntax.PAnnotated (p, ty) -> shape_of p
  | CoreSyntax.PAs (p, var) -> shape_of p
  | CoreSyntax.PTuple ps -> PTuple (List.map shape_of ps)
  | CoreSyntax.PRecord assoc -> PRecord (Assoc.map shape_of assoc)
  | CoreSyntax.PVariant (lbl, p_opt) -> 
    (
      match p_opt with
      | None -> PVariant (lbl, None)
      | Some p -> PVariant (lbl, Some (shape_of p))
    )

and bindings {it; at} = 
  match it with
  | CoreSyntax.PNonbinding -> [] 
  (* constant is not bound to anything.. it's here only for choosing the correct branch *)
  | CoreSyntax.PConst const -> []
  | CoreSyntax.PVar var -> [(var, [])]
  | CoreSyntax.PAnnotated (p, ty) -> bindings p
  | CoreSyntax.PAs (p, var) -> (var, []) :: bindings p
  | CoreSyntax.PTuple ps -> 
      let rec proj_tuple_patt i p = 
        let add_proj (var, pr_list) = (var, (Int i) :: pr_list) in
        List.map add_proj @@ bindings p
      in
        List.mapi proj_tuple_patt ps |> List.flatten
  | CoreSyntax.PRecord assoc -> 
      let rec proj_record_patt (f, p) = 
        let add_proj (var, pr_list) = (var, (Field f) :: pr_list) in
        List.map add_proj @@ bindings p
      in
        List.map proj_record_patt @@ Assoc.to_list assoc |> List.flatten
  | CoreSyntax.PVariant (lbl, p_opt) -> 
    (
      match p_opt with
      | None -> []
      | Some p -> 
        (* TODO is this just `bindings p`? *)
        let add_proj (var, pr_list) = (var, (Field lbl) :: pr_list) in
        List.map add_proj @@ bindings p
    )