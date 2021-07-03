open CoreUtils
open JsSyntax
module CoreSyntax = UntypedSyntax

(* ------------------------------------------------------------------------ *)
(* Translations *)

(** Conversion functions. *)
let rec of_expression {it; _} = match it with
  | CoreSyntax.Var v -> Var v
  | CoreSyntax.Const const -> Const const
  | CoreSyntax.Annotated (e, _) -> of_expression e
  | CoreSyntax.Tuple es -> List (List.map of_expression es)
  | CoreSyntax.Record assoc -> Record (Assoc.to_list @@ Assoc.map of_expression assoc)
  | CoreSyntax.Variant (lbl, e_opt) -> 
    (
      match e_opt with
      | None -> JsSyntax.Variant (lbl, None)
      | Some e -> JsSyntax.Variant (lbl, Some (of_expression e))
    )
  | CoreSyntax.Lambda abs -> Lambda (of_abstraction abs)
  | CoreSyntax.Effect eff -> Effect eff
  | CoreSyntax.Handler 
      {effect_clauses; value_clause; finally_clause} ->
    let eff_clauses = Assoc.to_list @@
      Assoc.map of_abstraction2 effect_clauses in
    Handler {
      effect_clauses=eff_clauses;
      value_clause=of_abstraction value_clause;
      finally_clause=of_abstraction finally_clause
    }

and of_computation {it; _} = match it with
  | CoreSyntax.Value e -> of_expression e
  | CoreSyntax.Let (p_c_lst, c) ->
    let to_bind abs acc = 
      let (v, ts, t) = of_abstraction_generic abs in
      (
        match ts with
        | [] -> Bind (t, (v, acc))
        | _ -> Bind (t, (v, Sequence(ts @ [Return acc])))
      )
    in
    List.fold_right to_bind p_c_lst @@ of_computation c
  | CoreSyntax.LetRec (var_abs_lst, c) ->
    let wrap_with_lambda (var, abs) = Let (var, Lambda (of_abstraction abs)) in
    let sequential_lets = List.map wrap_with_lambda var_abs_lst in
    Thunk (Sequence (sequential_lets @ [Return (of_computation c)]))
  | CoreSyntax.Match (e, abs_lst) ->
    let _match = CoreTypes.Variable.fresh "$match" in
    let of_abstraction_with_shape ((p, _) as abs) = (shape_of p, of_abstraction abs ~_match:_match) in
    Thunk (Sequence (
        Let (_match, of_expression e) ::
        Match (List.map of_abstraction_with_shape abs_lst) ::
        []
      ))
  | CoreSyntax.Apply (e1, e2) -> Apply (of_expression e1, of_expression e2)
  | CoreSyntax.Check _ -> Comment "Check is not supported"
  | CoreSyntax.Handle (e, c) -> Handle (of_expression e, of_computation c)

and of_abstraction_generic ?(_match = CoreTypes.Variable.fresh "$match") (p, c) = 
  let bindings = bindings p in 
  let wrap_with_projection (var, pr_list) = Let (var, Projection (_match, pr_list)) in
  let terms = List.map wrap_with_projection bindings in
  (_match, terms, of_computation c)

and of_abstraction ?(_match = CoreTypes.Variable.fresh "$match") abs = 
  let (v, ts, t) = of_abstraction_generic abs ~_match:_match in
  (v, Sequence (ts @ [Return t]))

and of_abstraction2 (p1, p2, c) = 
  let bindings1 = bindings p1 in 
  let bindings2 = bindings p2 in 
  let _match1 = CoreTypes.Variable.fresh "$match" in
  let _match2 = CoreTypes.Variable.fresh "$match" in
  let wrap_with_projection m (var, pr_list) = Let (var, Projection (m, pr_list)) in
  let terms1 = List.map (wrap_with_projection _match1) bindings1 in
  let terms2 = List.map (wrap_with_projection _match2) bindings2 in
  (_match1, _match2, Sequence (terms1 @ terms2 @ [Return (of_computation c)]))

and shape_of {it; _} = match it with
  | CoreSyntax.PNonbinding -> PArbitrary
  | CoreSyntax.PConst const -> PConst const
  | CoreSyntax.PVar _ -> PArbitrary
  | CoreSyntax.PAnnotated (p, _) -> shape_of p
  | CoreSyntax.PAs (p, _) -> shape_of p
  | CoreSyntax.PTuple ps -> PTuple (List.map shape_of ps)
  | CoreSyntax.PRecord assoc -> PRecord (Assoc.map shape_of assoc)
  | CoreSyntax.PVariant (lbl, p_opt) -> 
    (
      match p_opt with
      | None -> PVariant (lbl, None)
      | Some p -> PVariant (lbl, Some (shape_of p))
    )

and bindings {it; _} = match it with
  | CoreSyntax.PNonbinding -> [] 
  (* constant is not bound to anything.. it's here only for choosing the correct branch *)
  | CoreSyntax.PConst _ -> []
  | CoreSyntax.PVar var -> [(var, [])]
  | CoreSyntax.PAnnotated (p, _) -> bindings p
  | CoreSyntax.PAs (p, var) -> (var, []) :: bindings p
  | CoreSyntax.PTuple ps -> 
    let rec proj_tuple_patt i p = 
      let add_proj (var, projs) = (var, (Int i) :: projs) in
      List.map add_proj @@ bindings p
    in
    List.mapi proj_tuple_patt ps |> List.flatten
  | CoreSyntax.PRecord assoc -> 
    let rec proj_record_patt (f, p) = 
      let add_proj (var, projs) = (var, (Field f) :: projs) in
      List.map add_proj @@ bindings p
    in
      List.map proj_record_patt (Assoc.to_list assoc) |> List.flatten
  | CoreSyntax.PVariant (_, p_opt) -> 
    (
      match p_opt with
      | None -> []
      | Some p -> 
        let add_proj (var, projs) = (var, VariantProj :: projs) in
        List.map add_proj @@ bindings p
    )