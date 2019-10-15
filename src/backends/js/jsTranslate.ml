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

let rec of_abstraction (p, c) = (of_pattern p, of_computation c)

and of_abstraction2 (p1, p2, c) =
  (of_pattern p1, of_pattern p2, of_computation c)

and field_to_label f = f

(** Conversion functions. *)
and of_expression {it; at} =
  match it with
  | CoreSyntax.Var v -> Var v
  | CoreSyntax.Const const -> Const const
  | CoreSyntax.Annotated (e, ty) -> of_expression e
  | CoreSyntax.Tuple es -> List (List.map of_expression es)
  | CoreSyntax.Record assoc -> JsSyntax.Obj (None, Assoc.to_list (Assoc.map of_expression assoc))
  | CoreSyntax.Variant (lbl, e_opt) -> (
    match e_opt with
    | None -> JsSyntax.Obj (Some lbl, [])
    (* TODO lbl as a field label is not good.. plus.. isn't e actually a list of expressions??*)
    | Some e -> JsSyntax.Obj (Some lbl, [(lbl, of_expression e)]))
  | CoreSyntax.Lambda abs -> (
    (* TODO do we really need to transform to function keyword? it's a lambda.. *)
    (* Transform back to [function] keyword if possible *)
    (* match abs with
    | p, {it= CoreSyntax.Match (e, abs_lst)} -> (
        let p' = of_pattern p in
        let e' = of_expression e in
        match (p', e') with
        | PVar v1, Var v2
          when v1 = v2
               && CoreTypes.Variable.fold (fun desc _ -> desc = "$function") v1
          ->
            let converter abs = Return (of_abstraction abs) in
            Function (List.map converter abs_lst)
        | _ -> Lambda (of_abstraction abs) ) *)
    (*| _ ->*) Lambda (of_abstraction abs) )
  | CoreSyntax.Effect eff -> Effect eff
  | CoreSyntax.Handler {effect_clauses; value_clause; finally_clause} ->
      (* Non-trivial case *)
      (* TODO do we really need to transform it to EffectClause and ValueClause? it actually seems to be the easiest choice *)
      let effect_clauses' =
        List.map
          (fun (eff, abs) -> EffectClause (eff, of_abstraction2 abs))
          (Assoc.to_list effect_clauses)
      in
      let value_clause' = ValueClause (of_abstraction value_clause) in
      let finally_clause_abs = of_abstraction finally_clause in
      let ghost_bind = CoreTypes.Variable.fresh "$c_thunk" in
      let match_handler =
        Match
          (Apply (Var ghost_bind, List []), value_clause' :: effect_clauses')
      in
      if abstraction_is_id finally_clause_abs then
        Lambda (PVar ghost_bind, match_handler)
      else
        Lambda
          (PVar ghost_bind, Apply (Lambda finally_clause_abs, match_handler))


and of_computation {it; at} = 
  match it with
  | CoreSyntax.Value e -> Return (of_expression e)
  | CoreSyntax.Let (p_c_lst, c) ->
      let folder (p, comp) t = Bind (t, (of_pattern p, of_computation comp)) in
      List.fold_right folder p_c_lst (of_computation c)
  | CoreSyntax.LetRec (var_abs_lst, c) ->
      let ghost_bind = CoreTypes.Variable.fresh "$c_thunk" in
      let converter (var, abs) = Let (var, Match (Apply(Var ghost_bind, List[]), [ValueClause (of_abstraction abs)])) in
      let sequential_lets = List.map converter var_abs_lst in
      Sequence (sequential_lets @ [of_computation c])
  | CoreSyntax.Match (e, abs_lst) ->
      let converter abs = ValueClause (of_abstraction abs) in
      Match (of_expression e, List.map converter abs_lst)
  | CoreSyntax.Apply (e1, e2) -> Apply (of_expression e1, of_expression e2)
  | CoreSyntax.Check c -> Comment "Check is not supported"
  | CoreSyntax.Handle (e, c) ->
      (* Non-trivial case *)
      let modified_handler = of_expression e in
      let thunked_c = Lambda (PNonbinding, of_computation c) in
      Apply (modified_handler, thunked_c)
  

and of_pattern {it; at} =
  match it with
  | CoreSyntax.PVar var -> PVar var
  | CoreSyntax.PAnnotated (p, ty) -> of_pattern p
  (* TODO with the [as] keyword, the whole value should be passed on.. pattern matching is used only to get into a special case and not to deconstruct the parameter *)
  | CoreSyntax.PAs (p, var) -> of_pattern p
  | CoreSyntax.PTuple ps -> PTuple (List.map of_pattern ps)
  | CoreSyntax.PRecord assoc -> PRecord (Assoc.map of_pattern assoc)
  | CoreSyntax.PVariant (lbl, p_opt) -> (
    match p_opt with
    | None -> PVariant (lbl, None)
    | Some p -> PVariant (lbl, Some (of_pattern p)) )
  | CoreSyntax.PConst const -> PConst const
  | CoreSyntax.PNonbinding -> PNonbinding
