(* Evaluation of the intermediate language, big step. *)
open CoreUtils

module type BackendParameters = sig
  val output_file : string
end

module Backend (P : BackendParameters) : BackendSignature.T = struct
  (* ------------------------------------------------------------------------ *)
  (* Setup *)

  type state = {prog: JsSyntax.cmd list}

  let initial_state = {prog = []}

  (* Auxiliary functions *)
  let update state cmd =
    Print.debug "%t@?" (JsSyntax.print_cmd cmd) ;
    {prog = state.prog @ [cmd]}

  (* ------------------------------------------------------------------------ *)
  (* Processing functions *)
  let process_computation state c ty = 
    let t = JsTranslate.of_computation c in
    update state (Term t)

  let process_type_of state c ty = 
    Print.warning
      "[#typeof] commands are ignored when compiling to JavaScript." ;
    state

  let process_def_effect state (eff, (_, _)) = 
    update state @@ Term (Effect eff)

  let process_top_let state defs vars = 
    (* TODO do we need to make some variables up? or just use the match variable to get the correct bindings.. probably this.. *)
    let of_abs abs = JsTranslate.of_abstraction abs in
    let top_let = List.map of_abs defs in
    update state @@ TopLet top_let

  let process_top_let_rec state defs vars = 
    let wrap_with_lambda (var, abs) = JsSyntax.Let (var, Lambda (JsTranslate.of_abstraction abs)) in
    let sequential_lets = List.map wrap_with_lambda @@ Assoc.to_list defs in
    update state @@ Term (Sequence sequential_lets)

  let process_external state (x, ty, f) = 
    Print.warning
      "[external] commands are ignored when compiling to JavaScript." ;
    state

  let process_tydef state tydefs = 
    Print.warning
      "[tydef] commands are ignored when compiling to JavaScript." ;
    state

  let finalize state =
    let channel = open_out P.output_file in
    let output_ppf = Format.formatter_of_out_channel channel in
    List.iter (fun cmd -> JsSyntax.print_cmd cmd output_ppf) state.prog ;
    close_out channel
end
