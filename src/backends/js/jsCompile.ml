(* Evaluation of the intermediate language, big step. *)
open CoreUtils

module type BackendParameters = sig
  val output_file : string
end

module Backend (P : BackendParameters) : BackendSignature.T = struct
  (* ------------------------------------------------------------------------ *)
  (* Setup *)

  type state = {prog: JsSyntax.cmd list}

  let initial_state = {prog= []}

  (* Auxiliary functions *)
  let update state cmd =
    Print.debug "%t@?" (JsSyntax.print_cmd cmd) ;
    {prog= state.prog @ [cmd]}

  (* ------------------------------------------------------------------------ *)
  (* Processing functions *)
  let process_computation state c ty = 
    let t = JsTranslate.of_computation c in
    update state (Term t)

  let process_type_of state c ty = state

  let process_def_effect state (eff, (ty1, ty2)) = state

  let process_top_let state defs vars = state

  let process_top_let_rec state defs vars = state

  let process_external state (x, ty, f) = state

  let process_tydef state tydefs = state

  let finalize state =
    let channel = open_out P.output_file in
    let output_ppf = Format.formatter_of_out_channel channel in
    List.iter (fun cmd -> JsSyntax.print_cmd cmd output_ppf) state.prog ;
    close_out channel
end
