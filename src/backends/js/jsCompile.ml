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
  let process_computation state c _ = 
    let t = JsTranslate.of_computation c in
    update state @@ Term t

  let process_type_of state c _ = 
    Print.warning
      "[#typeof] commands are ignored when compiling to JavaScript." ;
    state

  let process_def_effect state (eff, _) = 
    Print.warning
      "[effect] commands are ignored when compiling to JavaScript." ;
    state

  let process_top_let state defs vars = 
    let top_let_terms = List.map JsTranslate.of_abstraction_generic defs in
    update state @@ TopLet top_let_terms

  let process_top_let_rec state defs vars = 
    let wrap_with_lambda (var, abs) = JsSyntax.Let (var, Lambda (JsTranslate.of_abstraction abs)) in
    let sequential_lets = List.map wrap_with_lambda @@ Assoc.to_list defs in
    update state @@ TopLetRec sequential_lets

  let process_external state (x, _, f) = 
    update state @@ External (x, f)

  let process_tydef state tydefs = 
    Print.warning
      "[tydef] commands are ignored when compiling to JavaScript." ;
    state

  let finalize state =
    let channel = open_out P.output_file in
    let output_ppf = Format.formatter_of_out_channel channel in
    let pChannel = open_in (Filename.concat Local.effdir "jsPervasives.js") in 
    let pFileContent = really_input_string pChannel (in_channel_length pChannel) in
    close_in pChannel ;
    Format.fprintf output_ppf "%s@." pFileContent ;
    List.iter (fun cmd -> JsSyntax.print_cmd cmd output_ppf) state.prog ;
    close_out channel
end
