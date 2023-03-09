(* This collects all the wasm function names that are used in the js_of_ocaml stubs.
 * We use this to make sure they are all properly exported in wasm.
 * See the test in the dune file. *)

let process_file f =
  let ic = open_in f in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  let re =
    Re.(
      compile
        (seq
           [
             str "wasm_call";
             rep space;
             str "(";
             rep space;
             str "'";
             group (rep1 wordc);
             rep space;
             str "'";
           ]))
  in
  let groups = Re.all re content in
  List.map (fun g -> Re.Group.get g 1) groups

let () =
  match Array.to_list Sys.argv with
  | [] | [_] -> exit 1
  | _ :: rest ->
      let call_wasm = List.concat_map process_file rest in
      let all = "_malloc" :: "_free" :: call_wasm in
      all |> List.sort_uniq (fun a b -> compare a b) |> List.iter print_endline
