(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Wasm_utils

(* [parse_binary_module module_name module_stream] parses a binary encoded
   module. Parsing outside of the PVM allows locations in case of errors. *)
let parse_binary_module name module_ =
  let bytes = Tezos_lazy_containers.Chunked_byte_vector.of_string module_ in
  Tezos_webassembly_interpreter.Decode.decode ~name ~bytes

(* [typecheck_module module_ast] runs the typechecker on the module, which is
   not done by the PVM. *)
let typecheck_module module_ =
  Repl_helpers.trap_exn (fun () ->
      Tezos_webassembly_interpreter.Valid.check_module module_)

(* [import_pvm_host_functions ()] registers the host functions of the PVM. *)
let import_pvm_host_functions () =
  let lookup name = Lwt.return (Tezos_scoru_wasm.Host_funcs.lookup name) in
  Repl_helpers.trap_exn (fun () ->
      Lwt.return
        (Tezos_webassembly_interpreter.Import.register
           ~module_name:"rollup_safe_core"
           lookup))

(* [link module_ast] checks a module actually uses the host functions with their
   correct type, outside of the PVM. *)
let link module_ =
  Repl_helpers.trap_exn (fun () ->
      Tezos_webassembly_interpreter.Import.link module_)

(* Starting point of the module after reading the kernel file: parsing,
   typechecking and linking for safety before feeding kernel to the PVM, then
   installation into a tree for the PVM interpreter. *)
let handle_module binary name module_ =
  let open Lwt_result_syntax in
  let* ast =
    Repl_helpers.trap_exn (fun () ->
        if binary then parse_binary_module name module_
        else Lwt.return (parse_module module_))
  in
  let* () = typecheck_module ast in
  let* () = import_pvm_host_functions () in
  let* _ = link ast in
  let*! tree = initial_tree ~from_binary:binary module_ in
  let*! tree = eval_until_input_requested tree in
  return tree

let start binary file =
  let open Lwt_result_syntax in
  let module_name = Filename.(file |> basename |> chop_extension) in
  let*! buffer = Repl_helpers.read_file file in
  handle_module binary module_name buffer

(* REPL main loop: reads an input, does something out of it, then loops. *)
let repl tree inboxes level =
  let open Lwt_result_syntax in
  let rec loop tree inboxes level =
    let*! () = Lwt_io.printf "> " in
    let* input =
      Lwt.catch
        (fun () ->
          let*! i = Lwt_io.read_line Lwt_io.stdin in
          return_some i)
        (fun _ -> return_none)
    in
    match input with
    | Some command ->
        let* tree, inboxes, level =
          Commands.handle_command command tree inboxes level
        in
        loop tree inboxes level
    | None -> return tree
  in
  loop tree (List.to_seq inboxes) level

let file_parameter =
  Tezos_clic.parameter (fun _ filename ->
      Repl_helpers.(trap_exn (fun () -> read_file filename)))

let wasm_param =
  let open Lwt_result_syntax in
  Tezos_clic.(
    param
      ~name:"module"
      ~desc:"wasm or wast file"
      (parameter (fun _ filename -> return filename)))

let input_arg =
  let open Tezos_clic in
  arg ~doc:"input file" ~long:"inputs" ~placeholder:"inputs.json" file_parameter

let main_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~desc:"Start the eval loop"
    (args1 input_arg)
    (wasm_param @@ stop)
    (fun inputs wasm_file () ->
      let config = Config.config () in
      let*? binary =
        if Filename.check_suffix wasm_file ".wasm" then Ok true
        else if Filename.check_suffix wasm_file ".wast" then Ok false
        else error_with "Kernels should have .wasm or .wast file extension"
      in
      let* tree = start binary wasm_file in
      let* inboxes =
        match inputs with
        | Some inputs -> Messages.parse_inboxes inputs config
        | None -> return []
      in
      let+ _tree = repl tree inboxes 0l in
      ())

(* List of program commands *)
let commands = [main_command]

let () =
  ignore
    Tezos_clic.(
      setup_formatter
        Format.std_formatter
        (if Unix.isatty Unix.stdout then Ansi else Plain)
        Short) ;
  let args = Array.to_list Sys.argv |> List.tl |> Option.value ~default:[] in
  let result = Lwt_main.run (Tezos_clic.dispatch commands () args) in
  match result with
  | Ok _ -> ()
  | Error e ->
      Format.eprintf
        "%a\n%!"
        Tezos_clic.(
          fun ppf errs ->
            pp_cli_errors
              ppf
              ~executable_name:"octez-wasm-repl"
              ~global_options:no_options
              ~default:pp
              errs)
        e ;
      exit 1
