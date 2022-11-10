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
open Repl_helpers

(* Possible commands for the REPL. *)
type commands = Show_inbox | Load_inputs | Unknown of string | Stop

let parse_commands s =
  let command = String.split_no_empty ' ' (String.trim s) in
  match command with
  | ["show"; "inbox"] -> Show_inbox
  | ["load"; "inputs"] -> Load_inputs
  | ["stop"] -> Stop
  | _ -> Unknown s

let set_raw_message_input_step level counter encoded_message tree =
  Wasm.set_input_step (input_info level counter) encoded_message tree

(* [check_input_request tree] checks the PVM is waiting for inputs, and returns
   an hint to go to the next input request state otherwise. *)
let check_input_request tree =
  let open Lwt_syntax in
  let* info = Wasm.get_info tree in
  match info.Wasm_pvm_state.input_request with
  | Input_required -> return_ok_unit
  | No_input_required ->
      return_error
        "The PVM is not expecting inputs yet. You can `step inbox` to evaluate \
         the current inbox."
  | Reveal_required _ ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4208 *)
      return_error
        "The PVM is expecting a reveal. This command is not implemented yet."

(* [load_inputs_gen inboxes level tree] reads the next inbox from [inboxes], set the
   messages at [level] in the [tree], and returns the remaining inbox, the next
   level and the tree. *)
let load_inputs_gen inboxes level tree =
  let open Lwt_result_syntax in
  match Seq.uncons inboxes with
  | Some (inputs, inboxes) ->
      let* tree =
        trap_exn (fun () ->
            set_full_input_step_gen set_raw_message_input_step inputs level tree)
      in
      let*! () =
        Lwt_io.printf
          "Loaded %d inputs at level %ld\n%!"
          (List.length inputs)
          level
      in
      return (tree, inboxes, Int32.succ level)
  | None ->
      let*! () = Lwt_io.printf "No more inputs at level %ld\n%!" level in
      return (tree, inboxes, level)

let load_inputs inboxes level tree =
  let open Lwt_result_syntax in
  let*! status = check_input_request tree in
  match status with
  | Ok () -> load_inputs_gen inboxes level tree
  | Error msg ->
      Format.printf "%s\n%!" msg ;
      return (tree, inboxes, level)

(* [show_inbox tree] prints the current input buffer and the number of messages
   it contains. *)
let show_inbox tree =
  let open Lwt_syntax in
  let* input_buffer = Wasm.Internal_for_tests.get_input_buffer tree in
  let* messages =
    Tezos_lazy_containers.Lazy_vector.(
      Mutable.ZVector.snapshot
        input_buffer.Tezos_webassembly_interpreter.Input_buffer.content
      |> ZVector.to_list)
  in
  let messages_sorted =
    List.sort Messages.compare_input_buffer_messages messages
  in
  let pp_message ppf
      Tezos_webassembly_interpreter.Input_buffer.
        {raw_level; message_counter; payload} =
    Format.fprintf
      ppf
      {|{ raw_level: %ld;
  counter: %s
  payload: %a }|}
      raw_level
      (Z.to_string message_counter)
      Messages.pp_input
      payload
  in
  let pp_messages () =
    Format.asprintf
      "%a"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
         pp_message)
      messages_sorted
  in

  let size =
    input_buffer.Tezos_webassembly_interpreter.Input_buffer.num_elements
  in
  Lwt_io.printf
    "Inbox has %s messages:\n%s\n%!"
    (Z.to_string size)
    (pp_messages ())

(* [handle_command command tree inboxes level] dispatches the commands to their
   actual implementation. *)
let handle_command c tree inboxes level =
  let open Lwt_result_syntax in
  let command = parse_commands c in
  let return ?(tree = tree) () = return (tree, inboxes, level) in
  match command with
  | Load_inputs -> load_inputs inboxes level tree
  | Show_inbox ->
      let*! () = show_inbox tree in
      return ()
  | Unknown s ->
      let*! () = Lwt_io.eprintf "Unknown command `%s`\n%!" s in
      return ()
  | Stop -> return ()
