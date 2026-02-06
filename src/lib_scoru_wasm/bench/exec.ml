(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

open Tezos_scoru_wasm_helpers
open Tezos_scoru_wasm
open Wasm_pvm_state.Internal_state
open Pvm_instance

type phase = Decoding | Initialising | Linking | Evaluating | Padding
[@@deriving show {with_path = false}]

let run_loop_once f a =
  Lwt_list.fold_left_s
    f
    a
    [Decoding; Linking; Initialising; Evaluating; Padding]

(** do [f a] until [reboot] returns false *)
let rec do_while continue f a =
  let open Lwt_syntax in
  let* next = f a in
  let* should_reboot = continue next in
  if should_reboot then (do_while [@tailcall]) continue f next else return next

let run_loop ?(reboot = None) f a =
  match reboot with
  | None -> run_loop_once f a
  | Some need_reboot -> do_while need_reboot (run_loop_once f) a

(** Predicate defining the different phases of an execution *)
let should_continue phase (pvm_state : pvm_state) =
  let continue =
    match (phase, pvm_state.tick_state) with
    | Decoding, Snapshot -> true
    | Initialising, Init _ -> true
    | Linking, Link _ -> true
    | Decoding, Decode _ -> true
    | Evaluating, Eval _ when Wasm_vm.eval_has_finished pvm_state.tick_state ->
        false
    | Evaluating, Eval _ -> true
    | Padding, Padding -> true
    | _, _ -> false
  in
  Lwt.return continue

let finish_top_level_call_on_state pvm_state =
  Wasm_vm.compute_step_many
    ~wasm_entrypoint:Constants.wasm_entrypoint
    ~max_steps:Int64.max_int
    ~write_debug:Noop
    pvm_state

let execute_fast ~reveal_builtins pvm_state =
  Wasm_fast_vm.compute_step_many
    ~wasm_entrypoint:Constants.wasm_entrypoint
    ~reveal_builtins
    ~max_steps:Int64.max_int
    ~write_debug:Noop
    pvm_state

let execute_on_state ~reveal_builtins phase state =
  match state.tick_state with
  | Stuck _ -> Lwt.return (state, 0L)
  | _ ->
      Wasm_vm.compute_step_many_until
        ~wasm_entrypoint:Constants.wasm_entrypoint
        ~reveal_builtins
        ~max_steps:Int64.max_int
        ~write_debug:Wasm_utils.write_debug_on_stdout
        (should_continue phase)
        state

let run kernel k =
  let open Lwt_syntax in
  let* res =
    Lwt_io.with_file ~mode:Lwt_io.Input kernel (fun channel ->
        let* kernel = Lwt_io.read channel in
        k kernel)
  in
  return res

let read_message name =
  let open Tezt.Base in
  let kernel_file =
    project_root // Filename.dirname __FILE__ // "messages" // name
  in
  read_file kernel_file

let initial_boot_sector_from_kernel ?(max_tick = 1000000000000L) kernel =
  Wasm_utils.initial_state
    ~version:V1
    ~ticks_per_snapshot:max_tick
    ~from_binary:true
    kernel

type input = File of string | Str of string

let read_input = function Str s -> s | File s -> read_message s

type message = Transfer of input | Other of input | Encoded of input

let encode_message = function
  | Transfer s ->
      Pvm_input_kind.(
        Internal_for_tests.to_binary_input
          (Internal Transfer)
          (Some (read_input s)))
  | Other s ->
      Pvm_input_kind.(
        Internal_for_tests.to_binary_input External (Some (read_input s)))
  | Encoded s -> read_input s

let set_internal_message level counter message state =
  let encoded_message = encode_message message in
  Wasm_utils.Wasm.set_input_step
    (Wasm_utils.input_info level counter)
    encoded_message
    state

let load_messages messages level tree =
  let open Lwt_syntax in
  (* Uses the test utilities as a reference. *)
  let* tree =
    Wasm_utils.set_inputs_step set_internal_message messages level tree
  in
  (* this should only advance the tick counter after executing the input steps
     until the next snapshot is reached. *)
  Wasm_utils.eval_to_snapshot ~max_steps:Int64.max_int ~write_debug:Noop tree
