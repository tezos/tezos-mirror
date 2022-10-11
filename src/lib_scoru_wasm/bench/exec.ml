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

open Test_scoru_wasm_test_helpers
open Tezos_scoru_wasm
open Wasm_pvm_state.Internal_state
open Pvm_instance

type phase = Decoding | Initialising | Linking | Evaluating | Padding
[@@deriving show {with_path = false}]

let run_loop f a =
  Lwt_list.fold_left_s
    f
    a
    [Decoding; Linking; Initialising; Evaluating; Padding]

(** Predicate defining the different phases of an execution *)
let should_continue phase (pvm_state : pvm_state) =
  let continue =
    match (phase, pvm_state.tick_state) with
    | Initialising, Init _ -> true
    | Linking, Link _ -> true
    | Decoding, Decode _ -> true
    | Evaluating, Eval _
      when Wasm.Internal_for_benchmark.eval_has_finished pvm_state.tick_state ->
        false
    | Evaluating, Eval _ -> true
    | Padding, Eval _ -> true
    | _, _ -> false
  in
  Lwt.return continue

let finish_top_level_call_on_state pvm_state =
  Wasm.Internal_for_benchmark.compute_step_many_pvm_state
    ~max_steps:Int64.max_int
    pvm_state

let execute_on_state phase state =
  Wasm.Internal_for_benchmark.compute_step_many_until_pvm_state
    ~max_steps:Int64.max_int
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

let set_input_step = Wasm_utils.set_input_step

let read_message name =
  let open Tezt.Base in
  let kernel_file =
    project_root // Filename.dirname __FILE__ // "messages" // name
  in
  read_file kernel_file

let initial_boot_sector_from_kernel ?(max_tick = 1000000000000L) kernel =
  let open Lwt_syntax in
  let+ tree = Wasm_utils.initial_tree ~max_tick ~from_binary:true kernel in
  tree
