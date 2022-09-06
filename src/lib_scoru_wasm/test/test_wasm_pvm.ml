(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** Testing
    -------
    Component:    Tree_encoding_decoding
    Invocation:   dune exec  src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "^WASM PVM$"
    Subject:      WASM PVM evaluation tests for the tezos-scoru-wasm library
*)

open Tztest
open Tezos_scoru_wasm
module Wasm = Wasm_pvm.Make (Test_encodings_util.Tree)

(* Kernel failing at `kernel_next` invocation. *)
let unreachable_kernel = "unreachable"

(* Kernel writing `"hello"` to debug output. *)
let test_write_debug_kernel = "test-write-debug"

(* Kernel checking the return of the store_has host func.

   This kernel expects a collection of values to exist:
   - `/durable/hi/bye`
   - `/durable/hello`
   - `/durable/hello/universe`
   and asserts that `store_has` returns the correct type for each.
*)
let test_store_has_kernel = "test-store-has"

(** [check_error kind reason error] checks a Wasm PVM error [error] is of a
    given [kind] with a possible [reason].

    - If [kind] is [None], returns true.

    - If [reason] is [None], it simply check the given kind, otherwise it
    actually check the reason in the error. *)
let check_error expected_kind expected_reason error =
  let check_reason actual_reason =
    match expected_reason with
    | None -> true
    | _ -> expected_reason = actual_reason
  in
  match (expected_kind, error) with
  | Some `Decode, Wasm_pvm_errors.Decode_error {explanation; _} ->
      check_reason explanation
  | Some `Init, Init_error {explanation; _} -> check_reason explanation
  | Some `Link, Link_error explanation -> check_reason (Some explanation)
  | Some `Eval, Eval_error {explanation; _} -> check_reason explanation
  | Some `Invalid_state, Invalid_state explanation ->
      check_reason (Some explanation)
  (* Unknown_error encapsulate a raw exception produced by `Printexc.to_string`.
     It depends on the backend, if there are registered printers or not, it is
     not safe to rely on its string representation. *)
  | Some `Unknown, Unknown_error _ -> true
  (* The expected step doesn't corresponds to the actual stuck step. *)
  | Some _, _ -> false
  (* No check to do, we simply assume the PVM is in a stuck state. *)
  | None, _ -> true

let is_stuck ?step ?reason = function
  | Wasm_pvm.Stuck err -> check_error step reason err
  | _ -> false

let initial_boot_sector_from_kernel kernel =
  let open Lwt_syntax in
  let* empty_tree = Test_encodings_util.empty_tree () in
  let origination_message =
    Data_encoding.Binary.to_string_exn
      Gather_floppies.origination_message_encoding
    @@ Gather_floppies.Complete_kernel (String.to_bytes kernel)
  in
  Wasm.Internal_for_tests.initial_tree_from_boot_sector
    ~empty_tree
    origination_message

let rec eval_until_input_requested ?(max_steps = Int64.max_int) tree =
  let open Lwt_syntax in
  let* info = Wasm.get_info tree in
  match info.input_request with
  | No_input_required ->
      let* tree = Wasm.Internal_for_tests.compute_step_many ~max_steps tree in
      eval_until_input_requested ~max_steps tree
  | Input_required -> return tree

let set_input_step message message_counter tree =
  let input_info =
    Wasm_pvm_sig.
      {
        inbox_level =
          Option.value_f ~default:(fun () -> assert false)
          @@ Tezos_base.Bounded.Non_negative_int32.of_value 0l;
        message_counter = Z.of_int message_counter;
      }
  in
  Wasm.set_input_step input_info message tree

let should_boot_unreachable_kernel ~max_steps kernel =
  let open Lwt_syntax in
  let* tree = initial_boot_sector_from_kernel kernel in
  (* Make the first ticks of the WASM PVM (parsing of origination
     message, parsing and init of the kernel), to switch it to
     “Input_requested” mode. *)
  let* tree = eval_until_input_requested ~max_steps tree in
  (* Feeding it with one input *)
  let* tree = set_input_step "test" 0 tree in
  (* running until waiting for input *)
  let* tree = eval_until_input_requested ~max_steps tree in
  let* info_after_first_message = Wasm.get_info tree in
  let* state_after_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The kernel is expected to fail, then ths PVM should be in stuck state, and
     have failed during the evaluation when evaluating a `Unreachable`
     instruction. *)
  assert (
    is_stuck
      ~step:`Eval
      ~reason:"unreachable executed"
      state_after_first_message) ;

  (* Feeding it with one input *)
  let* tree = set_input_step "test" 1 tree in
  (* running until waiting for input *)
  let* tree = eval_until_input_requested tree in
  let* info_after_second_message = Wasm.get_info tree in
  let* state_after_second_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The PVM should still be in `Stuck` state, but can still receive inputs and
     go forward, hence the tick after the second message should be greater. *)
  assert (
    Z.lt
      info_after_first_message.current_tick
      info_after_second_message.current_tick) ;
  assert (
    is_stuck
      ~step:`Eval
      ~reason:"unreachable executed"
      state_after_second_message) ;
  return_unit

let should_run_debug_kernel kernel =
  let open Lwt_syntax in
  let* tree = initial_boot_sector_from_kernel kernel in
  (* Make the first ticks of the WASM PVM (parsing of origination
     message, parsing and init of the kernel), to switch it to
     “Input_requested” mode. *)
  let* tree = eval_until_input_requested tree in
  (* Feeding it with one input *)
  let* tree = set_input_step "test" 0 tree in
  (* running until waiting for input *)
  let* tree = eval_until_input_requested tree in
  let+ state_after_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The kernel should not fail. *)
  assert (not @@ is_stuck state_after_first_message)

let should_run_store_has_kernel kernel =
  let open Lwt_syntax in
  let* tree = initial_boot_sector_from_kernel kernel in
  let add_value tree key_steps =
    let open Lazy_containers in
    let open Test_encodings_util in
    let value = Chunked_byte_vector.of_string "a very long value" in
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ("durable" :: List.append key_steps ["_"])
         Tree_encoding.chunked_byte_vector)
      value
      tree
  in
  let* tree = add_value tree ["hi"; "bye"] in
  let* tree = add_value tree ["hello"] in
  let* tree = add_value tree ["hello"; "universe"] in
  (* Make the first ticks of the WASM PVM (parsing of origination
     message, parsing and init of the kernel), to switch it to
     “Input_requested” mode. *)
  let* tree = eval_until_input_requested tree in
  let* state_before_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The kernel is not expected to fail, the PVM should not be in stuck state. *)
  assert (not @@ is_stuck state_before_first_message) ;
  (* We now delete the path ["hello"; "universe"] - this will cause the kernel
     assertion on this path to fail, and the PVM should become stuck. *)
  let* tree = set_input_step "test" 0 tree in
  let* tree =
    Test_encodings_util.Tree.remove tree ["durable"; "hello"; "universe"; "_"]
  in
  let* tree = eval_until_input_requested tree in
  let+ state_after_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The kernel is now expected to fail, the PVM should be in stuck state. *)
  assert (is_stuck state_after_first_message)

let test_with_kernel kernel test () =
  let open Lwt_result_syntax in
  let open Tezt.Base in
  (* Reading files using `Tezt_lib` can be fragile and not future-proof, see
     issue https://gitlab.com/tezos/tezos/-/issues/3746. *)
  let kernel_file =
    project_root // Filename.dirname __FILE__ // "wasm_kernels"
    // (kernel ^ ".wasm")
  in
  let*! () =
    Lwt_io.with_file ~mode:Lwt_io.Input kernel_file (fun channel ->
        let*! kernel = Lwt_io.read channel in
        test kernel)
  in
  return_unit

let tests =
  [
    tztest
      "Test unreachable kernel (tick per tick)"
      `Quick
      (test_with_kernel
         unreachable_kernel
         (should_boot_unreachable_kernel ~max_steps:1L));
    tztest
      "Test unreachable kernel (10 ticks at a time)"
      `Quick
      (test_with_kernel
         unreachable_kernel
         (should_boot_unreachable_kernel ~max_steps:10L));
    tztest
      "Test unreachable kernel (in one go)"
      `Quick
      (test_with_kernel
         unreachable_kernel
         (should_boot_unreachable_kernel ~max_steps:Int64.max_int));
    tztest
      "Test write_debug kernel"
      `Quick
      (test_with_kernel test_write_debug_kernel should_run_debug_kernel);
    tztest
      "Test store-has kernel"
      `Quick
      (test_with_kernel test_store_has_kernel should_run_store_has_kernel);
  ]
