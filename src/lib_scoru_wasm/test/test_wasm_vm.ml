(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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
    Component:    Lib_scoru_wasm
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_wasm_vm.ml
    Subject:      Tests for the tezos-scoru-wasm library
*)

open Tezos_scoru_wasm
open Wasm_utils
open Tztest_helper

let init_tree_with_empty_input ~version =
  let open Lwt_result_syntax in
  let module_ =
    {|
      (module
        (memory 1)
        (export "mem"(memory 0))
        (func (export "kernel_run")
          (nop)
        )
      )
    |}
  in
  let*! tree = initial_state ~version ~from_binary:false module_ in
  let*! tree = eval_until_input_requested tree in
  let*! tree = set_empty_inbox_step 0l tree in
  return tree

let test_padding_state ~version () =
  let open Lwt_result_syntax in
  let open Wasm_pvm_state.Internal_state in
  let* tree = init_tree_with_empty_input ~version in
  let*! pvm_state =
    Encodings_util.Tree_encoding_runner.decode
      Tezos_scoru_wasm.Wasm_pvm.pvm_state_encoding
      tree
  in
  let should_continue state =
    match state.tick_state with
    | Padding -> Lwt_syntax.return_false
    | _ -> Lwt_syntax.return_true
  in
  let*! pvm_state, _ =
    Wasm_vm.compute_step_many_until
      ~wasm_entrypoint:Constants.wasm_entrypoint
      ~write_debug:Noop
      ~max_steps:Int64.max_int
      should_continue
      pvm_state
  in
  let tick_state = pvm_state.tick_state in
  let* () =
    match tick_state with
    | Padding -> return_unit
    | _ -> failwith "Expected padding toto: %a" Wasm_utils.pp_state tick_state
  in
  let* () =
    if Wasm_vm.eval_has_finished tick_state then return_unit
    else failwith "Evaluation should be considered over"
  in
  return_unit

let test_set_input_step_in_padding ~version () =
  let open Lwt_result_syntax in
  let* tree = init_tree_with_empty_input ~version in
  (* Advance execution for relatively many steps in order to reach Padding tick_state *)
  let*! tree, _ =
    Wasm.compute_step_many
      ~wasm_entrypoint:Constants.wasm_entrypoint
      ~max_steps:(Int64.of_int 10000)
      tree
  in

  (* Ensure we are in Padding state *)
  let*! tick_state = Wasm.Internal_for_tests.get_tick_state tree in
  let* () =
    match tick_state with
    | Padding -> return_unit
    | _ -> failwith "Expected padding toto: %a" Wasm_utils.pp_state tick_state
  in

  let*! tree = set_input_step "test1" 1 tree in
  (* Ensure we are in Stuck state after supplying input being in Padding state *)
  let*! tick_state = Wasm.Internal_for_tests.get_tick_state tree in
  let* () =
    match tick_state with
    | Stuck
        (Wasm_pvm_errors.Invalid_state
           (Wasm_pvm_errors.Truncated "No input required during padding")) ->
        return_unit
    | _ ->
        failwith
          "Expecting tick_state to be in Stuck but got %a"
          Wasm_utils.pp_state
          tick_state
  in
  return_unit

let test_last_input_info_updated_in_set_input_step ~version () =
  let open Lwt_result_syntax in
  let* tree = init_tree_with_empty_input ~version in
  let assert_input_info ?(inp_req = Wasm_pvm_state.Input_required) tree level
      msg_counter =
    let expected_info =
      input_info (Int32.of_int level) (Z.of_int msg_counter)
    in
    let*! info = Wasm.get_info tree in
    assert (
      info.last_input_read = Some expected_info && info.input_request == inp_req) ;
    return_unit
  in

  (* Verify initial empty level supplied and last_input_info updated accordingly *)
  let* _initial_input_info =
    assert_input_info ~inp_req:No_input_required tree 0 2
  in

  let*! tree = eval_until_input_requested tree in
  let*! tree = Wasm_utils.set_sol_input 1l tree in
  let*! tree = Wasm_utils.set_info_per_level_input 1l tree in
  let*! tree = Wasm_utils.set_internal_message 1l (Z.of_int 2) "input1" tree in
  let*! tree = Wasm_utils.set_internal_message 1l (Z.of_int 3) "input2" tree in
  let*! tree = Wasm_utils.set_internal_message 1l (Z.of_int 4) "input3" tree in
  let* _input_info_during_collect = assert_input_info tree 1 4 in
  let*! tree = Wasm_utils.set_eol_input 1l (Z.of_int 5) tree in

  let*! tick_state = Wasm.Internal_for_tests.get_tick_state tree in
  assert (tick_state == Padding) ;
  (* Check that last_input_info updated accordingly even in Padding *)
  let*! tree = Wasm_utils.set_sol_input 2l tree in
  let* _input_info_in_padding_still_updated = assert_input_info tree 2 0 in

  let*! tick_state = Wasm.Internal_for_tests.get_tick_state tree in
  let* () =
    match tick_state with
    | Stuck _ -> return_unit
    | _ -> failwith "Unexpected tick state"
  in
  let*! tree = Wasm_utils.set_eol_input 2l Z.one tree in
  let* _input_info_in_stuck_still_updated = assert_input_info tree 2 1 in
  return_unit

let tests =
  tztests_with_all_pvms
    [
      ("Test eval_has_finished: padding", `Quick, test_padding_state);
      ("Test set_input_state: padding", `Quick, test_set_input_step_in_padding);
      ( "Test set_input_state: update last input info",
        `Quick,
        test_last_input_info_updated_in_set_input_step );
    ]

let () =
  Alcotest_lwt.run ~__FILE__ "test lib scoru wasm" [("WASM VM", tests)]
  |> Lwt_main.run
