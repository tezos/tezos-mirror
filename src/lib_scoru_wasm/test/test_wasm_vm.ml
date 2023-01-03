open Tztest
open Tezos_scoru_wasm
open Wasm_utils

let init_tree_with_empty_input () =
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
  let*! tree = initial_tree ~from_binary:false module_ in
  let*! tree = eval_until_input_requested tree in
  let*! tree = set_empty_inbox_step 0l tree in
  return tree

let test_padding_state () =
  let open Lwt_result_syntax in
  let open Wasm_pvm_state.Internal_state in
  let* tree = init_tree_with_empty_input () in
  let*! pvm_state =
    Test_encodings_util.Tree_encoding_runner.decode
      Tezos_scoru_wasm.Wasm_pvm.pvm_state_encoding
      tree
  in
  let should_continue state =
    match state.tick_state with
    | Padding -> Lwt_syntax.return false
    | _ -> Lwt_syntax.return true
  in
  let*! pvm_state, _ =
    Wasm_vm.compute_step_many_until
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

let test_set_input_step_in_padding () =
  let open Lwt_result_syntax in
  let* tree = init_tree_with_empty_input () in
  (* Advance execution for relatively many steps in order to reach Padding tick_state *)
  let*! tree, _ = Wasm.compute_step_many ~max_steps:(Int64.of_int 10000) tree in

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

let test_last_input_info_updated_in_set_input_step () =
  let open Lwt_result_syntax in
  let* tree = init_tree_with_empty_input () in
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
  [
    tztest "Test eval_has_finished: padding" `Quick test_padding_state;
    tztest "Test set_input_state: padding" `Quick test_set_input_step_in_padding;
    tztest
      "Test set_input_state: update last input info"
      `Quick
      test_last_input_info_updated_in_set_input_step;
  ]
