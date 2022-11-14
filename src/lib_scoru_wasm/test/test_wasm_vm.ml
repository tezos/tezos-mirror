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
        (func (export "kernel_next")
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

let tests =
  [
    tztest "Test eval_has_finished: padding" `Quick test_padding_state;
    tztest "Test set_input_state: padding" `Quick test_set_input_step_in_padding;
  ]
