open Tztest
open Tezos_scoru_wasm
open Wasm_utils

let test_padding_state () =
  let open Lwt_result_syntax in
  let open Wasm_pvm_state.Internal_state in
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
  let*! tree = set_input_step "test" 0 tree in
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

let tests = [tztest "Test eval_has_finished: padding" `Quick test_padding_state]
