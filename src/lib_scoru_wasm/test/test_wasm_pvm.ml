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
open Wasm_utils

let should_boot_unreachable_kernel ~max_steps kernel =
  let open Lwt_syntax in
  let* tree = initial_tree ~from_binary:true kernel in
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
  return_ok_unit

let should_run_debug_kernel () =
  let open Lwt_syntax in
  let module_ =
    {|
(module
 (import "rollup_safe_core" "write_debug"
         (func $write_debug (param i32 i32)))
 ;; Durable keys
 (data (i32.const 100) "hello")
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_next")
       (local $hello_address i32)
       (local $hello_length i32)

       (local.set $hello_address (i32.const 100))
       (local.set $hello_length (i32.const 5))

       (call $write_debug (local.get $hello_address) (local.get $hello_length))
       (nop)
       )
 )
|}
  in
  let* tree = initial_tree ~from_binary:false module_ in
  (* Make the first ticks of the WASM PVM (parsing of origination
     message, parsing and init of the kernel), to switch it to
     “Input_requested” mode. *)
  let* tree = eval_until_input_requested tree in
  (* Feeding it with one input *)
  let* tree = set_input_step "test" 0 tree in
  (* running until waiting for input *)
  let* tree = eval_until_input_requested tree in
  let* state_after_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The kernel should not fail. *)
  assert (not @@ is_stuck state_after_first_message) ;
  return_ok_unit

let add_value ?(content = "a very long value") tree key_steps =
  let open Tezos_lazy_containers in
  let open Test_encodings_util in
  let value = Chunked_byte_vector.of_string content in
  Tree_encoding_runner.encode
    Tezos_tree_encoding.(
      scope ("durable" :: List.append key_steps ["_"]) chunked_byte_vector)
    value
    tree

let should_run_store_has_kernel () =
  let open Lwt_syntax in
  let module_ =
    {|
(module
 (import "rollup_safe_core" "store_has"
         (func $store_has (param i32 i32) (result i32)))
 ;; Durable keys
 (data (i32.const 100) "/hi/bye/hello/other")
 (memory 1)
 (export "mem" (memory 0))
 (func $assert_eq
       (param $address i32) (param $length i32) (param $expected i32)
       (call $store_has (local.get $address) (local.get $length))
       (local.get $expected)
       (i32.ne)
       (if (then unreachable))
       )
 (func (export "kernel_next")
       (local $hi_address i32)
       (local $hi_length i32)
       (local $hi_bye_length i32)
       (local $hello_address i32)
       (local $hello_length i32)
       (local $hello_other_length i32)

       (local.set $hi_address (i32.const 100))
       (local.set $hi_length (i32.const 3))
       (local.set $hi_bye_length (i32.const 7))
       (local.set $hello_address (i32.const 107))
       (local.set $hello_length (i32.const 6))
       (local.set $hello_other_length (i32.const 12))

       (call $assert_eq
             (local.get $hi_address)
             (local.get $hi_length) (i32.const 2)) ;; Subtree = 2
       (call $assert_eq
             (local.get $hi_address)
             (local.get $hi_bye_length)
             (i32.const 1)) ;; Value = 1
       (call $assert_eq
             (local.get $hello_address)
             (local.get $hello_length) (i32.const 3)) ;; ValueWithSubtree = 3
       (call $assert_eq
             (local.get $hello_address)
             (local.get $hello_other_length)
             (i32.const 0)) ;; UnknownKey = 0
       (nop)
       )
 )
  |}
  in
  let* tree = initial_tree ~from_binary:false module_ in
  let* tree = add_value tree ["hi"; "bye"] in
  let* tree = add_value tree ["hello"] in
  let* tree = add_value tree ["hello"; "universe"] in
  (* Make the first ticks of the WASM PVM (parsing of origination message), to
     switch it to “Input_requested” mode. *)
  let* tree = eval_until_input_requested tree in
  let* state_before_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The kernel is not expected to fail, the PVM should not be in stuck state. *)
  assert (not @@ is_stuck state_before_first_message) ;
  (* We first evaluate the kernel normally, and it shouldn't fail. *)
  let* tree = set_input_step "test" 0 tree in
  let* tree = eval_until_input_requested tree in
  let* state_after_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The kernel is not expected to fail, the PVM should not be in stuck state. *)
  assert (not @@ is_stuck state_after_first_message) ;
  let* tree = set_input_step "test" 1 tree in
  (* We now delete the path ["hello"; "universe"] - this will cause gthe kernel
     assertion on this path to fail, and the PVM should become stuck on the
     third assert in the kernel. *)
  let* tree =
    Test_encodings_util.Tree.remove tree ["durable"; "hello"; "universe"; "_"]
  in
  let* tree = eval_until_input_requested tree in
  let* state_after_second_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The kernel is now expected to fail, the PVM should be in stuck state. *)
  assert (is_stuck state_after_second_message) ;
  return_ok_unit

(* The `should_run_store_list_size_kernel` asserts
   whether the tree has three subtrees. Note that the `_` subtree is included
   and so, after adding the `four` subtree the state will become stuck.*)
let should_run_store_list_size_kernel () =
  let open Lwt_syntax in
  let module_ =
    {|
(module
 (import "rollup_safe_core" "store_list_size"
         (func $store_list_size (param i32 i32) (result i64)))
 ;; Durable keys
 (data (i32.const 100) "/one")
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_next")
       (local $one_address i32)
       (local $one_length i32)

       (local.set $one_address (i32.const 100))
       (local.set $one_length (i32.const 4))

       (call $store_list_size
             (local.get $one_address) (local.get $one_length))
       (i64.ne (i64.const 3))
       (if (then unreachable))
       (nop)
       )
 )
  |}
  in
  let* tree = initial_tree ~from_binary:false module_ in
  let* tree = add_value tree ["one"] in
  let* tree = add_value tree ["one"; "two"] in
  let* tree = add_value tree ["one"; "three"] in
  (* Make the first ticks of the WASM PVM (parsing of origination
     message, parsing and init of the kernel), to switch it to
     “Input_requested” mode. *)
  let* tree = eval_until_input_requested tree in
  (* Feeding it with one input *)
  let* tree = set_input_step "test" 0 tree in
  (* running until waiting for input *)
  let* tree = eval_until_input_requested tree in
  let* state_after_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The kernel is not expected to fail, the PVM should not be in stuck state. *)
  assert (not @@ is_stuck state_after_first_message) ;
  (* We now add another value - this will cause the kernel
     assertion on this path to fail, as there are now four subtrees. *)
  let* tree = set_input_step "test" 1 tree in
  let* tree = add_value tree ["one"; "four"] in
  let* tree = eval_until_input_requested tree in
  let* state_after_second_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The kernel is now expected to fail, the PVM should be in stuck state. *)
  assert (is_stuck state_after_second_message) ;
  return_ok_unit

let should_run_store_delete_kernel () =
  let module_ =
    {|
(module
 (import "rollup_safe_core" "store_delete"
         (func $store_delete (param i32 i32) (result i32)))
 ;; Durable keys
 (data (i32.const 100) "/one") ;; /one
 (data (i32.const 104) "/two") ;; /two
 (data (i32.const 108) "/three") ;; /three
 (data (i32.const 114) "/four") ;; /four
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_next")
       (local $one_address i32)
       (local $one_length i32)
       (local $four_address i32)
       (local $four_length i32)

       (local.set $one_address (i32.const 100))
       (local.set $one_length (i32.const 4))
       (local.set $four_address (i32.const 108)) ;; /three + /four
       (local.set $four_length (i32.const 11)) ;; length(/three) + length(/four)

       (call $store_delete (local.get $one_address) (local.get $one_length))
       (drop)
       (call $store_delete (local.get $four_address) (local.get $four_length))
       (drop)
       (nop)
       )
 )

|}
  in
  let open Lwt_syntax in
  let open Test_encodings_util in
  let* tree = initial_tree ~from_binary:false module_ in
  let* tree = add_value tree ["one"] in
  let* tree = add_value tree ["one"; "two"] in
  let* tree = add_value tree ["three"] in
  let* tree = add_value tree ["three"; "four"] in
  (* Make the first ticks of the WASM PVM (parsing of origination
     message, parsing and init of the kernel), to switch it to
     “Input_requested” mode. *)
  (* Check that we have all the paths in place. *)
  let* result = Tree.find_tree tree ["durable"; "one"] in
  assert (Option.is_some result) ;
  let* result = Tree.find_tree tree ["durable"; "three"; "four"] in
  assert (Option.is_some result) ;
  let* result = Tree.find_tree tree ["durable"; "three"] in
  assert (Option.is_some result) ;
  (* Eval until input requested: the initial step will be "Snapshot", which is
     an input step. It needs an input to be forced to initialize. *)
  let* tree = eval_until_input_requested tree in
  let* tree = set_input_step "moving forward" 0 tree in
  let* tree = eval_until_input_requested tree in
  let* state = Wasm.Internal_for_tests.get_tick_state tree in
  (* The kernel is not expected to fail, the PVM should not be in stuck state. *)
  assert (not @@ is_stuck state) ;
  (* The paths /one & /three/four will have been deleted. *)
  let* result = Tree.find_tree tree ["durable"; "one"] in
  assert (Option.is_none result) ;
  let* result = Tree.find_tree tree ["durable"; "three"; "four"] in
  assert (Option.is_none result) ;
  let* result = Tree.find_tree tree ["durable"; "three"] in
  assert (Option.is_some result) ;
  return_ok_unit

(* This function can build snapshotable state out of a tree. It currently
   assumes it follows a tree resulting from `set_input_step`.*)
let build_snapshot_wasm_state_from_set_input
    ?(max_tick = Z.of_int64 default_max_tick) tree =
  let open Lwt_syntax in
  let open Wasm_pvm_state.Internal_state in
  (* Only serves to encode the `Snapshot` state. *)
  let state_encoding =
    Tezos_tree_encoding.(
      tagged_union
        (value [] Data_encoding.string)
        [
          case
            "snapshot"
            (value [] Data_encoding.unit)
            (function Snapshot -> Some () | _ -> None)
            (fun () -> Snapshot);
        ])
  in
  (* Serves to encode the `Input_requested` status. *)
  let* tree =
    Test_encodings_util.Tree_encoding_runner.encode
      (Tezos_tree_encoding.scope ["wasm"] state_encoding)
      Snapshot
      tree
  in
  (* Since we start directly after at an input_step, we need to offset the tick
     at the next max tick + the snapshot tick. *)
  let* current_tick =
    Test_encodings_util.Tree_encoding_runner.decode
      (Tezos_tree_encoding.value ["pvm"; "last_top_level_call"] Data_encoding.n)
      tree
  in
  let snapshot_tick = Z.(max_tick + current_tick) in
  let* tree =
    Test_encodings_util.Tree_encoding_runner.encode
      (Tezos_tree_encoding.value ["wasm"; "current_tick"] Data_encoding.n)
      snapshot_tick
      tree
  in
  let* tree = Wasm.Internal_for_tests.reset_reboot_counter tree in
  Test_encodings_util.Tree_encoding_runner.encode
    (Tezos_tree_encoding.value ["pvm"; "last_top_level_call"] Data_encoding.n)
    snapshot_tick
    tree

let test_snapshotable_state () =
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
  (* First evaluate until the snapshotable state. *)
  let*! tree = eval_until_input_requested tree in
  let*! state = Wasm.Internal_for_tests.get_tick_state tree in
  let* () =
    match state with
    | Snapshot -> return_unit
    | _ ->
        failwith
          "Unexpected state at input requested: %a"
          Wasm_utils.pp_state
          state
  in
  (* The "wasm"/"modules" key should not exist after the snapshot, since it
     contains the instance of the module after its evaluation. *)
  let*! wasm_tree_exists =
    Test_encodings_util.Tree.mem_tree tree ["wasm"; "modules"]
  in
  let*! wasm_value_exists =
    Test_encodings_util.Tree.mem tree ["wasm"; "modules"]
  in
  assert (not (wasm_tree_exists || wasm_value_exists)) ;
  (* Set a new input should go back to decoding *)
  let*! tree = set_input_step "test" 1 tree in
  let*! state = Wasm.Internal_for_tests.get_tick_state tree in
  match state with
  | Decode _ -> return_unit
  | _ ->
      failwith
        "Unexpected state after set_input_step: %a"
        Wasm_utils.pp_state
        state

let test_rebuild_snapshotable_state () =
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
  let*! tree = set_input_step "test" 0 tree in
  (* First evaluate until the snapshotable state. *)
  let*! tree_after_eval = eval_until_input_requested tree in
  (* From out initial tree, let's rebuild the expected snapshotable state as the
     Fast Node would do. This works because the kernel doesn't change anything. *)
  let*! rebuilded_tree = build_snapshot_wasm_state_from_set_input tree in

  (* Remove the "wasm" key from the tree after evaluation, and try to rebuild it
     as a snapshot. This would take into account any change in the input and
     output buffers and the durable storage. *)
  let*! tree_without_wasm = Test_encodings_util.Tree.remove tree ["wasm"] in
  let*! rebuilded_tree_without_wasm =
    build_snapshot_wasm_state_from_set_input tree_without_wasm
  in

  (* Both hash should be exactly the same. *)
  let hash_tree_after_eval = Test_encodings_util.Tree.hash tree_after_eval in
  let hash_rebuilded_tree = Test_encodings_util.Tree.hash rebuilded_tree in
  assert (Context_hash.equal hash_tree_after_eval hash_rebuilded_tree) ;

  (* The hash of the tree rebuilded from the evaluated one without the "wasm"
     part should also be exactly the same. *)
  let hash_rebuilded_tree_without_wasm =
    Test_encodings_util.Tree.hash rebuilded_tree_without_wasm
  in
  assert (
    Context_hash.equal hash_tree_after_eval hash_rebuilded_tree_without_wasm) ;

  (* To be sure, let's try to evaluate a new input from these two, either by the
     regular tree or the snapshoted one. *)
  let*! input_step_from_eval = set_input_step "test" 1 tree_after_eval in
  let*! input_step_from_snapshot = set_input_step "test" 1 rebuilded_tree in

  (* Their hash should still be the same, but the first test should have caught
     that. *)
  let hash_input_tree_after_eval =
    Test_encodings_util.Tree.hash input_step_from_eval
  in
  let hash_input_rebuilded_tree =
    Test_encodings_util.Tree.hash input_step_from_snapshot
  in

  assert (
    Context_hash.equal hash_input_tree_after_eval hash_input_rebuilded_tree) ;
  return_unit

let test_unkown_host_function_truncated () =
  let open Lwt_result_syntax in
  let rollup_safe_core = "rollup_safe_core" in
  let unknown_function = String.make 100 'a' in
  (* This module imports an unknown function of length 100, which is the maximum
     accepted by our interpreter. The error message will be trunctated *)
  let module_ =
    Format.sprintf
      {|
        (module
          (import "%s" "%s"
            (func $unkown_function (param i32 i32) (result i32))
          )
          (memory 1)
          (export "mem" (memory 0))
          (func (export "kernel_next")
            (unreachable)
          )
        )
    |}
      rollup_safe_core
      unknown_function
  in
  (* Let's first init the tree to compute. *)
  let*! tree = initial_tree ~from_binary:false module_ in
  (* The tree should be in snapshot state by default, hence in input step. *)
  let*! tree_snapshotted = eval_until_input_requested tree in
  let*! tree_with_dummy_input =
    set_input_step "dummy_input" 0 tree_snapshotted
  in
  let*! tree_stuck = eval_until_input_requested tree_with_dummy_input in
  let*! state = Wasm.Internal_for_tests.get_tick_state tree_stuck in
  (* The message as originally outputed before being
     truncated. *)
  let reason =
    Format.sprintf "Unexpected import: %s.%s" rollup_safe_core unknown_function
  in
  (* Let's check the message will be indeed truncated, otherwise this test is
     not relevant. *)
  assert (String.length reason > Wasm_pvm_errors.messages_maximum_size) ;
  (* The final reason for being stuck should be truncated after
     [Wasm_pvm_errors.messages_maximum_size]. *)
  let reason = String.sub reason 0 Wasm_pvm_errors.messages_maximum_size in
  assert (is_stuck ~step:`Link ~reason state) ;
  return_unit

let test_bulk_noops () =
  let open Lwt.Syntax in
  let module_ =
    {|
      (module
        (memory 0)
        (export "mem" (memory 0))
        (func (export "kernel_next")
          (nop)
        )
      )
    |}
  in
  let* base_tree = initial_tree ~max_tick:500L module_ in
  let* base_tree = set_input_step "dummy_input" 0 base_tree in

  let rec goto_snapshot ticks tree_slow =
    let* tree_fast, _ = Wasm.compute_step_many ~max_steps:ticks base_tree in
    let* tree_slow = Wasm.compute_step tree_slow in

    assert (
      Context_hash.(Test_encodings_util.Tree.(hash tree_fast = hash tree_slow))) ;

    let* stuck = Wasm_utils.Wasm.Internal_for_tests.is_stuck tree_fast in
    assert (Option.is_none stuck) ;

    let* state = Wasm.Internal_for_tests.get_tick_state tree_slow in
    if state = Snapshot then Lwt.return (ticks, tree_slow)
    else goto_snapshot (Int64.succ ticks) tree_slow
  in

  let* ticks, snapshot = goto_snapshot 1L base_tree in
  let* snapshot_info = Wasm_utils.Wasm.get_info snapshot in
  assert (snapshot_info.input_request = Input_required) ;

  (* Try to advance past the snapshot point. *)
  let* tree_fast, _ =
    Wasm.compute_step_many ~max_steps:(Int64.mul ticks 2L) base_tree
  in

  (* Because the Snapshot state is an input state, the [compute_step_many]
     invocation must not be able to zoom past it! *)
  assert (
    Context_hash.(Test_encodings_util.Tree.(hash tree_fast = hash snapshot))) ;

  Lwt_result_syntax.return_unit

let test_durable_store_io () =
  let open Lwt_result_syntax in
  let from_path = "/from/value" in
  let from_path_location = 100 in
  let to_path = "/to/value" in
  let to_path_location = from_path_location + String.length from_path in
  let read_offset = 2 in
  let write_offset = 0 in
  let buffer_location = 15 in
  let buffer_length = 1 in
  let modul =
    Format.sprintf
      {|
        (module
          (type (;0;) (func (param i32 i32 i32 i32 i32) (result i32)))
          (import "rollup_safe_core" "store_read"
            (func $store_read (type 0)))
          (import "rollup_safe_core" "store_write"
            (func $store_write (type 0)))
          (func (export "kernel_next")
            (local i32)
            (call $store_read (i32.const %d)
                              (i32.const %d)
                              (i32.const %d)
                              (i32.const %d)
                              (i32.const %d))
            drop
            (call $store_write (i32.const %d)
                               (i32.const %d)
                               (i32.const %d)
                               (i32.const %d)
                               (i32.const %d))
            drop)
          (memory (;0;) 17)
          (export "memory" (memory 0))
          (data (;0;) (i32.const %d) "%s%s"))
    |}
      (* store_read args *)
      from_path_location
      (String.length from_path)
      read_offset
      buffer_location
      buffer_length
      (* store_write args *)
      to_path_location
      (String.length to_path)
      write_offset
      buffer_location
      buffer_length
      (* data block *)
      from_path_location
      from_path
      to_path
  in
  (* Let's first init the tree to compute. *)
  let*! tree = initial_tree ~from_binary:false modul in
  let*! tree = eval_until_input_requested tree in

  (* Add content to '/from/value' key in durable *)
  let content = "abcde" in
  let*! tree = add_value tree ["from"; "value"] ~content in

  (* We should not have a byte written to '/to/value' *)
  let*! durable = wrap_as_durable_storage tree in
  let durable = Durable.of_storage_exn durable in
  let*! value =
    Durable.find_value durable (Durable.key_of_string_exn "/to/value")
  in
  assert (Option.is_none value) ;

  (* Set input and eval again. *)
  let*! tree = set_input_step "dummy_input" 0 tree in
  let*! tree = eval_until_input_requested tree in
  let*! state = Wasm.Internal_for_tests.get_tick_state tree in
  assert (not @@ is_stuck state) ;

  (* We should now have a byte written to '/to/value' *)
  let*! durable = wrap_as_durable_storage tree in
  let durable = Durable.of_storage_exn durable in
  let*! value =
    Durable.find_value_exn durable (Durable.key_of_string_exn "/to/value")
  in
  let*! value = Tezos_lazy_containers.Chunked_byte_vector.to_string value in
  let expected = String.sub content read_offset 1 in
  assert (expected = value) ;
  return_unit

let test_kernel_reboot_gen ~reboots ~expected_reboots ~pvm_max_reboots =
  let open Lwt_result_syntax in
  (* Extracted from the kernel, these are the constant values used to build the
     initial memory and the addresses where values are stored. *)
  let data_offset_start = 100 in

  (* Durable storage: reboot key *)
  let reboot_key = "/kernel/env/reboot" in
  let reboot_key_length = String.length reboot_key in
  let reboot_key_offset = data_offset_start in

  (* Durable storage: reboot counter *)
  let reboot_counter_key = "/reboot/counter" in
  let reboot_counter_key_length = String.length reboot_counter_key in
  let reboot_counter_key_offset = reboot_key_offset + reboot_key_length in

  (* Memory only: reboot flag and reboot counter *)
  let reboot_flag_in_memory_offset =
    reboot_counter_key_offset + reboot_counter_key_offset
  in
  let reboot_counter_in_memory_offset =
    reboot_flag_in_memory_offset
    + Tezos_webassembly_interpreter.Types.(num_size I32Type)
    (* Number of bytes taken by the flag in memory *)
  in

  let reboot_module =
    Format.sprintf
      {|
(module
 (import "rollup_safe_core" "store_write"
         (func $store_write (param i32 i32 i32 i32 i32) (result i32)))
 (import "rollup_safe_core" "store_read"
         (func $store_read (param i32 i32 i32 i32 i32) (result i32)))
 (import "rollup_safe_core" "store_has"
         (func $store_has (param i32 i32) (result i32)))
 ;; Durable keys
 (data (i32.const %d) "%s")
 (data (i32.const %d) "%s")

 (memory 1)
 (export "mem" (memory 0))

 ;; Finds the reboot counter in the durable storage and increase it, initializes
 ;; it otherwise.
 (func $incr_reboot_counter
       (param) (result i32)

       (local $reboot_counter_key i32)
       (local $reboot_counter_key_length i32)
       (local $reboot_counter_offset i32)
       (local $reboot_counter_value i32)

       (local.set $reboot_counter_key (i32.const %d))
       (local.set $reboot_counter_key_length (i32.const %d))
       (local.set $reboot_counter_offset (i32.const %d))
       (local.set $reboot_counter_value (i32.const 0)) ;; initial value of the counter

       ;; First check the counter exists in the durable storage
       (call $store_has
             (local.get $reboot_counter_key)
             (local.get $reboot_counter_key_length))
       ;; if store_has returns `1`, the key exists with a value without
       ;; subtrees, that's exactly what we expect.
       (i32.const 1)
       (i32.eq)

       (if
        ;; if it exists, load it and increment it into $reboot_counter_value
        (then
         (call $store_read
               (local.get $reboot_counter_key)
               (local.get $reboot_counter_key_length)
               (i32.const 0) ;; value at offset 0 in the durable storage
               (local.get $reboot_counter_offset)
               (i32.const 4)) ;; we expect at most 4 bytes, since the counter is
                              ;; an i32 in V128 encoding)
         (drop) ;; we drop the number of bytes read
         (local.set $reboot_counter_value
                    (i32.add (i32.load (local.get $reboot_counter_offset))
                             (i32.const 1)))
         )
        )
       (i32.store (local.get $reboot_counter_offset)
                  (local.get $reboot_counter_value))
       (call $store_write
             (local.get $reboot_counter_key)
             (local.get $reboot_counter_key_length)
             (i32.const 0) ;; value at offset 0 in the durable storage
             (local.get $reboot_counter_offset)
             (i32.const 4)) ;; we expect at most 4 bytes, since the counter is
                            ;; an i32 in V128 encoding
       (drop)
       (local.get $reboot_counter_value)
       )

 (func (export "kernel_next")
       (local $reboot_flag_key i32)
       (local $reboot_flag_length i32)
       (local $reboot_flag_value_offset i32)
       (local $reboot_flag_value i32)
       (local $reboot_max i32)

       (local.set $reboot_flag_key (i32.const %d))
       (local.set $reboot_flag_length (i32.const %d))
       (local.set $reboot_flag_value_offset (i32.const %d))
       (local.set $reboot_max (i32.const %ld))

       ;; first increase the reboot counter; and put the counter on the stack
       (call $incr_reboot_counter)
       ;; determine the reboot flag: if it is lesser than the maximum then
       ;; reboot, otherwise don't reboot
       (local.get $reboot_max)
       (i32.lt_s)
       ;; set the reboot flag
       (if
        (then
         (call $store_write
               (local.get $reboot_flag_key)
               (local.get $reboot_flag_length)
               (i32.const 0)
               (local.get $reboot_flag_value)
               (i32.const 4));; we expect at most 4 bytes, since the counter is
                             ;; an i32 in V128 encoding
         (drop))
       )

       ;; explicitely do nothing
       (nop)
       )
 )
|}
      (* Data section *)
      reboot_key_offset
      reboot_key
      reboot_counter_key_offset
      reboot_counter_key
      (* `incr_counter` function *)
      reboot_counter_key_offset
      reboot_counter_key_length
      reboot_counter_in_memory_offset
      (* `kernel_next` function *)
      data_offset_start (* == reboot_key's offset *)
      reboot_key_length
      reboot_flag_in_memory_offset
      reboots
  in
  (* Let's first init the tree to compute. *)
  let*! tree =
    initial_tree
      ~max_reboots:(Z.of_int32 pvm_max_reboots)
      ~from_binary:false
      reboot_module
  in
  let*! tree = eval_until_input_requested tree in
  let*! tree = set_input_step "dummy_input" 0 tree in
  let*! tree = eval_until_input_requested tree in
  let*! state = Wasm.Internal_for_tests.get_tick_state tree in

  (* If the expected number of reboots from the PVM is lower than the maximum
     asked by the kernel itself, this should lead to a stuck state with
     `Too_many_reboots`. *)
  if reboots <= pvm_max_reboots then assert (not @@ is_stuck state)
  else assert (is_stuck ~step:`Too_many_reboots state) ;

  let*! durable = wrap_as_durable_storage tree in
  let durable = Durable.of_storage_exn durable in
  let*! value =
    Durable.find_value durable (Durable.key_of_string_exn "/reboot/counter")
  in
  match value with
  | None ->
      failwith
        "Evaluation error: couldn't find the reboot counter in the durable \
         storage"
  | Some value ->
      let*! value = Tezos_lazy_containers.Chunked_byte_vector.to_bytes value in
      (* WASM Values in memories are encoded in little-endian order. *)
      let value = Bytes.get_int32_le value 0 in
      assert (value = expected_reboots) ;
      return_unit

let test_kernel_reboot () =
  (* The kernel doesn't accept more than 10 reboots between two inputs, this test will succeed. *)
  test_kernel_reboot_gen ~reboots:5l ~expected_reboots:5l ~pvm_max_reboots:10l

let test_kernel_reboot_failing () =
  (* The kernel doesn't accept more than 10 reboots between two inputs, it will
     then fail after 10. *)
  test_kernel_reboot_gen ~reboots:15l ~expected_reboots:10l ~pvm_max_reboots:10l

let tests =
  [
    tztest
      "Test unreachable kernel (tick per tick)"
      `Quick
      (test_with_kernel
         Kernels.unreachable_kernel
         (should_boot_unreachable_kernel ~max_steps:1L));
    tztest
      "Test unreachable kernel (10 ticks at a time)"
      `Quick
      (test_with_kernel
         Kernels.unreachable_kernel
         (should_boot_unreachable_kernel ~max_steps:10L));
    tztest
      "Test unreachable kernel (in one go)"
      `Quick
      (test_with_kernel
         Kernels.unreachable_kernel
         (should_boot_unreachable_kernel ~max_steps:Int64.max_int));
    tztest "Test write_debug kernel" `Quick should_run_debug_kernel;
    tztest "Test store-has kernel" `Quick should_run_store_has_kernel;
    tztest
      "Test store-list-size kernel"
      `Quick
      should_run_store_list_size_kernel;
    tztest "Test store-delete kernel" `Quick should_run_store_delete_kernel;
    tztest "Test snapshotable state" `Quick test_snapshotable_state;
    tztest
      "Test rebuild snapshotable state"
      `Quick
      test_rebuild_snapshotable_state;
    tztest
      "Test Stuck state is truncated on long messages"
      `Quick
      test_unkown_host_function_truncated;
    tztest "Test bulk no-ops function properly" `Quick test_bulk_noops;
    tztest "Test durable store io" `Quick test_durable_store_io;
    tztest "Test reboot" `Quick test_kernel_reboot;
    tztest
      "Test reboot takes too many reboots"
      `Quick
      test_kernel_reboot_failing;
  ]
