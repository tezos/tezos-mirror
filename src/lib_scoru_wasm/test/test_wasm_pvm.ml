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
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_wasm_pvm.ml
    Subject:      WASM PVM evaluation tests for the tezos-scoru-wasm library
*)

open Tezos_scoru_wasm
open Tezos_scoru_wasm_helpers.Encodings_util
open Wasm_utils
open Tztest_helper

let should_boot_unreachable_kernel ~version ~batch_size kernel =
  let open Lwt_syntax in
  (* we call recursively the eval function on given increment *)
  let rec loop_until_input_required batch_size tree =
    let* info = Wasm.get_info tree in
    match info.input_request with
    | No_input_required ->
        let* tree =
          eval_until_input_or_reveal_requested ~max_steps:batch_size tree
        in
        loop_until_input_required batch_size tree
    | _ -> return tree
  in
  let* tree = initial_tree ~version ~from_binary:true kernel in
  (* Feeding it with one input *)
  let* tree = set_empty_inbox_step 0l tree in
  (* running until waiting for input *)
  let* tree = loop_until_input_required batch_size tree in
  let* info_after_first_message = Wasm.get_info tree in
  (* The kernel is expected to fail, then ths PVM should
     have failed during the evaluation when evaluating a `Unreachable`
     instruction. *)
  let* stuck_flag = has_stuck_flag tree in
  assert stuck_flag ;
  (* Feeding it with one input *)
  let* tree = set_empty_inbox_step 1l tree in
  (* running until waiting for input *)
  let* tree = eval_until_input_or_reveal_requested tree in
  let* info_after_second_message = Wasm.get_info tree in
  (* The PVM should still have the `stuck` tag on, but can still receive inputs
     and go forward, hence the tick after the second message should be greater.
  *)
  assert (
    Z.lt
      info_after_first_message.current_tick
      info_after_second_message.current_tick) ;
  let* stuck_flag = has_stuck_flag tree in
  assert stuck_flag ;
  return_ok_unit

let should_run_debug_kernel ~version () =
  let open Lwt_syntax in
  let module_ =
    {|
(module
 (import "smart_rollup_core" "write_debug"
         (func $write_debug (param i32 i32)))
 ;; Durable keys
 (data (i32.const 100) "hello")
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
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
  let* tree = initial_tree ~version ~from_binary:false module_ in
  (* Feeding it with one input *)
  let* tree = set_empty_inbox_step 0l tree in
  (* running until waiting for input *)
  let* tree = eval_until_input_or_reveal_requested tree in
  let* stuck_flag = has_stuck_flag tree in
  (* The kernel should not fail. *)
  assert (not stuck_flag) ;
  return_ok_unit

let add_value ?(content = "a very long value") tree key_steps =
  let open Tezos_lazy_containers in
  let value = Chunked_byte_vector.of_string content in
  Tree_encoding_runner.encode
    Tezos_tree_encoding.(
      scope
        ("durable" :: List.append key_steps ["@"])
        Chunked_byte_vector.encoding)
    value
    tree

let should_run_store_has_kernel ~version () =
  let open Lwt_syntax in
  let module_ =
    {|
(module
 (import "smart_rollup_core" "store_has"
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
 (func (export "kernel_run")
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
  let* tree = initial_tree ~version ~from_binary:false module_ in
  let* tree = add_value tree ["hi"; "bye"] in
  let* tree = add_value tree ["hello"] in
  let* tree = add_value tree ["hello"; "universe"] in
  (* The kernel is not expected to fail, the PVM should not have stuck state on.
      *)
  let* stuck_flag = has_stuck_flag tree in
  assert (not stuck_flag) ;
  (* We first evaluate the kernel normally, and it shouldn't fail. *)
  let* tree = set_empty_inbox_step 0l tree in
  let* tree = eval_until_input_or_reveal_requested tree in
  (* The kernel is not expected to fail, the PVM should not have stuck state on.
      *)
  let* stuck_flag = has_stuck_flag tree in
  assert (not stuck_flag) ;
  let* tree = set_empty_inbox_step 1l tree in
  (* We now delete the path ["hello"; "universe"] - this will cause gthe kernel
     assertion on this path to fail, and the PVM should become stuck on the
     third assert in the kernel. *)
  let* tree = Tree.remove tree ["durable"; "hello"; "universe"; "@"] in
  let* tree = eval_until_input_or_reveal_requested tree in
  (* The kernel is now expected to fail, the PVM should have stuck tag on. *)
  let* stuck_flag = has_stuck_flag tree in
  assert stuck_flag ;
  return_ok_unit

(* The `should_run_store_list_size_kernel` asserts
   whether the tree has three subtrees. Note that the `_` subtree is included
   and so, after adding the `four` subtree the state will receive the stuck tag.*)
let should_run_store_list_size_kernel ~version () =
  let open Lwt_syntax in
  let module_ =
    {|
(module
 (import "smart_rollup_core" "store_list_size"
         (func $store_list_size (param i32 i32) (result i64)))
 ;; Durable keys
 (data (i32.const 100) "/one")
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
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
  let* tree = initial_tree ~version ~from_binary:false module_ in
  let* tree = add_value tree ["one"] in
  let* tree = add_value tree ["one"; "two"] in
  let* tree = add_value tree ["one"; "three"] in
  (* Feeding it with one input *)
  let* tree = set_empty_inbox_step 0l tree in
  (* running until waiting for input *)
  let* tree = eval_until_input_or_reveal_requested tree in
  let* state_after_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  (* The kernel is not expected to fail, the PVM should not be in stuck state. *)
  assert (not @@ is_stuck state_after_first_message) ;
  (* We now add another value - this will cause the kernel
     assertion on this path to fail, as there are now four subtrees. *)
  let* tree = set_empty_inbox_step 1l tree in
  let* tree = add_value tree ["one"; "four"] in
  let* tree = eval_until_input_or_reveal_requested tree in
  (* The kernel is now expected to fail, the PVM should have the stuck tag. *)
  let* stuck_flag = has_stuck_flag tree in
  assert stuck_flag ;
  return_ok_unit

let should_run_store_delete_kernel ~version () =
  let module_ =
    {|
(module
 (import "smart_rollup_core" "store_delete"
         (func $store_delete (param i32 i32) (result i32)))
 ;; Durable keys
 (data (i32.const 100) "/one") ;; /one
 (data (i32.const 104) "/two") ;; /two
 (data (i32.const 108) "/three") ;; /three
 (data (i32.const 114) "/four") ;; /four
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
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
  let* tree = initial_tree ~version ~from_binary:false module_ in
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
  let* tree = set_empty_inbox_step 0l tree in
  let* tree = eval_until_input_or_reveal_requested tree in
  (* The kernel is not expected to fail, the PVM should not have stuck flag. *)
  let* stuck_flag = has_stuck_flag tree in
  assert (not stuck_flag) ;
  (* The paths /one & /three/four will have been deleted. *)
  let* result = Tree.find_tree tree ["durable"; "one"] in
  assert (Option.is_none result) ;
  let* result = Tree.find_tree tree ["durable"; "three"; "four"] in
  assert (Option.is_none result) ;
  let* result = Tree.find_tree tree ["durable"; "three"] in
  assert (Option.is_some result) ;
  return_ok_unit

let assert_store_value tree path expected_value =
  let open Lwt_syntax in
  let open Tezos_lazy_containers in
  let* durable = wrap_as_durable_storage tree in
  let durable = Durable.of_storage_exn durable in
  let* value = Durable.find_value durable (Durable.key_of_string_exn path) in
  let+ value = Option.map_s Chunked_byte_vector.to_string value in
  assert (Option.equal String.equal value expected_value)

(* store_move *)
let should_run_store_move_kernel ~version () =
  let open Lwt_syntax in
  let from_path = "/a/b" in
  let from_path_location = 100 in
  let to_path = "/a/b/c" in
  let to_path_location = from_path_location + String.length from_path in
  let module_ =
    Format.sprintf
      {|
(module
  (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
  (import "smart_rollup_core" "store_move" (func $store_move (type 0)))
  (func (export "kernel_run")
    (local i32)
    (call $store_move
      (i32.const %d)
      (i32.const %d)
      (i32.const %d)
      (i32.const %d))
    drop)
  (memory (;0;) 17)
  (export "memory" (memory 0))
  (data (;0;) (i32.const %d) "%s%s"))
    |}
      (* store_move args *)
      from_path_location
      (String.length from_path)
      to_path_location
      (String.length to_path)
      (* data block *)
      from_path_location
      from_path
      to_path
  in
  let* tree = initial_tree ~version ~from_binary:false module_ in
  let* tree = add_value tree ["a"; "b"] ~content:"ab" in
  let* tree = add_value tree ["a"; "b"; "c"] ~content:"abc" in
  let* tree = add_value tree ["a"; "b"; "d"] ~content:"abd" in
  let* tree = add_value tree ["e"; "f"] ~content:"ef" in
  let* state_before_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  assert (not @@ is_stuck state_before_first_message) ;
  (* consume a message and run kernel_run until we need next message *)
  let* tree = set_empty_inbox_step 0l tree in
  let* tree = eval_until_input_or_reveal_requested tree in
  let* state_after_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  assert (not @@ is_stuck state_after_first_message) ;
  (* Test that values have been moved, and that original location has been
   * overwritten or removed *)
  let* () = assert_store_value tree "/a/b/c" (Some "ab") in
  let* () = assert_store_value tree "/a/b/c/c" (Some "abc") in
  let* () = assert_store_value tree "/a/b/c/d" (Some "abd") in
  let* () = assert_store_value tree "/e/f" (Some "ef") in
  let* () = assert_store_value tree "/a/b" None in
  let* () = assert_store_value tree "/a/b/d" None in
  return_ok_unit

(* store_copy *)
let should_run_store_copy_kernel ~version () =
  let open Lwt_syntax in
  let from_path = "/a/b" in
  let from_path_location = 100 in
  let to_path = "/a/b/c" in
  let to_path_location = from_path_location + String.length from_path in
  let module_ =
    Format.sprintf
      {|
(module
  (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
  (import "smart_rollup_core" "store_copy" (func $store_move (type 0)))
  (func (export "kernel_run")
    (local i32)
    (call $store_move
      (i32.const %d)
      (i32.const %d)
      (i32.const %d)
      (i32.const %d))
    drop)
  (memory (;0;) 17)
  (export "memory" (memory 0))
  (data (;0;) (i32.const %d) "%s%s"))
    |}
      (* store_move args *)
      from_path_location
      (String.length from_path)
      to_path_location
      (String.length to_path)
      (* data block *)
      from_path_location
      from_path
      to_path
  in
  let* tree = initial_tree ~version ~from_binary:false module_ in
  let* tree = add_value tree ["a"; "b"] ~content:"ab" in
  let* tree = add_value tree ["a"; "b"; "c"] ~content:"abc" in
  let* tree = add_value tree ["a"; "b"; "d"] ~content:"abd" in
  let* tree = add_value tree ["e"; "f"] ~content:"ef" in
  let* state_before_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  assert (not @@ is_stuck state_before_first_message) ;
  (* consume a message and run kernel_run until we need next message *)
  let* tree = set_empty_inbox_step 0l tree in
  let* tree = eval_until_input_or_reveal_requested tree in
  let* state_after_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  assert (not @@ is_stuck state_after_first_message) ;
  (* Test that values have been copied, and that original location has been
     preserved where it is not overwritten. *)
  let* () = assert_store_value tree "/a/b/c" (Some "ab") in
  let* () = assert_store_value tree "/a/b/c/c" (Some "abc") in
  let* () = assert_store_value tree "/a/b/c/d" (Some "abd") in
  let* () = assert_store_value tree "/a/b/d" (Some "abd") in
  let* () = assert_store_value tree "/e/f" (Some "ef") in
  let* () = assert_store_value tree "/a/b" (Some "ab") in
  return_ok_unit

let nop_module import_name import_params import_results =
  Format.sprintf
    {|
(module
 (import "smart_rollup_core" "%s"
         (func $%s (param %s) (result %s)))
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
    (nop)))
  |}
    import_name
    import_name
    (String.concat " " import_params)
    (String.concat " " import_results)

(* Note that import_name, import_params and import_results could be probably
   read from the host function definition directly in {Host_funcs}, this would
   ensure the error does not come from an invalid import type. *)
let try_availability_above_v1_only ~version import_name import_params
    import_results () =
  let open Lwt_syntax in
  let* tree =
    initial_tree
      ~version
      ~from_binary:false
      (nop_module import_name import_params import_results)
  in
  let* tree = set_empty_inbox_step 0l tree in
  let* tree = eval_until_input_or_reveal_requested tree in
  let* state = Wasm.Internal_for_tests.get_tick_state tree in
  let predicate state =
    match version with
    | Wasm_pvm_state.V0 -> is_stuck state
    | V1 | V2 | V3 | V4 | V5 | V6 -> not (is_stuck state)
  in
  assert (predicate state) ;
  Lwt_result_syntax.return_unit

let try_availability_above_v6_only ~version import_name import_params
    import_results () =
  let open Lwt_syntax in
  let* tree =
    initial_tree
      ~version
      ~from_binary:false
      (nop_module import_name import_params import_results)
  in
  let* tree = set_empty_inbox_step 0l tree in
  let* tree = eval_until_input_or_reveal_requested tree in
  let* state = Wasm.Internal_for_tests.get_tick_state tree in
  let predicate state =
    match version with
    | V0 | V1 | V2 | V3 | V4 | V5 -> is_stuck state
    | V6 -> not (is_stuck state)
  in
  assert (predicate state) ;
  Lwt_result_syntax.return_unit

let try_run_store_get_hash ~version =
  try_availability_above_v1_only
    ~version
    "__internal_store_get_hash"
    ["i32"; "i32"; "i32"; "i32"]
    ["i32"]

let try_run_store_delete_value ~version =
  try_availability_above_v1_only
    ~version
    "store_delete_value"
    ["i32"; "i32"]
    ["i32"]

let try_run_store_create ~version =
  try_availability_above_v1_only
    ~version
    "store_create"
    ["i32"; "i32"; "i32"]
    ["i32"]

let try_run_ec_pairing_check ~version =
  try_availability_above_v6_only
    ~version
    "ec_pairing_check_bls12_381"
    ["i32"; "i32"; "i32"; "i32"]
    ["i32"]

let test_modify_read_only_storage_kernel ~version () =
  let open Lwt_syntax in
  let module_ =
    {|
(module
 (import "smart_rollup_core" "store_write"
         (func $store_write (param i32 i32 i32 i32 i32) (result i32)))
 (import "smart_rollup_core" "store_delete"
         (func $store_delete (param i32 i32) (result i32)))
 (import "smart_rollup_core" "store_move"
         (func $store_move (param i32 i32 i32 i32) (result i32)))
 (import "smart_rollup_core" "store_copy"
         (func $store_copy (param i32 i32 i32 i32) (result i32)))
 ;; Durable keys
 (data (i32.const 100) "/readonly/kernel/boot.wasm")
 (memory 1)
 (export "mem" (memory 0))
 (func $assert_readonly_error
       (param $return_val i32)
       (local.get $return_val)
       (i32.const -9)
       (i32.ne)
       (if (then unreachable)))

 (func (export "kernel_run")
       (local $fallback_kernel_address i32)
       (local $fallback_kernel_length i32)
       (local $kernel_address i32)
       (local $kernel_length i32)

       (local.set $fallback_kernel_address (i32.const 100))
       (local.set $fallback_kernel_length (i32.const 26))
       (local.set $kernel_address (i32.const 109))
       (local.set $kernel_length (i32.const 17))

       ;; try writing "/readonly/kernel/boot.wasm"
       ;;          to "/readonly/kernel/boot.wasm"
       (call $assert_readonly_error
             (call $store_write (local.get $fallback_kernel_address)
                                (local.get $fallback_kernel_length)
                                (i32.const 0)
                                (local.get $fallback_kernel_address)
                                (local.get $fallback_kernel_length)))

       ;; try deleting "/readonly/kernel/boot.wasm"
       (call $assert_readonly_error
             (call $store_delete (local.get $fallback_kernel_address)
                                 (local.get $fallback_kernel_length)))

       ;; try copying "/readonly/kernel/boot.wasm" to "" (root of durable)
       (call $assert_readonly_error
             (call $store_copy (local.get $fallback_kernel_address)
                               (local.get $fallback_kernel_length)
                               (local.get $fallback_kernel_address)
                               (i32.const 0)))

       ;; try moving "/kernel/boot.wasm"
       ;;     to "/readonly/kernel/boot.wasm"
       (call $assert_readonly_error
             (call $store_move (local.get $kernel_address)
                               (local.get $kernel_length)
                               (local.get $fallback_kernel_address)
                               (local.get $fallback_kernel_length)))
       (nop))
 )
  |}
  in
  let* tree = initial_tree ~version ~from_binary:false module_ in
  (* The kernel is not expected to fail, the PVM should not have stuck state on.
      *)
  let* state = Wasm.Internal_for_tests.get_tick_state tree in
  assert (not @@ is_stuck state) ;
  let* stuck_flag = has_stuck_flag tree in
  assert (not stuck_flag) ;
  (* We first evaluate the kernel normally, and it shouldn't fail. *)
  let* tree = set_empty_inbox_step 0l tree in
  let* tree = eval_until_input_or_reveal_requested tree in
  (* The kernel is not expected to fail, the PVM should not have stuck state on.
      *)
  let* state = Wasm.Internal_for_tests.get_tick_state tree in
  assert (not @@ is_stuck state) ;
  let* stuck_flag = has_stuck_flag tree in
  assert (not stuck_flag) ;
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
            "collect"
            (return ())
            (function Collect -> Some () | _ -> None)
            (fun () -> Collect);
        ])
  in
  (* Serves to encode the `Input_requested` status. *)
  let* tree =
    Encodings_util.Tree_encoding_runner.encode
      (Tezos_tree_encoding.scope ["wasm"] state_encoding)
      Collect
      tree
  in
  (* Since we start directly after reading an inbox. *)
  let* previous_top_level_call =
    Encodings_util.Tree_encoding_runner.decode
      (Tezos_tree_encoding.value ["pvm"; "last_top_level_call"] Data_encoding.n)
      tree
  in

  let new_last_top_level_call = Z.(max_tick + previous_top_level_call) in

  let* tree =
    Encodings_util.Tree_encoding_runner.encode
      (Tezos_tree_encoding.value ["wasm"; "current_tick"] Data_encoding.n)
      new_last_top_level_call
      tree
  in
  let* tree = Wasm.Internal_for_tests.reset_reboot_counter tree in
  let* tree =
    Encodings_util.Tree_encoding_runner.encode
      (Tezos_tree_encoding.value ["pvm"; "last_top_level_call"] Data_encoding.n)
      new_last_top_level_call
      tree
  in
  (* The kernel had read three inputs (SOL, Info_per_level and EOL). *)
  let* tree =
    Encodings_util.Tree_encoding_runner.encode
      (Tezos_tree_encoding.value
         ["wasm"; "input"]
         Wasm_pvm_sig.input_info_encoding)
      {
        inbox_level =
          Stdlib.Option.get (Tezos_base.Bounded.Non_negative_int32.of_value 0l);
        message_counter = Z.(succ one);
      }
      tree
  in

  (* The input buffer is being cleared. *)
  let* tree =
    Encodings_util.Tree.remove tree ["value"; "pvm"; "buffers"; "input"]
  in
  let* tree =
    Encodings_util.Tree_encoding_runner.encode
      (Tezos_tree_encoding.value
         ["value"; "pvm"; "buffers"; "input"; "length"]
         Data_encoding.n)
      Z.zero
      tree
  in
  let* tree =
    Encodings_util.Tree_encoding_runner.encode
      (Tezos_tree_encoding.value
         ["value"; "pvm"; "buffers"; "input"; "head"]
         Data_encoding.n)
      Z.zero
      tree
  in

  (* The kernel will have been set as the fallback kernel. *)
  let* durable =
    Encodings_util.Tree_encoding_runner.decode
      (Tezos_tree_encoding.scope ["durable"] Durable.encoding)
      tree
  in
  let* durable =
    Durable.copy_tree_exn
      durable
      ~edit_readonly:true
      Constants.kernel_key
      Constants.kernel_fallback_key
  in
  (* The reboot flag has been set to reboot after the collecting phase. *)
  let* durable =
    Durable.write_value_exn
      ~edit_readonly:true
      durable
      Constants.reboot_flag_key
      0L
      ""
  in
  (* Entering a new collecting phase, the reboot counter is the succ
     of max reboot (to authorize the reboot after the collecting
     phase). *)
  let* durable =
    Durable.set_value_exn
      ~edit_readonly:true
      durable
      Constants.reboot_counter_key
      Data_encoding.(
        Binary.to_string_exn
          Data_encoding_utils.Little_endian.int32
          Int32.(succ (Z.to_int32 Constants.maximum_reboots_per_input)))
  in
  Encodings_util.Tree_encoding_runner.encode
    (Tezos_tree_encoding.scope ["durable"] Durable.encoding)
    durable
    tree

let test_snapshotable_state ~version () =
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
  let*! tree = initial_tree ~version ~from_binary:false module_ in
  let*! state = Wasm.Internal_for_tests.get_tick_state tree in
  let* () =
    match state with
    | Collect -> return_unit
    | _ ->
        failwith
          "Unexpected state at input requested: %a"
          Wasm_utils.pp_state
          state
  in
  (* The "wasm"/"modules" key should not exist after the snapshot, since it
     contains the instance of the module after its evaluation. *)
  let*! wasm_tree_exists =
    Encodings_util.Tree.mem_tree tree ["wasm"; "modules"]
  in
  let*! wasm_value_exists = Encodings_util.Tree.mem tree ["wasm"; "modules"] in
  assert (not (wasm_tree_exists || wasm_value_exists)) ;
  (* Set a new input should go back to decoding *)
  let*! tree = set_empty_inbox_step 1l tree in
  let*! state = Wasm.Internal_for_tests.get_tick_state tree in
  match state with
  | Snapshot -> return_unit
  | _ ->
      failwith
        "Unexpected state after set_empty_inbox_step: %a"
        Wasm_utils.pp_state
        state

let test_rebuild_snapshotable_state ~version () =
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
  let*! tree = initial_tree ~version ~from_binary:false module_ in
  let*! tree = set_empty_inbox_step 0l tree in
  (* First evaluate until the snapshotable state. *)
  let*! tree_after_eval = eval_to_snapshot tree in
  (* From out initial tree, let's rebuild the expected snapshotable state as the
     Fast Node would do. This works because the kernel doesn't change anything. *)
  let*! rebuilded_tree = build_snapshot_wasm_state_from_set_input tree in

  (* Remove the "wasm" key from the tree after evaluation, and try to rebuild it
     as a snapshot. This would take into account any change in the input and
     output buffers and the durable storage. *)
  let*! tree_without_wasm = Encodings_util.Tree.remove tree ["wasm"] in
  let*! rebuilded_tree_without_wasm =
    build_snapshot_wasm_state_from_set_input tree_without_wasm
  in
  (* Both hash should be exactly the same. *)
  let hash_tree_after_eval = Encodings_util.Tree.hash tree_after_eval in
  let hash_rebuilded_tree = Encodings_util.Tree.hash rebuilded_tree in
  assert (Context_hash.equal hash_tree_after_eval hash_rebuilded_tree) ;

  (* The hash of the tree rebuilded from the evaluated one without the "wasm"
     part should also be exactly the same. *)
  let hash_rebuilded_tree_without_wasm =
    Encodings_util.Tree.hash rebuilded_tree_without_wasm
  in
  assert (
    Context_hash.equal hash_tree_after_eval hash_rebuilded_tree_without_wasm) ;

  (* To be sure, let's try to evaluate a new input from these two, either by the
     regular tree or the snapshoted one. *)
  let*! input_step_from_eval = set_empty_inbox_step 1l tree_after_eval in
  let*! input_step_from_snapshot = set_empty_inbox_step 1l rebuilded_tree in

  (* Their hash should still be the same, but the first test should have caught
     that. *)
  let hash_input_tree_after_eval =
    Encodings_util.Tree.hash input_step_from_eval
  in
  let hash_input_rebuilded_tree =
    Encodings_util.Tree.hash input_step_from_snapshot
  in

  assert (
    Context_hash.equal hash_input_tree_after_eval hash_input_rebuilded_tree) ;
  return_unit

let test_unkown_host_function_truncated ~version () =
  let open Lwt_result_syntax in
  let smart_rollup_core = "smart_rollup_core" in
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
          (func (export "kernel_run")
            (unreachable)
          )
        )
    |}
      smart_rollup_core
      unknown_function
  in
  (* Let's first init the tree to compute. *)
  let*! tree = initial_tree ~version ~from_binary:false module_ in
  (* The tree should be in snapshot state by default, hence in input step. *)
  let*! tree_with_dummy_input = set_empty_inbox_step 0l tree in
  let*! tree_stuck =
    eval_until_input_or_reveal_requested tree_with_dummy_input
  in
  let*! state = Wasm.Internal_for_tests.get_tick_state tree_stuck in
  (* The message as originally outputed before being
     truncated. *)
  let reason =
    Format.sprintf "Unexpected import: %s.%s" smart_rollup_core unknown_function
  in
  (* Let's check the message will be indeed truncated, otherwise this test is
     not relevant. *)
  assert (String.length reason > Wasm_pvm_errors.messages_maximum_size) ;
  (* The final reason for being stuck should be truncated after
     [Wasm_pvm_errors.messages_maximum_size]. *)
  let reason = String.sub reason 0 Wasm_pvm_errors.messages_maximum_size in
  assert (is_stuck ~step:`No_fallback_link ~reason state) ;
  return_unit

let test_bulk_noops ~version () =
  let open Lwt.Syntax in
  let module_ =
    {|
      (module
        (memory 0)
        (export "mem" (memory 0))
        (func (export "kernel_run")
          (nop)
        )
      )
    |}
  in
  let* base_tree = initial_tree ~version ~ticks_per_snapshot:500L module_ in
  let* base_tree = set_empty_inbox_step 0l base_tree in

  let rec goto_snapshot ticks tree_slow =
    let* tree_fast, _ =
      Wasm.compute_step_many
        ~wasm_entrypoint:Constants.wasm_entrypoint
        ~max_steps:ticks
        base_tree
    in
    let* tree_slow =
      Wasm.compute_step ~wasm_entrypoint:Constants.wasm_entrypoint tree_slow
    in
    assert (Context_hash.(Encodings_util.Tree.(hash tree_fast = hash tree_slow))) ;

    let* stuck_flag = has_stuck_flag tree_fast in
    assert (not stuck_flag) ;

    let* state = Wasm.Internal_for_tests.get_tick_state tree_slow in
    if state = Snapshot || state = Collect then Lwt.return (ticks, tree_slow)
    else goto_snapshot (Int64.succ ticks) tree_slow
  in

  let* _ticks, snapshot = goto_snapshot 1L base_tree in
  let* snapshot_info = Wasm_utils.Wasm.get_info snapshot in
  assert (snapshot_info.input_request = Input_required) ;

  Lwt_result_syntax.return_unit

let test_durable_store_io ~version () =
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
          (import "smart_rollup_core" "store_read"
            (func $store_read (type 0)))
          (import "smart_rollup_core" "store_write"
            (func $store_write (type 0)))
          (func (export "kernel_run")
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
  let*! tree = initial_tree ~version ~from_binary:false modul in

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
  let*! tree = set_empty_inbox_step 0l tree in
  let*! tree = eval_until_input_or_reveal_requested tree in
  let*! stuck_flag = has_stuck_flag tree in
  assert (not stuck_flag) ;

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

let reveal_upgrade_kernel () =
  let path = "/kernel/boot.wasm" in
  let path_start = 23 in
  let path_len = String.length path in
  let hash_start = path_start + path_len in
  let buffer_start = hash_start + 33 in
  (* 33 bytes for reveal hash *)
  Format.sprintf
    {|
(module
  (import "smart_rollup_core" "store_write"
    (func $store_write (param i32 i32 i32 i32 i32) (result i32)))
  (import "smart_rollup_core" "store_delete"
    (func $store_delete (param i32 i32) (result i32)))
  (import "smart_rollup_core" "reveal_preimage"
    (func $reveal_preimage (param i32 i32 i32 i32) (result i32)))
  (func (export "kernel_run")
    (local $preimage_size i32)

    (local $hash_start i32)
    (local $buffer_start i32)
    (local $path_start i32)
    (local $path_len i32)

    (local.set $path_start (i32.const %s))
    (local.set $path_len (i32.const %s))
    (local.set $hash_start (i32.const %s))
    (local.set $buffer_start (i32.const %s))

    (local.set $preimage_size
     (call $reveal_preimage (local.get $hash_start)
                            (i32.const 33)
                            (local.get $buffer_start)
                            ;; buffer size
                            (i32.const 4096)))

    (call $store_delete (local.get $path_start)
                        (local.get $path_len))

    (call $store_write (local.get $path_start)
                       (local.get $path_len)
                       ;; write offset in store
                       (i32.const 0)
                       (local.get $buffer_start)
                       (local.get $preimage_size)))
  (table 1 1 funcref)
  (memory 17)
  (export "memory" (memory 0))
  (data (i32.const %s) "%s"))
|}
    (* locals *)
    (Int.to_string path_start)
    (Int.to_string path_len)
    (Int.to_string hash_start)
    (Int.to_string buffer_start)
    (* data *)
    (Int.to_string path_start)
    path

let assert_fallback_kernel tree expected_kernel =
  let open Lwt.Syntax in
  let* durable = wrap_as_durable_storage tree in
  let durable = Durable.of_storage_exn durable in
  let* value = Durable.find_value durable Constants.kernel_fallback_key in
  let+ value =
    Option.map_s Tezos_lazy_containers.Chunked_byte_vector.to_string value
  in
  assert (Option.equal String.equal value expected_kernel)

let assert_kernel tree expected_kernel =
  let open Lwt.Syntax in
  let* durable = wrap_as_durable_storage tree in
  let durable = Durable.of_storage_exn durable in
  let* value = Durable.find_value durable Constants.kernel_key in
  let+ value =
    Option.map_s Tezos_lazy_containers.Chunked_byte_vector.to_string value
  in
  assert (Option.equal String.equal value expected_kernel)

let test_reveal_upgrade_kernel_ok ~version () =
  let open Lwt_result_syntax in
  let*! modul = wat2wasm @@ reveal_upgrade_kernel () in
  (* Let's first init the tree to compute. *)
  let*! tree = initial_tree ~version ~from_binary:true modul in
  let*! state_before_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  assert (not @@ is_stuck state_before_first_message) ;
  (* At this point, the kernel should be installed, but not as fallback *)
  let*! () = assert_kernel tree @@ Some modul in
  let*! () = assert_fallback_kernel tree None in
  (* Run the kernel, it should request a preimage reveal *)
  let*! tree = set_empty_inbox_step 0l tree in
  let*! tree = eval_until_input_or_reveal_requested tree in
  (* At this point, the kernel should be installed, including as fallback *)
  let*! () = assert_kernel tree @@ Some modul in
  let*! () = assert_fallback_kernel tree @@ Some modul in
  (* Check that reveal is requested *)
  let*! state_after_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  assert (not @@ is_stuck state_after_first_message) ;
  let*! info = Wasm.get_info tree in
  let* () =
    let open Wasm_pvm_state in
    match info.Wasm_pvm_state.input_request with
    | Wasm_pvm_state.Reveal_required req -> (
        match Wasm_pvm_state.Compatibility.of_current_opt req with
        | Some (Reveal_raw_data hash) ->
            (* The PVM has reached a point where it’s asking for some
               preimage. Since the memory is left blank, we are looking
               for the zero hash *)
            let zero_hash = String.make 33 '\x00' in
            assert (hash = zero_hash) ;
            return_unit
        | _ -> assert false)
    | No_input_required | Input_required -> assert false
  in
  let new_kernel =
    {|
(module
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
       (nop)
       )
 )
|}
  in
  let*! preimage = wat2wasm new_kernel in
  let*! tree = Wasm.reveal_step (Bytes.of_string preimage) tree in
  let*! tree = eval_until_input_or_reveal_requested tree in
  let*! state_after_reveal = Wasm.Internal_for_tests.get_tick_state tree in
  assert (not @@ is_stuck state_after_reveal) ;
  (* At this point, the new_kernel should be installed, the old as fallback *)
  let*! () = assert_kernel tree @@ Some preimage in
  let*! () = assert_fallback_kernel tree @@ Some modul in
  (* run until proper input requested *)
  let*! tree = eval_until_input_or_reveal_requested tree in
  let*! state_after_reveal = Wasm.Internal_for_tests.get_tick_state tree in
  assert (not @@ is_stuck state_after_reveal) ;
  (* At this point, the new_kernel should be installed, including as fallback *)
  let*! () = assert_kernel tree @@ Some preimage in
  let*! () = assert_fallback_kernel tree @@ Some modul in
  (* run with new input, this should be the new kernel *)
  let*! tree = set_empty_inbox_step 2l tree in
  let*! tree = eval_until_input_or_reveal_requested tree in
  let*! state_after_second_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  assert (not @@ is_stuck state_after_second_message) ;
  (* There should have been no issues decoding the new kernel *)
  let*! () = assert_kernel tree @@ Some preimage in
  let*! () = assert_fallback_kernel tree @@ Some preimage in
  return_unit

let test_reveal_upgrade_kernel_fallsback_on_error ~version ~binary
    invalid_kernel () =
  let open Lwt_result_syntax in
  let*! modul = wat2wasm @@ reveal_upgrade_kernel () in
  (* Let's first init the tree to compute. *)
  let*! tree = initial_tree ~version ~from_binary:true modul in
  let*! tree = eval_until_input_or_reveal_requested tree in
  let*! state_before_first_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  assert (not @@ is_stuck state_before_first_message) ;
  (* At this point, the kernel should be installed, but not as fallback *)
  let*! () = assert_kernel tree @@ Some modul in
  let*! () = assert_fallback_kernel tree None in
  let*! preimage =
    if binary then Lwt.return invalid_kernel else wat2wasm invalid_kernel
  in
  (* Run the kernel, it should request a preimage reveal *)
  let run_installer tree level =
    let*! tree = set_empty_inbox_step level tree in
    let*! tree = eval_until_input_or_reveal_requested tree in
    (* At this point, the kernel should be installed, including as fallback *)
    let*! () = assert_kernel tree @@ Some modul in
    let*! () = assert_fallback_kernel tree @@ Some modul in
    (* Check that reveal is requested *)
    let*! state_after_first_message =
      Wasm.Internal_for_tests.get_tick_state tree
    in
    assert (not @@ is_stuck state_after_first_message) ;
    let*! info = Wasm.get_info tree in
    let* () =
      let open Wasm_pvm_state in
      match info.Wasm_pvm_state.input_request with
      | Wasm_pvm_state.Reveal_required req -> (
          match Wasm_pvm_state.Compatibility.of_current_opt req with
          | Some (Reveal_raw_data hash) ->
              (* The PVM has reached a point where it’s asking for some
                 preimage. Since the memory is left blank, we are looking
                 for the zero hash *)
              let zero_hash = String.make 33 '\000' in
              assert (hash = zero_hash) ;
              return_unit
          | _ -> assert false)
      | No_input_required | Input_required -> assert false
    in
    let*! tree = Wasm.reveal_step (Bytes.of_string preimage) tree in
    let*! state_after_reveal = Wasm.Internal_for_tests.get_tick_state tree in
    assert (not @@ is_stuck state_after_reveal) ;
    let*! tree = eval_until_input_or_reveal_requested tree in
    let*! state_after_reveal = Wasm.Internal_for_tests.get_tick_state tree in
    assert (not @@ is_stuck state_after_reveal) ;
    (* At this point, the new_kernel should be installed, including as fallback *)
    let*! () = assert_kernel tree @@ Some preimage in
    let*! () = assert_fallback_kernel tree @@ Some modul in
    Lwt.return_ok tree
  in
  let* tree = run_installer tree 0l in
  (* run with new input, this should be the new kernel *)
  let*! tree = set_empty_inbox_step 2l tree in
  let*! tree = eval_until_input_or_reveal_requested tree in
  let*! state_after_second_message =
    Wasm.Internal_for_tests.get_tick_state tree
  in
  let has_upgrade_error_flag tree =
    let*! durable = wrap_as_durable_storage tree in
    let durable = Durable.of_storage_exn durable in
    let*! found_error =
      Durable.find_value durable Constants.upgrade_error_flag_key
    in
    Lwt.return_ok @@ Option.is_some found_error
  in
  (* The pvm should have an upgrade error, but not be stuck *)
  assert (not @@ is_stuck state_after_second_message) ;
  let* has_upgrade_error = has_upgrade_error_flag tree in
  assert has_upgrade_error ;
  (* The kernel is set to the fallback one *)
  let*! () = assert_kernel tree @@ Some modul in
  let*! () = assert_fallback_kernel tree @@ Some modul in
  (* The next evaluation should delete the upgrade error. *)
  let* tree = run_installer tree 3l in
  let* has_upgrade_error = has_upgrade_error_flag tree in
  assert (not has_upgrade_error) ;
  return_unit

let test_pvm_reboot_counter ~version ~pvm_max_reboots () =
  let open Lwt_result_syntax in
  let reboot_kernel =
    {|
(module
 (import "smart_rollup_core" "store_write"
     (func $store_write (param i32 i32 i32 i32 i32) (result i32)))
 (data (i32.const 10) "/kernel/env/reboot")
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
  (call $store_write (i32.const 10) (i32.const 18) (i32.const 0) (i32.const 0) (i32.const 0))))
    |}
  in
  let read_reboot_counter_env tree =
    let*! durable = wrap_as_durable_storage tree in
    let durable = Durable.of_storage_exn durable in
    let*! value = Durable.find_value durable Constants.reboot_counter_key in
    match value with
    | None ->
        failwith
          "Evaluation error: couldn't find the reboot counter in the durable \
           storage"
    | Some value ->
        let*! value =
          Tezos_lazy_containers.Chunked_byte_vector.to_bytes value
        in
        (* WASM Values in memories are encoded in little-endian order. *)
        return @@ Bytes.get_int32_le value 0
  in

  let rec check_counter tree i =
    let*! info = Wasm.get_info tree in
    if pvm_max_reboots < i then (
      (* We have finished looping, the PVM has prevented another
         execution of kernel_run *)
      assert (info.input_request = Input_required) ;
      let* reboot_counter = read_reboot_counter_env tree in
      assert (reboot_counter = Int32.(succ pvm_max_reboots)) ;
      return_unit)
    else (
      assert (info.input_request = No_input_required) ;
      let* reboot_counter = read_reboot_counter_env tree in
      assert (reboot_counter = Int32.(sub pvm_max_reboots i)) ;
      let*! tree = eval_to_snapshot tree in
      check_counter tree Int32.(succ i))
  in

  let*! tree =
    initial_tree
      ~version
      ~max_reboots:(Z.of_int32 pvm_max_reboots)
      ~from_binary:false
      reboot_kernel
  in
  let*! tree = set_empty_inbox_step 0l tree in

  check_counter tree 0l

let test_kernel_reboot_gen ?hooks ~version ~reboots ~expected_reboots
    ~pvm_max_reboots () =
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
 (import "smart_rollup_core" "store_write"
         (func $store_write (param i32 i32 i32 i32 i32) (result i32)))
 (import "smart_rollup_core" "store_read"
         (func $store_read (param i32 i32 i32 i32 i32) (result i32)))
 (import "smart_rollup_core" "store_has"
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

 (func (export "kernel_run")
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
      (* `kernel_run` function *)
      data_offset_start
      (* == reboot_key's offset *)
      reboot_key_length
      reboot_flag_in_memory_offset
      reboots
  in
  (* Let's first init the tree to compute. *)
  let*! tree =
    initial_tree
      ~version
      ~max_reboots:(Z.of_int32 pvm_max_reboots)
      ~from_binary:false
      reboot_module
  in
  let*! tree = set_empty_inbox_step 0l tree in
  let*! tree = eval_until_input_or_reveal_requested tree ?hooks in
  let*! durable = wrap_as_durable_storage tree in
  let durable = Durable.of_storage_exn durable in
  let*! too_many_reboot =
    Durable.find_value durable Constants.too_many_reboot_flag_key
  in
  let too_many_reboot = Option.is_some too_many_reboot in
  (* If the expected number of reboots from the PVM is lower than the maximum
     asked by the kernel itself, this should lead to a stuck state with
     `Too_many_reboots`. *)
  let*! stuck_flag = has_stuck_flag tree in
  if reboots <= pvm_max_reboots then assert (not stuck_flag)
  else assert too_many_reboot ;

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

let test_kernel_reboot ~version () =
  (* The kernel doesn't accept more than 10 reboots between two inputs, this test will succeed. *)
  test_kernel_reboot_gen
    ~version
    ~reboots:5l
    ~expected_reboots:5l
    ~pvm_max_reboots:10l
    ()

let test_kernel_reboot_failing ~version () =
  (* The kernel doesn't accept more than 10 reboots between two inputs, it will
     then fail after 10. *)
  test_kernel_reboot_gen
    ~version
    ~reboots:15l
    ~expected_reboots:10l
    ~pvm_max_reboots:10l
    ()

let test_kernel_reboot_hook ~version () =
  let ctr = ref 0l in
  let hooks =
    Hooks.(
      no_hooks
      |> on_pvm_reboot (fun _ticks ->
             ctr := Int32.succ !ctr ;
             Lwt.return_unit))
  in
  let reboots = 5l in
  let res =
    test_kernel_reboot_gen
      ~version
      ~reboots
      ~expected_reboots:reboots
      ~pvm_max_reboots:10l
      ~hooks
      ()
  in
  (* 5 reboots and 1 yield for 6 hook calls *)
  assert (!ctr = Int32.succ reboots) ;
  res

(* Set a certain number `n` of dummy inputs and check the scheduling is
   consistent:
   - `Collect` as initial state
   - `Collect` after Start_of_level
   - `Collect` after `n` internal messages
   - `Eval` after End_of_level
*)
let test_set_inputs number_of_inputs level tree =
  let open Lwt_syntax in
  let next_message_counter = new_message_counter () in

  (* [set_input_and_check] takes a function that from a counter returns a tree,
     and the expected kind of scheduling status after `set_input`. *)
  let set_input_and_check set_input =
    let counter = next_message_counter () in
    let+ tree = set_input counter in
    tree
  in

  (* First set `Start_of_level` and check the result will be `Collect`: still
     waiting for inputs. *)
  let* tree_with_sol =
    set_input_and_check (fun _ -> set_sol_input level tree)
  in

  (* Then, adds `number_of_inputs` messages to the PVM, and check the scheduling
     status after each message. *)
  let inputs =
    List.init ~when_negative_length:[] number_of_inputs string_of_int
    |> Stdlib.Result.get_ok
  in
  let* tree_with_inputs =
    List.fold_left_s
      (fun tree input ->
        set_input_and_check (fun counter ->
            set_internal_message level counter input tree))
      tree_with_sol
      inputs
  in

  (* Finally, set `End_of_level`. The state should be padding. *)
  let* tree =
    set_input_and_check (fun counter ->
        set_eol_input level counter tree_with_inputs)
  in

  let+ state = Wasm.Internal_for_tests.get_tick_state tree in
  assert (state = Padding) ;
  tree

let test_inbox_cleanup ~version () =
  let open Lwt_syntax in
  let check_messages_count tree count =
    let+ buffer = Wasm.Internal_for_tests.get_input_buffer tree in
    let inputs_before_exec =
      Tezos_lazy_containers.Lazy_vector.Mutable.ZVector.num_elements buffer
    in
    assert (inputs_before_exec = Z.of_int count)
  in
  let module_ =
    {|
      (module
        (memory 0)
        (export "mem" (memory 0))
        (func (export "kernel_run")
          (nop)
        )
      )
    |}
  in
  let max_tick = 1000L in
  let* tree =
    initial_tree
      ~version
      ~ticks_per_snapshot:max_tick
      ~from_binary:false
      module_
  in
  let* tree = set_empty_inbox_step 0l tree in
  (* Before executing: EOL, Info_per_level and SOL. *)
  let* () = check_messages_count tree 3 in
  (* Go to the very last [Padding] state. *)
  let* tree, _ =
    Wasm.compute_step_many
      ~wasm_entrypoint:Constants.wasm_entrypoint
      ~max_steps:Int64.(pred max_tick)
      tree
  in
  (* Before yielding: EOL, Info_per_level and SOL still, since the module does
     not read the content of the inbox. *)
  let* () = check_messages_count tree 3 in
  (* Yield with one more step. *)
  let* tree =
    Wasm.compute_step ~wasm_entrypoint:Constants.wasm_entrypoint tree
  in
  (* After yielding, the inbox has been cleared. *)
  let* () = check_messages_count tree 0 in
  Lwt_result_syntax.return_unit

let test_wasm_entrypoint ~version () =
  let open Lwt_syntax in
  let module_ =
    {|
      (module
        (memory 0)
        (export "mem" (memory 0))
        (func (export "foo")
          (nop)
        )
      )
    |}
  in
  let max_tick = 1000L in
  let* tree =
    initial_tree
      ~version
      ~ticks_per_snapshot:max_tick
      ~from_binary:false
      module_
  in
  let* tree = set_empty_inbox_step 0l tree in
  let* tree, _ =
    Wasm.compute_step_many
      ~wasm_entrypoint:"foo"
      ~max_steps:Int64.(pred max_tick)
      tree
  in
  let* status = Wasm.Internal_for_tests.get_tick_state tree in
  assert (not @@ is_stuck status) ;
  Lwt_result_syntax.return_unit

let test_scheduling_multiple_inboxes ~version input_numbers =
  let open Lwt_result_syntax in
  let module_ =
    {|
      (module
        (memory 0)
        (export "mem" (memory 0))
        (func (export "kernel_run")
          (nop)
        )
      )
    |}
  in
  let*! initial_tree = initial_tree ~version ~from_binary:false module_ in
  let+ (_ : Wasm.tree) =
    List.fold_left_i_es
      (fun level tree input_number ->
        let*! tree = test_set_inputs input_number (Int32.of_int level) tree in
        let*! info_during_inputs = Wasm.get_info tree in
        (* Right after `set_input`, no new input is required and the current
           phase is Collect. *)
        assert (info_during_inputs.input_request = No_input_required) ;

        let*! tree = eval_to_snapshot tree in
        let*! info_end_of_inputs = Wasm.get_info tree in
        (* We evaluate until a snapshot, we now go into the `Eval` phase. *)
        assert (info_end_of_inputs.input_request = No_input_required) ;

        (* We finally evaluate an `Eval` phase that doesn't ask for reboot: it
           should now wait for inputs and be in `Collect` phase. *)
        let*! tree = eval_to_snapshot tree (* Snapshot *) in
        let*! info_after_eval = Wasm.get_info tree in
        assert (info_after_eval.input_request = Input_required) ;
        return tree)
      initial_tree
      input_numbers
  in
  ()

let test_scheduling_one_inbox ~version () =
  test_scheduling_multiple_inboxes ~version [10]

let test_scheduling_five_inboxes ~version () =
  test_scheduling_multiple_inboxes ~version [10; 5; 15; 8; 2]

(* Module only outputting one message. *)
let output_only_module message =
  Format.sprintf
    {|
(module
 (import "smart_rollup_core" "write_output"
         (func $write_output (param i32 i32) (result i32)))
 (data (i32.const 100) "%s")
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
       (call $write_output (i32.const 100) (i32.const %d))
       (drop)
       )
)|}
    message
    (String.length message)

(* Test helper that loads inputs and runs a kernel and test the outboxes after
   each yield of the PVM, up to [max_level]. *)
let eval_and_test_outboxes_gen test_outbox max_level tree =
  let open Lwt_syntax in
  let rec go curr_level tree =
    let* tree = set_empty_inbox_step curr_level tree in
    let* tree = eval_until_input_or_reveal_requested tree in

    (* The PVM shouldn't be stuck, it only outputs one message. *)
    let* status = Wasm.Internal_for_tests.get_tick_state tree in
    assert (not @@ is_stuck status) ;

    (* Lets check the outboxes are consistent and all exist. *)
    let* buffer = Wasm.Internal_for_tests.get_output_buffer tree in
    let* () = test_outbox buffer curr_level in

    (* Take another round of evaluation. *)
    if curr_level >= max_level then return_unit
    else go (Int32.succ curr_level) tree
  in
  go 0l tree

let test_outboxes_at_each_level ~version () =
  let open Lwt_syntax in
  let open Tezos_webassembly_interpreter.Output_buffer in
  let output_message = "output_message" in

  (* Checks that each outbox exists for levels 0 to [outbox_level] *)
  let rec check_outboxes buffer outbox_level =
    let* message = get_message buffer {outbox_level; message_index = Z.zero} in
    assert (message = Bytes.of_string output_message) ;
    if outbox_level <= 0l then return_unit
    else check_outboxes buffer (Int32.pred outbox_level)
  in

  let* tree =
    initial_tree
      ~version
      ~ticks_per_snapshot:1000L (* This kernel takes about 838 ticks to run. *)
      ~from_binary:false
      (output_only_module output_message)
  in
  let* () = eval_and_test_outboxes_gen check_outboxes 5l tree in
  return_ok_unit

let test_outbox_validity_period ~version () =
  let open Lwt_syntax in
  let open Tezos_webassembly_interpreter.Output_buffer in
  let open Tezos_webassembly_interpreter.Output_buffer.Internal_for_tests in
  let output_message = "output_message" in

  let outbox_validity_period = 2l in

  (* Check each outboxes: if the level offset of the output buffer is below the
     currently asked level, the message should be available. If it not
     available, this implies the level is outdsated, and the outbox level is
     below the output buffer's offset *)
  let rec check_outbox buffer outbox_level =
    Lwt.catch
      (fun () ->
        let* message =
          get_message buffer {outbox_level; message_index = Z.zero}
        in
        let level_is_not_outdated =
          Option.fold
            ~none:false
            ~some:(fun first_level -> first_level <= outbox_level)
            (first_level buffer)
        in
        assert (
          message = Bytes.of_string output_message && level_is_not_outdated) ;
        if outbox_level <= 0l then return_unit
        else check_outbox buffer (Int32.pred outbox_level))
      (fun exn ->
        let level_is_outdated =
          Option.fold
            ~none:false
            ~some:(fun first_level -> first_level > outbox_level)
            (first_level buffer)
        in
        assert (level_is_outdated && exn = Outdated_level) ;
        if outbox_level <= 0l then return_unit
        else check_outbox buffer (Int32.pred outbox_level))
  in

  let* tree =
    initial_tree
      ~version
      ~ticks_per_snapshot:1000L (* This kernel takes about 838 ticks to run. *)
      ~from_binary:false
      ~outbox_validity_period
      (output_only_module output_message)
  in
  let* () = eval_and_test_outboxes_gen check_outbox 5l tree in
  return_ok_unit

let tests =
  tztests_with_all_pvms
    [
      ( "Test __internal_store_get_hash available",
        `Quick,
        try_run_store_get_hash );
      ("Test store_delete_value available", `Quick, try_run_store_delete_value);
      ("Test store_create available", `Quick, try_run_store_create);
      ("Test ec_pairing_check available", `Quick, try_run_ec_pairing_check);
      ( "Test unreachable kernel (tick per tick)",
        `Quick,
        fun ~version ->
          test_with_kernel
            Kernels.unreachable_kernel
            (should_boot_unreachable_kernel ~version ~batch_size:1L) );
      ( "Test unreachable kernel (10 ticks at a time)",
        `Quick,
        fun ~version ->
          test_with_kernel
            Kernels.unreachable_kernel
            (should_boot_unreachable_kernel ~version ~batch_size:10L) );
      ( "Test unreachable kernel (in one go)",
        `Quick,
        fun ~version ->
          test_with_kernel
            Kernels.unreachable_kernel
            (should_boot_unreachable_kernel ~version ~batch_size:Int64.max_int)
      );
      ("Test write_debug kernel", `Quick, should_run_debug_kernel);
      ("Test store-has kernel", `Quick, should_run_store_has_kernel);
      ("Test store-list-size kernel", `Quick, should_run_store_list_size_kernel);
      ("Test store-delete kernel", `Quick, should_run_store_delete_kernel);
      ("Test store-move kernel", `Quick, should_run_store_move_kernel);
      ("Test store-copy kernel", `Quick, should_run_store_copy_kernel);
      ( "Test modifying read-only storage fails",
        `Quick,
        test_modify_read_only_storage_kernel );
      ("Test snapshotable state", `Quick, test_snapshotable_state);
      ( "Test rebuild snapshotable state",
        `Quick,
        test_rebuild_snapshotable_state );
      ( "Test Stuck state is truncated on long messages",
        `Quick,
        test_unkown_host_function_truncated );
      ("Test bulk no-ops function properly", `Quick, test_bulk_noops);
      ("Test durable store io", `Quick, test_durable_store_io);
      ( "Test /readonly/kernel/env/reboot_counter",
        `Quick,
        test_pvm_reboot_counter ~pvm_max_reboots:10l );
      ("Test reboot", `Quick, test_kernel_reboot);
      ("Test reboot takes too many reboots", `Quick, test_kernel_reboot_failing);
      ("Test reboot hook", `Quick, test_kernel_reboot_hook);
      ("Test kernel upgrade ok", `Quick, test_reveal_upgrade_kernel_ok);
      ( "Test kernel upgrade fallsback on decoding error",
        `Quick,
        test_reveal_upgrade_kernel_fallsback_on_error
          ~binary:true
          "INVALID WASM!!!" );
      ( "Test kernel upgrade fallsback on linking error",
        `Quick,
        test_reveal_upgrade_kernel_fallsback_on_error
          ~binary:false
          {|
(module
 (import "invalid_module" "write_debug"
         (func $write_debug (param i32 i32))))
|}
      );
      ( "Test kernel upgrade fallsback on initing error",
        `Quick,
        test_reveal_upgrade_kernel_fallsback_on_error
          ~binary:false
          "(module (memory 1))" );
      ( "Test scheduling with 10 inputs in a unique inbox",
        `Quick,
        test_scheduling_one_inbox );
      ( "Test scheduling with 5 inboxes with a different input number",
        `Quick,
        test_scheduling_five_inboxes );
      ("Test inbox clean-up", `Quick, test_inbox_cleanup);
      ( "Test outboxes are created at each level",
        `Quick,
        test_outboxes_at_each_level );
      ( "Test outbox validity period clean-up",
        `Quick,
        test_outbox_validity_period );
      ( "Test can execute arbitrary WASM entrypoint",
        `Quick,
        test_wasm_entrypoint );
    ]

let () =
  Alcotest_lwt.run ~__FILE__ "test lib scoru wasm" [("WASM PVM", tests)]
  |> Lwt_main.run
