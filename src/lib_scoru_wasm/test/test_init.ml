(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

open Tztest
open Tezos_scoru_wasm
open Wasm_utils

let test_memory0_export () =
  let open Lwt_result_syntax in
  (* This module does not export its memory therefore it should fail. *)
  let*! bad_module_tree = initial_tree {|
    (module (memory 1))
  |} in
  let*! bad_module_tree = set_input_step "dummy_input" 0 bad_module_tree in
  let* stuck, _ = eval_until_stuck bad_module_tree in
  assert (
    check_error
      ~expected_kind:`Init
      ~expected_reason:"Module must export memory 0"
      stuck) ;

  (* This module exports its memory should therefore reach the "unreachable"
     trap which is treated below. *)
  let*! good_module_tree =
    initial_tree
      {|
        (module
          (memory 1)
          (export "mem"(memory 0))
          (func (export "kernel_next")
            (unreachable)
          )
        )
      |}
  in
  let*! good_module_tree = set_input_step "dummy_input" 0 good_module_tree in
  let+ stuck, _ = eval_until_stuck good_module_tree in
  assert (
    check_error
      ~expected_kind:`Eval
      ~expected_reason:"unreachable executed"
      stuck)

let test_module_name_size () =
  let open Lwt_result_syntax in
  let build_module size =
    let b = Buffer.create size in
    let rec build size =
      if size = 0 then Buffer.contents b
      else (
        Buffer.add_char b 'a' ;
        build (size - 1))
    in
    Format.sprintf
      {|
        (module
          (memory 1)
          (export "mem"(memory 0))
          (func (export "%s")
            (unreachable)
          )
          (func (export "kernel_next")
            (unreachable)
          )
        )|}
      (build size)
  in
  let*! bad_module_tree = initial_tree (build_module 513) in
  let*! bad_module_tree = set_input_step "dummy_input" 0 bad_module_tree in
  let* stuck, _ = eval_until_stuck bad_module_tree in
  assert (
    check_error
      ~expected_kind:`Decode
      ~expected_reason:"Names cannot exceed 512 bytes"
      stuck) ;
  let*! good_module_tree = initial_tree (build_module 512) in
  let*! good_module_tree = set_input_step "dummy_input" 0 good_module_tree in
  let+ stuck, _ = eval_until_stuck good_module_tree in
  assert (
    check_error
      ~expected_kind:`Eval
      ~expected_reason:"unreachable executed"
      stuck)

let test_imports () =
  let open Lwt_result_syntax in
  let build_module module_name item_name =
    Format.sprintf
      {|
        (module
          (import "%s" "%s"
            (func $%s (param i32 i32 i32 i32) (result i32)))
          (memory 1)
          (export "mem"(memory 0))
          (func (export "kernel_next")
            (unreachable)
          )
        )
      |}
      module_name
      item_name
      item_name
  in
  let bad_module_name = "external_module" in
  let bad_item_name = "f" in
  let*! bad_module_tree =
    initial_tree (build_module bad_module_name bad_item_name)
  in
  let*! bad_module_tree = set_input_step "dummy_input" 0 bad_module_tree in
  let* stuck, _ = eval_until_stuck bad_module_tree in
  let* () =
    let expected_error =
      Wasm_pvm_errors.link_error
        `Module
        ~module_name:bad_module_name
        ~item_name:bad_item_name
    in
    if stuck = expected_error then return_unit
    else failwith "Unexpected stuck state!"
  in
  let good_module_name = "rollup_safe_core" in
  let*! bad_host_func_tree =
    initial_tree (build_module good_module_name bad_item_name)
  in
  let*! bad_host_func_tree =
    set_input_step "dummy_input" 0 bad_host_func_tree
  in
  let* stuck, _ = eval_until_stuck bad_host_func_tree in
  let* () =
    let expected_error =
      Wasm_pvm_errors.link_error
        `Item
        ~module_name:good_module_name
        ~item_name:bad_item_name
    in
    if stuck = expected_error then return_unit
    else failwith "Unexpected stuck state!"
  in
  let good_item_name = "read_input" in
  let*! good_module_tree =
    initial_tree (build_module good_module_name good_item_name)
  in
  let*! good_module_tree = set_input_step "dummy_input" 0 good_module_tree in
  let+ stuck, _ = eval_until_stuck good_module_tree in
  assert (
    check_error
      ~expected_kind:`Eval
      ~expected_reason:"unreachable executed"
      stuck)

let test_host_func_start_restriction () =
  let open Lwt_result_syntax in
  let*! state =
    initial_tree
      {|
        (module
          (import "rollup_safe_core" "write_output"
            (func $write_output (param i32 i32) (result i32))
          )
          (memory 1)
          (export "mem" (memory 0))
          (func $foo
            (call $write_output (i32.const 0) (i32.const 0))
          )
          (start $foo)
          (func (export "kernel_next")
            (unreachable)
          )
        )
    |}
  in
  let*! state = set_input_step "dummy_input" 0 state in
  let+ stuck, _ = eval_until_stuck state in
  assert (
    check_error
      ~expected_kind:`Init
      ~expected_reason:
        "host functions must not access memory during initialisation"
      stuck)

let tests =
  [
    tztest "init requires memory 0 export" `Quick test_memory0_export;
    tztest "names are limited to 512 bytes" `Quick test_module_name_size;
    tztest "imports only PVM host functions" `Quick test_imports;
    tztest
      "host functions are restricted in start"
      `Quick
      test_host_func_start_restriction;
  ]
