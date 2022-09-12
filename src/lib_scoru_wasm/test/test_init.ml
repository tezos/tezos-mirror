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
open Test_encodings_util
open Tezos_scoru_wasm
module Wasm = Wasm_pvm.Make (Tree)

let initial_tree code =
  let open Lwt.Syntax in
  let* empty_tree = empty_tree () in
  let* code = Wasm_utils.wat2wasm code in
  let boot_sector =
    Data_encoding.Binary.to_string_exn
      Gather_floppies.origination_message_encoding
      (Gather_floppies.Complete_kernel (Bytes.of_string code))
  in
  Wasm.Internal_for_tests.initial_tree_from_boot_sector ~empty_tree boot_sector

let eval_until_stuck tree =
  let open Lwt.Syntax in
  let rec go counter tree =
    let* tree = Wasm.compute_step tree in
    let* stuck = Wasm.Internal_for_tests.is_stuck tree in
    match stuck with
    | Some stuck -> Lwt_result.return stuck
    | _ ->
        if counter > 0 then go (pred counter) tree
        else failwith "Failed to get stuck in time"
  in
  go 10000 tree

let test_memory0_export () =
  let open Lwt_result_syntax in
  (* This module does not export its memory therefore it should fail. *)
  let*! bad_module_tree = initial_tree {|
    (module (memory 1))
  |} in
  let* stuck = eval_until_stuck bad_module_tree in
  let* () =
    match stuck with
    | Init_error {explanation = Some "Module must export memory 0"; _} ->
        Lwt_result.return ()
    | _ -> failwith "Unexpected stuck state!"
  in

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
  let* stuck = eval_until_stuck good_module_tree in
  match stuck with
  | Eval_error {explanation = Some "unreachable executed"; _} -> return_unit
  | _ -> failwith "Unexpected stuck state!"

let tests = [tztest "init requires memory 0 export" `Quick test_memory0_export]
