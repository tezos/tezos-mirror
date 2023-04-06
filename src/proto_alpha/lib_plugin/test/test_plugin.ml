(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Shell (Plugin)
    Invocation:   dune exec src/proto_alpha/lib_plugin/test/main.exe
    Subject:      Unit tests the plugin
*)

open Plugin.Mempool
open Test_utils

let count = Some 1000

(** This test checks that [flush] empties the filter state. *)
let test_flush () =
  let l =
    QCheck2.Gen.generate
      ~n:(Option.value ~default:1 count)
      Generators.filter_state_with_operation_gen
  in
  List.iter_es
    (fun (ops_state, (oph, (op_info : manager_op_info))) ->
      let ops_state = add_manager_op ops_state oph op_info `No_replace in
      let open Lwt_result_syntax in
      let* block, _contract = Context.init1 () in
      let* incremental = Incremental.begin_construction block in
      let ctxt = Incremental.alpha_ctxt incremental in
      let head = block.header.shell in
      let round_durations = Alpha_context.Constants.round_durations ctxt in
      let hard_gas_limit_per_block =
        Alpha_context.Constants.hard_gas_limit_per_block ctxt
      in
      let* filter_state =
        init_state ~head round_durations hard_gas_limit_per_block
      in
      let filter_state = {filter_state with ops_state} in
      let* flushed_state = flush filter_state ~head in
      if eq_state flushed_state.ops_state empty_ops_state then return_unit
      else
        failwith
          "Flushed state is not empty : %a"
          pp_state
          flushed_state.ops_state)
    l

let () =
  Alcotest_lwt.run
    ~__FILE__
    "Plugin"
    [
      ( Protocol.name ^ ": on_flush",
        [
          Tztest.tztest
            "[on_flush ~validation_state ...] yields an empty state "
            `Quick
            test_flush;
        ] );
    ]
  |> Lwt_main.run
