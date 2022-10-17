(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
open Wasm_utils
module Context = Tezos_context_memory.Context_binary

(* Test that one N-ticks executions(^1) and N one-tick executions(^2)
   are equivalent.

   (^1): Executing in one decoding-encoding loop N ticks.
   (^2): Executing one decoding-encoding loop per ticks. *)
let test_execution_correspondance skip count () =
  test_with_kernel
    Kernels.unreachable_kernel
    (fun kernel ->
      let open Lwt_result_syntax in
      let*! tree = initial_tree ~from_binary:true ~max_tick:40_000L kernel in
      let*! tree_snapshotted = eval_until_input_requested tree in
      let*! tree_with_dummy_input =
        set_input_step "dummy_input" 0 tree_snapshotted
      in
      let*! tree =
        if skip = 0L then Lwt.return tree_with_dummy_input
        else
          Lwt.map fst
          @@ Wasm.compute_step_many ~max_steps:skip tree_with_dummy_input
      in
      let rec explore tree' n =
        let*! tree_ref, _ = Wasm.compute_step_many ~max_steps:n tree in
        let*! tree' = Wasm.compute_step tree' in
        assert (
          Context_hash.(Context.Tree.hash tree_ref = Context.Tree.hash tree')) ;
        if n < count then explore tree' (Int64.succ n) else return_unit
      in
      explore tree 1L)
    ()

let tests =
  [
    tztest
      "Executions correspondence (ticks 0 to 1,000)"
      `Quick
      (* Parsing is way slower, so we limit ourselves to 1,000 ticks. *)
      (test_execution_correspondance 0L 1_000L);
    tztest
      "Executions correspondence (ticks 10,000 to 11,000)"
      `Quick
      (* Parsing is way slower, so we limit ourselves to 1,000 ticks. *)
      (test_execution_correspondance 10_000L 1_000L);
    tztest
      "Executions correspondence (ticks 20,000 to 25,000)"
      `Quick
      (test_execution_correspondance 20_000L 5_000L);
    tztest
      "Executions correspondence (ticks 30,000 to 35,000)"
      `Quick
      (test_execution_correspondance 30_000L 5_000L);
  ]
