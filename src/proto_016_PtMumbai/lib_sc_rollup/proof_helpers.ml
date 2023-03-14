(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol.Alpha_context

let origination_proof ~boot_sector kind =
  let aux = function
    | Sc_rollup.Kind.Example_arith ->
        let open Lwt_result_syntax in
        let context = Context_helpers.In_memory.make_empty_context () in
        let* proof =
          Pvm.Arith_pvm_in_memory.produce_origination_proof context boot_sector
        in
        let*? proof =
          Sc_rollup.Proof.serialize_pvm_step
            ~pvm:(module Pvm.Arith_pvm_in_memory)
            proof
        in
        return proof
    | Sc_rollup.Kind.Wasm_2_0_0 ->
        let open Lwt_result_syntax in
        let context = Context_helpers.In_memory.make_empty_context () in
        let* proof =
          Pvm.Wasm_pvm_in_memory.produce_origination_proof context boot_sector
        in
        let*? proof =
          Sc_rollup.Proof.serialize_pvm_step
            ~pvm:(module Pvm.Wasm_pvm_in_memory)
            proof
        in
        return proof
  in
  aux kind
