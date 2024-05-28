(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

open Node_context
open Context_sigs
open Context_wrapper.Irmin

let get_state_of_lcc node_ctxt =
  let open Lwt_result_syntax in
  let lcc = Reference.get node_ctxt.lcc in
  let* block_hash = Node_context.hash_of_level node_ctxt lcc.level in
  let* ctxt = Node_context.checkout_context node_ctxt block_hash in
  let*! state = Context.PVMState.find ctxt in
  return state

let proof_of_output node_ctxt output =
  let open Lwt_result_syntax in
  let* state = get_state_of_lcc node_ctxt in
  let lcc = Reference.get node_ctxt.lcc in
  match state with
  | None ->
      (*
           This case should never happen as origination creates an LCC which
           must have been considered by the rollup node at startup time.
        *)
      failwith "Error producing outbox proof (no cemented state in the node)"
  | Some state -> (
      let module PVM = (val Pvm.of_kind node_ctxt.kind) in
      let*! proof =
        PVM.produce_output_proof
          (of_node_context node_ctxt.context).index
          (of_node_pvmstate state)
          output
      in
      match proof with
      | Ok proof ->
          let serialized_proof =
            Data_encoding.Binary.to_string_exn PVM.output_proof_encoding proof
          in
          return @@ (lcc.commitment, serialized_proof)
      | Error err ->
          failwith
            "Error producing outbox proof (%a)"
            Environment.Error_monad.pp
            err)

let proof_of_output_simple node_ctxt ~outbox_level ~message_index =
  let open Lwt_result_syntax in
  let outbox_level = Protocol.Alpha_context.Raw_level.to_int32 outbox_level in
  let* state = get_state_of_lcc node_ctxt in
  let lcc = Reference.get node_ctxt.lcc in
  match state with
  | None ->
      (*
           This case should never happen as origination creates an LCC which
           must have been considered by the rollup node at startup time.
        *)
      failwith "Error producing outbox proof (no cemented state in the node)"
  | Some state ->
      let+ proof =
        Pvm_plugin.produce_serialized_output_proof
          node_ctxt
          state
          ~outbox_level
          ~message_index
      in
      (lcc.commitment, proof)
