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

(** This module provides helper to interact with PVM outboxes. *)

open Node_context
open Protocol.Alpha_context

module Make (PVM : Pvm.S) = struct
  let get_state_of_lcc node_ctxt =
    let open Lwt_result_syntax in
    let*! lcc_level =
      Store.Last_cemented_commitment_level.get node_ctxt.store
    in
    let*! block_hash =
      State.hash_of_level node_ctxt.store (Raw_level.to_int32 lcc_level)
    in
    let* ctxt = Node_context.checkout_context node_ctxt block_hash in
    let*! state = PVM.State.find ctxt in
    return state

  let proof_of_output node_ctxt output =
    let open Lwt_result_syntax in
    let*! commitment_hash =
      Store.Last_cemented_commitment_hash.get node_ctxt.store
    in
    let* state = get_state_of_lcc node_ctxt in
    match state with
    | None ->
        (*
           This case should never happen as origination creates an LCC which
           must have been considered by the rollup node at startup time.
        *)
        failwith "Error producing outbox proof (no cemented state in the node)"
    | Some state -> (
        let*! proof = PVM.produce_output_proof node_ctxt.context state output in
        match proof with
        | Ok proof ->
            let serialized_proof =
              Data_encoding.Binary.to_string_exn PVM.output_proof_encoding proof
            in
            return @@ (commitment_hash, serialized_proof)
        | Error err ->
            failwith
              "Error producing outbox proof (%a)"
              Environment.Error_monad.pp
              err)
end
