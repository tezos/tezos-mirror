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

open Protocol
open Alpha_context

let commitment_of_inbox ~predecessor level (inbox : Inbox.t) =
  let message_results = Inbox.proto_message_results inbox in
  let messages =
    List.map Tx_rollup_message_result_hash.hash_uncarbonated message_results
  in
  let inbox_merkle_root = Inbox.merkle_root inbox in
  let predecessor = predecessor.L2block.header.commitment in
  Tx_rollup_commitment.{level; messages; predecessor; inbox_merkle_root}

let commit_block ~operator tx_rollup block =
  let open Lwt_result_syntax in
  match block.L2block.commitment with
  | None -> return_unit
  | Some commitment ->
      let manager_operation =
        Manager (Tx_rollup_commit {tx_rollup; commitment})
      in
      let hash = L1_operation.hash_manager_operation manager_operation in
      Injector.add_pending_operation
        {L1_operation.hash; source = operator; manager_operation}
