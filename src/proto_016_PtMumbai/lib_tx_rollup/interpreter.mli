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

(** Error result when the message's application produces a too large proof.
    It overrides the layer2 apply message result. *)
type error += Tx_rollup_message_proof_too_large of {limit : int; actual : int}

(** Interpreting the [messages] in the context.

    It uses internally the {!Prover_apply} to produce a proof associated
    to the interpretation of each message. In the case where the proof is
    larger than the configuration limit, the message's interpretation is
    discarded alongside the modified context.
*)
val interpret_messages :
  rejection_max_proof_size:int ->
  Context.context ->
  Protocol.Tx_rollup_l2_apply.parameters ->
  Protocol.Alpha_context.Tx_rollup_message.t trace ->
  (Context.context * Inbox.message list option) tzresult Lwt.t

(** Interpreting the [batch] in the context.

    Similarly to {!interp_messages}, it uses internally the {!Prover_apply}.
    However, the function fails if the interpretation produces a proof
    that is larger than the configuration limit.
    We here want to check only if the batch is interpretable, the modified
    tree is discarded.

    TODO/TORU: maybe we could check the results for each transaction in the batch
*)
val interpret_batch :
  rejection_max_proof_size:int ->
  Context.context ->
  Protocol.Tx_rollup_l2_apply.parameters ->
  ( Protocol.Indexable.unknown,
    Protocol.Indexable.unknown )
  Protocol.Tx_rollup_l2_batch.t ->
  unit tzresult Lwt.t
