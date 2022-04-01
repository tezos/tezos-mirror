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

(** The internal state of the batcher. In particular, the batcher maintains an
    incremental context on top of the current head to apply incoming
    transactions. *)
type state

(** Initialize the internal state of the batcher. *)
val init :
  #Client_context.full ->
  rollup:Tx_rollup.t ->
  signer:string option ->
  Context.index ->
  Protocol.Alpha_context.Constants.parametric ->
  state option tzresult Lwt.t

(** Updates the incremental context in the batcher's state.

    {b Warning:} Use with caution. This function can introduce race conditions
    on the incremental context. *)
val update_incr_context : state -> Context.t -> unit

(** Retrieve an L2 transaction from the queue. *)
val find_transaction : state -> L2_transaction.hash -> L2_transaction.t option

(** List all queued transactions in the order they appear in the queue, i.e. the
    message that were added first to the queue are at the end of list. *)
val get_queue : state -> L2_transaction.t list

(** [register_transaction ?apply state tx] registers a new L2 transaction [tx]
    in the queue of the batcher for future injection on L1. If [apply] is [true]
    (defaults to [true]), the transaction is applied on the batcher's incremental
    context. In this case, when the application fails, the transaction is not
    queued. A batch is injected asynchronously if a full batch can be constructed
    and [eager_batch] is [true]. *)
val register_transaction :
  ?eager_batch:bool ->
  ?apply:bool ->
  state ->
  L2_transaction.t ->
  L2_transaction.hash tzresult Lwt.t

(** Create L2 batches of operations from the queue and pack them in an L1 batch
    operation. The batch operation is injected on the Tezos node by the
    signer. If the injection to L1 fails, the transactions are not removed from
    the queue. Nothing is injected if [at_least_one_full_batch] is [true] (by
    default [false]) and there isn't at least a full batch to inject. *)
val batch_and_inject :
  ?at_least_one_full_batch:bool ->
  state ->
  Operation_hash.t option tzresult Lwt.t

(** Same as [batch_and_inject] but asynchronous. In particular, the potential
    failures are not reported here. *)
val async_batch_and_inject : ?at_least_one_full_batch:bool -> state -> unit
