(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

open Alpha_context
open Tx_rollup_l2_context_sig
open Tx_rollup_l2_batch

(** This module introduces the interpretation (off-chain) of layer2 operations
    read from its inboxes.

    The main concern is now to interpret operations in the layer2 context
    with a high-throughput and process a significant number of operations
    “per second.”

    The operations can be crafted with indexes (see {!Indexable}). It is meant
    to reduce the size of operations, and therefore, increase the number
    of those in a message to the layer2. You will see in this file indexables
    which are yet unknowns (see {!Indexable.unknown}). They can be later on
    modified to indexes only (see {!Indexable.index_only}) when the potential
    values have been replaced by their indexes.

    Therefore, we need to have the minimal number of accesses to the context.
    Thus, when a value is read from its index in the context, it *must* be
    done once.
*)

type error +=
  | Counter_mismatch of {
      account : Tx_rollup_l2_address.t;
      expected : int64;
      provided : int64;
    }
  | Incorrect_aggregated_signature
  | Unallocated_metadata of int32
  | Multiple_operations_for_signer of Bls.Public_key.t
  | Invalid_transaction_encoding
  | Invalid_batch_encoding
  | Unexpectedly_indexed_ticket
  | Missing_ticket of Ticket_hash.t
  | Unknown_address of Tx_rollup_l2_address.t
  | Invalid_self_transfer
  | Invalid_zero_transfer
  | Maximum_withdraws_per_message_exceeded of {current : int; maximum : int}

(** Applying operations in the layer2 can result in creating indexes
    associated to both the addresses and the ticket hashes. We keep track
    of these creations in order to replace the values by their indexes
    in future operations. *)
type indexes = {
  address_indexes :
    (Tx_rollup_l2_address.t * Tx_rollup_l2_address.Indexable.index) list;
  ticket_indexes : (Ticket_hash.t * Ticket_indexable.index) list;
}

module Message_result : sig
  (** A transaction inside a batch can either be a success or a failure.

      In the case of a failure, we store the operation's index which failed
      with the reason it failed. *)
  type transaction_result =
    | Transaction_success
    | Transaction_failure of {index : int; reason : error}

  (** A deposit can either be a success or a failure. The created indexes
      must are kept only when the deposit is a success. In the other case,
      we return the reason why the deposit failed. *)
  type deposit_result = Deposit_success of indexes | Deposit_failure of error

  (** The operations are versioned (see {!Tx_rollup_l2_batch}). Therefore, we
      introduce the operation results for each version. *)

  module Batch_V1 : sig
    type t =
      | Batch_result of {
          results :
            ((Indexable.index_only, Indexable.unknown) V1.transaction
            * transaction_result)
            list;
          indexes : indexes;
        }
  end

  type message_result =
    | Deposit_result of deposit_result
    | Batch_V1_result of Batch_V1.t

  (* In addition to [message_result] the result contains the list of
     withdrawals that result from failing deposits and layer2-to-layer1
     transfers. *)
  type t = message_result * Tx_rollup_withdraw.t list

  val encoding : t Data_encoding.t
end

(** The record of parameters used during the application of messages. *)
type parameters = {
  (* Maximum number of allowed L2-to-L1 withdraws per batch *)
  tx_rollup_max_withdrawals_per_batch : int;
}

module Make (Context : CONTEXT) : sig
  open Context

  type ctxt = t

  (** The operations are versioned (see {!Tx_rollup_l2_batch}),
      so their interpretations are. *)

  module Batch_V1 : sig
    open Tx_rollup_l2_batch.V1

    (** [apply_batch ctxt parameters batch] interprets the batch
        {!Tx_rollup_l2_batch.V1.t}.

        By construction, a failing transaction will not affect the [ctxt]
        and other transactions will still be interpreted.
        That is, this function can only fail because of internals errors.
        Otherwise, the errors that caused the transactions to fail can be
        observed in the result (see {!Message_result.Batch_V1.t}).

        The counters are incremented when the operation is part of a transaction
        that is correctly signed and whose every operations have the expected
        counter. In particular, the result of the application is not important
        (i.e. the counters are updated even if the transaction failed).

        In addition, the list of withdrawals resulting from each
        layer2-to-layer1 transfer message in the batch is returned.
    *)
    val apply_batch :
      ctxt ->
      parameters ->
      (Indexable.unknown, Indexable.unknown) t ->
      (ctxt * Message_result.Batch_V1.t * Tx_rollup_withdraw.t list) m

    (** [check_signature ctxt batch] asserts that [batch] is correctly signed.

        We recall that [batch] may contain indexes, that is integers which
        replace larger values. The [signer] field of the
        {!Tx_rollup_l2_batch.operation} type is concerned. This field is either
        the public key to be used to check the signature, or an index.
        In case of the public key, [check_signature] will check whether or not
        the related {!Tx_rollup_l2_address.t} has already an index assigned,
        and allocate one if not.

        Overall, [check_signature] returns the revised context, the list of
        newly allocated indexes, and an updated version of the batches where
        all [signer] field have been replaced by valid indexes.

        {b Note:} What a user is expected to sign is the version of the
        operation it sends to the network. This is potentially unsafe,
        because it means the user signs indexes, not addresses nor
        ticket hashes. This poses two threats: Tezos reorganization,
        and malicious provider of indexes. A Tezos reorganization may
        imply that an index allocated to one address in a given branch
        is allocated to another address in another branch. We deal with
        this issue by making the rollup node aware of the Tezos level at
        each time an index is allocated. This allows to implement a RPC that
        can safely tell a client to use either the full value or the index,
        thanks to Tenderbake finality. To prevent the rollup node to lie,
        we will make the rollup node provide Merkle proofs that allows the
        client to verify that the index is correct.
    *)
    val check_signature :
      ctxt ->
      (Indexable.unknown, Indexable.unknown) t ->
      (ctxt * indexes * (Indexable.index_only, Indexable.unknown) t) m
  end

  (** [apply_deposit ctxt deposit] credits a quantity of tickets to a layer2
      address in [ctxt].

      This function can fail if the [deposit.amount] is not strictly-positive.

      If the [deposit] causes an error, then a withdrawal returning
      the funds to the deposit's sender is returned.
  *)
  val apply_deposit :
    ctxt ->
    Tx_rollup_message.deposit ->
    (ctxt * Message_result.deposit_result * Tx_rollup_withdraw.t option) m

  (** [apply_message ctxt parameters message] interprets the [message] in the
      [ctxt].

      That is,

      {ul {li Deposit tickets if the message is a deposit. }
          {li Decodes the batch and interprets it for the
              correct batch version. }}

      The function can fail with {!Invalid_batch_encoding} if it's not able
      to decode the batch.

      The function can also return errors from subsequent functions,
      see {!apply_deposit} and batch interpretations for various versions.

      The list of withdrawals in the message result followed the ordering
      of the contents in the message.
  *)
  val apply_message :
    ctxt -> parameters -> Tx_rollup_message.t -> (ctxt * Message_result.t) m
end
