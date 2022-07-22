(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** This module supports advancing the ledger state by applying [operation]s.

    Each operation application takes and returns an [Alpha_context.t], representing
    the old and new state, respectively.

    The [Main] module provides wrappers for the functionality in this module,
    satisfying the Protocol signature.
 *)

open Alpha_context
open Apply_results
open Apply_internal_results

type error +=
  | Internal_operation_replay of packed_internal_operation
  | Tx_rollup_feature_disabled
  | Tx_rollup_invalid_transaction_ticket_amount
  | Sc_rollup_feature_disabled
  | Empty_transaction of Contract.t
  | Wrong_voting_period of {expected : int32; provided : int32}

val begin_partial_construction :
  context ->
  predecessor_level:Level.t ->
  toggle_vote:Liquidity_baking_repr.liquidity_baking_toggle_vote ->
  (t
  * packed_successful_manager_operation_result list
  * Liquidity_baking.Toggle_EMA.t)
  tzresult
  Lwt.t

type 'a full_construction = {
  ctxt : context;
  protocol_data : 'a;
  payload_producer : Signature.public_key_hash;
  block_producer : Signature.public_key_hash;
  round : Round.t;
  implicit_operations_results : packed_successful_manager_operation_result list;
  liquidity_baking_toggle_ema : Liquidity_baking.Toggle_EMA.t;
}

val begin_full_construction :
  context ->
  predecessor_timestamp:Time.t ->
  predecessor_level:Level.t ->
  predecessor_round:Round.t ->
  round:Round.t ->
  Block_header.contents ->
  Block_header.contents full_construction tzresult Lwt.t

val begin_application :
  context ->
  Chain_id.t ->
  Block_header.t ->
  Fitness.t ->
  predecessor_timestamp:Time.t ->
  predecessor_level:Level.t ->
  predecessor_round:Round.t ->
  (t
  * Signature.public_key
  * Signature.public_key_hash
  * packed_successful_manager_operation_result list
  * Liquidity_baking.Toggle_EMA.t)
  tzresult
  Lwt.t

type apply_mode =
  | Application  (** Both partial and normal *)
  | Full_construction  (** For a baker *)
  | Partial_construction of {predecessor_level : Level.t option}
      (** This mode is mainly intended to be used by a mempool, in which
          case the [predecessor_level] should be provided. However, an RPC
          might use it with [None]. *)

(** Apply an operation, i.e. update the given context in accordance
    with the operation's semantic (or return an error if the operation
    is not applicable).

    The {!type:Validate_operation.stamp} argument enforces that an
    operation needs to be validated by {!Validate_operation} before it
    can be applied.

    For non-manager operations, the application of a validated
    operation should always fully succeed.

    TODO: https://gitlab.com/tezos/tezos/-/issues/2603

    Currently, {!Validate_operation.validate_operation} does nothing
    on voting operations. The "validation" of these operations is
    instead handled by [apply_operation], which may thus return an
    error if the operation is ill-formed. Once [validate_operation] has
    been extended to every kind of operation, [apply_operation] should
    never return an error.

    For manager operations, the application has two stages. The first
    stage consists in updating the context to:

    - take the fees;

    - increment the account's counter;

    - decrease of the available block gas by operation's [gas_limit].

    These updates are mandatory. In particular, taking the fees is
    critically important. The {!Validate_operation} module (from which
    we get the {!Validate_opoeration.stamp} as explained above) is
    responsible for ensuring that the operation is solvable, i.e. that
    fees can be taken, i.e. that the first stage of manager operation
    application cannot fail. If this stage fails nevertheless, the
    function returns an error.

    The second stage of this function (still in the case of a manager
    operation) consists in applying all the other effects, in
    accordance with the semantic of the operation's kind.

    An error may happen during this second phase: in that case, the
    function returns the context obtained at the end of the first
    stage, and metadata that contain the error. This means that the
    operation has no other effects than those described above during
    the first phase. *)
val apply_operation :
  context ->
  Chain_id.t ->
  apply_mode ->
  payload_producer:public_key_hash ->
  Validate_operation.stamp ->
  Operation_hash.t ->
  'a operation ->
  (context * 'a operation_metadata) tzresult Lwt.t

type finalize_application_mode =
  | Finalize_full_construction of {
      level : Raw_level.t;
      predecessor_round : Round.t;
    }
  | Finalize_application of Fitness.t

val finalize_application :
  context ->
  finalize_application_mode ->
  Block_header.contents ->
  payload_producer:public_key_hash ->
  block_producer:public_key_hash ->
  Liquidity_baking.Toggle_EMA.t ->
  packed_successful_manager_operation_result list ->
  round:Round.t ->
  predecessor:Block_hash.t ->
  migration_balance_updates:Receipt.balance_updates ->
  (context * Fitness.t * block_metadata, error trace) result Lwt.t

(** [value_of_key ctxt k] builds a value identified by key [k]
    so that it can be put into the cache. *)
val value_of_key :
  context -> Context.Cache.key -> Context.Cache.value tzresult Lwt.t

(** Check if endorsements are required for a given level. *)
val are_endorsements_required :
  context -> level:Raw_level.t -> bool tzresult Lwt.t

(** Check if a block's endorsing power is at least the minim required. *)
val check_minimum_endorsements :
  endorsing_power:int -> minimum:int -> unit tzresult
