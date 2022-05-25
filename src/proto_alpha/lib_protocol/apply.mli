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
  | Internal_operation_replay of packed_internal_contents
  | Gas_quota_exceeded_init_deserialize
  | Insufficient_gas_for_manager
  | Tx_rollup_feature_disabled
  | Tx_rollup_invalid_transaction_ticket_amount
  | Sc_rollup_feature_disabled
  | Inconsistent_counters
  | Incorrect_reveal_position
  | Inconsistent_sources

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
  | Application of {
      predecessor_block : Block_hash.t;
      payload_hash : Block_payload_hash.t;
      locked_round : Round.t option;
      predecessor_level : Level.t;
      predecessor_round : Round.t;
      round : Round.t;
    }
    (* Both partial and normal *)
  | Full_construction of {
      predecessor_block : Block_hash.t;
      payload_hash : Block_payload_hash.t;
      predecessor_level : Level.t;
      predecessor_round : Round.t;
      round : Round.t;
    }
  | Partial_construction of {
      predecessor_level : Level.t;
      predecessor_round : Round.t;
      grand_parent_round : Round.t;
    }

val apply_operation :
  context ->
  Chain_id.t ->
  apply_mode ->
  Script_ir_translator.unparsing_mode ->
  payload_producer:public_key_hash ->
  Operation_list_hash.elt ->
  'a operation ->
  (context * 'a operation_metadata, error trace) result Lwt.t

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

val apply_contents_list :
  context ->
  Chain_id.t ->
  apply_mode ->
  Script_ir_translator.unparsing_mode ->
  payload_producer:public_key_hash ->
  'kind operation ->
  'kind contents_list ->
  (context * 'kind contents_result_list) tzresult Lwt.t

(** Update the context to reflect the application of a manager
    operation.

    This function first updates the context to:

    - take the fees;

    - increment the account's counter;

    - decrease of the available block gas by operation's [gas_limit].

    These updates are mandatory. In particular, taking the fees is
    critically important. That's why [apply_manager_operation] **should
    only be called after {!precheck_manager_contents_list}**,
    which is responsible for ensuring that the operation is solvable,
    i.e. this first stage of [apply_manager_operation] will not
    fail. If this stage fails nevertheless, the function returns an
    error.

    The second stage of this function consists in applying all the
    other effects, in accordance with the semantic of the operation's
    kind.

    An error may happen during this second phase: in that case, the
    function returns the context obtained at the end of the first
    stage, and a [contents_result_list] that contains the error. This
    means that the operation has no other effects than those described
    above during the first phase. *)
val apply_manager_operation :
  context ->
  Script_ir_translator.unparsing_mode ->
  payload_producer:public_key_hash ->
  Chain_id.t ->
  mempool_mode:bool ->
  'a Kind.manager contents_list ->
  (context * 'a Kind.manager contents_result_list) tzresult Lwt.t

(** Check the solvability of the given standalone manager operation or
    batch of manager operations, except that the signature is not
    checked here. Return the contract's public key. This function is
    effect-free. *)
val precheck_manager_contents_list :
  context ->
  'kind Kind.manager contents_list ->
  mempool_mode:bool ->
  public_key tzresult Lwt.t

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
