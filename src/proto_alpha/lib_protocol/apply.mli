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

type error += Wrong_consensus_operation_branch of Block_hash.t * Block_hash.t

type error +=
  | Wrong_level_for_consensus_operation of {
      expected : Raw_level.t;
      provided : Raw_level.t;
    }
  | Wrong_round_for_consensus_operation of {
      expected : Round.t;
      provided : Round.t;
    }
  | Preendorsement_round_too_high of {block_round : Round.t; provided : Round.t}

type error += Internal_operation_replay of packed_internal_operation

type denunciation_kind = Preendorsement | Endorsement | Block

type error += Invalid_denunciation of denunciation_kind

type error +=
  | Inconsistent_denunciation of {
      kind : denunciation_kind;
      delegate1 : Signature.Public_key_hash.t;
      delegate2 : Signature.Public_key_hash.t;
    }

type error +=
  | Too_early_denunciation of {
      kind : denunciation_kind;
      level : Raw_level.t;
      current : Raw_level.t;
    }

type error +=
  | Outdated_denunciation of {
      kind : denunciation_kind;
      level : Raw_level.t;
      last_cycle : Cycle.t;
    }

type error +=
  | Invalid_double_baking_evidence of {
      hash1 : Block_hash.t;
      level1 : Raw_level.t;
      round1 : Round.t;
      hash2 : Block_hash.t;
      level2 : Raw_level.t;
      round2 : Round.t;
    }

type error += Invalid_activation of {pkh : Ed25519.Public_key_hash.t}

type error += Gas_quota_exceeded_init_deserialize

type error += Inconsistent_sources

type error += (* `Permanent *) Failing_noop_error

val begin_partial_construction :
  t ->
  predecessor_level:Level.t ->
  escape_vote:bool ->
  ( t
    * packed_successful_manager_operation_result list
    * Liquidity_baking.escape_ema,
    error trace )
  result
  Lwt.t

type 'a full_construction = {
  ctxt : t;
  protocol_data : 'a;
  payload_producer : Signature.public_key_hash;
  block_producer : Signature.public_key_hash;
  round : Round.t;
  implicit_operations_results : packed_successful_manager_operation_result list;
  liquidity_baking_escape_ema : Liquidity_baking.escape_ema;
}

val begin_full_construction :
  t ->
  predecessor_timestamp:Time.t ->
  predecessor_level:Level.t ->
  predecessor_round:Round.t ->
  round:Round.t ->
  Block_header.contents ->
  Block_header.contents full_construction tzresult Lwt.t

val begin_application :
  t ->
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
  * Liquidity_baking.escape_ema)
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
    } (* Both partial and normal *)
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
  t ->
  Chain_id.t ->
  apply_mode ->
  Script_ir_translator.unparsing_mode ->
  payload_producer:public_key_hash ->
  Operation_list_hash.elt ->
  'a operation ->
  (t * 'a operation_metadata, error trace) result Lwt.t

type finalize_application_mode =
  | Finalize_full_construction of {
      level : Raw_level.t;
      predecessor_round : Round.t;
    }
  | Finalize_application of Fitness.t

val finalize_application :
  t ->
  finalize_application_mode ->
  Alpha_context.Block_header.contents ->
  payload_producer:public_key_hash ->
  block_producer:public_key_hash ->
  Liquidity_baking.escape_ema ->
  packed_successful_manager_operation_result list ->
  round:Round.t ->
  predecessor:Block_hash.t ->
  migration_balance_updates:Receipt.balance_updates ->
  (t * Fitness.t * block_metadata, error trace) result Lwt.t

val apply_manager_contents_list :
  t ->
  Script_ir_translator.unparsing_mode ->
  payload_producer:public_key_hash ->
  Chain_id.t ->
  ('a Kind.manager, Receipt.balance_updates) prechecked_contents_list ->
  (t * 'a Kind.manager contents_result_list) Lwt.t

val apply_contents_list :
  t ->
  Chain_id.t ->
  apply_mode ->
  Script_ir_translator.unparsing_mode ->
  payload_producer:public_key_hash ->
  'kind operation ->
  'kind contents_list ->
  (t * 'kind contents_result_list) tzresult Lwt.t

(** [value_of_key ctxt k] builds a value identified by key [k]
    so that it can be put into the cache. *)
val value_of_key : t -> Context.Cache.key -> Context.Cache.value tzresult Lwt.t

(** [cache_layout] describes how the caches needed by the protocol.
   The length of the list defines the number of caches while each
   element of this list corresponds to the size limit of each cache. *)
val cache_layout : int list

(** Check if endorsements are required for a given level. *)
val are_endorsements_required : t -> level:Raw_level.t -> bool tzresult Lwt.t

(** Check if a block's endorsing power is at least the minim required. *)
val check_minimum_endorsements :
  endorsing_power:int -> minimum:int -> unit tzresult Lwt.t
