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

    Each operation application takes and returns an [application_state], representing
    the old and new state, respectively.

    The [Main] module provides wrappers for the functionality in this module,
    satisfying the Protocol signature.
 *)

open Alpha_context

type error +=
  | Faulty_validation_wrong_slot
  | Set_deposits_limit_on_unregistered_delegate of Signature.Public_key_hash.t
  | Set_deposits_limit_when_automated_staking_off
  | Error_while_taking_fees
  | Update_consensus_key_on_unregistered_delegate of Signature.Public_key_hash.t
  | Empty_transaction of Contract.t
  | Non_empty_transaction_from of Destination.t
  | Internal_operation_replay of
      Apply_internal_results.packed_internal_operation
  | Multiple_revelation
  | Invalid_transfer_to_sc_rollup
  | Invalid_sender of Destination.t
  | Invalid_self_transaction_destination
  | Staking_for_delegator_while_external_staking_disabled
  | Staking_to_delegate_that_refuses_external_staking
  | Stake_modification_with_no_delegate_set
  | Invalid_nonzero_transaction_amount of Tez.t
  | Invalid_staking_parameters_sender
  | Missing_shell_header

type mode =
  | Application of {
      block_header : Block_header.t;
      fitness : Fitness.t;
      payload_producer : Consensus_key.t;
      block_producer : Consensus_key.t;
      predecessor_level : Level.t;
      predecessor_round : Round.t;
    }
  | Full_construction of {
      block_data_contents : Block_header.contents;
      predecessor_hash : Block_hash.t;
      payload_producer : Consensus_key.t;
      block_producer : Consensus_key.t;
      round : Round.t;
      predecessor_level : Level.t;
      predecessor_round : Round.t;
    }
  | Partial_construction of {predecessor_fitness : Fitness.raw}
      (** This mode is mainly intended to be used by a mempool. *)

type application_state = {
  ctxt : context;
  chain_id : Chain_id.t;
  mode : mode;
  op_count : int;
  migration_balance_updates : Receipt.balance_updates;
  liquidity_baking_toggle_ema : Per_block_votes.Liquidity_baking_toggle_EMA.t;
  adaptive_issuance_vote_ema : Per_block_votes.Adaptive_issuance_launch_EMA.t;
  adaptive_issuance_launch_cycle : Cycle.t option;
  implicit_operations_results :
    Apply_results.packed_successful_manager_operation_result list;
}

(** Initialize an {!application_state} for the application of an
    existing block. *)
val begin_application :
  context ->
  Chain_id.t ->
  migration_balance_updates:Receipt.balance_updates ->
  migration_operation_results:Migration.origination_result list ->
  predecessor_fitness:Fitness.raw ->
  Block_header.t ->
  application_state tzresult Lwt.t

(** Initialize an {!application_state} for the construction of a
    fresh block. *)
val begin_full_construction :
  context ->
  Chain_id.t ->
  migration_balance_updates:Receipt.balance_updates ->
  migration_operation_results:Migration.origination_result list ->
  predecessor_timestamp:Time.t ->
  predecessor_level:Level.t ->
  predecessor_round:Round.t ->
  predecessor_hash:Block_hash.t ->
  timestamp:Time.t ->
  Block_header.contents ->
  application_state tzresult Lwt.t

(** Initialize an {!application_state} for the partial construction of
    a block. This is similar to construction but less information is
    required as this will not yield a final valid block. *)
val begin_partial_construction :
  context ->
  Chain_id.t ->
  migration_balance_updates:Receipt.balance_updates ->
  migration_operation_results:Migration.origination_result list ->
  predecessor_hash:Block_hash.t ->
  predecessor_fitness:Fitness.raw ->
  application_state tzresult Lwt.t

(** Apply an operation, i.e. update the given context in accordance
    with the operation's semantic (or return an error if the operation
    is not applicable).

    For non-manager operations, the application of a validated
   operation should always fully succeed.

    For manager operations, the application has two stages. The first
   stage consists in updating the context to:

    - take the fees;

    - increment the account's counter;

    - decrease of the available block gas by operation's [gas_limit].

    These updates are mandatory. In particular, taking the fees is
   critically important. The {!Validate} module is responsible for
   ensuring that the operation is solvable, i.e. that fees can be
   taken, i.e. that the first stage of manager operation application
   cannot fail. If this stage fails nevertheless, the function returns
   an error.

    The second stage of this function (still in the case of a manager
   operation) consists in applying all the other effects, in
   accordance with the semantic of the operation's kind.

    An error may happen during this second phase: in that case, the
   function returns the context obtained at the end of the first
   stage, and metadata that contain the error. This means that the
   operation has no other effects than those described above during
   the first phase. *)
val apply_operation :
  application_state ->
  Operation_hash.t ->
  packed_operation ->
  (application_state * Apply_results.packed_operation_metadata) tzresult Lwt.t

(** Finalize the application of a block depending on its mode. *)
val finalize_block :
  application_state ->
  Block_header.shell_header option ->
  (Updater.validation_result * Apply_results.block_metadata) tzresult Lwt.t

(** [value_of_key ctxt k] builds a value identified by key [k]
    so that it can be put into the cache. *)
val value_of_key :
  context -> Context.Cache.key -> Context.Cache.value tzresult Lwt.t

module Internal_for_benchmark : sig
  val take_fees : context -> 'a Kind.manager contents_list -> unit
end
