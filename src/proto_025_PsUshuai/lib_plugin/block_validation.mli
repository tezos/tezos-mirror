(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type Environment.Error_monad.error +=
  | Increase_paid_storage_amount_overflow of Z.t
  | Delegate_cannot_stake_during_finalization_of_slashed_period

(** Fast shell-side state threaded through block validation. *)
type block_validation_state

val init_block_validation_state :
  Protocol.validation_state -> block_validation_state

val check_double_baking_evidence :
  Protocol.block_header ->
  Protocol.block_header ->
  (unit, Environment.Error_monad.error) result

val check_block_operation :
  block_validation_state ->
  Protocol.Alpha_context.packed_operation ->
  block_validation_state tzresult Lwt.t

(** Raised/returned for a smart-rollup refutation whose DAL page proof targets a
    [published_level] greater than the level at which the operation is
    processed. Used by both block validation and the mempool plugin. *)
type Environment.Error_monad.error +=
  | Sc_rollup_refute_dal_proof_future_published_level of {
      published_level : Protocol.Alpha_context.Raw_level.t;
      level : Protocol.Alpha_context.Raw_level.t;
    }

(** [find_future_dal_refute ~level op] returns [Some published_level] if [op]
    contains a [Sc_rollup_refute] whose [Proof] move carries a DAL page proof
    with [published_level > level], and [None] otherwise. [level] is the level
    at which the operation is processed (the block's own level for block
    validation; [head + 1] for the mempool). *)
val find_future_dal_refute :
  level:Protocol.Alpha_context.Raw_level.t ->
  Protocol.Alpha_context.packed_operation ->
  Protocol.Alpha_context.Raw_level.t option

(** Raised when an external staker's [`stake`] amount is too small to mint any
    staking pseudotoken at the delegate's current conversion rate (the
    down-rounded credit would be zero), which would transfer the tez into the
    delegate's frozen deposits with no pseudotoken minted in exchange. Exposed
    so that the operation-simulation RPC can reject such operations with the
    same error as block validation. *)
type Environment.Error_monad.error +=
  | Stake_amount_too_small of Protocol.Alpha_context.Tez.t

(** [stake_mints_no_pseudotoken ctxt ~staker ~amount] returns [true] when a
    [`stake`] of [amount] by [staker] would mint no staking pseudotoken given
    the current [ctxt] (external staker whose amount rounds down to zero
    pseudotokens). Returns [false] for cases that either mint a pseudotoken or
    are rejected by the protocol on their own (self-stake, no delegate,
    non-positive amount, uninitialized or fully-slashed pool). *)
val stake_mints_no_pseudotoken :
  Protocol.Alpha_context.t ->
  staker:Protocol.Alpha_context.public_key_hash ->
  amount:Protocol.Alpha_context.Tez.t ->
  bool Environment.Error_monad.tzresult Lwt.t

(** [delegate_stake_while_slashed_in_current_finalization_delay ctxt ~delegate]
    returns [true] if [delegate] is a delegate that has been slashed during the
    last `unstake_finalization_delay + 1` cycles. This prevents a delegate to
    re-stake non finalizable tez from unstake requests that have been
    slashed. *)

val delegate_stake_while_slashed_in_current_finalization_delay :
  Protocol.Alpha_context.t ->
  delegate:Protocol.Alpha_context.public_key_hash ->
  bool Environment.Error_monad.tzresult Lwt.t

(** [check_execute_outbox_message context ~rollup ~output_proof] checks the
    validity of a given [output_proof] against the rollup. Returns an error if
    not the case. *)
val check_execute_outbox_message :
  Protocol.Alpha_context.t ->
  rollup:Protocol.Alpha_context.Sc_rollup.t ->
  output_proof:string ->
  unit Environment.Error_monad.tzresult Lwt.t
