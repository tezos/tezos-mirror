(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Raised by {!check_block_operation} when a refutation [Proof] move carries an
    inbox proof whose claimed level differs from the inbox level actually proven
    by its inclusion proof (the inbox level-confusion mitigation). Exposed so the
    rejection can be asserted precisely in tests. *)
type Environment.Error_monad.error +=
  | Sc_rollup_inbox_proof_claimed_level_mismatch of {
      claimed_level : Protocol.Alpha_context.Raw_level.t;
      proven_level : Protocol.Alpha_context.Raw_level.t;
    }

(** Raised by {!check_block_operation} when a refutation [Proof] move is played,
    during the [Dissecting] phase, on a distance-one section whose agreed start
    state is absent ([None]): no PVM proof can start from [None], so the move
    traps the game in an indefensible [Final_move] that resolves as a [Draw] and
    burns the honest player's bond. Exposed so the rejection can be asserted
    precisely in tests. Carries the tick of the section's start chunk. *)
type Environment.Error_monad.error +=
  | Sc_rollup_proof_on_missing_start_state_during_dissecting of Z.t

(** Fast shell-side state threaded through block validation. *)
type block_validation_state

val init_block_validation_state :
  Protocol.validation_state -> block_validation_state

val check_block_operation :
  block_validation_state ->
  Protocol.Alpha_context.packed_operation ->
  block_validation_state tzresult Lwt.t
