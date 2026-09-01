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

(** Fast shell-side state threaded through block validation. *)
type block_validation_state

val init_block_validation_state :
  Protocol.validation_state -> block_validation_state

val check_block_operation :
  block_validation_state ->
  Protocol.Alpha_context.packed_operation ->
  block_validation_state tzresult Lwt.t
