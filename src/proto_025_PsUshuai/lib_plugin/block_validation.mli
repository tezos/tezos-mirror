(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

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
