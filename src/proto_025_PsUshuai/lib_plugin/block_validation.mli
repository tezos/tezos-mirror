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
