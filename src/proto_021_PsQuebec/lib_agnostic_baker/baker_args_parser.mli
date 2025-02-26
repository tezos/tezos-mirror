(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Protocol.Alpha_context

val parse_configuration :
  Configuration.t ->
  string option
  * bool
  * string option
  * Tez.t
  * Q.t
  * Q.t
  * int option
  * bool
  * Per_block_votes.per_block_vote option
  * Per_block_votes.per_block_vote option
  * string option
  * Baking_configuration.Operations_source.t option
  * Uri.t option
  * bool
  * Baking_configuration.state_recorder_config
  * Q.t option
  * Q.t option

val parse_baking_mode : string option -> Baking_commands.baking_mode
