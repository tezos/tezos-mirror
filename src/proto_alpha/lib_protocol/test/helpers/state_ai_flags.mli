(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module handles the logic of flags for AI/AS in the [State] *)

module AI_Activation : sig
  (** This module takes care of the flags [force_activation] and
      [activation_vote_enable], and updates and check the record field
      [ai_activation_cycle] in the state. *)

  (** [AI_Activation] is enabled iff either of the above flags are set to true,
      or the vote threshold is set to 0 (regardless of votes).
      "Enabled" here means that AI *can* be activated (either by vote
      or by force), but does not mean that AI is activated *)
  val enabled : State.t -> bool

  (** Checks the [ai_activation_cycle] is set as expected.
      Run at the beginning, and after every block to check the activation
      cycle is set only once ever. *)
  val check_activation_cycle : Block.t -> State.t -> State.t tzresult Lwt.t
end
