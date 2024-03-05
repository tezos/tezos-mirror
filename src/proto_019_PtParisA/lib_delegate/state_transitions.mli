(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module, and in particular the {!step} function, modifies the automaton
    state, while {!Baking_actions} performs potentially failing side-effects. *)

open Protocol
open Alpha_context
open Baking_state
open Baking_actions

val do_nothing : state -> (state * action) Lwt.t

type proposal_acceptance = Invalid | Outdated_proposal | Valid_proposal

val is_acceptable_proposal_for_current_level :
  state -> proposal -> proposal_acceptance Lwt.t

val make_consensus_vote_batch :
  state -> proposal -> consensus_vote_kind -> unsigned_consensus_vote_batch

val may_update_proposal :
  is_proposal_applied:bool -> state -> proposal -> state Lwt.t

val preattest : state -> proposal -> (state * action) Lwt.t

val extract_pqc :
  state -> proposal -> (Kind.preattestation operation list * Round.t) option

val handle_proposal :
  is_proposal_applied:bool -> state -> proposal -> (state * action) Lwt.t

(** Propose a block at the start of the given round for the given delegate,
    given that there was already a proposal at the current level, the last one
    being [last_proposal]. *)
val propose_block_action :
  state ->
  consensus_key_and_delegate ->
  Round.t ->
  last_proposal:proposal ->
  action Lwt.t

(** Increase the current round and propose at the new round (same
    level), if the baker has a proposer slot. *)
val end_of_round : state -> Round.t -> (state * action) Lwt.t

(** Propose for the first time at a level at the given round. There was no
    previous proposal at the current level. *)
val time_to_prepare_next_level_block :
  state -> Round.t -> (state * action) Lwt.t

val update_locked_round : state -> Round.t -> Block_payload_hash.t -> state

val prepare_attest_action : state -> proposal -> action

val prequorum_reached_when_awaiting_preattestations :
  state ->
  Operation_worker.candidate ->
  Kind.preattestation operation list ->
  (state * action) Lwt.t

val quorum_reached_when_waiting_attestations :
  state ->
  Operation_worker.candidate ->
  Kind.attestation operation list ->
  (state * action) Lwt.t

val step : state -> event -> (state * action) Lwt.t
