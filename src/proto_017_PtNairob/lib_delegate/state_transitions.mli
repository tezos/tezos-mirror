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

val make_consensus_list :
  state -> proposal -> (consensus_key_and_delegate * consensus_content) list

val may_update_proposal :
  is_proposal_applied:bool -> state -> proposal -> state Lwt.t

val preendorse : state -> proposal -> (state * action) Lwt.t

val extract_pqc :
  state -> proposal -> (Kind.preendorsement operation list * Round.t) option

val handle_proposal :
  is_proposal_applied:bool -> state -> proposal -> (state * action) Lwt.t

val round_proposer :
  state ->
  (consensus_key_and_delegate * endorsing_slot) SlotMap.t ->
  Round.t ->
  (consensus_key_and_delegate * endorsing_slot) option

val propose_fresh_block_action :
  endorsements:Kind.endorsement Operation.t list ->
  dal_attestations:Kind.dal_attestation Operation.t list ->
  ?last_proposal:block_info ->
  predecessor:block_info ->
  state ->
  consensus_key_and_delegate ->
  Round.t ->
  action Lwt.t

val propose_block_action :
  state -> consensus_key_and_delegate -> Round.t -> proposal -> action Lwt.t

(** Increase the current round and propose at the new round (same
   level), if the baker has a proposer slot. *)
val end_of_round : state -> Round.t -> (state * action) Lwt.t

(** Propose (if possible) for the first time at a new level. *)
val time_to_bake_at_next_level : state -> Round.t -> (state * action) Lwt.t

val update_locked_round : state -> Round.t -> Block_payload_hash.t -> state

val make_endorse_action : state -> proposal -> action

val prequorum_reached_when_awaiting_preendorsements :
  state ->
  Operation_worker.candidate ->
  Kind.preendorsement operation list ->
  (state * action) Lwt.t

val quorum_reached_when_waiting_endorsements :
  state ->
  Operation_worker.candidate ->
  Kind.endorsement operation list ->
  (state * action) Lwt.t

val step : state -> event -> (state * action) Lwt.t
