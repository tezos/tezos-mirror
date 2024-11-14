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

open Protocol
open Alpha_context
open Baking_state

type action =
  | Do_nothing
  | Prepare_block of {block_to_bake : block_to_bake}
  | Prepare_preattestations of {preattestations : unsigned_consensus_vote_batch}
  | Prepare_attestations of {attestations : unsigned_consensus_vote_batch}
  | Prepare_consensus_votes of {
      preattestations : unsigned_consensus_vote_batch;
      attestations : unsigned_consensus_vote_batch;
    }
  | Inject_block of {
      prepared_block : prepared_block;
      force_injection : bool;
      asynchronous : bool;
    }
  | Inject_preattestation of {signed_preattestation : signed_consensus_vote}
  | Inject_attestations of {signed_attestations : signed_consensus_vote_batch}
  | Update_to_level of level_update
  | Synchronize_round of round_update
  | Watch_prequorum
  | Watch_quorum

and level_update = {
  new_level_proposal : proposal;
  compute_new_state :
    current_round:Round.t ->
    delegate_slots:delegate_slots ->
    next_level_delegate_slots:delegate_slots ->
    (state * action) Lwt.t;
}

and round_update = {
  new_round_proposal : proposal;
  handle_proposal : state -> (state * action) Lwt.t;
}

type t = action

val pp_action : Format.formatter -> action -> unit

val generate_seed_nonce_hash :
  Baking_configuration.nonce_config ->
  consensus_key ->
  Level.t ->
  (Nonce_hash.t * Nonce.t) option tzresult Lwt.t

val prepare_block :
  global_state -> block_to_bake -> prepared_block tzresult Lwt.t

val inject_block :
  ?force_injection:bool ->
  ?asynchronous:bool ->
  state ->
  prepared_block ->
  state tzresult Lwt.t

val may_get_dal_content :
  state -> unsigned_consensus_vote -> dal_content option tzresult Lwt.t

val authorized_consensus_votes :
  global_state ->
  unsigned_consensus_vote_batch ->
  unsigned_consensus_vote list tzresult Lwt.t

val forge_and_sign_consensus_vote :
  global_state ->
  branch:Block_hash.t ->
  unsigned_consensus_vote ->
  signed_consensus_vote tzresult Lwt.t

val sign_consensus_votes :
  global_state ->
  unsigned_consensus_vote_batch ->
  signed_consensus_vote_batch tzresult Lwt.t

val inject_consensus_votes :
  state -> signed_consensus_vote_batch -> unit tzresult Lwt.t

val prepare_waiting_for_quorum :
  state -> int * (slot:Slot.t -> int option) * Operation_worker.candidate

val start_waiting_for_preattestation_quorum : state -> unit Lwt.t

val start_waiting_for_attestation_quorum : state -> unit Lwt.t

val update_to_level : state -> level_update -> (state * t) tzresult Lwt.t

val compute_round : proposal -> Round.round_durations -> Round.t tzresult

val perform_action : state -> t -> state tzresult Lwt.t
