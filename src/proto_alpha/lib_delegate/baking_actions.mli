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
open Baking_state_types

(** {2 Action types}  *)

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
    delegate_infos:delegate_infos ->
    next_level_delegate_infos:delegate_infos ->
    (state * action) Lwt.t;
}

and round_update = {
  new_round_proposal : proposal;
  handle_proposal : state -> (state * action) Lwt.t;
}

type t = action

val pp_action : Format.formatter -> action -> unit

(** {2 Functions used by the baker} *)

(** [prepare_block global_state block_to_bake] prepares a block by:
    - inferring the block timestamp

    - recovering the operations from the mempool if the block is a new proposal,
    or reusing the operations from [block_to_bake]'s payload if the block is a
    reproposal

    - generating the seed nonce hash if needed

    - setting the vote for liquidity baking according to the
    per_block_vote_file

    - calling [Block_forge.forge] to forge the block

    - signing the block header

    - registering the seed nonce if needed
*)
val prepare_block :
  global_state -> block_to_bake -> prepared_block tzresult Lwt.t

(** [authorized_consensus_votes global_state unsigned_consensus_vote_batch]
    records and returns the list of unsigned_consensus_vote authorized according
    to the [Baking_highwatermarks]. This function emits an event for each
    unauthorized consensus vote.*)
val authorized_consensus_votes :
  global_state ->
  unsigned_consensus_vote_batch ->
  unsigned_consensus_vote list tzresult Lwt.t

(** [forge_and_sign_consensus_vote global_state branch unsigned_consensus_vote]
    forges a consensus operation by encoding the operation consensus content
    from [unsigned_consensus_vote] with the [branch] and then sign it. *)
val forge_and_sign_consensus_vote :
  global_state ->
  branch:Block_hash.t ->
  unsigned_consensus_vote ->
  signed_consensus_vote tzresult Lwt.t

(** [compute_round proposal round_durations] computes the round from the current
    timestamp, the [proposal]'s timestamp and the [round_durations]. *)
val compute_round : proposal -> Round.round_durations -> Round.t tzresult

(** [perform_action state action] performs the given [action] using the
    [state] *)
val perform_action : state -> t -> state tzresult Lwt.t

(** {2 Functions only needed for the baking_lib}  *)

(** [sign_consensus_votes global_state unsigned_consensus_vote_batch] recovers
    the authorized consensus votes by calling [authorized_consensus_votes], then
    signs these operation by calling [forge_and_sign_consensus_vote], and then
    creates a batch with [Baking_state.make_signed_consensus_vote_batch]. *)
val sign_consensus_votes :
  global_state ->
  unsigned_consensus_vote_batch ->
  signed_consensus_vote_batch tzresult Lwt.t

(** [inject_consensus_votes state signed_consensus_vote_batch] injects consensus
    votes from [signed_consensus_vote_batch]. *)
val inject_consensus_votes :
  state -> signed_consensus_vote_batch -> unit tzresult Lwt.t

(** [update_to_level state level_update] computes the delegate slots for the given
    level and the next one, computes round information by calling [compute_round]
    and updates the state accordingly. *)
val update_to_level : state -> level_update -> (state * t) tzresult Lwt.t

val only_if_dal_feature_enabled :
  state ->
  default_value:'a ->
  (Tezos_rpc.Context.generic -> 'a Lwt.t) ->
  'a Lwt.t

(** [may_get_dal_content state consensus_vote] retrieves DAL attestable slots
    by querying the [Dal_attestable_slots_worker] for the corresponding
    [attestation_level]. *)
val may_get_dal_content :
  state -> unsigned_consensus_vote -> dal_content option Lwt.t
