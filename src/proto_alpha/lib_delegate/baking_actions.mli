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

type block_kind =
  | Fresh of Operation_pool.pool
  | Reproposal of {
      consensus_operations : packed_operation list;
      payload_hash : Block_payload_hash.t;
      payload_round : Round.t;
      payload : Operation_pool.payload;
    }

type block_to_bake = {
  predecessor : block_info;
  round : Round.t;
  delegate : consensus_key_and_delegate;
  kind : block_kind;
  force_apply : bool;
      (** if true, while baking the block, try and apply the block and its
          operations instead of only validating them. this can be permanently
          set using the [--force-apply] flag (see [force_apply_switch_arg] in
          [baking_commands.ml]). *)
}

type inject_block_kind =
  | Forge_and_inject of block_to_bake
      (** Forge and inject a freshly forged block. [block_to_bake] should be
          used in the forging process. *)
  | Inject_only of signed_block
      (** Inject [signed_block]. The baker can pre-emptively forge a signed
          block with the [Forge_block] action if it knows it is the next baker
          and it is idle. *)

type action =
  | Do_nothing
  | Inject_block of {kind : inject_block_kind; updated_state : state}
  | Forge_block of {block_to_bake : block_to_bake; updated_state : state}
  | Inject_preattestations of {
      preattestations : (consensus_key_and_delegate * consensus_content) list;
    }
  | Inject_attestations of {
      attestations : (consensus_key_and_delegate * consensus_content) list;
    }
  | Update_to_level of level_update
  | Synchronize_round of round_update
  | Watch_proposal

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

val generate_seed_nonce_hash :
  Baking_configuration.nonce_config ->
  consensus_key ->
  Level.t ->
  (Nonce_hash.t * Nonce.t) option tzresult Lwt.t

val inject_block :
  updated_state:state -> state -> signed_block -> state tzresult Lwt.t

val sign_consensus_votes :
  state ->
  (consensus_key_and_delegate * consensus_content) list ->
  [`Preattestation | `Attestation] ->
  ((consensus_key * public_key_hash) * packed_operation * int32 * Round.t) list
  tzresult
  Lwt.t

val inject_consensus_votes :
  state ->
  (consensus_key_and_delegate * consensus_content) list ->
  [`Preattestation | `Attestation] ->
  unit tzresult Lwt.t

val sign_dal_attestations :
  state ->
  (consensus_key_and_delegate * Dal.Attestation.operation * int32) list ->
  (consensus_key_and_delegate
  * packed_operation
  * Dal.Attestation.operation
  * int32)
  list
  tzresult
  Lwt.t

val get_dal_attestations :
  state ->
  (consensus_key_and_delegate * Dal.Attestation.operation * int32) list tzresult
  Lwt.t

val prepare_waiting_for_quorum :
  state -> int * (slot:Slot.t -> int option) * Operation_worker.candidate

val start_waiting_for_preattestation_quorum : state -> unit Lwt.t

val start_waiting_for_attestation_quorum : state -> unit Lwt.t

val update_to_level : state -> level_update -> (state * t) tzresult Lwt.t

val pp_action : Format.formatter -> t -> unit

val compute_round : proposal -> Round.round_durations -> Round.t tzresult

val perform_action :
  state_recorder:(new_state:state -> unit tzresult Lwt.t) ->
  state ->
  t ->
  state tzresult Lwt.t
