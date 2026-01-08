(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Alpha_context

type error +=
  | (* `Permanent *)
      Insufficient_attesting_power of {
      attesting_power : int64;
      consensus_threshold : int64;
    }

type ordered_slots = private {
  delegate : Signature.public_key_hash;
  consensus_key : Signature.public_key_hash;
  companion_key : Bls.Public_key_hash.t option;
  rounds : Round.t list;
  attesting_power : int64;
  attestation_slot : Slot.t;
}

(** For a given level computes who has the right to include an attestation in
   the next block.

   @return map from delegates with such rights to their attesting slots, in
   increasing order.

   This function is only used by the 'validators' RPC.  *)
val attesting_rights :
  context ->
  attested_level:Level.t ->
  (context * ordered_slots Signature.Public_key_hash.Map.t) tzresult Lwt.t

(** Computes attesting rights for a given level.

   @return map from allocated first slots to their owner's public key, consensus
     attesting power, and DAL attesting power. *)
val attesting_rights_by_first_slot :
  context ->
  attested_level:Level.t ->
  (context * Consensus_key.power Slot.Map.t) tzresult Lwt.t

(** Computes the bonus baking reward depending on the attestation power.

    [attested_level] is the level written in the attestations whose
    total [attesting_power] is provided, not the level of the block
    being baked. *)
val bonus_baking_reward :
  context ->
  attested_level:Level.t ->
  attesting_power:Attesting_power.t ->
  (context * Tez.t) tzresult Lwt.t
