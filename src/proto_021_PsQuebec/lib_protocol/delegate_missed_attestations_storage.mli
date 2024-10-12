(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
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

(** This modules deals with delegates' participation in consensus.

    This module is responsible for maintaining the
    {!Storage.Contract.Missed_attestations} table.  *)

val expected_slots_for_given_active_stake :
  Raw_context.t ->
  total_active_stake_weight:int64 ->
  active_stake_weight:int64 ->
  int

type level_participation = Participated | Didn't_participate

(** Record the participation of a delegate as a validator. *)
val record_attesting_participation :
  Raw_context.t ->
  delegate:Signature.Public_key_hash.t ->
  participation:level_participation ->
  attesting_power:int ->
  Raw_context.t tzresult Lwt.t

(** Sets the payload and block producer as active. Pays the baking
   reward and the fees to the payload producer and the reward bonus to
   the payload producer (if the reward_bonus is not None).*)
val record_baking_activity_and_pay_rewards_and_fees :
  Raw_context.t ->
  payload_producer:Signature.Public_key_hash.t ->
  block_producer:Signature.Public_key_hash.t ->
  baking_reward:Tez_repr.t ->
  reward_bonus:Tez_repr.t option ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** Check that a delegate participated enough in the last cycle
   (returns [true] if it did), and then reset the participation for
   preparing the next cycle. *)
val check_and_reset_delegate_participation :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  (Raw_context.t * bool) tzresult Lwt.t

module For_RPC : sig
  (** Participation information. We denote by:
      - "static" information that does not change during the cycle
      - "dynamic" information that may change during the cycle *)
  type participation_info = {
    expected_cycle_activity : int;
        (** The total expected slots to be attested in the cycle. (static) *)
    minimal_cycle_activity : int;
        (** The minimal attesting slots in the cycle to get attesting rewards.
          (static) *)
    missed_slots : int;
        (** The number of missed attesting slots in the cycle. (dynamic) *)
    missed_levels : int;
        (** The number of missed attesting levels in the cycle. (dynamic) *)
    remaining_allowed_missed_slots : int;
        (** Remaining amount of attesting slots that can be missed in the
      cycle before forfeiting the rewards. (dynamic) *)
    expected_attesting_rewards : Tez_repr.t;
        (** Attesting rewards that will be distributed at the end of the
     cycle if activity at that point will be greater than the minimal
     required. If the activity is already known to be below the
     required minimum, then the rewards are zero. (dynamic) *)
  }

  (** Only use this function for RPC: this is expensive.

      [delegate_participation_info] and [!val:check_delegate] forms the
      implementation of RPC call "/context/delegates/<pkh>/participation".
 *)
  val participation_info :
    Raw_context.t ->
    Signature.Public_key_hash.t ->
    participation_info tzresult Lwt.t
end
