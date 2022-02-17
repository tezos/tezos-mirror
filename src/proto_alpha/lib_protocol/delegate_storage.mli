(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Allow to register a delegate when creating an account. *)
val init :
  Raw_context.t ->
  Contract_repr.t ->
  Signature.Public_key_hash.t ->
  Raw_context.t tzresult Lwt.t

val pubkey :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Signature.Public_key.t tzresult Lwt.t

(** Updating the delegate of a contract.

    When calling this function on an "implicit contract" and setting
    the delegate to the contract manager registers it as a delegate. One
    cannot unregister a delegate for now. The associate contract is now
    'undeletable'. *)
val set :
  Raw_context.t ->
  Contract_repr.t ->
  Signature.Public_key_hash.t option ->
  Raw_context.t tzresult Lwt.t

val frozen_deposits_limit :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Tez_repr.t option tzresult Lwt.t

val set_frozen_deposits_limit :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Tez_repr.t option ->
  Raw_context.t Lwt.t

type error +=
  | (* `Permanent *) No_deletion of Signature.Public_key_hash.t
  | (* `Temporary *) Active_delegate
  | (* `Temporary *) Current_delegate
  | (* `Permanent *) Empty_delegate_account of Signature.Public_key_hash.t
  | (* `Permanent *) Unregistered_delegate of Signature.Public_key_hash.t
  | (* `Permanent *) Unassigned_validation_slot_for_level of Level_repr.t * int
  | (* `Permanent *)
      Cannot_find_active_stake of {
      cycle : Cycle_repr.t;
      delegate : Signature.Public_key_hash.t;
    }
  | (* `Temporary *) Not_registered of Signature.Public_key_hash.t

(** Check that a given implicit account is a registered delegate. *)
val check_delegate :
  Raw_context.t -> Signature.Public_key_hash.t -> unit tzresult Lwt.t

(** Participation information. We denote by:
    - "static" information that does not change during the cycle
    - "dynamic" information that may change during the cycle *)
type participation_info = {
  expected_cycle_activity : int;
      (** The total expected slots to be endorsed in the cycle. (static) *)
  minimal_cycle_activity : int;
      (** The minimal endorsing slots in the cycle to get endorsing
      rewards. (static) *)
  missed_slots : int;
      (** The number of missed endorsing slots in the cycle. (dynamic) *)
  missed_levels : int;
      (** The number of missed endorsing levels in the cycle. (dynamic) *)
  remaining_allowed_missed_slots : int;
      (** Remaining amount of endorsing slots that can be missed in the
      cycle before forfeiting the rewards. (dynamic) *)
  expected_endorsing_rewards : Tez_repr.t;
      (** Endorsing rewards that will be distributed at the end of the
     cycle if activity at that point will be greater than the minimal
     required. If the activity is already known to be below the
     required minimum, then the rewards are zero. (dynamic) *)
}

(** Only use this function for RPC: this is expensive.

   [delegate_participation_info] and [!val:check_delegate] forms the
   implementation of RPC call "/context/delegates/<pkh>/participation".
 *)
val delegate_participation_info :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  participation_info tzresult Lwt.t

(** Iterate on all registered delegates. *)
val fold :
  Raw_context.t ->
  order:[`Sorted | `Undefined] ->
  init:'a ->
  f:(Signature.Public_key_hash.t -> 'a -> 'a Lwt.t) ->
  'a Lwt.t

(** List all registered delegates. *)
val list : Raw_context.t -> Signature.Public_key_hash.t list Lwt.t

val balance :
  Raw_context.t -> Signature.public_key_hash -> Tez_repr.tez tzresult Lwt.t

type level_participation = Participated | Didn't_participate

(** Record the participation of a delegate as a validator. *)
val record_endorsing_participation :
  Raw_context.t ->
  delegate:Signature.Public_key_hash.t ->
  participation:level_participation ->
  endorsing_power:int ->
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

(** Trigger the context maintenance at the end of cycle 'n', i.e.:
   unfreeze the endorsing rewards, potentially deactivate delegates.
   Return the corresponding balances updates and the list of
   deactivated delegates. *)
val cycle_end :
  Raw_context.t ->
  Cycle_repr.t ->
  Storage.Seed.unrevealed_nonce list ->
  (Raw_context.t
  * Receipt_repr.balance_updates
  * Signature.Public_key_hash.t list)
  tzresult
  Lwt.t

(** Returns true if the given delegate has already been slashed
   for double baking for the given level. *)
val already_slashed_for_double_baking :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  bool tzresult Lwt.t

(** Returns true if the given delegate has already been slashed
   for double preendorsing or double endorsing for the given level. *)
val already_slashed_for_double_endorsing :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  bool tzresult Lwt.t

(** Burn some frozen deposit for a delegate at a given level. Returns
    the burned amount. *)
val punish_double_endorsing :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  (Raw_context.t * Tez_repr.t * Receipt_repr.balance_updates) tzresult Lwt.t

val punish_double_baking :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  (Raw_context.t * Tez_repr.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** Returns a delegate's frozen deposits, both the current amount and
   the initial freezed amount.

    A delegate's frozen balance is only composed of frozen deposits;
   rewards and fees are not frozen, but simply credited at the right
   moment.  *)
val frozen_deposits :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Storage.deposits tzresult Lwt.t

(** Returns the full 'balance' of the implicit contract associated to
    a given key, i.e. the sum of the spendable balance and of the
    frozen balance. The frozen balance includes all frozen bonds associated
    to the contract of the delegate.

    Only use this function for RPCs: this is expensive. *)
val full_balance :
  Raw_context.t -> Signature.Public_key_hash.t -> Tez_repr.t tzresult Lwt.t

val staking_balance :
  Raw_context.t -> Signature.Public_key_hash.t -> Tez_repr.t tzresult Lwt.t

(** Only use this function for RPCs: this is expensive. *)
val delegated_balance :
  Raw_context.t -> Signature.Public_key_hash.t -> Tez_repr.t tzresult Lwt.t

val deactivated :
  Raw_context.t -> Signature.Public_key_hash.t -> bool tzresult Lwt.t

(** Participation slots potentially associated to accounts. The
   accounts that didn't place a deposit will be excluded from this
   list. This function should only be used to compute the deposits to
   freeze or initialize the protocol while stitching. RPCs can use this
   function to predict an approximation of long term future slot
   allocations. It shouldn't be used in the baker. *)
val slot_owner :
  Raw_context.t ->
  Level_repr.t ->
  Slot_repr.t ->
  (Raw_context.t * (Signature.Public_key.t * Signature.Public_key_hash.t))
  tzresult
  Lwt.t

val baking_rights_owner :
  Raw_context.t ->
  Level_repr.t ->
  round:Round_repr.round ->
  (Raw_context.t
  * Slot_repr.t
  * (Signature.public_key * Signature.public_key_hash))
  tzresult
  Lwt.t

val freeze_deposits_do_not_call_except_for_migration :
  Raw_context.t ->
  new_cycle:Cycle_repr.t ->
  balance_updates:Receipt_repr.balance_updates ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** [init_first_cycles ctxt] computes and records the distribution of the total
    active stake among active delegates. This concerns the total active stake
    involved in the calculation of baking rights for all cycles in the range
    [0, preserved_cycles]. *)
val init_first_cycles : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** [compute_snapshot_index ctxt cycle max_snapshot_index] Returns the index of
    the selected snapshot for the [cycle] passed as argument, and for the max
    index of snapshots taken so far, [max_snapshot_index] (see
    [Stake_storage.max_snapshot_index]. *)
val compute_snapshot_index :
  Raw_context.t -> Cycle_repr.t -> max_snapshot_index:int -> int tzresult Lwt.t
