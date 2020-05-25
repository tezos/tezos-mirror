(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

type frozen_balance = {
  deposit : Tez_repr.t;
  fees : Tez_repr.t;
  rewards : Tez_repr.t;
}

type error +=
  | (* `Temporary *)
      Already_active of Baker_hash.t
  | (* `Temporary *)
      Already_inactive of Baker_hash.t
  | (* `Temporary *)
      Balance_too_low_for_deposit of {
      baker : Baker_hash.t;
      deposit : Tez_repr.t;
      balance : Tez_repr.t;
    }
  | (* `Permanent *)
      Unregistered_baker of Baker_hash.t
  | (* `Permanent *)
      Already_registered_baker of Baker_hash.t
  | (* `Temporary *)
      Already_used_consensus_key of Signature.Public_key.t
  | (* `Temporary *)
      Allocated_consensus_key_account of
      Signature.Public_key_hash.t

val fresh_baker_from_current_nonce :
  Raw_context.t -> (Raw_context.t * Baker_hash.t) tzresult Lwt.t

(** Register a new baker contract. *)
val register :
  ?baker_hash:Baker_hash.t ->
  ?prepaid_bootstrap_storage:bool ->
  Raw_context.t ->
  balance:Tez_repr.t ->
  threshold:int ->
  owner_keys:Signature.Public_key.t list ->
  consensus_key:Signature.Public_key.t ->
  (Raw_context.t * Baker_hash.t) tzresult Lwt.t

(** Given baker must registered, fails with [[Unregistered_baker]] otherwise. *)
val must_be_registered : Raw_context.t -> Baker_hash.t -> unit tzresult Lwt.t

(** Active or deactivate a baker. *)
val set_active :
  Raw_context.t -> Baker_hash.t -> bool -> Raw_context.t tzresult Lwt.t

(** Check if a given baker hash is registered as a baker. *)
val registered : Raw_context.t -> Baker_hash.t -> bool Lwt.t

(** Iterate on all registered bakers. *)
val fold :
  Raw_context.t -> init:'a -> f:(Baker_hash.t -> 'a -> 'a Lwt.t) -> 'a Lwt.t

(** List all registered bakers. *)
val list : Raw_context.t -> Baker_hash.t list Lwt.t

(** Returns the list of contracts (implicit or originated) that delegated towards a given baker *)
val delegated_contracts :
  Raw_context.t -> Baker_hash.t -> Contract_repr.t list Lwt.t

(** Various functions to 'freeze' tokens.  A frozen 'deposit' keeps its
    associated rolls. When frozen, 'fees' may trigger new rolls
    allocation. Rewards won't trigger new rolls allocation until
    unfrozen. *)
val freeze_deposit :
  Raw_context.t -> Baker_hash.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

val freeze_fees :
  Raw_context.t -> Baker_hash.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

val freeze_rewards :
  Raw_context.t -> Baker_hash.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

val init_first_cycles : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** Trigger the context maintenance at the end of cycle 'n', i.e.:
    unfreeze deposit/fees/rewards from 'n - preserved_cycle' ; punish the
    provided unrevealed seeds (typically seed from cycle 'n - 1').
    Returns a list of account with the amount that was unfrozen for each
    and the list of deactivated bakers. *)
val cycle_end :
  Raw_context.t ->
  Cycle_repr.t ->
  Nonce_storage.unrevealed list ->
  (Raw_context.t * Receipt_repr.balance_updates * Baker_hash.t list) tzresult
  Lwt.t

(** Burn all then frozen deposit/fees/rewards for a baker at a given
    cycle. Returns the burned amounts. *)
val punish :
  Raw_context.t ->
  Baker_hash.t ->
  Cycle_repr.t ->
  (Raw_context.t * frozen_balance) tzresult Lwt.t

(** Has the given key some frozen tokens in its implicit contract? *)
val has_frozen_balance :
  Raw_context.t -> Baker_hash.t -> Cycle_repr.t -> bool tzresult Lwt.t

(** Returns the amount of frozen deposit, fees and rewards associated
    to a given baker. *)
val frozen_balance : Raw_context.t -> Baker_hash.t -> Tez_repr.t tzresult Lwt.t

val frozen_balance_encoding : frozen_balance Data_encoding.t

val frozen_balance_by_cycle_encoding :
  frozen_balance Cycle_repr.Map.t Data_encoding.t

(** Returns the amount of frozen deposit, fees and rewards associated
    to a given baker, indexed by the cycle by which at the end the
    balance will be unfrozen. *)
val frozen_balance_by_cycle :
  Raw_context.t -> Baker_hash.t -> frozen_balance Cycle_repr.Map.t Lwt.t

(** Returns the full 'balance' of the implicit contract associated to
    a given key, i.e. the sum of the spendable balance and of the
    frozen balance. *)
val full_balance : Raw_context.t -> Baker_hash.t -> Tez_repr.t tzresult Lwt.t

val staking_balance :
  Raw_context.t -> Baker_hash.t -> Tez_repr.t tzresult Lwt.t

val delegated_balance :
  Raw_context.t -> Baker_hash.t -> Tez_repr.t tzresult Lwt.t

val deactivated : Raw_context.t -> Baker_hash.t -> bool tzresult Lwt.t

val grace_period : Raw_context.t -> Baker_hash.t -> Cycle_repr.t tzresult Lwt.t

module Proof : sig
  (** Has any proof ever been used at this level for this delegate*)
  val mem :
    Raw_context.t -> Baker_hash.t -> Raw_level_repr.t -> bool tzresult Lwt.t

  (** add a level to already known proofs level for this delegate*)
  val add :
    Raw_context.t ->
    Baker_hash.t ->
    Raw_level_repr.t ->
    Raw_context.t tzresult Lwt.t

  (** clean proof*)
  val cleanup : Raw_context.t -> Raw_context.t tzresult Lwt.t

  val all :
    Raw_context.t -> Baker_hash.t -> Raw_level_repr.LSet.t tzresult Lwt.t
end

(** Find if any baker uses the given public key hash as their pending consensus
    keys *)
val is_pending_consensus_key :
  Raw_context.t -> Contract_repr.t -> bool tzresult Lwt.t

(** Try to find a baker that uses the given public key hash as their
    consensus keys *)
val is_consensus_key :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Baker_hash.t option tzresult Lwt.t

val get_consensus_key :
  ?level:Raw_level_repr.t ->
  ?offset:int32 ->
  Raw_context.t ->
  Baker_hash.t ->
  Signature.Public_key.t tzresult Lwt.t

val set_consensus_key :
  Raw_context.t ->
  Baker_hash.t ->
  Signature.Public_key.t ->
  Raw_context.t tzresult Lwt.t

val init_set_pvss_key :
  Raw_context.t ->
  Baker_hash.t ->
  Pvss_secp256k1.Public_key.t ->
  Raw_context.t Lwt.t

val get_pvss_key :
  Raw_context.t ->
  Baker_hash.t ->
  Pvss_secp256k1.Public_key.t option tzresult Lwt.t
