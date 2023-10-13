(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** Tezos Protocol Implementation - Typed storage

    This module hides the hierarchical (key x value) database under
    pre-allocated typed accessors for all persistent entities of the
    tezos context.

    This interface enforces no invariant on the contents of the
    database. Its goal is to centralize all accessors in order to have
    a complete view over the database contents and avoid key
    collisions. *)

open Storage_sigs

module type Simple_single_data_storage = sig
  type value

  val get : Raw_context.t -> value tzresult Lwt.t

  val update : Raw_context.t -> value -> Raw_context.t tzresult Lwt.t

  val init : Raw_context.t -> value -> Raw_context.t tzresult Lwt.t
end

module Block_round : Simple_single_data_storage with type value = Round_repr.t

type missed_attestations_info = {remaining_slots : int; missed_levels : int}

module Slashed_deposits_history : sig
  type slashed_percentage = Percentage.t

  type t = (Cycle_repr.t * slashed_percentage) list

  (** [add cycle percentage history] adds the [percentage] for the [cycle] in
      the [history].
      If the cycle exists, the associated percentage is updated and capped at
      100 and the cycle order in the list is unchanged.
      If the cycle does not exist, the new pair [(cycle, percentage)] is added
      at the beginning of the list.
  *)
  val add : Cycle_repr.t -> slashed_percentage -> t -> t

  (** [get cycle history] returns the percentage for [cycle] in [history] or
      0 if there is no such cycle. *)
  val get : Cycle_repr.t -> t -> slashed_percentage
end

module Unstake_request : sig
  type request = Cycle_repr.t * Tez_repr.t

  type requests = request list

  type t = {delegate : Signature.Public_key_hash.t; requests : requests}

  val add : Cycle_repr.t -> Tez_repr.t -> requests -> requests tzresult
end

module Contract : sig
  (** Storage from this submodule must only be accessed through the
      module `Contract`. *)

  module Global_counter :
    Simple_single_data_storage with type value = Manager_counter_repr.t

  (** The domain of alive contracts *)
  val fold :
    Raw_context.t ->
    order:[`Sorted | `Undefined] ->
    init:'a ->
    f:(Contract_repr.t -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  val list : Raw_context.t -> Contract_repr.t list Lwt.t

  (** see {!Raw_context_intf.T.local_context} *)
  type local_context

  (** see {!Raw_context_intf.T.with_local_context} *)
  val with_local_context :
    Raw_context.t ->
    Contract_repr.t ->
    (local_context -> (local_context * 'a) tzresult Lwt.t) ->
    (Raw_context.t * 'a) tzresult Lwt.t

  (** The tez possessed by a contract and that can be used. A contract
     may also possess tez in frozen deposits. Empty balances (of zero
     tez) are only allowed for originated contracts, not for implicit
     ones. *)
  module Spendable_balance :
    Indexed_data_storage_with_local_context
      with type key = Contract_repr.t
       and type value = Tez_repr.t
       and type t := Raw_context.t
       and type local_context := local_context

  (** If the value is not set, the delegate didn't miss any attesting
     opportunity.  If it is set, this value is a record of type
     [missed_attestations_info], where:
   - [remaining_slots] is the difference between the maximum number of
     slots that can be missed and the number of missed slots;
     therefore, when the number is positive, it represents the number
     of slots that a delegate can still miss before forfeiting its
     attesting rewards for the current cycle; when the number is zero
     it means rewards are not lost, but no further slots can be
     missed anymore;
   - [missed_levels] represents the number of missed levels (for
     attesting). *)
  module Missed_attestations :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = missed_attestations_info
       and type t := Raw_context.t

  (** The manager of a contract *)
  module Manager :
    Indexed_data_storage_with_local_context
      with type key = Contract_repr.t
       and type value = Manager_repr.t
       and type t := Raw_context.t
       and type local_context := local_context

  (** The active consensus key of a delegate *)
  module Consensus_key :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Signature.Public_key.t
       and type t := Raw_context.t

  (** The delegate of a contract, if any. *)
  module Delegate :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Signature.Public_key_hash.t
       and type t := Raw_context.t

  module Staking_parameters :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Staking_parameters_repr.t
       and type t := Raw_context.t

  (** All contracts (implicit and originated) that are delegated, if any  *)
  module Delegated :
    Data_set_storage
      with type elt = Contract_repr.t
       and type t = Raw_context.t * Contract_repr.t

  (** Tez that were part of frozen deposits (either [own_frozen] or
      [staked_frozen] in {!Staking_balance}) but have been requested to be
      unstaked by a staker.
      They won't be part of the stake for future distributions.
      For cycles [current_cycle - preserved_cycles - max_slashing_period + 1] to
      [current_cycle] they are still slashable.
      For cycle [current_cycle - preserved_cycles - max_slashing_period] they are
      not slashable anymore and hence any other older cycles must be squashed
      into this one at cycle end. *)
  module Unstaked_frozen_deposits :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Unstaked_frozen_deposits_repr.t
       and type t := Raw_context.t

  (** The contract's unstake requests that haven't been finalized yet. *)
  module Unstake_requests :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Unstake_request.t
       and type t := Raw_context.t

  (** The sum of all pseudotokens owned by stakers
      corresponding to shares of the [staked_frozen] in {!Staking_balance}. *)
  module Frozen_deposits_pseudotokens :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Staking_pseudotoken_repr.t
       and type t := Raw_context.t

  (** Share of the contract's delegate frozen deposits the contract owns. *)
  module Staking_pseudotokens :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Staking_pseudotoken_repr.t
       and type t := Raw_context.t

  (** If there is a value, the frozen balance for the contract won't
     exceed it (starting in preserved_cycles + 1). *)
  module Frozen_deposits_limit :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Tez_repr.t
       and type t := Raw_context.t

  module Inactive_delegate :
    Data_set_storage with type elt = Contract_repr.t and type t = Raw_context.t

  (** The last cycle where the delegate is considered active; that is,
     at the next cycle it will be considered inactive. *)
  module Delegate_last_cycle_before_deactivation :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Cycle_repr.t
       and type t := Raw_context.t

  module Counter :
    Indexed_data_storage_with_local_context
      with type key = Contract_repr.t
       and type value = Manager_counter_repr.t
       and type t := Raw_context.t
       and type local_context := local_context

  module Code :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Contract_repr.t
       and type value = Script_repr.lazy_expr
       and type t := Raw_context.t

  module Storage :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Contract_repr.t
       and type value = Script_repr.lazy_expr
       and type t := Raw_context.t

  (** Current storage space in bytes.
      Includes code, global storage and big map elements. *)
  module Used_storage_space :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Z.t
       and type t := Raw_context.t

  (** Maximal space available without needing to burn new fees. *)
  module Paid_storage_space :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Z.t
       and type t := Raw_context.t

  (** History of slashed deposits: an associative list of cycles to slashed
      percentages.

      This storage is inefficient but is not expected to grow large (as of
      2023-11-28, the last slashing on mainnet dates back to:
      - 2021-12-17 for double baking (154 events in total),
      - 2019-08-08 for double endorsing (24 events in total).
      Since slashings are here grouped by baker and cycle, there would only be
      a few elements in each list.

      The slashing percentages are used to compute the real value of stake
      withdrawals.
      Currently there is no limit to the age of the events we need to store
      because there is no such limit for stake withdrawals.
      At worst we can revisit this decision in a later protocol amendment (in
      25 cycles) or clean up this storage manually or automatically.
  *)
  module Slashed_deposits :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Slashed_deposits_history.t
       and type t := Raw_context.t

  (** Associates a contract and a bond_id with a bond, i.e. an amount of tez
      that is frozen. *)
  module Frozen_bonds :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Bond_id_repr.t
       and type value = Tez_repr.t
       and type t := Raw_context.t * Contract_repr.t

  val fold_bond_ids :
    Raw_context.t * Contract_repr.t ->
    order:[`Sorted | `Undefined] ->
    init:'a ->
    f:(Bond_id_repr.t -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  (** Associates a contract with the total of all its frozen bonds. *)
  module Total_frozen_bonds :
    Indexed_data_storage
      with type key = Contract_repr.t
       and type value = Tez_repr.t
       and type t := Raw_context.t

  (** Stores the amount of tokens currently present on chain *)
  module Total_supply :
    Single_data_storage with type value = Tez_repr.t and type t := Raw_context.t
end

module Big_map : sig
  type id = Lazy_storage_kind.Big_map.Id.t

  module Next : sig
    val incr : Raw_context.t -> (Raw_context.t * id) tzresult Lwt.t

    val init : Raw_context.t -> Raw_context.t tzresult Lwt.t
  end

  (** The domain of alive big maps *)
  val fold :
    Raw_context.t ->
    order:[`Sorted | `Undefined] ->
    init:'a ->
    f:(id -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  val list : Raw_context.t -> id list Lwt.t

  val remove : Raw_context.t -> id -> Raw_context.t Lwt.t

  val copy : Raw_context.t -> from:id -> to_:id -> Raw_context.t tzresult Lwt.t

  type key = Raw_context.t * id

  val rpc_arg : id RPC_arg.t

  module Contents : sig
    include
      Non_iterable_indexed_carbonated_data_storage
        with type key = Script_expr_hash.t
         and type value = Script_repr.expr
         and type t := key

    val list_key_values :
      ?offset:int ->
      ?length:int ->
      Raw_context.t * id ->
      (Raw_context.t * (Script_expr_hash.t * Script_repr.expr) list) tzresult
      Lwt.t
  end

  module Total_bytes :
    Indexed_data_storage_with_local_context
      with type key = id
       and type value = Z.t
       and type t := Raw_context.t

  module Key_type :
    Indexed_data_storage
      with type key = id
       and type value = Script_repr.expr
       and type t := Raw_context.t

  module Value_type :
    Indexed_data_storage
      with type key = id
       and type value = Script_repr.expr
       and type t := Raw_context.t
end

module Sapling : sig
  type id = Lazy_storage_kind.Sapling_state.Id.t

  val rpc_arg : id RPC_arg.t

  module Next : sig
    val incr : Raw_context.t -> (Raw_context.t * id) tzresult Lwt.t

    val init : Raw_context.t -> Raw_context.t tzresult Lwt.t
  end

  val copy : Raw_context.t -> from:id -> to_:id -> Raw_context.t tzresult Lwt.t

  val remove : Raw_context.t -> id -> Raw_context.t Lwt.t

  module Total_bytes :
    Indexed_data_storage
      with type key = id
       and type value = Z.t
       and type t := Raw_context.t

  (* Used by both Commitments and Ciphertexts *)
  module Commitments_size :
    Single_data_storage with type t := Raw_context.t * id and type value = int64

  module Memo_size :
    Single_data_storage with type t := Raw_context.t * id and type value = int

  module Commitments :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t * id
       and type key = int64
       and type value = Sapling.Hash.t

  val commitments_init : Raw_context.t -> id -> Raw_context.t Lwt.t

  module Ciphertexts :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t * id
       and type key = int64
       and type value = Sapling.Ciphertext.t

  val ciphertexts_init : Raw_context.t -> id -> Raw_context.t Lwt.t

  module Nullifiers_size :
    Single_data_storage with type t := Raw_context.t * id and type value = int64

  module Nullifiers_ordered :
    Non_iterable_indexed_data_storage
      with type t := Raw_context.t * id
       and type key = int64
       and type value = Sapling.Nullifier.t

  module Nullifiers_hashed :
    Carbonated_data_set_storage
      with type t := Raw_context.t * id
       and type elt = Sapling.Nullifier.t

  val nullifiers_init : Raw_context.t -> id -> Raw_context.t Lwt.t

  module Roots :
    Non_iterable_indexed_data_storage
      with type t := Raw_context.t * id
       and type key = int32
       and type value = Sapling.Hash.t

  module Roots_pos :
    Single_data_storage with type t := Raw_context.t * id and type value = int32

  module Roots_level :
    Single_data_storage
      with type t := Raw_context.t * id
       and type value = Raw_level_repr.t
end

(** Set of all registered delegates. *)
module Delegates :
  Data_set_storage
    with type t := Raw_context.t
     and type elt = Signature.Public_key_hash.t

(** Set of all active consensus keys in cycle `current + preserved_cycles + 1` *)
module Consensus_keys :
  Data_set_storage
    with type t := Raw_context.t
     and type elt = Signature.Public_key_hash.t

(** The pending consensus key of a delegate at the given cycle *)
module Pending_consensus_keys :
  Indexed_data_storage
    with type t := Raw_context.t * Cycle_repr.t
     and type key = Contract_repr.t
     and type value = Signature.public_key

(** All denunciations of the current and previous cycles that will have an effect
    (slashing, reward), i.e. all below 100%, deferred to the end of their
    slashing period. *)
module Pending_denunciations :
  Indexed_data_storage
    with type t := Raw_context.t
     and type key = Signature.public_key_hash
     and type value = Denunciations_repr.t

(** This type is used to track which denunciations have already been
    recorded, to avoid slashing multiple times the same event. *)
type denounced = {for_double_attesting : bool; for_double_baking : bool}

(** {!denounced} with all fields set to [false]. *)
val default_denounced : denounced

(** Set used to avoid slashing multiple times the same event *)
module Already_denounced :
  Indexed_data_storage
    with type t := Raw_context.t * Cycle_repr.t
     and type key =
      (Raw_level_repr.t * Round_repr.t) * Signature.Public_key_hash.t
     and type value = denounced

(** Needed for the stitching from Oxford to P. Remove this in Q. *)
module Already_denounced__Oxford :
  Indexed_data_storage
    with type t := Raw_context.t * Cycle_repr.t
     and type key = Raw_level_repr.t * Signature.Public_key_hash.t
     and type value = denounced

module Pending_staking_parameters :
  Indexed_data_storage
    with type t := Raw_context.t * Cycle_repr.t
     and type key = Contract_repr.t
     and type value = Staking_parameters_repr.t

module Stake : sig
  (** The map of all the stake of all delegates, including those with
      less than {!Constants_parametric_repr.minimal_stake}. It might
      be large. *)
  module Staking_balance :
    Indexed_data_storage
      with type key = Signature.Public_key_hash.t
       and type value = Full_staking_balance_repr.t
       and type t := Raw_context.t

  (** This should be fairly small compared to staking balance *)
  module Active_delegates_with_minimal_stake :
    Data_set_storage
      with type elt = Signature.Public_key_hash.t
       and type t := Raw_context.t

  (** List of active stake *)
  module Selected_distribution_for_cycle :
    Indexed_data_storage
      with type key = Cycle_repr.t
       and type value = (Signature.Public_key_hash.t * Stake_repr.t) list
       and type t := Raw_context.t

  (** Sum of the active stakes of all the delegates with
      {!Constants_parametric_repr.minimal_stake} *)
  module Total_active_stake :
    Indexed_data_storage
      with type key = Cycle_repr.t
       and type value = Stake_repr.t
       and type t := Raw_context.t
end

(** State of the sampler used to select delegates. Managed synchronously
    with [Stake.Selected_distribution_for_cycle]. *)
module Delegate_sampler_state :
  Indexed_data_storage
    with type key = Cycle_repr.t
     and type value = Raw_context.consensus_pk Sampler.t
     and type t := Raw_context.t

(** Compounding reward bonus for Adaptive Issuance *)
module Issuance_bonus :
  Indexed_data_storage
    with type key = Cycle_repr.t
     and type value = Issuance_bonus_repr.t
     and type t := Raw_context.t

(** Multiplicative coefficient for rewards under Adaptive Issuance
    (Includes the bonus) *)
module Issuance_coeff :
  Indexed_data_storage
    with type key = Cycle_repr.t
     and type value = Q.t
     and type t := Raw_context.t

(** Votes *)

module Vote : sig
  module Pred_period_kind :
    Single_data_storage
      with type value = Voting_period_repr.kind
       and type t := Raw_context.t

  module Current_period :
    Single_data_storage
      with type value = Voting_period_repr.t
       and type t := Raw_context.t

  (** Participation exponential moving average, in centile of percentage *)
  module Participation_ema :
    Single_data_storage with type value = int32 and type t := Raw_context.t

  module Current_proposal :
    Single_data_storage
      with type value = Protocol_hash.t
       and type t := Raw_context.t

  (** Sum of voting weights of all delegates. *)
  module Voting_power_in_listings :
    Single_data_storage with type value = int64 and type t := Raw_context.t

  (** Contains all delegates with their assigned voting weight. *)
  module Listings :
    Indexed_data_storage
      with type key = Signature.Public_key_hash.t
       and type value = int64
       and type t := Raw_context.t

  (** Set of protocol proposal with corresponding proposer delegate *)
  module Proposals :
    Data_set_storage
      with type elt = Protocol_hash.t * Signature.Public_key_hash.t
       and type t := Raw_context.t

  (** Keeps for each delegate the number of proposed protocols *)
  module Proposals_count :
    Indexed_data_storage
      with type key = Signature.Public_key_hash.t
       and type value = int
       and type t := Raw_context.t

  (** Contains for each delegate its ballot *)
  module Ballots :
    Indexed_data_storage
      with type key = Signature.Public_key_hash.t
       and type value = Vote_repr.ballot
       and type t := Raw_context.t
end

module type FOR_CYCLE = sig
  val init :
    Raw_context.t ->
    Cycle_repr.t ->
    Seed_repr.seed ->
    Raw_context.t tzresult Lwt.t

  val mem : Raw_context.t -> Cycle_repr.t -> bool Lwt.t

  val get : Raw_context.t -> Cycle_repr.t -> Seed_repr.seed tzresult Lwt.t

  val update :
    Raw_context.t ->
    Cycle_repr.t ->
    Seed_repr.seed ->
    Seed_repr.seed_status ->
    Raw_context.t tzresult Lwt.t

  val remove_existing :
    Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t
end

(** Seed *)

module Seed_status :
  Simple_single_data_storage with type value = Seed_repr.seed_status

module Seed : sig
  (** Storage from this submodule must only be accessed through the
      module `Seed`. *)

  type unrevealed_nonce = {
    nonce_hash : Nonce_hash.t;
    delegate : Signature.Public_key_hash.t;
  }

  type nonce_status =
    | Unrevealed of unrevealed_nonce
    | Revealed of Seed_repr.nonce

  module Nonce :
    Non_iterable_indexed_data_storage
      with type key := Level_repr.t
       and type value := nonce_status
       and type t := Raw_context.t

  module VDF_setup :
    Single_data_storage
      with type value = Seed_repr.vdf_setup
       and type t := Raw_context.t

  module For_cycle : FOR_CYCLE

  val get_status : Raw_context.t -> Seed_repr.seed_status tzresult Lwt.t
end

(** Commitments *)

module Commitments :
  Indexed_data_storage
    with type key = Blinded_public_key_hash.t
     and type value = Tez_repr.t
     and type t := Raw_context.t

(** Ramp up rewards *)
module Ramp_up : sig
  type reward = {
    baking_reward_fixed_portion : Tez_repr.t;
    baking_reward_bonus_per_slot : Tez_repr.t;
    attesting_reward_per_slot : Tez_repr.t;
  }

  module Rewards :
    Indexed_data_storage
      with type key = Cycle_repr.t
       and type value := reward
       and type t := Raw_context.t
end

module Pending_migration : sig
  module Balance_updates :
    Single_data_storage
      with type value = Receipt_repr.balance_updates
       and type t := Raw_context.t

  module Operation_results :
    Single_data_storage
      with type value = Migration_repr.origination_result list
       and type t := Raw_context.t

  val remove :
    Raw_context.t ->
    (Raw_context.t
    * Receipt_repr.balance_updates
    * Migration_repr.origination_result list)
    tzresult
    Lwt.t
end

module Liquidity_baking : sig
  (** Exponential moving average (ema) of flags set in protocol_data.contents.
    The liquidity baking subsidy is not sent to the CPMM if this EMA is above
    the threshold set in constants. **)
  module Toggle_ema :
    Single_data_storage with type t := Raw_context.t and type value = Int32.t

  (** Constant product market maker contract that receives liquidity baking subsidy. **)
  module Cpmm_address :
    Single_data_storage
      with type t := Raw_context.t
       and type value = Contract_hash.t
end

module Adaptive_issuance : sig
  (** Exponential moving average (ema) of votes set in the block header
      protocol_data.contents. Once the feature is activated, it can no
      longer be deactivated without a protocol amendment. **)
  module Launch_ema :
    Single_data_storage with type t := Raw_context.t and type value = Int32.t

  (** Cycle [Some c] from which adaptive issuance is (or will be)
     active, or [None] if the feature is not yet planned to activate. **)
  module Activation :
    Single_data_storage
      with type t := Raw_context.t
       and type value = Cycle_repr.t option
end

(** A map of [Script_repr.expr] values, indexed by their hash ([Script_expr_hash.t]).
    Values from this map can be incorporated by any contract via the primitive
    [Michelson_v1_primitives.H_constant]. *)
module Global_constants : sig
  module Map :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t
       and type key = Script_expr_hash.t
       and type value = Script_repr.expr
end

(** This module exposes a balance table for tracking ticket ownership.
    The table is a mapping from keys to values where the keys consist of a
    hashed representation of:
      - A ticketer, i.e. the creator of the ticket
      - The content of a the ticket
      - The contract that owns some amount of the ticket
    The values of the table are the amounts owned by each key.
 *)
module Ticket_balance : sig
  module Table :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t
       and type key = Ticket_hash_repr.t
       and type value = Z.t

  module Paid_storage_space :
    Single_data_storage with type t := Raw_context.t and type value = Z.t

  module Used_storage_space :
    Single_data_storage with type t := Raw_context.t and type value = Z.t
end

(** Tenderbake *)

module Tenderbake : sig
  (** [First_level_of_protocol] stores the level of the first block of
      this protocol. *)
  module First_level_of_protocol :
    Single_data_storage
      with type t := Raw_context.t
       and type value = Raw_level_repr.t

  (** [Attestation_branch] stores a single value composed of the
      grandparent hash and the predecessor's payload (computed with
      the grandparent hash) used to verify the validity of
      attestations. *)
  module Attestation_branch :
    Single_data_storage
      with type value = Block_hash.t * Block_payload_hash.t
       and type t := Raw_context.t

  (** [Forbidden_delegates] stores the set of delegates that are not
      allowed to bake or attest blocks. *)
  module Forbidden_delegates :
    Single_data_storage
      with type value = Signature.Public_key_hash.Set.t
       and type t := Raw_context.t
end

module Sc_rollup : sig
  (** Smart contract rollup.

      Storage from this submodule must only be accessed through the
      module `Sc_rollup_storage`.

      Each smart contract rollup is associated to:

      - a PVM kind (provided at creation time, read-only)
      - a metadata (generated at creation time, read-only)
      - a boot sector (provided at creation time, read-only)
      - a parameters type specifying the types of parameters the rollup accepts
      - the L1 block level at which the rollup was created
      - a merkelized inbox, of which only the root hash is stored
      - a map from stakers to their newest staked commitments
      - a map from stakers to commitments
      - a map from commitments to the time (level) of their first insertion

      For performance reasons we also store (per rollup):

      - the total number of active stakers;
      - the number of stakers per commitment.
      - the commitments per inbox level.

      See module {!Sc_rollup_repr.Commitment} for details.
  *)
  module PVM_kind :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Sc_rollup_repr.t
       and type value = Sc_rollups.Kind.t
       and type t := Raw_context.t

  module Parameters_type :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Sc_rollup_repr.t
       and type value = Script_repr.lazy_expr
       and type t := Raw_context.t

  module Genesis_info :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Sc_rollup_repr.t
       and type value = Sc_rollup_commitment_repr.genesis_info
       and type t := Raw_context.t

  module Inbox :
    Single_data_storage
      with type value = Sc_rollup_inbox_repr.t
       and type t := Raw_context.t

  module Last_cemented_commitment :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Sc_rollup_repr.t
       and type value = Sc_rollup_commitment_repr.Hash.t
       and type t := Raw_context.t

  (** Contains the current latest attributed index for stakers. *)
  module Staker_index_counter :
    Single_data_storage
      with type value = Sc_rollup_staker_index_repr.t
       and type t = Raw_context.t * Sc_rollup_repr.t

  (** Contains the index of any staker that currently have stake. *)
  module Staker_index :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Signature.Public_key_hash.t
       and type value = Sc_rollup_staker_index_repr.t
       and type t = Raw_context.t * Sc_rollup_repr.t

  (** Contains the most recent inbox level staked by an active staker. *)
  module Stakers :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Sc_rollup_staker_index_repr.t
       and type value = Raw_level_repr.t
       and type t = Raw_context.t * Sc_rollup_repr.t

  module Commitments :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Sc_rollup_commitment_repr.Hash.t
       and type value = Sc_rollup_commitment_repr.t
       and type t = Raw_context.t * Sc_rollup_repr.t

  (** Contains for all commitment not yet cemented the list of stakers that have
      staked on it. *)
  module Commitment_stakers :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Sc_rollup_commitment_repr.Hash.t
       and type value = Sc_rollup_staker_index_repr.t list
       and type t = Raw_context.t * Sc_rollup_repr.t

  (** This storage contains for each rollup and inbox level not yet cemented the
      level of publication of the first commitment. This is used to compute the
      curfew for a given rollup and inbox level.

      The storage size is bounded for each rollup by

                          [max_lookahead / commitment_period]

      Since the storage is cleaned when commitments are cemented, this storage
      space is only temporarily bought by stakers with their deposits.
  *)
  module Commitment_first_publication_level :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Raw_level_repr.t
       and type value = Raw_level_repr.t
       and type t = Raw_context.t * Sc_rollup_repr.t

  (** Stores the commitments published for an inbox level. *)
  module Commitments_per_inbox_level :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Raw_level_repr.t
       and type value = Sc_rollup_commitment_repr.Hash.t list
       and type t = Raw_context.t * Sc_rollup_repr.t

  module Commitment_added :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Sc_rollup_commitment_repr.Hash.t
       and type value = Raw_level_repr.t
       and type t = Raw_context.t * Sc_rollup_repr.t

  module Game_info :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Sc_rollup_game_repr.Index.t
       and type value = Sc_rollup_game_repr.t
       and type t = Raw_context.t * Sc_rollup_repr.t

  (** Refutation games are indexed by the rollup, by one staker, and
      by its opponent staker. Hence, each game appears twice. This is
      convenient to quickly compute the opponents of a given staker. *)
  module Game :
    Indexed_carbonated_data_storage
      with type key = Signature.Public_key_hash.t
       and type value = Sc_rollup_game_repr.Index.t
       and type t =
        (Raw_context.t * Sc_rollup_repr.t) * Signature.Public_key_hash.t

  (** [Game_timeout] stores the block level at which the staker whose
      turn it is to move will (become vulnerable to) timeout. The staker
      pair should always be in lexical order to ensure that this value is
      not duplicated.
  *)
  module Game_timeout :
    Non_iterable_indexed_carbonated_data_storage
      with type key = Sc_rollup_game_repr.Index.t
       and type value = Sc_rollup_game_repr.timeout
       and type t = Raw_context.t * Sc_rollup_repr.t

  (** A carbonated storage for keeping track of applied outbox messages for a
      a SCORU.

      The [key] is an [int32] value that represents the index of a SCORU's
      outbox level. An outbox level is mapped to the index through:

      [index = outbox_level % sc_rollup_max_active_outbox_levels]

      The rationale is to keep a limited number of entries. The current value of
      an entry contains the most recently added level that maps to the index.

      The [value] is a pair of the actual outbox level and a bitset containing
      the set of applied messages.
    *)
  module Applied_outbox_messages :
    Non_iterable_indexed_carbonated_data_storage
      with type t = Raw_context.t * Sc_rollup_repr.t
       and type key = int32
       and type value = Raw_level_repr.t * Bitset.t

  (** A carbonated storage for stakers (identified by their public key hashes)
      that are able to stake on commitments. If the storage is
      empty then the rollup is public (anyone can publish commitments for the rollup),
      otherwise it is private (only the members of the whitelist can publish commitments). *)
  module Whitelist :
    Carbonated_data_set_storage
      with type t := Raw_context.t * Sc_rollup_repr.t
       and type elt = Signature.Public_key_hash.t

  (** Maximal space available for the whitelist without needing to burn new fees. *)
  module Whitelist_paid_storage_space :
    Indexed_data_storage
      with type key = Sc_rollup_repr.t
       and type value = Z.t
       and type t = Raw_context.t

  (** Current storage space in bytes used by the whitelist. *)
  module Whitelist_used_storage_space :
    Indexed_data_storage
      with type t = Raw_context.t
       and type key = Sc_rollup_repr.t
       and type value = Z.t

  (** Outbox level and message of the latest whitelist update of a given rollup. *)
  module Last_whitelist_update :
    Non_iterable_indexed_carbonated_data_storage
      with type t = Raw_context.t
       and type key = Sc_rollup_repr.t
       and type value = Sc_rollup_whitelist_repr.last_whitelist_update
end

module Dal : sig
  module Slot : sig
    (** This is a temporary storage for slot headers proposed onto the L1. *)
    module Headers :
      Non_iterable_indexed_data_storage
        with type t = Raw_context.t
         and type key = Raw_level_repr.t
         and type value = Dal_slot_repr.Header.t list

    (** This is a permanent storage for slot headers confirmed by the L1. *)
    module History :
      Single_data_storage
        with type t := Raw_context.t
         and type value = Dal_slot_repr.History.t
  end
end

module Zk_rollup : sig
  (** ZK rollup.

      Each ZK rollup is associated to:

      - an Account, as described in [Zk_rollup_repr]
      - a pending list description, consisting of its head's index and
        a counter
      - a map from integer indeces to L2 operations, to store the actual
        pending list
  *)
  module Account :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t
       and type key = Zk_rollup_repr.t
       and type value = Zk_rollup_account_repr.t

  module Pending_list :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t
       and type key = Zk_rollup_repr.t
       and type value = Zk_rollup_repr.pending_list

  module Pending_operation :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t * Zk_rollup_repr.t
       and type key = int64
       and type value = Zk_rollup_operation_repr.t * Ticket_hash_repr.t option
end

module Legacy : sig
  (** [Grand_parent_branch] stores a single value composed of the
      great-grand parent hash and the grand parent's payload *)
  module Grand_parent_branch :
    Single_data_storage
      with type value = Block_hash.t * Block_payload_hash.t
       and type t := Raw_context.t
end
