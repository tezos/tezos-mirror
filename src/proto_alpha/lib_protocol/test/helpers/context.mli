(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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
open Environment

type t = B of Block.t | I of Incremental.t

val branch : t -> Block_hash.t

val get_level : t -> Raw_level.t tzresult

val get_endorsers : t -> Plugin.RPC.Validators.t list tzresult Lwt.t

val get_first_different_endorsers :
  t -> (Plugin.RPC.Validators.t * Plugin.RPC.Validators.t) tzresult Lwt.t

val get_endorser : t -> (public_key_hash * Slot.t list) tzresult Lwt.t

val get_endorser_n : t -> int -> (public_key_hash * Slot.t list) tzresult Lwt.t

val get_endorsing_power_for_delegate :
  t -> ?levels:Raw_level.t list -> public_key_hash -> int tzresult Lwt.t

val get_voting_power :
  t -> public_key_hash -> int64 Environment.Error_monad.shell_tzresult Lwt.t

val get_total_voting_power :
  t -> int64 Environment.Error_monad.shell_tzresult Lwt.t

val get_bakers :
  ?filter:(Plugin.RPC.Baking_rights.t -> bool) ->
  t ->
  public_key_hash list tzresult Lwt.t

val get_baker : t -> round:int -> public_key_hash tzresult Lwt.t

val get_first_different_baker :
  public_key_hash -> public_key_hash trace -> public_key_hash

val get_first_different_bakers :
  t -> (public_key_hash * public_key_hash) tzresult Lwt.t

val get_seed_nonce_hash : t -> Nonce_hash.t tzresult Lwt.t

(** Returns the seed of the cycle to which the block belongs to. *)
val get_seed : t -> Seed.seed tzresult Lwt.t

(** Returns all the constants of the protocol *)
val get_constants : t -> Constants.t tzresult Lwt.t

(** The default constants used in the test framework. To be used with
    [init_with_constants]. *)
val default_test_contants : Constants.parametric

val get_baking_reward_fixed_portion : t -> Tez.t tzresult Lwt.t

val get_bonus_reward : t -> endorsing_power:int -> Tez.t tzresult Lwt.t

val get_endorsing_reward :
  t -> expected_endorsing_power:int -> Tez.t tzresult Lwt.t

val get_liquidity_baking_subsidy : t -> Tez.t tzresult Lwt.t

val get_liquidity_baking_cpmm_address : t -> Contract.t tzresult Lwt.t

module Vote : sig
  val get_ballots : t -> Vote.ballots tzresult Lwt.t

  val get_ballot_list :
    t -> (Signature.Public_key_hash.t * Vote.ballot) list tzresult Lwt.t

  val get_current_period : t -> Voting_period.info tzresult Lwt.t

  val get_current_quorum : t -> int32 tzresult Lwt.t

  val get_participation_ema : Block.t -> int32 tzresult Lwt.t

  val get_listings :
    t -> (Signature.Public_key_hash.t * int64) list tzresult Lwt.t

  val get_proposals : t -> int64 Protocol_hash.Map.t tzresult Lwt.t

  val get_current_proposal : t -> Protocol_hash.t option tzresult Lwt.t

  val get_protocol : Block.t -> Protocol_hash.t Lwt.t

  val set_participation_ema : Block.t -> int32 -> Block.t Lwt.t
end

module Contract : sig
  val pp : Format.formatter -> Contract.t -> unit

  val equal : Contract.t -> Contract.t -> bool

  val pkh : Contract.t -> public_key_hash tzresult Lwt.t

  (** Returns the balance of a contract, by default the main balance.
      If the contract is implicit the frozen balances are available too:
      deposit, fees or rewards. *)
  val balance : t -> Contract.t -> Tez.t tzresult Lwt.t

  val frozen_bonds : t -> Contract.t -> Tez.t tzresult Lwt.t

  val balance_and_frozen_bonds : t -> Contract.t -> Tez.t tzresult Lwt.t

  val counter : t -> Contract.t -> Z.t tzresult Lwt.t

  val manager : t -> Contract.t -> Account.t tzresult Lwt.t

  val is_manager_key_revealed : t -> Contract.t -> bool tzresult Lwt.t

  val delegate : t -> Contract.t -> public_key_hash tzresult Lwt.t

  val delegate_opt : t -> Contract.t -> public_key_hash option tzresult Lwt.t

  val storage : t -> Contract.t -> Script.expr tzresult Lwt.t

  val script : t -> Contract.t -> Script.expr tzresult Lwt.t

  val script_hash : t -> Contract.t -> Script_expr_hash.t tzresult Lwt.t
end

module Delegate : sig
  type info = Delegate_services.info = {
    full_balance : Tez.t;
    current_frozen_deposits : Tez.t;
    frozen_deposits : Tez.t;
    staking_balance : Tez.t;
    frozen_deposits_limit : Tez.t option;
    delegated_contracts : Alpha_context.Contract.t list;
    delegated_balance : Tez.t;
    deactivated : bool;
    grace_period : Cycle.t;
    voting_power : int64;
  }

  val info : t -> public_key_hash -> Delegate_services.info tzresult Lwt.t

  val full_balance : t -> public_key_hash -> Tez.t tzresult Lwt.t

  val current_frozen_deposits : t -> public_key_hash -> Tez.t tzresult Lwt.t

  (** calls the RPC [frozen_deposits]: we're using a different name to
     be more easily distinguishable from [current_frozen_deposits] *)
  val initial_frozen_deposits : t -> public_key_hash -> Tez.t tzresult Lwt.t

  val staking_balance : t -> public_key_hash -> Tez.t tzresult Lwt.t

  val frozen_deposits_limit :
    t -> public_key_hash -> Tez.t option tzresult Lwt.t

  val deactivated : t -> public_key_hash -> bool tzresult Lwt.t

  val participation :
    t -> public_key_hash -> Delegate.participation_info tzresult Lwt.t
end

module Tx_rollup : sig
  val state : t -> Tx_rollup.t -> Tx_rollup_state.t tzresult Lwt.t

  (** [inbox ctxt tx_rollup] returns the inbox of this transaction
      rollup at the current level. If the inbox does not exist, the
      function returns an error. *)
  val inbox :
    t -> Tx_rollup.t -> Tx_rollup_level.t -> Tx_rollup_inbox.t tzresult Lwt.t
end

(** [init n] : returns an initial block with [n] initialized accounts
    and the associated implicit contracts *)
val init :
  ?rng_state:Random.State.t ->
  ?commitments:Commitment.t list ->
  ?initial_balances:int64 list ->
  ?consensus_threshold:int ->
  ?min_proposal_quorum:int32 ->
  ?bootstrap_contracts:Parameters.bootstrap_contract list ->
  ?level:int32 ->
  ?cost_per_byte:Tez.t ->
  ?liquidity_baking_subsidy:Tez.t ->
  ?endorsing_reward_per_slot:Tez.t ->
  ?baking_reward_bonus_per_slot:Tez.t ->
  ?baking_reward_fixed_portion:Tez.t ->
  ?origination_size:int ->
  ?blocks_per_cycle:int32 ->
  ?cycles_per_voting_period:int32 ->
  ?tx_rollup_enable:bool ->
  ?sc_rollup_enable:bool ->
  int ->
  (Block.t * Alpha_context.Contract.t list) tzresult Lwt.t

(** [init1] : returns an initial block with 1 initialized bootstrap account
    and the associated implicit contract *)
val init1 :
  ?rng_state:Random.State.t ->
  ?commitments:Commitment.t list ->
  ?initial_balances:int64 list ->
  ?consensus_threshold:int ->
  ?min_proposal_quorum:int32 ->
  ?level:int32 ->
  ?cost_per_byte:Tez.t ->
  ?liquidity_baking_subsidy:Tez.t ->
  ?endorsing_reward_per_slot:Tez.t ->
  ?baking_reward_bonus_per_slot:Tez.t ->
  ?baking_reward_fixed_portion:Tez.t ->
  ?origination_size:int ->
  ?blocks_per_cycle:int32 ->
  ?cycles_per_voting_period:int32 ->
  ?tx_rollup_enable:bool ->
  ?sc_rollup_enable:bool ->
  unit ->
  (Block.t * Alpha_context.Contract.t) tzresult Lwt.t

(** [init2] : returns an initial block with 2 initialized bootstrap accounts
    and the associated implicit contracts *)
val init2 :
  ?rng_state:Random.State.t ->
  ?commitments:Commitment.t list ->
  ?initial_balances:int64 list ->
  ?consensus_threshold:int ->
  ?min_proposal_quorum:int32 ->
  ?level:int32 ->
  ?cost_per_byte:Tez.t ->
  ?liquidity_baking_subsidy:Tez.t ->
  ?endorsing_reward_per_slot:Tez.t ->
  ?baking_reward_bonus_per_slot:Tez.t ->
  ?baking_reward_fixed_portion:Tez.t ->
  ?origination_size:int ->
  ?blocks_per_cycle:int32 ->
  ?cycles_per_voting_period:int32 ->
  ?tx_rollup_enable:bool ->
  ?sc_rollup_enable:bool ->
  unit ->
  (Block.t * Alpha_context.Contract.t * Alpha_context.Contract.t) tzresult Lwt.t

val init_with_constants :
  Constants.parametric ->
  int ->
  (Block.t * Alpha_context.Contract.t list) tzresult Lwt.t

(** [default_raw_context] returns a [Raw_context.t] for use in tests
    below [Alpha_context] *)
val default_raw_context : unit -> Raw_context.t tzresult Lwt.t
