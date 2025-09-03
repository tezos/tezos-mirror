(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

type t = B of Block.t | I of Incremental.t

val get_alpha_ctxt : t -> context tzresult Lwt.t

val branch : t -> Block_hash.t

val pred_branch : t -> Block_hash.t

val get_level : t -> Raw_level.t tzresult

(** A delegate's keys and attesting slots at a given level. *)
type attester = Plugin.RPC.Validators.t = {
  level : Raw_level.t;
  delegate : Signature.public_key_hash;
  consensus_key : Signature.public_key_hash;
  companion_key : Signature.Bls.Public_key_hash.t option;
  slots : Slot.t list;
}

(** Retrieves the attesting rights at the level of the given context
    by calling {!Plugin.RPC.Validators.S.validators}. *)
val get_attesters : t -> attester list tzresult Lwt.t

(** Returns an attester at the level of the given context.

    If [manager_pkh] is provided, returns the attester with this
    manager key ({!field-delegate}) and fails if there is no such
    attester. If [manager_pkh] is omitted, returns the first element
    of the output of {!get_attesters}. *)
val get_attester : ?manager_pkh:public_key_hash -> t -> attester tzresult Lwt.t

(** Return the two first elements of the list returns by [get_attesters]. *)
val get_first_different_attesters :
  t -> (Plugin.RPC.Validators.t * Plugin.RPC.Validators.t) tzresult Lwt.t

(** Return the [n]th element of the list returns by [get_attesters]. *)
val get_attester_n : t -> int -> (public_key_hash * Slot.t list) tzresult Lwt.t

(** Whether the {!type-attester}'s **consensus key** is a BLS key. *)
val attester_has_bls_key : attester -> bool

(** Same as {!get_attesters} but returns only attesters with a BLS
    consensus key. *)
val get_attesters_with_bls_key : t -> attester list tzresult Lwt.t

(** Returns an attester with a BLS consensus key (the first eligible
    attester returned by {!get_attesters}). *)
val get_attester_with_bls_key : t -> attester tzresult Lwt.t

(** Counts the number of attesting slots that the given delegate has
    in the requested level. If ommited, [level] defaults to the next
    level. *)
val get_attesting_power_for_delegate :
  t -> ?level:Raw_level.t -> public_key_hash -> int tzresult Lwt.t

(** Sums the result of [get_attesting_power_for_delegate] over a list
    of levels. *)
val get_cumulated_attesting_power_for_delegate :
  t -> levels:Raw_level.t list -> public_key_hash -> int tzresult Lwt.t

val get_current_voting_power :
  t -> public_key_hash -> int64 Environment.Error_monad.shell_tzresult Lwt.t

val get_voting_power :
  t -> public_key_hash -> int64 Environment.Error_monad.shell_tzresult Lwt.t

val get_total_voting_power :
  t -> int64 Environment.Error_monad.shell_tzresult Lwt.t

val get_current_baking_power :
  t -> public_key_hash -> int64 Environment.Error_monad.shell_tzresult Lwt.t

val get_bakers :
  ?filter:(Plugin.RPC.Baking_rights.t -> bool) ->
  ?cycle:Cycle.t ->
  t ->
  public_key_hash list tzresult Lwt.t

val get_baker : t -> round:Round.t -> public_key_hash tzresult Lwt.t

val get_first_different_baker :
  public_key_hash -> public_key_hash trace -> public_key_hash

val get_first_different_bakers :
  ?excluding:public_key_hash list ->
  t ->
  (public_key_hash * public_key_hash) tzresult Lwt.t

val get_seed_nonce_hash : t -> Nonce_hash.t tzresult Lwt.t

(** Returns the seed of the cycle to which the block belongs to. *)
val get_seed : t -> Seed.seed tzresult Lwt.t

val get_seed_computation : t -> Seed.seed_computation_status tzresult Lwt.t

(** Returns all the constants of the protocol *)
val get_constants : t -> Constants.t tzresult Lwt.t

(** The default constants used in the test framework. To be used with
    [init_with_constants]. *)
val default_test_constants : Constants.Parametric.t

val get_issuance_per_minute : t -> Tez.t tzresult Lwt.t

val get_baking_reward_fixed_portion : t -> Tez.t tzresult Lwt.t

val get_bonus_reward : t -> attesting_power:int -> Tez.t tzresult Lwt.t

val get_attesting_reward :
  t -> expected_attesting_power:int -> Tez.t tzresult Lwt.t

val get_liquidity_baking_subsidy : t -> Tez.t tzresult Lwt.t

val get_liquidity_baking_cpmm_address : t -> Contract_hash.t tzresult Lwt.t

val get_adaptive_issuance_launch_cycle : t -> Cycle.t option tzresult Lwt.t

val get_total_frozen_stake : t -> Tez.t tzresult Lwt.t

val get_total_supply : t -> Tez.t tzresult Lwt.t

val get_seed_nonce_revelation_tip : t -> Tez.t tzresult Lwt.t

val get_vdf_revelation_tip : t -> Tez.t tzresult Lwt.t

val get_ai_current_yearly_rate : t -> string tzresult Lwt.t

val get_ai_current_yearly_rate_exact : t -> Q.t tzresult Lwt.t

val get_ai_expected_issuance :
  t -> Adaptive_issuance_services.expected_rewards list tzresult Lwt.t

val get_denunciations :
  t ->
  (Signature.Public_key_hash.t * Denunciations_repr.item) list tzresult Lwt.t

val get_denunciations_for_delegate :
  t ->
  Signature.Public_key_hash.t ->
  Denunciations_repr.item list tzresult Lwt.t

val get_consecutive_round_zero : t -> Int32.t tzresult Lwt.t

val estimated_shared_pending_slashed_amount :
  t -> public_key_hash -> Tez.t tzresult Lwt.t

val estimated_own_pending_slashed_amount :
  t -> public_key_hash -> Tez.t tzresult Lwt.t

module Vote : sig
  val get_ballots : t -> Vote.ballots tzresult Lwt.t

  val get_ballot_list :
    t -> (Signature.Public_key_hash.t * Vote.ballot) list tzresult Lwt.t

  val get_current_period : t -> Voting_period.info tzresult Lwt.t

  val get_current_quorum : t -> int32 tzresult Lwt.t

  val get_participation_ema : Block.t -> int32 tzresult Lwt.t

  val get_listings :
    t -> (Signature.Public_key_hash.t * int64) list tzresult Lwt.t

  val get_proposals : t -> int64 Environment.Protocol_hash.Map.t tzresult Lwt.t

  val get_current_proposal :
    t -> Environment.Protocol_hash.t option tzresult Lwt.t

  val get_protocol : Block.t -> Protocol_hash.t Lwt.t

  val set_participation_ema : Block.t -> int32 -> Block.t Lwt.t

  type delegate_info = Alpha_context.Vote.delegate_info = {
    voting_power : Int64.t option;
    current_ballot : Alpha_context.Vote.ballot option;
    current_proposals : Protocol_hash.t list;
    remaining_proposals : int;
  }

  (** See {!Vote_storage.get_delegate_proposal_count}.

      Note that unlike most functions in the current module, this one
      does not call an RPC. *)
  val get_delegate_proposal_count : t -> public_key_hash -> int tzresult Lwt.t
end

module Dal : sig
  val shards :
    t ->
    ?level:Raw_level.t ->
    ?delegates:Signature.public_key_hash list ->
    unit ->
    Plugin.RPC.Dal.S.shards_output tzresult Lwt.t
end

module Contract : sig
  val pp : Format.formatter -> Contract.t -> unit

  val equal : Contract.t -> Contract.t -> bool

  val pkh : Contract.t -> public_key_hash

  (** Returns the balance of a contract, by default the main balance.
      If the contract is implicit the frozen balances are available too:
      deposit, fees or rewards. *)
  val balance : t -> Contract.t -> Tez.t tzresult Lwt.t

  val frozen_bonds : t -> Contract.t -> Tez.t tzresult Lwt.t

  val balance_and_frozen_bonds : t -> Contract.t -> Tez.t tzresult Lwt.t

  val staked_balance : t -> Contract.t -> Tez.t option tzresult Lwt.t

  val unstaked_frozen_balance : t -> Contract.t -> Tez.t option tzresult Lwt.t

  val unstaked_finalizable_balance :
    t -> Contract.t -> Tez.t option tzresult Lwt.t

  (** Calls
      [/chains/<chain_id>/blocks/<block_id>/contracts/<contract_id>/full_balance].

      If the contract is a delegate, also calls
      [/chains/<chain_id>/blocks/<block_id>/delegates/<contract_pkh>/own_full_balance]
      and checks that both RPCs return the same value. *)
  val full_balance : ?__LOC__:string -> t -> Contract.t -> Tez.t tzresult Lwt.t

  val staking_numerator : t -> Contract.t -> Z.t tzresult Lwt.t

  val counter : t -> Contract.t -> Manager_counter.t tzresult Lwt.t

  val manager : t -> Contract.t -> Account.t tzresult Lwt.t

  val is_manager_key_revealed : t -> Contract.t -> bool tzresult Lwt.t

  val delegate : t -> Contract.t -> public_key_hash tzresult Lwt.t

  val delegate_opt : t -> Contract.t -> public_key_hash option tzresult Lwt.t

  val storage : t -> Contract_hash.t -> Script.expr tzresult Lwt.t

  val script : t -> Contract_hash.t -> Script.expr tzresult Lwt.t

  val script_hash : t -> Contract_hash.t -> Script_expr_hash.t tzresult Lwt.t
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
    min_delegated_in_current_cycle : Tez.t * Level_repr.t option;
    total_delegated_stake : Tez.t;
    staking_denominator : Staking_pseudotoken.t;
    deactivated : bool;
    grace_period : Cycle.t;
    pending_denunciations : bool;
    voting_info : Vote.delegate_info;
    active_consensus_key : Signature.Public_key_hash.t;
    pending_consensus_keys : (Cycle.t * Signature.Public_key_hash.t) list;
    active_companion_key : Signature.Bls.Public_key_hash.t option;
    pending_companion_keys : (Cycle.t * Signature.Bls.Public_key_hash.t) list;
  }

  type stake = {frozen : Tez.t; weighted_delegated : Tez.t}

  val info : t -> public_key_hash -> Delegate_services.info tzresult Lwt.t

  (** Calls RPCs
      [/chains/<chain_id>/blocks/<blocsk_id>/contracts/<Implicit
      pkh>/full_balance] and
      [/chains/<chain_id>/blocks/<block_id>/delegates/<pkh>/own_full_balance],
      checks that both RPCs output the same value, then returns this
      value. *)
  val full_balance :
    ?__LOC__:string -> t -> public_key_hash -> Tez.t tzresult Lwt.t

  val current_frozen_deposits : t -> public_key_hash -> Tez.t tzresult Lwt.t

  (** calls the RPC [frozen_deposits]: we're using a different name to
     be more easily distinguishable from [current_frozen_deposits] *)
  val initial_frozen_deposits : t -> public_key_hash -> Tez.t tzresult Lwt.t

  val staking_balance : t -> public_key_hash -> Tez.t tzresult Lwt.t

  val unstaked_frozen_deposits :
    t ->
    public_key_hash ->
    Plugin.Delegate_services.deposit_per_cycle list tzresult Lwt.t

  val staking_denominator : t -> public_key_hash -> Z.t tzresult Lwt.t

  val frozen_deposits_limit :
    t -> public_key_hash -> Tez.t option tzresult Lwt.t

  val deactivated : t -> public_key_hash -> bool tzresult Lwt.t

  val grace_period : t -> public_key_hash -> Cycle.t tzresult Lwt.t

  val voting_info : t -> public_key_hash -> Vote.delegate_info tzresult Lwt.t

  val consensus_key :
    t -> public_key_hash -> Delegate_services.consensus_keys_info tzresult Lwt.t

  val companion_key :
    t -> public_key_hash -> Delegate_services.companion_keys_info tzresult Lwt.t

  val participation :
    t -> public_key_hash -> Delegate.For_RPC.participation_info tzresult Lwt.t

  val dal_participation :
    t ->
    public_key_hash ->
    Delegate.For_RPC.dal_participation_info tzresult Lwt.t

  val is_forbidden : t -> public_key_hash -> bool tzresult Lwt.t

  val stake_for_cycle : t -> Cycle.t -> public_key_hash -> stake tzresult Lwt.t
end

module Sc_rollup : sig
  val inbox : t -> Sc_rollup.Inbox.t tzresult Lwt.t

  val whitelist :
    t -> Sc_rollup.t -> Sc_rollup.Whitelist.t option tzresult Lwt.t

  val commitment :
    t ->
    Sc_rollup.t ->
    Sc_rollup.Commitment.Hash.t ->
    Sc_rollup.Commitment.t tzresult Lwt.t

  val genesis_info :
    t -> Sc_rollup.t -> Sc_rollup.Commitment.genesis_info tzresult Lwt.t

  val timeout :
    t ->
    Sc_rollup.t ->
    Signature.Public_key_hash.t ->
    Signature.Public_key_hash.t ->
    Sc_rollup.Game.timeout option tzresult Lwt.t

  val ongoing_games_for_staker :
    t ->
    Sc_rollup.t ->
    Signature.public_key_hash ->
    (Sc_rollup.Game.t * Signature.public_key_hash * Signature.public_key_hash)
    list
    tzresult
    Lwt.t
end

type (_, _) tup =
  | T1 : ('a, 'a) tup
  | T2 : ('a, 'a * 'a) tup
  | T3 : ('a, 'a * 'a * 'a) tup
  | TList : int -> ('a, 'a list) tup

val tup_hd : ('a, 'elts) tup -> 'elts -> 'a

type 'accounts init :=
  ?rng_state:Random.State.t ->
  ?commitments:Commitment.t list ->
  ?bootstrap_balances:int64 list ->
  ?bootstrap_delegations:Signature.Public_key_hash.t option list ->
  ?bootstrap_consensus_keys:Signature.Public_key.t option list ->
  ?consensus_committee_size:int ->
  ?consensus_threshold_size:int ->
  ?min_proposal_quorum:int32 ->
  ?bootstrap_contracts:Parameters.bootstrap_contract list ->
  ?level:int32 ->
  ?cost_per_byte:Tez.t ->
  ?issuance_weights:Constants.Parametric.issuance_weights ->
  ?origination_size:int ->
  ?blocks_per_cycle:int32 ->
  ?cycles_per_voting_period:int32 ->
  ?sc_rollup_arith_pvm_enable:bool ->
  ?sc_rollup_private_enable:bool ->
  ?sc_rollup_riscv_pvm_enable:bool ->
  ?dal_enable:bool ->
  ?dal_incentives_enable:bool ->
  ?zk_rollup_enable:bool ->
  ?hard_gas_limit_per_block:Gas.Arith.integral ->
  ?nonce_revelation_threshold:int32 ->
  ?dal:Constants.Parametric.dal ->
  ?adaptive_issuance:Constants.Parametric.adaptive_issuance ->
  ?allow_tz4_delegate_enable:bool ->
  ?aggregate_attestation:bool ->
  unit ->
  (Block.t * 'accounts) tzresult Lwt.t

(** Returns an initial block and the implicit contracts corresponding
    to its bootstrap accounts. The number of bootstrap accounts, and
    the structure of the returned contracts, are specified by the [tup]
    argument. *)
val init_gen : (Alpha_context.Contract.t, 'accounts) tup -> 'accounts init

(** [init_n n] : returns an initial block with [n] initialized accounts
    and the associated implicit contracts *)
val init_n : int -> Alpha_context.Contract.t list init

(** [init1] : returns an initial block with 1 initialized bootstrap account
    and the associated implicit contract *)
val init1 : Alpha_context.Contract.t init

(** [init2] : returns an initial block with 2 initialized bootstrap accounts
    and the associated implicit contracts *)
val init2 : (Alpha_context.Contract.t * Alpha_context.Contract.t) init

(** [init3] : returns an initial block with 3 initialized bootstrap accounts
    and the associated implicit contracts *)
val init3 :
  (Alpha_context.Contract.t
  * Alpha_context.Contract.t
  * Alpha_context.Contract.t)
  init

val init_with_constants_gen :
  ?algo:Signature.algo ->
  (Alpha_context.Contract.t, 'contracts) tup ->
  Constants.Parametric.t ->
  (Block.t * 'contracts) tzresult Lwt.t

val init_with_constants_n :
  ?algo:Signature.algo ->
  Constants.Parametric.t ->
  int ->
  (Block.t * Alpha_context.Contract.t list) tzresult Lwt.t

val init_with_constants_algo_list :
  Constants.Parametric.t ->
  Signature.algo option list ->
  (Block.t * Alpha_context.Contract.t list) tzresult Lwt.t

val init_with_constants1 :
  Constants.Parametric.t -> (Block.t * Alpha_context.Contract.t) tzresult Lwt.t

val init_with_constants2 :
  Constants.Parametric.t ->
  (Block.t * (Alpha_context.Contract.t * Alpha_context.Contract.t)) tzresult
  Lwt.t

(** [init_with_parameters_gen tup params] returns an initial block parametrised
    with [params] and the implicit contracts corresponding to its bootstrap
    accounts. The number of bootstrap accounts, and the structure of the
    returned contracts, are specified by the [tup] argument. *)
val init_with_parameters_gen :
  (Alpha_context.Contract.t, 'contracts) tup ->
  Parameters.t ->
  (Block.t * 'contracts) tzresult Lwt.t

(** [init_with_parameters_n params n] returns an initial block parametrized
    with [params] with [n] initialized accounts and the associated implicit
    contracts *)
val init_with_parameters_n :
  Parameters.t ->
  int ->
  (Block.t * Alpha_context.Contract.t list) tzresult Lwt.t

(** [init_with_parameters1 params] returns an initial block parametrized with
    [params] with one initialized account and the associated implicit
    contract. *)
val init_with_parameters1 :
  Parameters.t -> (Block.t * Alpha_context.Contract.t) tzresult Lwt.t

(** [init_with_parameters2 params] returns an initial block parametrized with
    [params] with two initialized accounts and the associated implicit
    contracts *)
val init_with_parameters2 :
  Parameters.t ->
  (Block.t * (Alpha_context.Contract.t * Alpha_context.Contract.t)) tzresult
  Lwt.t

(** [default_raw_context] returns a [Raw_context.t] for use in tests
    below [Alpha_context] *)
val default_raw_context : unit -> Raw_context.t tzresult Lwt.t

(** [raw_context_from_constants] returns a [Raw_context.t] for use in tests
    below [Alpha_context] *)
val raw_context_from_constants :
  Constants.Parametric.t -> Raw_context.t tzresult Lwt.t
