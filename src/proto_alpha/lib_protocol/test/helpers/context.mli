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

val pred_branch : t -> Block_hash.t

val get_level : t -> Raw_level.t tzresult

(** Given a context, returns the list of endorsers charactized by
    the [level], the public key hash of the [delegate], its [consensus_key]
    and its assigned [slots].
    see {! Plugin.RPC.Validator.t}. *)
val get_endorsers : t -> Plugin.RPC.Validators.t list tzresult Lwt.t

(** Return the two first elements of the list returns by [get_endorsers]. *)
val get_first_different_endorsers :
  t -> (Plugin.RPC.Validators.t * Plugin.RPC.Validators.t) tzresult Lwt.t

(** Return the first element [delegate,slot] of the list returns by
    [get_endorsers], where [delegate] is the [consensus key] when
    is set. *)
val get_endorser : t -> (public_key_hash * Slot.t list) tzresult Lwt.t

(** Given a [delegate], and a context [ctxt], if [delegate] is in
    [get_endorsers ctxt] returns the [slots] of [delegate] otherwise
    return [None]. *)
val get_endorser_slot :
  t -> public_key_hash -> Slot.t list option tzresult Lwt.t

(** Return the [n]th element of the list returns by [get_endorsers]. *)
val get_endorser_n : t -> int -> (public_key_hash * Slot.t list) tzresult Lwt.t

val get_endorsing_power_for_delegate :
  t -> ?levels:Raw_level.t list -> public_key_hash -> int tzresult Lwt.t

val get_voting_power :
  t -> public_key_hash -> int64 Environment.Error_monad.shell_tzresult Lwt.t

val get_total_voting_power :
  t -> int64 Environment.Error_monad.shell_tzresult Lwt.t

val get_bakers :
  ?filter:(Plugin.RPC.Baking_rights.t -> bool) ->
  ?cycle:Cycle.t ->
  t ->
  public_key_hash list tzresult Lwt.t

val get_baker : t -> round:Round.t -> public_key_hash tzresult Lwt.t

val get_first_different_baker :
  public_key_hash -> public_key_hash trace -> public_key_hash

val get_first_different_bakers :
  t -> (public_key_hash * public_key_hash) tzresult Lwt.t

val get_seed_nonce_hash : t -> Nonce_hash.t tzresult Lwt.t

(** Returns the seed of the cycle to which the block belongs to. *)
val get_seed : t -> Seed.seed tzresult Lwt.t

val get_seed_computation : t -> Seed.seed_computation_status tzresult Lwt.t

(** Returns all the constants of the protocol *)
val get_constants : t -> Constants.t tzresult Lwt.t

(** The default constants used in the test framework. To be used with
    [init_with_constants]. *)
val default_test_constants : Constants.Parametric.t

val get_baking_reward_fixed_portion : t -> Tez.t tzresult Lwt.t

val get_bonus_reward : t -> endorsing_power:int -> Tez.t tzresult Lwt.t

val get_endorsing_reward :
  t -> expected_endorsing_power:int -> Tez.t tzresult Lwt.t

val get_liquidity_baking_subsidy : t -> Tez.t tzresult Lwt.t

val get_liquidity_baking_cpmm_address : t -> Contract_hash.t tzresult Lwt.t

val get_adaptive_inflation_launch_cycle : t -> Cycle.t option tzresult Lwt.t

val get_seed_nonce_revelation_tip : t -> Tez.t tzresult Lwt.t

val get_vdf_revelation_tip : t -> Tez.t tzresult Lwt.t

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
    delegated_contracts : Alpha_context.Contract.t list;
    delegated_balance : Tez.t;
    deactivated : bool;
    grace_period : Cycle.t;
    voting_info : Vote.delegate_info;
    active_consensus_key : Signature.Public_key_hash.t;
    pending_consensus_keys : (Cycle.t * Signature.Public_key_hash.t) list;
  }

  val info : t -> public_key_hash -> Delegate_services.info tzresult Lwt.t

  val full_balance : t -> public_key_hash -> Tez.t tzresult Lwt.t

  val current_frozen_deposits : t -> public_key_hash -> Tez.t tzresult Lwt.t

  (** calls the RPC [frozen_deposits]: we're using a different name to
     be more easily distinguishable from [current_frozen_deposits] *)
  val initial_frozen_deposits : t -> public_key_hash -> Tez.t tzresult Lwt.t

  val staking_balance : t -> public_key_hash -> Tez.t tzresult Lwt.t

  val deactivated : t -> public_key_hash -> bool tzresult Lwt.t

  val voting_info : t -> public_key_hash -> Vote.delegate_info tzresult Lwt.t

  val consensus_key :
    t -> public_key_hash -> Delegate_services.consensus_keys_info tzresult Lwt.t

  val participation :
    t -> public_key_hash -> Delegate.participation_info tzresult Lwt.t
end

module Sc_rollup : sig
  val inbox : t -> Sc_rollup.Inbox.t tzresult Lwt.t

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
  ?consensus_threshold:int ->
  ?min_proposal_quorum:int32 ->
  ?bootstrap_contracts:Parameters.bootstrap_contract list ->
  ?level:int32 ->
  ?cost_per_byte:Tez.t ->
  ?reward_weights:Constants.Parametric.reward_weights ->
  ?origination_size:int ->
  ?blocks_per_cycle:int32 ->
  ?cycles_per_voting_period:int32 ->
  ?sc_rollup_enable:bool ->
  ?sc_rollup_arith_pvm_enable:bool ->
  ?dal_enable:bool ->
  ?zk_rollup_enable:bool ->
  ?adaptive_inflation_enable:bool ->
  ?hard_gas_limit_per_block:Gas.Arith.integral ->
  ?nonce_revelation_threshold:int32 ->
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
  (Alpha_context.Contract.t, 'contracts) tup ->
  Constants.Parametric.t ->
  (Block.t * 'contracts) tzresult Lwt.t

val init_with_constants_n :
  Constants.Parametric.t ->
  int ->
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
