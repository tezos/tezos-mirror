(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

val get_endorsers : t -> Plugin.RPC.Endorsing_rights.t list tzresult Lwt.t

val get_endorser : t -> (public_key_hash * int list) tzresult Lwt.t

val get_voting_power :
  t -> public_key_hash -> int32 Environment.Error_monad.shell_tzresult Lwt.t

val get_total_voting_power :
  t -> int32 Environment.Error_monad.shell_tzresult Lwt.t

val get_bakers : t -> public_key_hash list tzresult Lwt.t

val get_seed_nonce_hash : t -> Nonce_hash.t tzresult Lwt.t

(** Returns the seed of the cycle to which the block belongs to. *)
val get_seed : t -> Seed.seed tzresult Lwt.t

(** Returns all the constants of the protocol *)
val get_constants : t -> Constants.t tzresult Lwt.t

val get_minimal_valid_time :
  t -> priority:int -> endorsing_power:int -> Time.t tzresult Lwt.t

val get_baking_reward :
  t -> priority:int -> endorsing_power:int -> Tez.t tzresult Lwt.t

val get_endorsing_reward :
  t -> priority:int -> endorsing_power:int -> Tez.t tzresult Lwt.t

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
    t -> (Signature.Public_key_hash.t * int32) list tzresult Lwt.t

  val get_proposals : t -> int32 Protocol_hash.Map.t tzresult Lwt.t

  val get_current_proposal : t -> Protocol_hash.t option tzresult Lwt.t

  val get_protocol : Block.t -> Protocol_hash.t Lwt.t

  val set_participation_ema : Block.t -> int32 -> Block.t Lwt.t
end

module Contract : sig
  val pp : Format.formatter -> Contract.t -> unit

  val equal : Contract.t -> Contract.t -> bool

  val pkh : Contract.t -> public_key_hash tzresult Lwt.t

  type balance_kind = Main | Deposit | Fees | Rewards

  (** Returns the balance of a contract, by default the main balance.
      If the contract is implicit the frozen balances are available too:
      deposit, fees or rewards. *)
  val balance : ?kind:balance_kind -> t -> Contract.t -> Tez.t tzresult Lwt.t

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
    balance : Tez.t;
    frozen_balance : Tez.t;
    frozen_balance_by_cycle : Delegate.frozen_balance Cycle.Map.t;
    staking_balance : Tez.t;
    delegated_contracts : Alpha_context.Contract.t list;
    delegated_balance : Tez.t;
    deactivated : bool;
    grace_period : Cycle.t;
    voting_power : int32;
  }

  val info : t -> public_key_hash -> Delegate_services.info tzresult Lwt.t
end

(** [init n] : returns an initial block with [n] initialized accounts
    and the associated implicit contracts *)
val init :
  ?rng_state:Random.State.t ->
  ?endorsers_per_block:int ->
  ?with_commitments:bool ->
  ?initial_balances:int64 list ->
  ?initial_endorsers:int ->
  ?min_proposal_quorum:int32 ->
  ?time_between_blocks:Period.t list ->
  ?minimal_block_delay:Period.t ->
  ?delay_per_missing_endorsement:Period.t ->
  ?bootstrap_contracts:Parameters.bootstrap_contract list ->
  ?level:int32 ->
  ?cost_per_byte:Tez.t ->
  ?liquidity_baking_subsidy:Tez.t ->
  int ->
  (Block.t * Alpha_context.Contract.t list) tzresult Lwt.t

(** [init1] : returns an initial block with 1 initialized bootstrap account
    and the associated implicit contract *)
val init1 :
  ?rng_state:Random.State.t ->
  ?endorsers_per_block:int ->
  ?with_commitments:bool ->
  ?initial_balances:int64 list ->
  ?initial_endorsers:int ->
  ?min_proposal_quorum:int32 ->
  ?time_between_blocks:Period.t list ->
  ?minimal_block_delay:Period.t ->
  ?delay_per_missing_endorsement:Period.t ->
  ?bootstrap_contracts:Parameters.bootstrap_contract list ->
  ?level:int32 ->
  ?cost_per_byte:Tez.t ->
  ?liquidity_baking_subsidy:Tez.t ->
  unit ->
  (Block.t * Alpha_context.Contract.t) tzresult Lwt.t

(** [init2] : returns an initial block with 2 initialized bootstrap accounts
    and the associated implicit contracts *)
val init2 :
  ?rng_state:Random.State.t ->
  ?endorsers_per_block:int ->
  ?with_commitments:bool ->
  ?initial_balances:int64 list ->
  ?initial_endorsers:int ->
  ?min_proposal_quorum:int32 ->
  ?time_between_blocks:Period.t list ->
  ?minimal_block_delay:Period.t ->
  ?delay_per_missing_endorsement:Period.t ->
  ?bootstrap_contracts:Parameters.bootstrap_contract list ->
  ?level:int32 ->
  ?cost_per_byte:Tez.t ->
  ?liquidity_baking_subsidy:Tez.t ->
  unit ->
  (Block.t * Alpha_context.Contract.t * Alpha_context.Contract.t) tzresult Lwt.t
