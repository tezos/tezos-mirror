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

(** This module provides the means to test extensively the Liquidity
    Baking (LB) feature.  We recall that this feature is built upon
    three smart contracts: (1) a CPMM contract initially based on
    Dexter 2, and (2) two tokens contracts.  Our objective is to run
    “scenarios” consisting in interleaved, realistic calls to these
    contracts, and to assert these scenarios do not yield any
    undesirable behaviors.

    To that end, three “machines” are provided.

    - The {! SymbolicMachine} allows to simulate scenarios involving
      the LB feature completely off-chain. It can be seen as an
      abstraction of the concrete implementation provided by the Tezos
      node.
    - The {! ConcreteMachine } allows to execute scenarios on-chain.
    - The {! ValidationMachine } combines the two previously mentioned
      machines. In other words, the {! ValidationMachine} makes the {!
      SymbolicMachine} and the [ConcreteMachine] execute the same
      scenarios, and asserts they remain synchronized after each baked
      block.

    The {! ValidationMachine} allows to (1) validate the {!
    SymbolicMachine} ({i i.e.,} the reimplementation of the LB
    contracts logic) against the real implementation provided by
    Tezos, {b and} the contracts originated by the protocol correctly
    implement the LB logic, as implemented by the {! SymbolicMachine}.
    That is, the {! ValidationMachine} reports desynchronization of
    the two machines, but cannot explain this desynchronization. *)

(** {1 Machine State Characterization} *)

type xtz = int64

type tzbtc = int

type liquidity = int

(** As far as liquidity baking is concerned, an account can hold three
    kinds of tokens: [xtz], [tzbtc], and [liquidity]. *)
type balances = {xtz : xtz; tzbtc : tzbtc; liquidity : liquidity}

val pp_balances : Format.formatter -> balances -> unit

(** A value of type [specs] allows to specify an initial state of a
    “machine”.

    In a nutshell, it consists in specifying the minimal balances of
    the CPMM contracts and a set of implicit contracts.  The two
    machines provided by this module has a [build] function which
    turns a [specs] into a consistent initial state for this
    machine. *)
type specs = {
  cpmm_min_xtz_balance : xtz;
  cpmm_min_tzbtc_balance : tzbtc;
  accounts_balances : balances list;
}

val pp_specs : Format.formatter -> specs -> unit

(** A value of type ['a env] (where ['a] is the type of contract
    identifiers) summarizes the different contracts involved in the LB
    feature.

    Values of type [env] are constructed by the [build] function of
    the machines. *)
type 'a env = private {
  cpmm_contract : 'a;
  tzbtc_contract : 'a;
  tzbtc_admin : 'a;
  liquidity_contract : 'a;
  liquidity_admin : 'a;
  implicit_accounts : 'a list;
  holder : 'a;
  subsidy : xtz;
}

(** A value of type ['a step] (where ['a] is the type used to identify
    contracts) describes a consistent sequence of LB smart contract
    calls.

    For instance, [SellTzBTC] consists in approving an allowance in
    the [TzBTC] contract, then calling the [token_to_xtz] entry point
    of the [CPMM]. *)
type 'a step =
  | SellTzBTC of {source : 'a; destination : 'a; tzbtc_deposit : tzbtc}
  | BuyTzBTC of {source : 'a; destination : 'a; xtz_deposit : xtz}
  | AddLiquidity of {source : 'a; destination : 'a; xtz_deposit : xtz}
  | RemoveLiquidity of {source : 'a; destination : 'a; lqt_burned : liquidity}

val pp_step :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a step -> unit

(** A summary of the state of a machine, parameterized by the type of
    contract identifier. *)
type 'a state = {
  cpmm_total_liquidity : liquidity;
  accounts_balances : ('a * balances) list;
}

(** {1 The Symbolic Machine} *)

(** In the {! SymbolicMachine}, a contract is identified by a symbolic
    value. *)
type contract_id =
  | Cpmm
  | Holder
  | TzBTC
  | TzBTCAdmin
  | Liquidity
  | LiquidityAdmin
  (* We use integers to distinguish between implicit account because
     this integer has the extra benefit of being the index of the
     related account in [env.implicit_accounts]. *)
  | ImplicitAccount of int

val pp_contract_id : Format.formatter -> contract_id -> unit

module SymbolicMachine : sig
  (** The state of the {! SymbolicMachine}. *)
  type t = contract_id state

  (** [get_xtz_balance c state] returns the amount of mutez owned by
      [c] in [state]. *)
  val get_xtz_balance : contract_id -> t -> xtz

  (** [get_tzbtc_balance c env state] returns the amount of TzBTC
      owned by [c] in [state], according to the [TzBTC] contract. *)
  val get_tzbtc_balance : contract_id -> contract_id env -> t -> tzbtc

  (** [get_liquidity_balance c env state] returns the amount of
      liquidity token owned by [c] in [state], according to the
      [Liquidity] contract. *)
  val get_liquidity_balance : contract_id -> contract_id env -> t -> liquidity

  (** [get_cpmm_total_liquidity env state] fetches the current amount
      of liquidity tokens distributed by the CPMM contract from the
      state [state]. *)
  val get_cpmm_total_liquidity : contract_id env -> t -> liquidity

  (** [predict_required_tzbtc_deposit xtz_deposit env state] predicts
      the deposit in TzBTC which will be required by the CPMM contract
      when executing a step [AddLiquidity] with [xtz_deposit] from
      [state]. *)
  val predict_required_tzbtc_deposit : xtz -> contract_id env -> t -> tzbtc

  (** [build specs] computes (1) an initial state for the {!
      SymbolicMachine}, and (2) the environment associated to this
      state.

      The machine enforces the resulting state is consistent with the
      [specs] given as inputs, and raises an [Assert_failure]
      exception if it does not.

      One can use the optional argument [subsidy] to set the subsidy
      amount to a given value (by default, we use the same as the main
      chain). Additionally, the [invariant] optional argument can be
      used to verify that a given invariant holds at the end of the
      initialization. *)
  val build :
    ?invariant:(contract_id env -> t -> bool) ->
    ?subsidy:xtz ->
    specs ->
    t * contract_id env

  (** [step s env state] executes a single step [s] from [state].

      The [invariant] optional argument can be used to verify that a
      given invariant holds after each baked block. *)
  val step :
    ?invariant:(contract_id env -> t -> bool) ->
    contract_id step ->
    contract_id env ->
    t ->
    t

  (** [run steps env state] executes a list of steps from [state].

      The [invariant] optional argument can be used to verify that a
      given invariant holds after each baked block. *)
  val run :
    ?invariant:(contract_id env -> t -> bool) ->
    contract_id step list ->
    contract_id env ->
    t ->
    t
end

(** A machine that can execute scenarios onchain. *)
module ConcreteMachine : sig
  (** The state of the {! ConcreteMachine}. *)
  type t = Block.t

  (** [get_xtz_balance c state] returns the amount of mutez owned by
      [c] in [state]. *)
  val get_xtz_balance : Contract.t -> t -> xtz tzresult Lwt.t

  (** [get_tzbtc_balance c env state] returns the amount of TzBTC
      owned by [c] in [state], according to the [TzBTC] contract. *)
  val get_tzbtc_balance :
    Contract.t -> Contract.t env -> t -> tzbtc tzresult Lwt.t

  (** [get_liquidity_balance c env state] returns the amount of
      liquidity token owned by [c] in [state], according to the
      [Liquidity] contract. *)
  val get_liquidity_balance :
    Contract.t -> Contract.t env -> t -> liquidity tzresult Lwt.t

  (** [get_cpmm_total_liquidity env state] fetches the current amount
      of liquidity tokens distributed by the CPMM contract from the
      state [state]. *)
  val get_cpmm_total_liquidity : Contract.t env -> t -> liquidity tzresult Lwt.t

  (** [build specs] asynchronously computes (1) an initial block for
      the {! ConcreteMachine}, and (2) the environment associated to
      this block.

      The machine enforces the resulting state is consistent with the
      [specs] given as inputs, and raises an [Assert_failure]
      exception if it does not. It also enforces that the machines
      used underneath remain in sync.

      One can use the optional argument [subsidy] to set the subsidy
      amount to a given value (by default, we use the same as the main
      chain). Additionally, the [invariant] optional argument can be
      used to verify that a given invariant holds at the end of the
      initialization. *)
  val build :
    ?invariant:(Contract.t env -> t -> bool tzresult Lwt.t) ->
    ?subsidy:xtz ->
    specs ->
    (t * Contract.t env) tzresult Lwt.t

  (** [step s env state] asynchronously executes a single step [s]
      from [state].

      The [invariant] optional argument can be used to verify that a
      given invariant holds after each baked block. *)
  val step :
    ?invariant:(Contract.t env -> t -> bool tzresult Lwt.t) ->
    Contract.t step ->
    Contract.t env ->
    t ->
    t tzresult Lwt.t

  (** [run lss env state] asynchronously executes a list of steps from
      [state].

      The [invariant] optional argument can be used to verify that a
      given invariant holds after each baked block. *)
  val run :
    ?invariant:(Contract.t env -> t -> bool tzresult Lwt.t) ->
    contract_id step list ->
    Contract.t env ->
    t ->
    t tzresult Lwt.t
end

module ValidationMachine : sig
  (** The state of the {! ValidationMachine}. *)
  type t = ConcreteMachine.t * Contract.t state

  module Symbolic : sig
    (** A collections of functions to introspect the symbolic part of
        the [ValidationMachine] state. *)

    (** [get_xtz_balance c state] returns the amount of mutez owned by
        [c] in the symbolic part of [state]. *)
    val get_xtz_balance : Contract.t -> t -> xtz tzresult Lwt.t

    (** [get_tzbtc_balance c env state] returns the amount of TzBTC
        owned by [c] in the symbolic part of [state], according to the
        [TzBTC] contract. *)
    val get_tzbtc_balance :
      Contract.t -> Contract.t env -> t -> tzbtc tzresult Lwt.t

    (** [get_liquidity_balance c env state] returns the amount of
        liquidity token owned by [c] in the symbolic part of [state],
        according to the [Liquidity] contract. *)
    val get_liquidity_balance :
      Contract.t -> Contract.t env -> t -> liquidity tzresult Lwt.t

    (** [get_cpmm_total_liquidity env state] fetches the   current
        amount of liquidity tokens distributed by the CPMM   contract
        using the symbolic part of the state [state]. *)
    val get_cpmm_total_liquidity :
      Contract.t env -> t -> liquidity tzresult Lwt.t
  end

  module Concrete : sig
    (** A collections of functions to introspect the concrete part of
        the [ValidationMachine] state. *)

    (** [get_xtz_balance c state] returns the amount of mutez owned by
        [c] in the concrete part of [state]. *)
    val get_xtz_balance : Contract.t -> t -> xtz tzresult Lwt.t

    (** [get_tzbtc_balance c env state] returns the amount of TzBTC
        owned by [c] in the concrete part of [state], according to the
        [TzBTC] contract. *)
    val get_tzbtc_balance :
      Contract.t -> Contract.t env -> t -> tzbtc tzresult Lwt.t

    (** [get_liquidity_balance c env state] returns the amount of
        liquidity token owned by [c] in the concrete part of [state],
        according to the [Liquidity] contract. *)
    val get_liquidity_balance :
      Contract.t -> Contract.t env -> t -> liquidity tzresult Lwt.t

    (** [get_cpmm_total_liquidity env state] fetches the current
        amount of liquidity tokens distributed by the CPMM contract
        using the concrete part of the state [state]. *)
    val get_cpmm_total_liquidity :
      Contract.t env -> t -> liquidity tzresult Lwt.t
  end

  (** [build specs] asynchronously computes (1) an initial state for
      the {! ValidationMachine}, and (2) the environment associated to
      this state.

      The machine enforces the resulting state is consistent with the
      [specs] given as inputs, and raises an [Assert_failure]
      exception if it does not. It also enforces that the machines
      used underneath remain in sync.

      One can use the optional argument [subsidy] to set the subsidy
      amount to a given value (by default, we use the same as the main
      chain). Additionally, the [invariant] optional argument can be
      used to verify that a given invariant holds at the end of the
      initialization. *)
  val build :
    ?invariant:(Contract.t env -> t -> bool tzresult Lwt.t) ->
    ?subsidy:xtz ->
    specs ->
    (t * Contract.t env) tzresult Lwt.t

  (** [step s env state] asynchronously executes a single step [s]
      from [state].

      The [invariant] optional argument can be used to verify that a
      given invariant holds after each baked block. *)
  val step :
    ?invariant:(Contract.t env -> t -> bool tzresult Lwt.t) ->
    Contract.t step ->
    Contract.t env ->
    t ->
    t tzresult Lwt.t

  (** [run lss env state] asynchronously executes a list of steps from
      [state].

      The [invariant] optional argument can be used to verify that a
      given invariant holds after each baked block. *)
  val run :
    ?invariant:(Contract.t env -> t -> bool tzresult Lwt.t) ->
    contract_id step list ->
    Contract.t env ->
    t ->
    t tzresult Lwt.t
end
