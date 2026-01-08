(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  (** [balance address block_param] returns the [address]'s balance at block
      [block_param]. *)
  val balance :
    Ethereum_types.address ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.quantity tzresult Lwt.t

  (** [nonce address block_param] returns the [address]'s nonce at
    block [block_param]. *)
  val nonce :
    Ethereum_types.address ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.quantity option tzresult Lwt.t

  (** [code address block_param] returns the [address]'s code at block
    [block_param]. *)
  val code :
    Ethereum_types.address ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.hex tzresult Lwt.t

  (** [inject_transactions ~timestamp ~smart_rollup_address
    ~transactions] crafts the hashes and chunks of each transaction
    of [transactions]. Injects the chunks and returns the hashes of
    injected transactions. *)
  val inject_transactions :
    timestamp:Time.Protocol.t ->
    smart_rollup_address:string ->
    transactions:(string * Transaction_object.t) list ->
    Ethereum_types.hash list tzresult Lwt.t

  (** [base_fee_per_gas ()] returns base fee defined by the rollup. *)
  val base_fee_per_gas : unit -> Ethereum_types.quantity tzresult Lwt.t

  (** [backlog ()] returns the current backlog of the chain, used to determine
    the base fee per gas for the next block. *)
  val backlog : unit -> Z.t tzresult Lwt.t

  (** [minimum_base_fee_per_gas ()] returns the floor price for one unit of
    gas. *)
  val minimum_base_fee_per_gas : unit -> Z.t tzresult Lwt.t

  (** [simulate_call call_info block_param state_override] simulates a call on
    context [block_param] (optionally updated with [state_override]) and
    returns the result. *)
  val simulate_call :
    overwrite_tick_limit:bool ->
    Ethereum_types.call ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.state_override ->
    Simulation.call_result Simulation.simulation_result tzresult Lwt.t

  (** [estimate_gas call_info] asks the rollup to simulate a call, and
    returns the gas used to execute the call. *)
  val estimate_gas :
    Ethereum_types.call ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.state_override ->
    Simulation.call_result Simulation.simulation_result tzresult Lwt.t

  (** [storage_at address pos block_param] returns the value at index
    [pos] of the account [address]'s storage on block
    [block_param]. *)
  val storage_at :
    Ethereum_types.address ->
    Ethereum_types.quantity ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.hex tzresult Lwt.t

  val replay :
    Ethereum_types.quantity ->
    Ethereum_types.legacy_transaction_object Ethereum_types.block tzresult Lwt.t

  (** [coinbase ()] returns the sequencer pool address if it exists,
    or the zero address. *)
  val coinbase : unit -> Ethereum_types.address tzresult Lwt.t
end
