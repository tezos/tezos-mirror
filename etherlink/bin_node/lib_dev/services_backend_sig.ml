(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  (** [balance address] returns the [address]'s balance. *)
  val balance : Ethereum_types.address -> Ethereum_types.quantity tzresult Lwt.t

  (** [nonce address] returns the [address]'s nonce. *)
  val nonce :
    Ethereum_types.address -> Ethereum_types.quantity option tzresult Lwt.t

  (** [code address] returns the [address]'s code. *)
  val code : Ethereum_types.address -> Ethereum_types.hex tzresult Lwt.t

  (** [inject_raw_transactions ~timestamp ~smart_rollup_address
      ~transactions] crafts the hashes and chunks of each transaction
      of [transactions]. Injects the chunks and returns the hashes of
      injected transactions. *)
  val inject_raw_transactions :
    timestamp:Time.Protocol.t ->
    smart_rollup_address:string ->
    transactions:string list ->
    delayed:Ethereum_types.Delayed_transaction.t list ->
    Ethereum_types.hash list tzresult Lwt.t

  (** [current_block ~full_transaction_object] returns the most recent
      processed and stored block.

      If [full_transaction_object] is [true], returns the transaction objects,
      the transactions hashes otherwise.
    *)
  val current_block :
    full_transaction_object:bool -> Ethereum_types.block tzresult Lwt.t

  (** [current_block_number ()] returns the most recent processed and
      stored block number. *)
  val current_block_number : unit -> Ethereum_types.block_height tzresult Lwt.t

  (** [nth_block ~full_transaction_object n] returns the [n]th
      processed and stored block.

      If [full_transaction_object] is [true], returns the transaction objects,
      the transactions hashes otherwise.
    *)
  val nth_block :
    full_transaction_object:bool -> Z.t -> Ethereum_types.block tzresult Lwt.t

  (** [block_by_hash ~full_transaction_object hash] returns the block with the
      given [hash].

      If [full_transaction_object] is [true], returns the transaction objects,
      the transactions hashes otherwise.
    *)
  val block_by_hash :
    full_transaction_object:bool ->
    Ethereum_types.block_hash ->
    Ethereum_types.block tzresult Lwt.t

  (** [transaction_receipt tx_hash] returns the receipt of [tx_hash]. *)
  val transaction_receipt :
    Ethereum_types.hash ->
    Ethereum_types.transaction_receipt option tzresult Lwt.t

  (** [transaction_object tx_hash] returns the informations of [tx_hash]. *)
  val transaction_object :
    Ethereum_types.hash ->
    Ethereum_types.transaction_object option tzresult Lwt.t

  (** [chain_id ()] returns chain id defined by the rollup. *)
  val chain_id : unit -> Ethereum_types.quantity tzresult Lwt.t

  (** [base_fee_per_gas ()] returns base fee defined by the rollup. *)
  val base_fee_per_gas : unit -> Ethereum_types.quantity tzresult Lwt.t

  (** [kernel_version ()] returns the internal kernel version (i.e the
      commit hash where the kernel was compiled). *)
  val kernel_version : unit -> string tzresult Lwt.t

  (** [simulate_call call_info] asks the rollup to simulate a call,
      and returns the result. *)
  val simulate_call :
    Ethereum_types.call ->
    Simulation.call_result Simulation.simulation_result tzresult Lwt.t

  (** [estimate_gas call_info] asks the rollup to simulate a call, and
      returns the gas used to execute the call. *)
  val estimate_gas :
    Ethereum_types.call ->
    Simulation.call_result Simulation.simulation_result tzresult Lwt.t

  (** [is_tx_valid tx_raw] checks if the transaction is valid. Checks
      if the nonce is correct and returns the associated public key of
      transaction. *)
  val is_tx_valid :
    string ->
    Simulation.validation_result Simulation.simulation_result tzresult Lwt.t

  (** [storage_at address pos] returns the value at index [pos] of the
      account [address]'s storage. *)
  val storage_at :
    Ethereum_types.address ->
    Ethereum_types.quantity ->
    Ethereum_types.hex tzresult Lwt.t

  (**/**)

  (** [inject_kernel_upgrade ~payload] injects the kernel upgrade
      payload [payload] in the local state. *)
  val inject_kernel_upgrade : payload:string -> unit tzresult Lwt.t
end

module type Backend = sig
  module READER : Durable_storage.READER

  module TxEncoder : Publisher.TxEncoder

  module Publisher : Publisher.Publisher with type messages = TxEncoder.messages

  module SimulatorBackend : Simulator.SimulationBackend

  val inject_kernel_upgrade : payload:string -> unit tzresult Lwt.t
end

module Make (Backend : Backend) : S = struct
  include Durable_storage.Make (Backend.READER)
  include Publisher.Make (Backend.TxEncoder) (Backend.Publisher)
  include Simulator.Make (Backend.SimulatorBackend)

  let inject_kernel_upgrade = Backend.inject_kernel_upgrade
end
