(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  module Reader : Durable_storage.READER

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

  (** [inject_raw_transactions ~timestamp ~smart_rollup_address
      ~transactions] crafts the hashes and chunks of each transaction
      of [transactions]. Injects the chunks and returns the hashes of
      injected transactions. *)
  val inject_raw_transactions :
    timestamp:Time.Protocol.t ->
    smart_rollup_address:string ->
    transactions:string list ->
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
  val current_block_number : unit -> Ethereum_types.quantity tzresult Lwt.t

  val block_param_to_block_number :
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.quantity tzresult Lwt.t

  (** [nth_block_hash n] returns the hash of the [n]th processed and
      stored block. *)
  val nth_block_hash : Z.t -> Ethereum_types.block_hash option tzresult Lwt.t

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

  (** [block_receipts n] returns the receipts of the [n]th
      processed and stored block.
    *)
  val block_receipts : Z.t -> Transaction_receipt.t list tzresult Lwt.t

  (** [transaction_receipt tx_hash] returns the receipt of [tx_hash]. *)
  val transaction_receipt :
    Ethereum_types.hash -> Transaction_receipt.t option tzresult Lwt.t

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

  (** [kernel_root_hash ()] returns the internal kernel root hash (i.e the
      latest root hash that was applied during an upgrade). *)
  val kernel_root_hash : unit -> string option tzresult Lwt.t

  (** [simulate_call call_info block_param] simulates a call on context
      [block_param] and returns the result. *)
  val simulate_call :
    Ethereum_types.call ->
    Ethereum_types.Block_parameter.extended ->
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

  (** [storage_at address pos block_param] returns the value at index
      [pos] of the account [address]'s storage on block
      [block_param]. *)
  val storage_at :
    Ethereum_types.address ->
    Ethereum_types.quantity ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.hex tzresult Lwt.t

  val smart_rollup_address : string

  (** [trace_transaction hash tracer] replays the block containing the
      transaction [hash], and traces this transaction with the specified
      [tracer]. *)
  val trace_transaction :
    Ethereum_types.hash ->
    Tracer_types.config ->
    Tracer_types.output tzresult Lwt.t

  (** [coinbase ()] returns the sequencer pool address if it exists,
      or the zero address. *)
  val coinbase : unit -> Ethereum_types.address tzresult Lwt.t

  val trace_call :
    Ethereum_types.call ->
    Ethereum_types.Block_parameter.extended ->
    Tracer_types.config ->
    Tracer_types.output tzresult Lwt.t
end

module type Backend = sig
  module Reader : Durable_storage.READER

  module TxEncoder : Publisher.TxEncoder

  module Publisher : Publisher.Publisher with type messages = TxEncoder.messages

  module SimulatorBackend : Simulator.SimulationBackend

  (** [block_param_to_block_number block_param] returns the block number of the
      block identified by [block_param]. *)
  val block_param_to_block_number :
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.quantity tzresult Lwt.t

  module Tracer : Tracer_sig.Backend

  val smart_rollup_address : string
end

module Make (Backend : Backend) : S = struct
  module Reader = Backend.Reader
  include Durable_storage.Make (Backend.Reader)
  include Publisher.Make (Backend.TxEncoder) (Backend.Publisher)
  include Simulator.Make (Backend.SimulatorBackend)

  let block_param_to_block_number = Backend.block_param_to_block_number

  include
    Tracer_sig.Make
      (struct
        let transaction_receipt = transaction_receipt
      end)
      (Backend.Tracer)

  let smart_rollup_address = Backend.smart_rollup_address
end
