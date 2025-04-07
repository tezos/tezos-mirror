(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  module Reader : Durable_storage.READER

  module Block_storage : Block_storage_sig.S

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
    transactions:(string * Ethereum_types.legacy_transaction_object) list ->
    Ethereum_types.hash list tzresult Lwt.t

  val block_param_to_block_number :
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.quantity tzresult Lwt.t

  (** [chain_id ()] returns chain id defined by the rollup. *)
  val chain_id : unit -> Ethereum_types.chain_id tzresult Lwt.t

  (** [base_fee_per_gas ()] returns base fee defined by the rollup. *)
  val base_fee_per_gas : unit -> Ethereum_types.quantity tzresult Lwt.t

  (** [kernel_version ()] returns the internal kernel version (i.e the
      commit hash where the kernel was compiled). *)
  val kernel_version : unit -> string tzresult Lwt.t

  (** [kernel_root_hash ()] returns the internal kernel root hash (i.e the
      latest root hash that was applied during an upgrade). *)
  val kernel_root_hash : unit -> string option tzresult Lwt.t

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
    Ethereum_types.Block_parameter.t ->
    Simulation.call_result Simulation.simulation_result tzresult Lwt.t

  (** [storage_at address pos block_param] returns the value at index
      [pos] of the account [address]'s storage on block
      [block_param]. *)
  val storage_at :
    Ethereum_types.address ->
    Ethereum_types.quantity ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.hex tzresult Lwt.t

  val smart_rollup_address : string

  val replay :
    Ethereum_types.quantity ->
    Ethereum_types.legacy_transaction_object Ethereum_types.block tzresult Lwt.t

  (** [coinbase ()] returns the sequencer pool address if it exists,
      or the zero address. *)
  val coinbase : unit -> Ethereum_types.address tzresult Lwt.t

  include Tracer_sig.S
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

module Make (Backend : Backend) (Executor : Evm_execution.S) : S = struct
  module Reader = Backend.Reader
  include Durable_storage.Make (Backend.Reader)
  module Block_storage = Durable_storage.Make_block_storage (Backend.Reader)
  include Publisher.Make (Backend.TxEncoder) (Backend.Publisher)
  include Simulator.Make (Backend.SimulatorBackend)

  let block_param_to_block_number = Backend.block_param_to_block_number

  include Tracer_sig.Make (Executor) (Block_storage) (Backend.Tracer)

  let replay number =
    let open Lwt_result_syntax in
    let* result =
      Executor.replay ~log_file:"replay_rpc" ~profile:false number
    in
    match result with
    | Apply_success {block; _} -> return block
    | Apply_failure -> failwith "Could not replay the block"

  let smart_rollup_address = Backend.smart_rollup_address
end
