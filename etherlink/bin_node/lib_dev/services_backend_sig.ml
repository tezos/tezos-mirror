(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
module type S = sig
  module Reader : Durable_storage.READER

  module Block_storage : Block_storage_sig.S

  module Tezlink_backend : Tezlink_backend_sig.S

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
  val chain_id : unit -> L2_types.chain_id tzresult Lwt.t

  (** [chain_family chain_id] returns chain family defined for the chain with id chain_id. *)
  val chain_family : L2_types.chain_id -> L2_types.chain_family tzresult Lwt.t

  (** [base_fee_per_gas ()] returns base fee defined by the rollup. *)
  val base_fee_per_gas : unit -> Ethereum_types.quantity tzresult Lwt.t

  (** [backlog ()] returns the current backlog of the chain, used to determine
      the base fee per gas for the next block. *)
  val backlog : unit -> Z.t tzresult Lwt.t

  (** [storage_version ()] returns the latest storage version known to the
      current kernel. This can be used to determine which features are and are
      not supported. *)
  val storage_version : unit -> int tzresult Lwt.t

  (** [minimum_base_fee_per_gas ()] returns the floor price for one unit of
      gas. *)
  val minimum_base_fee_per_gas : unit -> Z.t tzresult Lwt.t

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

  (** [is_multichain_enabled ()] returns the value of the multichain feature flag
      this method targets proxy nodes that do not have direct access
      to the durable storage. *)
  val is_multichain_enabled : unit -> bool tzresult Lwt.t

  val list_l1_l2_levels :
    from_l1_level:int32 ->
    (int32 * Evm_store.L1_l2_finalized_levels.t) list tzresult Lwt.t

  val l2_levels_of_l1_level :
    int32 -> Evm_store.L1_l2_finalized_levels.t option tzresult Lwt.t

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

  val list_l1_l2_levels :
    from_l1_level:int32 ->
    (int32 * Evm_store.L1_l2_finalized_levels.t) list tzresult Lwt.t

  val l2_levels_of_l1_level :
    int32 -> Evm_store.L1_l2_finalized_levels.t option tzresult Lwt.t
end

module Make (Backend : Backend) (Executor : Evm_execution.S) : S = struct
  module Reader = Backend.Reader
  include Durable_storage.Make (Backend.Reader)
  module Block_storage = Durable_storage.Make_block_storage (Backend.Reader)
  include Publisher.Make (Backend.TxEncoder) (Backend.Publisher)
  include Simulator.Make (Backend.SimulatorBackend)

  module Tezlink_backend = Tezlink_services_impl.Make (struct
    include Backend.Reader

    let block_param_to_block_number = Backend.block_param_to_block_number

    let nth_block_hash = Block_storage.nth_block_hash

    let tez_nth_block = Block_storage.tez_nth_block
  end)

  let block_param_to_block_number = Backend.block_param_to_block_number

  include Tracer_sig.Make (Executor) (Block_storage) (Backend.Tracer)

  let replay number =
    let open Lwt_result_syntax in
    let* result = Executor.replay ~log_file:"replay_rpc" number in
    match result with
    | Apply_success {block = Eth block; _} -> return block
    | Apply_success {block = Tez _; _} ->
        failwith "Could not replay a tezlink block"
    | Apply_failure -> failwith "Could not replay the block"

  let smart_rollup_address = Backend.smart_rollup_address

  let list_l1_l2_levels = Backend.list_l1_l2_levels

  let l2_levels_of_l1_level = Backend.l2_levels_of_l1_level
end

(** Inject transactions with either RPCs or on a websocket connection. *)
type endpoint = Rpc of Uri.t | Websocket of Websocket_client.t

(** [Tx_container] is the signature of the module that deals with
    storing and forwarding transactions. the module type is used by
    {!Services.dispatch_request} to request informations about pending
    transactions. *)
module type Tx_container = sig
  (** [nonce ~next_nonce address] must returns the next gap nonce
      available. *)
  val nonce :
    next_nonce:Ethereum_types.quantity ->
    Ethereum_types.address ->
    Ethereum_types.quantity tzresult Lwt.t

  (** [add ~next_nonce tx_object raw_tx] returns the next gap nonce
      available based on the pending transaction of the tx_container.
      [next_nonce] is the next expected nonce found in the backend. *)
  val add :
    next_nonce:Ethereum_types.quantity ->
    Ethereum_types.legacy_transaction_object ->
    raw_tx:Ethereum_types.hex ->
    (Ethereum_types.hash, string) result tzresult Lwt.t

  (** [find hash] returns the transaction_object found in tx
      container. *)
  val find : Ethereum_types.hash -> Transaction_object.t option tzresult Lwt.t

  (** [content ()] returns all the transactions found in tx
      container. *)
  val content : unit -> Ethereum_types.txpool tzresult Lwt.t

  (** [shutdown ()] stops the tx container, waiting for the ongoing request
    to be processed. *)
  val shutdown : unit -> unit tzresult Lwt.t

  (** [clear ()] removes the container data but keeps the allocated space *)
  val clear : unit -> unit tzresult Lwt.t

  (** Trigger a tick in the [Tx_queue]. *)
  val tx_queue_tick : evm_node_endpoint:endpoint -> unit tzresult Lwt.t

  (** [tx_queue_beacon ~evm_node_endpoint ~tick_interval] is a never fulfilled
    promise which triggers a tick in the [Tx_queue] every
    [tick_interval] seconds. *)
  val tx_queue_beacon :
    evm_node_endpoint:endpoint -> tick_interval:float -> unit tzresult Lwt.t

  (** [lock_transactions] locks the transactions in the queue, new
    transactions can be added but nothing can be retrieved with
    {!pop_transactions}. *)
  val lock_transactions : unit -> unit tzresult Lwt.t

  (** [unlock_transactions] unlocks the transactions if it was locked by
    {!lock_transactions}. *)
  val unlock_transactions : unit -> unit tzresult Lwt.t
end
