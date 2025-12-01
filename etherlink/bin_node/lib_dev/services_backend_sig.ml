(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
module type S = sig
  module Reader : Durable_storage.READER

  module Etherlink_block_storage : Block_storage_sig.S

  module Tezos : Tezlink_backend_sig.S

  module Tezlink : Tezlink_backend_sig.S

  module Etherlink : Etherlink_backend_sig.S

  module Tracer_etherlink : Tracer_sig.S

  module SimulatorBackend : Simulator.SimulationBackend

  val block_param_to_block_number :
    chain_family:_ L2_types.chain_family ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.quantity tzresult Lwt.t

  (** [current_block_number ~root] returns the current block number of the L2 chain
      prefixed by [root] (depending on the chain_family in the configuration) *)
  val current_block_number :
    root:string -> Ethereum_types.quantity tzresult Lwt.t

  (** [chain_id ()] returns chain id defined by the rollup. *)
  val chain_id : unit -> L2_types.chain_id tzresult Lwt.t

  (** [chain_family chain_id] returns chain family defined for the chain with id chain_id. *)
  val chain_family :
    L2_types.chain_id -> L2_types.ex_chain_family tzresult Lwt.t

  (** [single_chain_id_and_family] should only be called if the
    node is expected to follow a single chain. It compares the
    configuration of the node and the one of the kernel regarding the
    chain id and chain family.

    If the multichain feature is disabled in both the kernel and the
    node, the default value [(None, EVM)] is returned.

    If the multichain feature is enabled in the kernel, check that it
    is also enabled in the node, that the node is configured to follow
    exactly one of the chains, that its chain id is among the
    configured ones in the kernel, and that the chain families for
    this chain agree in the node and the kernel.

    If the multichain feature is disabled in the kernel but enabled in
    the node, a warning is emmitted, the configured chain family is
    ignored and the returned chain family is the default [EVM]. *)
  val single_chain_id_and_family :
    config:Configuration.t ->
    enable_multichain:bool ->
    (L2_types.chain_id option * L2_types.ex_chain_family) tzresult Lwt.t

  (** [storage_version ()] returns the latest storage version known to the
      current kernel. This can be used to determine which features are and are
      not supported. *)
  val storage_version : unit -> int tzresult Lwt.t

  (** [kernel_version ()] returns the internal kernel version (i.e the
      commit hash where the kernel was compiled). *)
  val kernel_version : unit -> string tzresult Lwt.t

  (** [kernel_root_hash ()] returns the internal kernel root hash (i.e the
      latest root hash that was applied during an upgrade). *)
  val kernel_root_hash : unit -> string option tzresult Lwt.t

  val smart_rollup_address : string

  val is_multichain_enabled : unit -> bool tzresult Lwt.t

  val list_l1_l2_levels :
    from_l1_level:int32 ->
    (int32 * Evm_store.L1_l2_finalized_levels.t) list tzresult Lwt.t

  val l2_levels_of_l1_level :
    int32 -> Evm_store.L1_l2_finalized_levels.t option tzresult Lwt.t

  (** [list_runtimes ()] returns the list of runtimes activated in the kernel,
      according to the feature flags set in durable storage. *)
  val list_runtimes : unit -> Tezosx.runtime list tzresult Lwt.t
end

module type Backend = sig
  module Reader : Durable_storage.READER

  module TxEncoder : Publisher.TxEncoder

  module Publisher : Publisher.Publisher with type messages = TxEncoder.messages

  module SimulatorBackend : Simulator.SimulationBackend

  (** [block_param_to_block_number block_param] returns the block number of the
      block identified by [block_param]. *)
  val block_param_to_block_number :
    chain_family:_ L2_types.chain_family ->
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
  module Etherlink_block_storage =
    Etherlink_durable_storage.Make_block_storage (Backend.Reader)

  module Etherlink = struct
    include Etherlink_durable_storage.Make (Backend.Reader)
    include Publisher.Make (Backend.TxEncoder) (Backend.Publisher)
    include Simulator.MakeEtherlink (Backend.SimulatorBackend)

    let replay number =
      let open Lwt_result_syntax in
      let* result = Executor.replay ~log_file:"replay_rpc" number in
      match result with
      | Apply_success {block = Eth block; _} -> return block
      | Apply_success {block = Tez _; _} ->
          failwith "Could not replay a tezlink block"
      | Apply_failure -> failwith "Could not replay the block"
  end

  module Tracer_etherlink =
    Tracer_sig.Make (Executor) (Etherlink_block_storage) (Backend.Tracer)
  module Tezlink_block_storage =
    Tezlink_durable_storage.Make_block_storage (Backend.Reader)
  module SimulatorBackend = Backend.SimulatorBackend
  module Tezos = Tezos_backend.Make (Backend.SimulatorBackend)

  module Tezlink =
    Tezlink_services_impl.Make
      (struct
        include Backend.SimulatorBackend

        let block_param_to_block_number =
          Backend.block_param_to_block_number ~chain_family:L2_types.Michelson
      end)
      (Tezlink_block_storage)

  let block_param_to_block_number = Backend.block_param_to_block_number

  include Durable_storage.Make (Reader)

  let smart_rollup_address = Backend.smart_rollup_address

  let list_l1_l2_levels = Backend.list_l1_l2_levels

  let l2_levels_of_l1_level = Backend.l2_levels_of_l1_level

  let single_chain_id_and_family ~(config : Configuration.t) ~enable_multichain
      =
    let open Lwt_result_syntax in
    match (config.experimental_features.l2_chains, enable_multichain) with
    | None, false -> return (None, L2_types.Ex_chain_family EVM)
    | None, true -> tzfail Node_error.Singlechain_node_multichain_kernel
    | Some [l2_chain], false ->
        let*! () = Events.multichain_node_singlechain_kernel () in
        return (Some l2_chain.chain_id, L2_types.Ex_chain_family EVM)
    | Some [l2_chain], true ->
        let chain_id = l2_chain.chain_id in
        let* chain_family = chain_family chain_id in
        if l2_chain.chain_family = chain_family then
          return (Some chain_id, chain_family)
        else
          tzfail
            (Node_error.Mismatched_chain_family
               {
                 chain_id;
                 node_family = l2_chain.chain_family;
                 kernel_family = chain_family;
               })
    | _ -> tzfail Node_error.Unexpected_multichain
end

(** Represents the different ways transactions can be injected into the system *)
type endpoint =
  | Rpc of Uri.t
    (* Send transactions through standard RPC calls to the node at uri *)
  | Websocket of Websocket_client.t
    (* Use an active websocket client to push transactions in real time *)
  | Block_producer (* Inject transactions directly into the block producer. *)

(** [Tx_container] is the signature of the module that deals with
    storing and forwarding transactions. the module type is used by
    {!Services.dispatch_request} to request informations about pending
    transactions. *)
module type Tx_container = sig
  type address

  type transaction_object

  (** [nonce ~next_nonce address] must returns the next gap nonce
      available. *)
  val nonce :
    next_nonce:Ethereum_types.quantity ->
    address ->
    Ethereum_types.quantity tzresult Lwt.t

  (** [add ~next_nonce tx_object raw_tx] returns the next gap nonce
      available based on the pending transaction of the tx_container.
      [next_nonce] is the next expected nonce found in the backend. *)
  val add :
    ?wait_confirmation:bool ->
    next_nonce:Ethereum_types.quantity ->
    transaction_object ->
    raw_tx:Ethereum_types.hex ->
    (Ethereum_types.hash, string) result tzresult Lwt.t

  (** [find hash] returns the transaction_object found in tx
      container. *)
  val find : Ethereum_types.hash -> transaction_object option tzresult Lwt.t

  (** [content ()] returns all the transactions found in tx
      container. *)
  val content : unit -> Transaction_object.txqueue_content tzresult Lwt.t

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

  (** [is_locked] checks if the queue is locked. *)
  val is_locked : unit -> bool tzresult Lwt.t

  (** The Tx_queue has a table of pending transactions. There are two
      ways for transactions to be removed from this table; either they
      are confirmed because they have been seen in a block or they are
      dropped.

      [confirm_transactions ~clear_pending_queue_after ~confirmed_txs]
      confirms [confirmed_txs] hash. If [clear_pending_queue_after]
      then any other pending transactions in the tx_queue are
      dropped. *)
  val confirm_transactions :
    clear_pending_queue_after:bool ->
    confirmed_txs:Ethereum_types.hash Seq.t ->
    unit tzresult Lwt.t

  (** The Tx_queue has a table of pending transactions. There are two
      ways for transactions to be removed from this table; either they
      are confirmed because they have been seen in a block or they are
      dropped.

      [dropped_transaction ~dropped_tx] drops [dropped_tx] hash. *)
  val dropped_transaction :
    dropped_tx:Ethereum_types.hash -> reason:string -> unit tzresult Lwt.t

  (** The Tx_pool pops transactions until the sum of the sizes of the
      popped transactions reaches maximum_cumulative_size; it ignores
      the [validate_tx] and [initial_validation_state] arguments, The
      Tx_queue however ignores [maximum_cumulative_size] and instead
      uses [validate_tx] to pop valid transactions until either `Drop
      or `Stop is returned. *)
  val pop_transactions :
    maximum_cumulative_size:int ->
    validate_tx:
      ('a ->
      string ->
      transaction_object ->
      [`Keep of 'a | `Drop of string | `Stop] tzresult Lwt.t) ->
    initial_validation_state:'a ->
    (string * transaction_object) list tzresult Lwt.t

  (** [size_info] returns the size of the tx container. *)
  val size_info : unit -> Metrics.Tx_pool.size_info tzresult Lwt.t
end

(** ['f tx_container] is a GADT parametrized by the same type argument
    as [L2_types.chain_family]. It is useful to statically guarantee
    that the launched tx-container uses types compatible with the
    chain's chain family. *)

type 'f tx_container =
  | Evm_tx_container :
      (module Tx_container
         with type address = Ethereum_types.address
          and type transaction_object = Transaction_object.t)
      -> L2_types.evm_chain_family tx_container
  | Michelson_tx_container :
      (module Tx_container
         with type transaction_object = Tezos_types.Operation.t)
      -> L2_types.michelson_chain_family tx_container

(** Some functions of the Tx_container module, such as [add], have
    interfaces which actually depend on the chain family but many
    others, such as [clear] don't.

    In the former case, we usually know statically the type of the
    chain family and hence of the tx-container and we can for example
    invoke [add] as follows:

    {[
      let Evm_tx_container (module Tx_container) = tx_container in
      let** hash = Tx_container.add ~next_nonce ~raw_tx tx_obj in
    ]}

    In the latter case, statically knowing the type of the chain
    family is not required and the following [tx_container_module]
    function can be used to get a [Tx_container] module:

    {[
      let (module Tx_container) = Services_backend_sig.tx_container_module tx_container in
      let* () = Tx_container.clear () in
    ]}


*)
let tx_container_module (type f) (tx_container : f tx_container) =
  match tx_container with
  | Evm_tx_container m -> (m :> (module Tx_container))
  | Michelson_tx_container m -> (m :> (module Tx_container))

type ex_tx_container = Ex_tx_container : _ tx_container -> ex_tx_container
