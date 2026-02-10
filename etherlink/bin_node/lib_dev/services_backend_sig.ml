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

  module Tezos =
    Tezos_backend.Make
      (struct
        include Backend.SimulatorBackend

        let block_param_to_block_number =
          Backend.block_param_to_block_number ~chain_family:L2_types.Michelson
      end)
      (Tezlink_block_storage)

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

(* Reuses [Tx_queue_types.preconfirmed_transactions_result] so sequencer can
   pass [Block_producer.preconfirm_transactions] directly without adapters. *)
type preconfirm_transactions =
  transactions:(string * Tx_queue_types.transaction_object_t) list ->
  Tx_queue_types.preconfirmed_transactions_result tzresult Lwt.t

type error += IC_disabled

type endpoint =
  | Rpc of Uri.t
    (* Send transactions through standard RPC calls to the node at uri *)
  | Websocket of Websocket_client.t
    (* Use an active websocket client to push transactions in real time *)
  | Block_producer of preconfirm_transactions
(* Inject transactions directly into the block producer.
       Callback form avoids a Tx_queue -> Block_producer dependency cycle. *)

type callback_status = [`Accepted | `Confirmed | `Dropped | `Refused]

type 'a variant_callback = 'a -> unit Lwt.t

(** A [callback] is called by the [Tx_queue] at various stages of a
    submitted transaction's life.

    The next tick after its insertion in the queue, a transaction is submitted
    to the relay node within a batch of [eth_sendRawTransaction] requests.

    {ul
      {li Depending on the result of the RPC, its [callback] is called with
          either [`Accepted] or [`Refused]).}
      {li As soon as the transaction appears in a blueprint, its callback is
          called with [`Confirmed]. If this does not happen before 2s, the
          [callback] is called with [`Dropped].}} *)
type callback = callback_status variant_callback
