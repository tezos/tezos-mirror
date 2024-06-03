(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Parameters

module Daemon = struct
  type t = {
    daemon : unit tzresult Lwt.t;
    head_stream_stopper : Tezos_rpc.Context.stopper;
  }

  (** [make_stream_daemon ~on_head ~head_stream] calls [on_head] on
      each newly received value from [head_stream].

      It returns a couple [(p, stopper)] where [p] is a promise
      resolving when the stream closes and [stopper] is a function
      closing the stream. *)
  let make_stream_daemon ~on_head ~head_stream =
    let open Lwt_result_syntax in
    let* head_stream, head_stream_stopper = head_stream in
    let rec stream_processor () =
      let*! head_element = Lwt_stream.get head_stream in
      match head_element with
      | None -> return_unit
      | Some element ->
          let*! processed_head = on_head element in
          let*! () =
            match processed_head with
            | Ok () -> Lwt.return_unit
            | Error trace -> Rpc_process_events.(emit daemon_error) trace
          in
          stream_processor ()
    in
    return {daemon = stream_processor (); head_stream_stopper}

  let shutdown {head_stream_stopper; _} =
    let open Lwt_syntax in
    let* () = Rpc_process_events.(emit shutting_head_daemon) () in
    head_stream_stopper () ;
    return_unit
end

let init_store ~allow_testchains ~readonly parameters =
  (* Invariant: the Store.init must be called after the Store.init of
     the node is finished. Otherwise, it may create an race because of
     the consistency checks. *)
  let config = parameters.config in
  let store_dir = Data_version.store_dir config.data_dir in
  let context_dir = Data_version.context_dir config.data_dir in
  Store.init
    ?history_mode:config.shell.history_mode
    ~store_dir
    ~context_dir
    ~allow_testchains
    ~readonly
    config.blockchain_network.genesis

let sync_store (dynamic_store : Store.t option ref) last_status parameters
    (block_hash, (header : Tezos_base.Block_header.t)) =
  let open Lwt_result_syntax in
  let block_level = header.shell.level in
  let* () =
    match !dynamic_store with
    | Some store ->
        let*! () =
          Rpc_process_events.(emit start_synchronization)
            (block_level, block_hash)
        in
        let* store, current_status, cleanups =
          Store.sync ~last_status:!last_status ~trigger_hash:block_hash store
        in
        last_status := current_status ;
        dynamic_store := Some store ;
        let*! () = cleanups () in
        let*! () =
          Rpc_process_events.(emit synchronized) (block_hash, block_level)
        in
        return_unit
    | None ->
        let* store =
          init_store ~allow_testchains:false ~readonly:true parameters
        in
        dynamic_store := Some store ;
        return_unit
  in
  return_unit

let handle_new_head (dynamic_store : Store.t option ref) last_status parameters
    (head_watcher : (Block_hash.t * Block_header.t) Lwt_watcher.input)
    (block_hash, (header : Tezos_base.Block_header.t)) =
  let open Lwt_result_syntax in
  let block_level = header.shell.level in
  let*! () = Rpc_process_events.(emit new_head) block_level in
  let* () =
    sync_store dynamic_store last_status parameters (block_hash, header)
  in
  (* The monitor_head wrapped stream used by clients is finally
     notified. *)
  Lwt_watcher.notify head_watcher (block_hash, header) ;
  return_unit

let init (dynamic_store : Store.t option ref) parameters
    (stream : (Block_hash.t * Block_header.t) Lwt_watcher.input) =
  let open Lwt_result_syntax in
  let ctx =
    Forward_handler.build_socket_redirection_ctx parameters.rpc_comm_socket_path
  in
  let module CustomRetryClient = struct
    include RPC_client_unix.RetryClient

    let call ?ctx:_ = call ~ctx
  end in
  let module Custom_rpc_client =
    RPC_client.Make (Resto_cohttp_client.Client.OfCohttp (CustomRetryClient)) in
  let rpc_config =
    Custom_rpc_client.
      {
        media_type = Media_type.Command_line.Any;
        endpoint = Uri.of_string Forward_handler.socket_forwarding_uri;
        logger = null_logger;
      }
  in
  let rpc_ctxt =
    new Custom_rpc_client.http_ctxt
      rpc_config
      (Media_type.Command_line.of_command_line rpc_config.media_type)
  in
  let config = parameters.config in
  let store_dir = Data_version.store_dir config.data_dir in
  let store_directory = Naming.store_dir ~dir_path:store_dir in
  let chain_id =
    Chain_id.of_block_hash config.blockchain_network.genesis.Genesis.block
  in
  let chain_dir = Naming.chain_dir store_directory chain_id in
  let status_file = Naming.block_store_status_file chain_dir in
  let* stored_status = Stored_data.load status_file in
  let*! initial_status = Stored_data.get stored_status in
  let* store = init_store ~allow_testchains:false ~readonly:true parameters in
  dynamic_store := Some store ;
  Daemon.make_stream_daemon
    ~on_head:
      (handle_new_head dynamic_store (ref initial_status) parameters stream)
    ~head_stream:(Tezos_shell_services.Monitor_services.heads rpc_ctxt `Main)
