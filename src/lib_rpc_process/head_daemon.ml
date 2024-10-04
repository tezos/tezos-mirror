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
    applied_block_stream_stopper : Tezos_rpc.Context.stopper;
  }

  (** [fair_lwt_stream_get push s1 s2] aims to get the value available
      from [s1] and [s2] and [push] them to a stream, so that, all the
      values pushed to that stream are interleaved to preserve some
      fairness. *)
  let fair_lwt_stream_get push s1 s2 =
    let s1l = Lwt_stream.get_available s1 in
    let s2l = Lwt_stream.get_available s2 in
    match (s1l, s2l) with
    | [], [] -> () (* assert false *)
    | l1, l2 ->
        Seq.iter
          (fun v -> push (Some v))
          (Seq.interleave (List.to_seq l1) (List.to_seq l2))

  (** [make_stream_daemon ~on_head ~on_applied_block ~head_stream
      ~applied_block_stream] calls [on_head] or [on_applied_block]
      depending on the value received from the stream composed of
      [head_stream] and [applied_block_stream]. The composed stream is
      interleaved for fairness.
      It returns a couple [(p, stopper)] where [p] is a promise
      resolving when the stream closes and [stopper] is a function
      closing the stream. *)
  let make_stream_daemon ~on_head ~on_applied_block
      ~(head_stream :
         ((Block_hash.t * Block_header.t) Lwt_stream.t
         * Tezos_rpc.Context.stopper)
         tzresult
         Lwt.t)
      ~(applied_block_stream :
         ((Chain_id.t * Block_hash.t * Block_header.t * Operation.t trace trace)
          Lwt_stream.t
         * Tezos_rpc.Context.stopper)
         tzresult
         Lwt.t) =
    let open Lwt_result_syntax in
    let master_stream, push = Lwt_stream.create () in
    let* head_stream, head_stream_stopper = head_stream in
    let head_stream = Lwt_stream.map (fun v -> (`Head, v)) head_stream in
    let* applied_block_stream, applied_block_stream_stopper =
      applied_block_stream
    in
    let applied_block_stream =
      Lwt_stream.map
        (fun (_, hash, header, _) -> (`Applied, (hash, header)))
        applied_block_stream
    in
    let rec stream_aggregator () =
      let*! block_element =
        Lwt.choose
          [Lwt_stream.peek head_stream; Lwt_stream.peek applied_block_stream]
      in
      let*! () = Lwt.pause () in
      match block_element with
      | None -> stream_aggregator ()
      | Some _ ->
          fair_lwt_stream_get push head_stream applied_block_stream ;
          stream_aggregator ()
    in
    let _ = stream_aggregator () in
    let rec stream_processor () =
      let*! block_element = Lwt_stream.get master_stream in
      match block_element with
      | None -> return_unit
      | Some (`Head, v) ->
          let*! processed_head = on_head v in
          let*! () =
            match processed_head with
            | Ok () -> Lwt.return_unit
            | Error trace -> Rpc_process_events.(emit daemon_error) trace
          in
          stream_processor ()
      | Some (`Applied, v) ->
          let*! processed_block = on_applied_block v in
          let*! () =
            match processed_block with
            | Ok () -> Lwt.return_unit
            | Error trace ->
                let*! () = Rpc_process_events.(emit daemon_error) trace in
                Lwt.return_unit
          in
          stream_processor ()
    in
    return
      {
        daemon = stream_processor ();
        head_stream_stopper;
        applied_block_stream_stopper;
      }

  let shutdown {head_stream_stopper; applied_block_stream_stopper; _} =
    let open Lwt_syntax in
    let* () = Rpc_process_events.(emit shutting_head_daemon) () in
    head_stream_stopper () ;
    applied_block_stream_stopper () ;
    return_unit
end

let init_store ~allow_testchains ~readonly parameters =
  (* Invariant: the Store.init must be called after the Store.init of
     the node is finished. Otherwise, it may create a race because of
     the consistency checks. *)
  let config = parameters.config in
  let store_dir = Data_version.store_dir config.data_dir in
  let context_root_dir = config.data_dir in
  Store.init
    ?history_mode:config.shell.history_mode
    ~store_dir
    ~context_root_dir
    ~allow_testchains
    ~readonly
    config.blockchain_network.genesis

let sync_store (dynamic_store : Store.t option ref) last_status parameters
    (block_hash, (header : Tezos_base.Block_header.t)) =
  let open Lwt_result_syntax in
  let block_level = header.shell.level in
  match !dynamic_store with
  | Some store ->
      let*! () =
        Rpc_process_events.(emit start_synchronization) (block_level, block_hash)
      in
      let* store, current_status, cleanups =
        Store.sync ~last_status:!last_status store
      in
      last_status := current_status ;
      dynamic_store := Some store ;
      let*! () = cleanups () in
      let*! () =
        Rpc_process_events.(emit store_synchronized_on_head)
          (block_hash, block_level)
      in
      return store
  | None ->
      let* store =
        init_store ~allow_testchains:false ~readonly:true parameters
      in
      dynamic_store := Some store ;
      return store

let handle_new_head (dynamic_store : Store.t option ref) last_status parameters
    (head_watcher : (Block_hash.t * Block_header.t) Lwt_watcher.input)
    (block_hash, (header : Tezos_base.Block_header.t)) =
  let open Lwt_result_syntax in
  let block_level = header.shell.level in
  let*! () = Rpc_process_events.(emit new_head) block_level in
  let* (_ : Store.t) =
    sync_store dynamic_store last_status parameters (block_hash, header)
  in
  (* The monitor_head wrapped stream used by clients is finally
     notified. *)
  Lwt_watcher.notify head_watcher (block_hash, header) ;
  return_unit

let handle_new_applied_block (dynamic_store : Store.t option ref) last_status
    parameters (applied_block_watcher : Directory.applied_watcher_kind ref)
    (block_hash, (header : Tezos_base.Block_header.t)) =
  let open Lwt_result_syntax in
  match !applied_block_watcher with
  | Empty -> return_unit
  | Filled w ->
      let block_level = header.shell.level in
      let*! () = Rpc_process_events.(emit new_applied_block) block_level in
      let* store =
        sync_store dynamic_store last_status parameters (block_hash, header)
      in
      let chain_store = Store.main_chain_store store in
      let* block = Store.Block.read_block chain_store block_hash in
      Lwt_watcher.notify w (chain_store, block) ;
      return_unit

let init (dynamic_store : Store.t option ref) parameters
    (head_watcher : (Block_hash.t * Block_header.t) Lwt_watcher.input)
    (applied_block_watcher : Directory.applied_watcher_kind ref) =
  let open Lwt_result_syntax in
  let ctx =
    Forward_handler.build_socket_redirection_ctx parameters.rpc_comm_socket_path
  in
  let module CustomRetryClient = struct
    include RPC_client_unix.RetryClient

    let call ?ctx:_ = call ~ctx

    let call_with_closefn ?ctx:_ = call_with_closefn ~ctx
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
  let _ = Tezos_shell_services.Monitor_services.applied_blocks rpc_ctxt () in
  dynamic_store := Some store ;
  Daemon.make_stream_daemon
    ~on_head:
      (handle_new_head
         dynamic_store
         (ref initial_status)
         parameters
         head_watcher)
    ~head_stream:(Tezos_shell_services.Monitor_services.heads rpc_ctxt `Main)
    ~on_applied_block:
      (handle_new_applied_block
         dynamic_store
         (ref initial_status)
         parameters
         applied_block_watcher)
    ~applied_block_stream:
      (Tezos_shell_services.Monitor_services.applied_blocks rpc_ctxt ())
