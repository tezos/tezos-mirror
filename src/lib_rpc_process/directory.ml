(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2024 Nomadic Labs. <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

(* This is a wrapper on top of the monitor_head streamed RPC so that
   the stream to monitor is handled by the RPC process itself. It
   allows the RPC process to run some internal synchronization before
   notifying data on this stream.
   For example, as soon as a new block is advertized to the RPC
   process, it synchronizes its internal state to make sure the new
   block can be queried, and then notifies it on this stream. *)
let register_monitor_head dir (store : Store.t option ref)
    (head_watcher : (Block_hash.t * Block_header.t) Lwt_watcher.input) =
  let dir = ref dir in
  let gen_register1 s f =
    dir := Tezos_rpc.Directory.gen_register !dir s (fun ((), a) p q -> f a p q)
  in
  gen_register1 Tezos_shell_services.Monitor_services.S.heads (fun chain q () ->
      let open Lwt_syntax in
      let head_watcher = Lwt_watcher.create_stream head_watcher in
      let* store =
        match !store with
        | Some store -> return (ref store)
        | None -> Lwt.fail Not_found
      in
      Tezos_shell.Monitor_directory.monitor_head ~head_watcher store chain q) ;
  !dir

type applied_watcher_kind =
  | Empty
  | Filled of (Store.chain_store * Store.Block.t) Lwt_watcher.input

let register_monitor_applied_blocks dir
    (applied_blocks_watcher : applied_watcher_kind ref) =
  let dir = ref dir in
  let gen_register0 s f =
    dir := Tezos_rpc.Directory.gen_register !dir s (fun () p q -> f p q)
  in
  gen_register0
    Tezos_shell_services.Monitor_services.S.applied_blocks
    (fun q () ->
      let open Lwt_syntax in
      let* applied_blocks_watcher =
        match !applied_blocks_watcher with
        | Filled v -> return v
        | Empty ->
            (* The applied_blocks_watcher is initialized only if it is
               requested at least once. *)
            let watcher = Lwt_watcher.create_input () in
            applied_blocks_watcher := Filled watcher ;
            return watcher
      in
      let applied_blocks_watcher =
        Lwt_watcher.create_stream applied_blocks_watcher
      in
      Tezos_shell.Monitor_directory.applied_blocks ~applied_blocks_watcher q) ;
  !dir

let build_rpc_directory node_version config dynamic_store
    ~(head_watcher : (Block_hash.t * Block_header.t) Lwt_watcher.input)
    ~(applied_blocks_watcher : applied_watcher_kind ref) =
  let static_dir = Tezos_shell.Version_directory.rpc_directory node_version in
  let static_dir =
    Tezos_shell.Config_directory.build_rpc_directory_for_rpc_process
      ~user_activated_upgrades:
        config.Config_file.blockchain_network.user_activated_upgrades
      ~user_activated_protocol_overrides:
        config.blockchain_network.user_activated_protocol_overrides
      ~dal_config:config.blockchain_network.dal_config
      static_dir
  in
  let static_dir =
    Tezos_rpc.Directory.register0
      static_dir
      Node_services.S.config
      (fun () () -> Lwt.return_ok config)
  in
  let static_dir =
    Tezos_rpc.Directory.merge
      static_dir
      (Tezos_shell.Health_directory.build_rpc_directory ())
  in
  let static_dir =
    register_monitor_head static_dir dynamic_store head_watcher
  in
  let static_dir =
    register_monitor_applied_blocks static_dir applied_blocks_watcher
  in
  Tezos_rpc.Directory.register_dynamic_directory
    static_dir
    (Tezos_rpc.Path.subst1 Tezos_shell_services.Chain_services.path)
    (fun ((), chain) ->
      let dir =
        Tezos_shell.Chain_directory.rpc_directory_without_validator ()
      in
      let dir =
        Tezos_rpc.Directory.map
          (fun ((), _chain) ->
            let store = !dynamic_store in
            match store with
            | None -> Lwt.fail Not_found
            | Some store ->
                Tezos_shell.Chain_directory.get_chain_store_exn store chain)
          dir
      in
      Lwt.return dir)
