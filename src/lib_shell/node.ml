(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2024 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_base

type error += Non_recoverable_context

type error += Failed_to_init_P2P

let () =
  register_error_kind
    `Permanent
    ~id:"context.non_recoverable_context"
    ~title:"Non recoverable context"
    ~description:"Cannot recover from a corrupted context."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "@[The context may have been corrupted after crashing while writing \
         data on disk. Its state appears to be non-recoverable. Import a \
         snapshot or re-synchronize from an empty node data directory.@]")
    Data_encoding.unit
    (function Non_recoverable_context -> Some () | _ -> None)
    (fun () -> Non_recoverable_context) ;
  register_error_kind
    `Permanent
    ~id:"main.run.failed_to_init_p2p"
    ~title:"Cannot start node: P2P initialization failed"
    ~description:
      "Tezos node could not be started because of a network problem while \
       initializing P2P."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Tezos node could not be started because of a network problem.")
    Data_encoding.(obj1 @@ req "error" @@ constant "Failed_to_init_P2P")
    (function Failed_to_init_P2P -> Some () | _ -> None)
    (fun () -> Failed_to_init_P2P)

type t = {
  store : Store.t;
  distributed_db : Distributed_db.t;
  validator : Validator.t;
  mainchain_validator : Chain_validator.t;
  p2p : Distributed_db.p2p;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  dal_config : Tezos_crypto_dal.Cryptobox.Config.t;
  operation_metadata_size_limit : Shell_limits.operation_metadata_size_limit;
  (* For P2P RPCs *)
  shutdown : unit -> unit Lwt.t;
}

let get_version node =
  let commit_info =
    ({
       commit_hash = Tezos_version_value.Current_git_info.commit_hash;
       commit_date = Tezos_version_value.Current_git_info.committer_date;
     }
      : Tezos_version.Node_version.commit_info)
  in
  let version = Tezos_version_value.Current_git_info.octez_version in
  let network_version = P2p.announced_version node.p2p in
  Tezos_version.Node_version.
    {version; commit_info = Some commit_info; network_version}

let peer_metadata_cfg : _ P2p_params.peer_meta_config =
  {
    peer_meta_encoding = Peer_metadata.encoding;
    peer_meta_initial = Peer_metadata.empty;
    score = Peer_metadata.score;
  }

let connection_metadata_cfg cfg : _ P2p_params.conn_meta_config =
  {
    conn_meta_encoding = Connection_metadata.encoding;
    private_node = (fun {private_node; _} -> private_node);
    conn_meta_value = (fun () -> cfg);
  }

let init_connection_metadata opt disable_mempool =
  let open Connection_metadata in
  match opt with
  | None -> {disable_mempool = false; private_node = false}
  | Some c -> {disable_mempool; private_node = c.P2p.private_mode}

let init_p2p chain_name p2p_params disable_mempool =
  let open Lwt_result_syntax in
  let message_cfg = Distributed_db_message.cfg chain_name in
  match p2p_params with
  | None ->
      let c_meta = init_connection_metadata None disable_mempool in
      let*! () = Node_event.(emit p2p_event) "p2p_layer_disabled" in
      return (P2p.faked_network message_cfg peer_metadata_cfg c_meta)
  | Some (config, limits) ->
      let c_meta = init_connection_metadata (Some config) disable_mempool in
      let conn_metadata_cfg = connection_metadata_cfg c_meta in
      let*! () = Node_event.(emit p2p_event) "bootstrapping" in
      let* p2p =
        P2p.create
          ~config
          ~limits
          ~received_msg_hook:
            Shell_metrics.Distributed_db.Messages.on_received_msg
          ~sent_msg_hook:Shell_metrics.Distributed_db.Messages.on_sent_msg
          ~broadcasted_msg_hook:
            Shell_metrics.Distributed_db.Messages.on_broadcasted_msg
          peer_metadata_cfg
          conn_metadata_cfg
          message_cfg
      in
      let*! () = Node_event.(emit p2p_event) "p2p_maintenance_started" in
      return p2p |> trace Failed_to_init_P2P

type config = {
  genesis : Genesis.t;
  chain_name : Distributed_db_version.Name.t;
  sandboxed_chain_name : Distributed_db_version.Name.t;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  operation_metadata_size_limit : Shell_limits.operation_metadata_size_limit;
  data_dir : string;
  internal_events : Tezos_base.Internal_event_config.t;
  store_root : string;
  context_root : string;
  protocol_root : string;
  patch_context :
    (Tezos_protocol_environment.Context.t ->
    Tezos_protocol_environment.Context.t tzresult Lwt.t)
    option;
  p2p : (P2p.config * P2p_limits.t) option;
  target : (Block_hash.t * int32) option;
  disable_mempool : bool;
  enable_testchain : bool;
  dal_config : Tezos_crypto_dal.Cryptobox.Config.t;
}

(* These protocols are linked with the node and
   do not have their actual hash on purpose. *)
let test_protocol_hashes =
  List.map
    (fun s -> Protocol_hash.of_b58check_exn s)
    [
      "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK";
      "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT";
      "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp";
      "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im";
    ]

let store_known_protocols store =
  let open Lwt_syntax in
  let embedded_protocols = Registered_protocol.seq_embedded () in
  Seq.S.iter
    (fun protocol_hash ->
      match Store.Protocol.mem store protocol_hash with
      | true -> Node_event.(emit store_protocol_already_included) protocol_hash
      | false -> (
          match Registered_protocol.get_embedded_sources protocol_hash with
          | None -> Node_event.(emit store_protocol_missing_files) protocol_hash
          | Some protocol -> (
              let hash = Protocol.hash protocol in
              if not (Protocol_hash.equal hash protocol_hash) then
                if
                  List.mem
                    ~equal:Protocol_hash.equal
                    protocol_hash
                    test_protocol_hashes
                then Lwt.return_unit
                  (* noop. test protocol should not be stored *)
                else
                  Node_event.(emit store_protocol_incorrect_hash) protocol_hash
              else
                let* o = Store.Protocol.store store hash protocol in
                match o with
                | Some hash' ->
                    assert (hash = hash') ;
                    Node_event.(emit store_protocol_success) protocol_hash
                | None ->
                    Node_event.(emit store_protocol_already_included)
                      protocol_hash)))
    embedded_protocols

let check_context_consistency store =
  let open Lwt_result_syntax in
  let main_chain_store = Store.main_chain_store store in
  let*! block = Store.Chain.current_head main_chain_store in
  let*! b = Store.Block.context_exists main_chain_store block in
  match b with
  | true ->
      let*! () = Node_event.(emit storage_context_already_consistent ()) in
      return_unit
  | false ->
      let*! () = Node_event.(emit storage_corrupted_context_detected ()) in
      tzfail Non_recoverable_context

let create ?(sandboxed = false) ?sandbox_parameters ~singleprocess ~version
    ~commit_info
    {
      genesis;
      chain_name;
      sandboxed_chain_name;
      user_activated_upgrades;
      user_activated_protocol_overrides;
      operation_metadata_size_limit;
      data_dir;
      internal_events;
      store_root;
      context_root;
      protocol_root;
      patch_context;
      p2p = p2p_params;
      target;
      disable_mempool;
      enable_testchain;
      dal_config;
    } peer_validator_limits block_validator_limits prevalidator_limits
    chain_validator_limits history_mode =
  let open Lwt_result_syntax in
  let start_prevalidator, start_testchain =
    match p2p_params with
    | Some _ -> (not disable_mempool, enable_testchain)
    | None -> (true, true)
  in
  let* p2p =
    init_p2p
      (if sandboxed then sandboxed_chain_name else chain_name)
      p2p_params
      disable_mempool
  in
  Shell_metrics.Version.init ~version ~commit_info p2p ;
  let* validator_process, store =
    let open Block_validator_process in
    let validator_environment =
      {
        user_activated_upgrades;
        user_activated_protocol_overrides;
        operation_metadata_size_limit;
      }
    in
    if singleprocess then
      let* store =
        Store.init
          ?patch_context
          ?history_mode
          ~store_dir:store_root
          ~context_dir:context_root
          ~allow_testchains:start_testchain
          ~readonly:false
          genesis
      in
      let main_chain_store = Store.main_chain_store store in
      let* validator_process =
        init validator_environment (Internal main_chain_store)
      in
      return (validator_process, store)
    else
      let* validator_process =
        init
          validator_environment
          (External
             {
               data_dir;
               readonly = false;
               genesis;
               context_root;
               protocol_root;
               process_path = Sys.executable_name;
               sandbox_parameters;
               dal_config;
               internal_events;
             })
      in
      let commit_genesis ~chain_id =
        Block_validator_process.commit_genesis validator_process ~chain_id
      in
      let* store =
        Store.init
          ?patch_context
          ?history_mode
          ~commit_genesis
          ~store_dir:store_root
          ~context_dir:context_root
          ~allow_testchains:start_testchain
          ~readonly:false
          genesis
      in
      return (validator_process, store)
  in
  let* () = check_context_consistency store in
  let main_chain_store = Store.main_chain_store store in
  let* () =
    Option.iter_es
      (fun target_descr -> Store.Chain.set_target main_chain_store target_descr)
      target
  in
  let distributed_db = Distributed_db.create store p2p in
  let*! () = store_known_protocols store in
  let* validator =
    Validator.create
      store
      distributed_db
      peer_validator_limits
      block_validator_limits
      validator_process
      prevalidator_limits
      chain_validator_limits
      ~start_testchain
  in
  let* mainchain_validator =
    Validator.activate
      validator
      ~start_prevalidator
      ~validator_process
      main_chain_store
  in
  let shutdown () =
    (* Shutdown workers in the reverse order of creation *)
    let*! () = Node_event.(emit shutdown_validator) () in
    let*! () = Validator.shutdown validator in
    let*! () = Node_event.(emit shutdown_ddb) () in
    let*! () = Distributed_db.shutdown distributed_db in
    let*! () = Node_event.(emit shutdown_store) () in
    let*! _ = Store.close_store store in
    let*! () = Node_event.(emit shutdown_p2p_layer) () in
    let*! () = P2p.shutdown p2p in
    Lwt.return_unit
  in
  return
    {
      store;
      distributed_db;
      validator;
      mainchain_validator;
      p2p;
      user_activated_upgrades;
      user_activated_protocol_overrides;
      dal_config;
      operation_metadata_size_limit;
      shutdown;
    }

let shutdown node = node.shutdown ()

let build_rpc_directory ~node_version ~commit_info node =
  let dir : unit Tezos_rpc.Directory.t ref = ref Tezos_rpc.Directory.empty in
  let merge d = dir := Tezos_rpc.Directory.merge !dir d in
  let register0 s f =
    dir := Tezos_rpc.Directory.register !dir s (fun () p q -> f p q)
  in
  merge
    (Protocol_directory.build_rpc_directory
       (Block_validator.running_worker ())
       node.store) ;
  merge
    (Monitor_directory.build_rpc_directory
       ~commit_info
       node.validator
       node.mainchain_validator) ;
  merge (Injection_directory.build_rpc_directory node.validator) ;
  merge (Chain_directory.build_rpc_directory node.validator) ;
  merge (P2p_directory.build_rpc_directory node.p2p) ;
  merge (Worker_directory.build_rpc_directory node.store) ;
  merge (Stat_directory.rpc_directory ()) ;
  merge
    (Config_directory.build_rpc_directory
       ~user_activated_upgrades:node.user_activated_upgrades
       ~user_activated_protocol_overrides:node.user_activated_protocol_overrides
       ~dal_config:node.dal_config
       ~mainchain_validator:node.mainchain_validator
       node.store) ;
  merge (Version_directory.rpc_directory node_version) ;
  merge (Health_directory.build_rpc_directory ()) ;
  register0 Tezos_rpc.Service.error_service (fun () () ->
      Lwt.return_ok (Data_encoding.Json.schema Error_monad.error_encoding)) ;
  !dir
