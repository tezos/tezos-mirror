(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2019 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Lwt.Infix
open Tezos_base

type t = {
  state : State.t;
  distributed_db : Distributed_db.t;
  validator : Validator.t;
  mainchain_validator : Chain_validator.t;
  p2p : Distributed_db.p2p;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  (* For P2P RPCs *)
  shutdown : unit -> unit Lwt.t;
}

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
  | None ->
      {disable_mempool = false; private_node = false}
  | Some c ->
      {disable_mempool; private_node = c.P2p.private_mode}

let init_p2p chain_name p2p_params disable_mempool =
  let message_cfg = Distributed_db_message.cfg chain_name in
  match p2p_params with
  | None ->
      let c_meta = init_connection_metadata None disable_mempool in
      Node_event.(emit p2p_event) "p2p_layer_disabled"
      >>= fun () ->
      return (P2p.faked_network message_cfg peer_metadata_cfg c_meta)
  | Some (config, limits) ->
      let c_meta = init_connection_metadata (Some config) disable_mempool in
      let conn_metadata_cfg = connection_metadata_cfg c_meta in
      Node_event.(emit p2p_event) "bootstrapping"
      >>= fun () ->
      P2p.create
        ~config
        ~limits
        peer_metadata_cfg
        conn_metadata_cfg
        message_cfg
      >>=? fun p2p ->
      Node_event.(emit p2p_event) "p2p_maintain_started"
      >>= fun () -> return p2p

type config = {
  genesis : Genesis.t;
  chain_name : Distributed_db_version.Name.t;
  sandboxed_chain_name : Distributed_db_version.Name.t;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  data_dir : string;
  store_root : string;
  context_root : string;
  protocol_root : string;
  patch_context : (Context.t -> Context.t tzresult Lwt.t) option;
  p2p : (P2p.config * P2p.limits) option;
  checkpoint : Block_header.t option;
  disable_mempool : bool;
  enable_testchain : bool;
}

let default_backlog_size = 300

let default_backlog_level = Internal_event.Info

let default_workers_limits =
  {
    Worker_types.backlog_size = default_backlog_size;
    backlog_level = default_backlog_level;
  }

let default_block_validator_limits =
  let open Block_validator in
  {
    protocol_timeout = Time.System.Span.of_seconds_exn 120.;
    worker_limits = default_workers_limits;
  }

let default_prevalidator_limits =
  let open Prevalidator in
  {
    operation_timeout = Time.System.Span.of_seconds_exn 10.;
    max_refused_operations = 1000;
    worker_limits = default_workers_limits;
    operations_batch_size = 50;
  }

let default_peer_validator_limits =
  let open Peer_validator in
  {
    block_header_timeout = Time.System.Span.of_seconds_exn 300.;
    block_operations_timeout = Time.System.Span.of_seconds_exn 300.;
    protocol_timeout = Time.System.Span.of_seconds_exn 600.;
    new_head_request_timeout = Time.System.Span.of_seconds_exn 90.;
    worker_limits = default_workers_limits;
  }

let default_chain_validator_limits =
  let open Chain_validator in
  {
    synchronisation = {latency = 150; threshold = 4};
    worker_limits = default_workers_limits;
  }

let may_update_checkpoint chain_state checkpoint history_mode =
  match checkpoint with
  | None ->
      return_unit
  | Some checkpoint -> (
      State.best_known_head_for_checkpoint chain_state checkpoint
      >>= fun new_head ->
      Chain.set_head chain_state new_head
      >>= fun _old_head ->
      match history_mode with
      | History_mode.Archive ->
          State.Chain.set_checkpoint chain_state checkpoint
          >>= fun () -> return_unit
      | Full ->
          State.Chain.set_checkpoint_then_purge_full chain_state checkpoint
      | Rolling ->
          State.Chain.set_checkpoint_then_purge_rolling chain_state checkpoint
      )

(* These protocols are linked with the node and
   do not have their actual hash on purpose. *)
let test_protocol_hashes =
  List.map
    (fun s -> Protocol_hash.of_b58check_exn s)
    [ "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK";
      "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT";
      "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp";
      "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im" ]

let store_known_protocols state =
  let embedded_protocols = Registered_protocol.seq_embedded () in
  Seq.iter_s
    (fun protocol_hash ->
      State.Protocol.known state protocol_hash
      >>= function
      | true ->
          Node_event.(emit store_protocol_already_included) protocol_hash
      | false -> (
        match Registered_protocol.get_embedded_sources protocol_hash with
        | None ->
            Node_event.(emit store_protocol_missing_files) protocol_hash
        | Some protocol -> (
            let hash = Protocol.hash protocol in
            if not (Protocol_hash.equal hash protocol_hash) then
              if List.mem protocol_hash test_protocol_hashes then
                Lwt.return_unit (* noop. test protocol should not be stored *)
              else
                Node_event.(emit store_protocol_incorrect_hash) protocol_hash
            else
              State.Protocol.store state protocol
              >>= function
              | Some hash' ->
                  assert (hash = hash') ;
                  Node_event.(emit store_protocol_success) protocol_hash
              | None ->
                  Node_event.(emit store_protocol_already_included)
                    protocol_hash ) ))
    embedded_protocols

type error += Non_recoverable_context

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
    (fun () -> Non_recoverable_context)

let check_and_fix_storage_consistency state vp =
  let restore_context_integrity () =
    Node_event.(emit storage_corrupted_context_detected) ()
    >>= fun () ->
    (* Corrupted context for current block, backtracking head *)
    Block_validator_process.restore_context_integrity vp
    >>= function
    | Ok (Some n) ->
        Node_event.(emit storage_restored_context_integrity) n
        >>= fun () -> return_unit
    | Ok None ->
        Node_event.(emit storage_context_already_consistent) ()
        >>= fun () -> return_unit
    | Error err ->
        Node_event.(emit storage_restore_context_integrity_error) err
        >>= fun () -> fail Non_recoverable_context
  in
  State.Chain.all state
  >>= fun chains ->
  let rec check_block to_be_cleaned n chain_state block =
    fail_unless (n > 0) Validation_errors.Bad_data_dir
    >>=? fun () ->
    Lwt.catch
      (fun () -> State.Block.context_exists block >>= fun b -> return b)
      (fun _exn ->
        restore_context_integrity ()
        >>=? fun () ->
        (* Corrupted commit has been purged. We need to backtrack the
            head. Returning false will do the trick. *)
        return_false)
    >>=? fun is_context_known ->
    if is_context_known then
      (* Found a known context for the block: setting as consistent head *)
      Chain.set_head chain_state block
      >>=? fun _ ->
      (* Make sure to remove the block only after updating the head *)
      List.iter_es State.Block.remove to_be_cleaned
    else
      (* Did not find a known context. Need to backtrack the head up *)
      let header = State.Block.header block in
      State.Block.read chain_state header.shell.predecessor
      >>=? fun pred ->
      (check_block [@ocaml.tailcall])
        (block :: to_be_cleaned)
        (n - 1)
        chain_state
        pred
  in
  Seq.iter_es
    (fun chain_state ->
      Chain.head chain_state
      >>= fun block -> check_block [] 500 chain_state block)
    chains

let create ?(sandboxed = false) ?sandbox_parameters ~singleprocess
    { genesis;
      chain_name;
      sandboxed_chain_name;
      user_activated_upgrades;
      user_activated_protocol_overrides;
      data_dir;
      store_root;
      context_root;
      protocol_root;
      patch_context;
      p2p = p2p_params;
      disable_mempool;
      enable_testchain;
      checkpoint } peer_validator_limits block_validator_limits
    prevalidator_limits chain_validator_limits history_mode =
  let (start_prevalidator, start_testchain) =
    match p2p_params with
    | Some _ ->
        (not disable_mempool, enable_testchain)
    | None ->
        (true, true)
  in
  init_p2p
    (if sandboxed then sandboxed_chain_name else chain_name)
    p2p_params
    disable_mempool
  >>=? fun p2p ->
  (let open Block_validator_process in
  let validator_environment =
    {genesis; user_activated_upgrades; user_activated_protocol_overrides}
  in
  if singleprocess then
    State.init ~store_root ~context_root ?history_mode ?patch_context genesis
    >>=? fun (state, mainchain_state, context_index, history_mode) ->
    init validator_environment (Internal context_index)
    >>=? fun validator_process ->
    return (validator_process, state, mainchain_state, history_mode)
  else
    init
      validator_environment
      (External
         {
           data_dir;
           context_root;
           protocol_root;
           process_path = Sys.executable_name;
           sandbox_parameters;
         })
    >>=? fun validator_process ->
    let commit_genesis ~chain_id =
      Block_validator_process.commit_genesis validator_process ~chain_id
    in
    State.init
      ~store_root
      ~context_root
      ?history_mode
      ?patch_context
      ~commit_genesis
      genesis
    >>=? fun (state, mainchain_state, _context_index, history_mode) ->
    return (validator_process, state, mainchain_state, history_mode))
  >>=? fun (validator_process, state, mainchain_state, history_mode) ->
  check_and_fix_storage_consistency state validator_process
  >>=? fun () ->
  may_update_checkpoint mainchain_state checkpoint history_mode
  >>=? fun () ->
  let distributed_db = Distributed_db.create state p2p in
  store_known_protocols state
  >>= fun () ->
  Validator.create
    state
    distributed_db
    peer_validator_limits
    block_validator_limits
    validator_process
    prevalidator_limits
    chain_validator_limits
    ~start_testchain
  >>=? fun validator ->
  (* TODO : Check that the testchain is correctly activated after a node restart *)
  Validator.activate
    validator
    ~start_prevalidator
    ~validator_process
    mainchain_state
  >>=? fun mainchain_validator ->
  let shutdown () =
    (* Shutdown workers in the reverse order of creation *)
    Node_event.(emit shutdown_validator) ()
    >>= fun () ->
    Validator.shutdown validator
    >>= fun () ->
    Node_event.(emit shutdown_ddb) ()
    >>= fun () ->
    Distributed_db.shutdown distributed_db
    >>= fun () ->
    Node_event.(emit shutdown_state) ()
    >>= fun () ->
    State.close state
    >>= fun () ->
    Node_event.(emit shutdown_p2p_layer) ()
    >>= fun () -> P2p.shutdown p2p >>= fun () -> Lwt.return_unit
  in
  return
    {
      state;
      distributed_db;
      validator;
      mainchain_validator;
      p2p;
      user_activated_upgrades;
      user_activated_protocol_overrides;
      shutdown;
    }

let shutdown node = node.shutdown ()

let build_rpc_directory node =
  let dir : unit RPC_directory.t ref = ref RPC_directory.empty in
  let merge d = dir := RPC_directory.merge !dir d in
  let register0 s f =
    dir := RPC_directory.register !dir s (fun () p q -> f p q)
  in
  merge
    (Protocol_directory.build_rpc_directory
       (Block_validator.running_worker ())
       node.state) ;
  merge
    (Monitor_directory.build_rpc_directory
       node.validator
       node.mainchain_validator) ;
  merge (Injection_directory.build_rpc_directory node.validator) ;
  merge
    (Chain_directory.build_rpc_directory
       ~user_activated_upgrades:node.user_activated_upgrades
       ~user_activated_protocol_overrides:
         node.user_activated_protocol_overrides
       node.validator) ;
  merge (P2p_directory.build_rpc_directory node.p2p) ;
  merge (Worker_directory.build_rpc_directory node.state) ;
  merge (Stat_directory.rpc_directory ()) ;
  merge
    (Config_directory.build_rpc_directory
       ~user_activated_upgrades:node.user_activated_upgrades
       ~user_activated_protocol_overrides:
         node.user_activated_protocol_overrides) ;
  merge (Version_directory.rpc_directory node.p2p) ;
  register0 RPC_service.error_service (fun () () ->
      return (Data_encoding.Json.schema Error_monad.error_encoding)) ;
  !dir
