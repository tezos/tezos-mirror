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

[@@@ocaml.warning "-30"]

open Lwt.Infix
open Tezos_base

module Initialization_event = struct
  type t = {
    time_stamp : float;
    status : [`P2p_layer_disabled | `Bootstrapping | `P2p_maintain_started];
  }

  let status_names =
    [ ("p2p_layer_disabled", `P2p_layer_disabled);
      ("bootstrapping", `Bootstrapping);
      ("p2p_maintain_started", `P2p_maintain_started) ]

  module Definition = struct
    let name = "shell-node"

    type nonrec t = t

    let encoding =
      let open Data_encoding in
      let v0_encoding =
        conv
          (function {time_stamp; status} -> (time_stamp, status))
          (fun (time_stamp, status) -> {time_stamp; status})
          (obj2
             (req "time-stamp" float)
             (req "status" (string_enum status_names)))
      in
      With_version.(encoding ~name (first_version v0_encoding))

    let pp ppf {status; _} =
      Format.fprintf
        ppf
        "%s initialization: %s"
        name
        (List.find (fun (_, s) -> s = status) status_names |> fst)

    let doc = "Status of the initialization of the P2P layer."

    let level _ = Internal_event.Notice
  end

  module Event = Internal_event.Make (Definition)

  let lwt_emit status =
    let time_stamp = Unix.gettimeofday () in
    Event.emit (fun () -> {time_stamp; status})
    >>= function
    | Ok () ->
        Lwt.return_unit
    | Error el ->
        Format.kasprintf
          Lwt.fail_with
          "Initialization_event.emit: %a"
          pp_print_error
          el
end

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
      Initialization_event.lwt_emit `P2p_layer_disabled
      >>= fun () ->
      return (P2p.faked_network message_cfg peer_metadata_cfg c_meta)
  | Some (config, limits) ->
      let c_meta = init_connection_metadata (Some config) disable_mempool in
      let conn_metadata_cfg = connection_metadata_cfg c_meta in
      Initialization_event.lwt_emit `Bootstrapping
      >>= fun () ->
      P2p.create
        ~config
        ~limits
        peer_metadata_cfg
        conn_metadata_cfg
        message_cfg
      >>=? fun p2p ->
      Initialization_event.lwt_emit `P2p_maintain_started
      >>= fun () -> return p2p

type config = {
  genesis : Genesis.t;
  chain_name : Distributed_db_version.Name.t;
  sandboxed_chain_name : Distributed_db_version.Name.t;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  store_root : string;
  context_root : string;
  protocol_root : string;
  patch_context : (Context.t -> Context.t tzresult Lwt.t) option;
  p2p : (P2p.config * P2p.limits) option;
  checkpoint : Block_header.t option;
  disable_mempool : bool;
  enable_testchain : bool;
}

and peer_validator_limits = Peer_validator.limits = {
  new_head_request_timeout : Time.System.Span.t;
  block_header_timeout : Time.System.Span.t;
  block_operations_timeout : Time.System.Span.t;
  protocol_timeout : Time.System.Span.t;
  worker_limits : Worker_types.limits;
}

and prevalidator_limits = Prevalidator.limits = {
  max_refused_operations : int;
  operation_timeout : Time.System.Span.t;
  worker_limits : Worker_types.limits;
  operations_batch_size : int;
}

and block_validator_limits = Block_validator.limits = {
  protocol_timeout : Time.System.Span.t;
  worker_limits : Worker_types.limits;
}

and chain_validator_limits = Chain_validator.limits = {
  bootstrap_threshold : int;
  worker_limits : Worker_types.limits;
}

let default_block_validator_limits =
  {
    protocol_timeout = Time.System.Span.of_seconds_exn 120.;
    worker_limits = {backlog_size = 1000; backlog_level = Internal_event.Debug};
  }

let default_prevalidator_limits =
  {
    operation_timeout = Time.System.Span.of_seconds_exn 10.;
    max_refused_operations = 1000;
    worker_limits = {backlog_size = 1000; backlog_level = Internal_event.Info};
    operations_batch_size = 50;
  }

let default_peer_validator_limits =
  {
    block_header_timeout = Time.System.Span.of_seconds_exn 300.;
    block_operations_timeout = Time.System.Span.of_seconds_exn 300.;
    protocol_timeout = Time.System.Span.of_seconds_exn 600.;
    new_head_request_timeout = Time.System.Span.of_seconds_exn 90.;
    worker_limits = {backlog_size = 1000; backlog_level = Internal_event.Info};
  }

let default_chain_validator_limits =
  {
    bootstrap_threshold = 4;
    worker_limits = {backlog_size = 1000; backlog_level = Internal_event.Info};
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

module Local_logging = Internal_event.Legacy_logging.Make_semantic (struct
  let name = "node.worker"
end)

let store_known_protocols state =
  let open Local_logging in
  let embedded_protocols = Registered_protocol.list_embedded () in
  Lwt_list.iter_s
    (fun protocol_hash ->
      State.Protocol.known state protocol_hash
      >>= function
      | true ->
          lwt_log_info
            Tag.DSL.(
              fun f ->
                f "protocol %a is already in store: nothing to do"
                -% a Protocol_hash.Logging.tag protocol_hash
                -% t event "embedded_protocol_already_stored")
      | false -> (
        match Registered_protocol.get_embedded_sources protocol_hash with
        | None ->
            lwt_log_info
              Tag.DSL.(
                fun f ->
                  f "protocol %a won't be stored: missing source files"
                  -% a Protocol_hash.Logging.tag protocol_hash
                  -% t event "embedded_protocol_missing_sources")
        | Some protocol -> (
            let hash = Protocol.hash protocol in
            if not (Protocol_hash.equal hash protocol_hash) then
              lwt_log_info
                Tag.DSL.(
                  fun f ->
                    f "protocol %a won't be stored: wrong hash"
                    -% a Protocol_hash.Logging.tag protocol_hash
                    -% t event "embedded_protocol_inconsistent_hash")
            else
              State.Protocol.store state protocol
              >>= function
              | Some hash' ->
                  assert (hash = hash') ;
                  lwt_log_info
                    Tag.DSL.(
                      fun f ->
                        f "protocol %a successfully stored"
                        -% a Protocol_hash.Logging.tag protocol_hash
                        -% t event "embedded_protocol_stored")
              | None ->
                  lwt_log_info
                    Tag.DSL.(
                      fun f ->
                        f "protocol %a is already in store: nothing to do"
                        -% a Protocol_hash.Logging.tag protocol_hash
                        -% t event "embedded_protocol_already_stored") ) ))
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
    let open Local_logging in
    Local_logging.lwt_log_error
      Tag.DSL.(
        fun f ->
          f
            "Context corruption detected: restoring integrity. This may take \
             a while..."
          -% t event "corrupted_context_detected")
    >>= fun () ->
    (* Corrupted context for current block, backtracking head *)
    Block_validator_process.restore_context_integrity vp
    >>= function
    | Ok (Some n) ->
        Local_logging.lwt_log_notice
          Tag.DSL.(
            fun f ->
              f "Successfully restored context integrity - repaired %a entries"
              -% a (Tag.def ~doc:"" "entries" Format.pp_print_int) n
              -% t event "restored_context_integrity")
        >>= fun () -> return_unit
    | Ok None ->
        Local_logging.lwt_log_notice
          Tag.DSL.(
            fun f ->
              f "No corruption detected while scanning the context."
              -% t event "context_already_consistent")
        >>= fun () -> return_unit
    | Error err ->
        Local_logging.lwt_log_error
          Tag.DSL.(fun f -> f "@[Error: %a@]" -% a Error_monad.errs_tag err)
        >>= fun () -> fail Non_recoverable_context
  in
  State.Chain.all state
  >>= fun chains ->
  let rec check_block n chain_state block =
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
      Chain.set_head chain_state block >>=? fun _ -> return_unit
    else
      (* Did not find a known context. Need to backtrack the head up *)
      let header = State.Block.header block in
      State.Block.read chain_state header.shell.predecessor
      >>=? fun pred ->
      check_block (n - 1) chain_state pred
      >>=? fun () ->
      (* Make sure to remove the block only after updating the head *)
      State.Block.remove block
  in
  iter_s
    (fun chain_state ->
      Chain.head chain_state >>= fun block -> check_block 500 chain_state block)
    chains

let create ?(sandboxed = false) ?sandbox_parameters ~singleprocess
    { genesis;
      chain_name;
      sandboxed_chain_name;
      user_activated_upgrades;
      user_activated_protocol_overrides;
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
  if singleprocess then
    State.init ~store_root ~context_root ?history_mode ?patch_context genesis
    >>=? fun (state, mainchain_state, context_index, history_mode) ->
    init
      ~genesis
      ~user_activated_upgrades
      ~user_activated_protocol_overrides
      (Internal context_index)
    >>=? fun validator_process ->
    return (validator_process, state, mainchain_state, history_mode)
  else
    init
      ~genesis
      ~user_activated_upgrades
      ~user_activated_protocol_overrides
      (External
         {
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
    let open Local_logging in
    lwt_log_info
      Tag.DSL.(
        fun f -> f "Shutting down the p2p layer..." -% t event "shutdown")
    >>= fun () ->
    P2p.shutdown p2p
    >>= fun () ->
    lwt_log_info
      Tag.DSL.(
        fun f ->
          f "Shutting down the distributed database..." -% t event "shutdown")
    >>= fun () ->
    Distributed_db.shutdown distributed_db
    >>= fun () ->
    lwt_log_info
      Tag.DSL.(
        fun f -> f "Shutting down the validator..." -% t event "shutdown")
    >>= fun () ->
    Validator.shutdown validator
    >>= fun () ->
    lwt_log_info
      Tag.DSL.(fun f -> f "Closing down the state..." -% t event "shutdown")
    >>= fun () -> State.close state
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
  RPC_directory.register_describe_directory_service
    !dir
    RPC_service.description_service
