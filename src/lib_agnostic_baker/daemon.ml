(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Errors

module Profiler = struct
  include (val Profiler.wrap Agnostic_baker_profiler.agnostic_baker_profiler)

  let[@warning "-32"] reset_block_section =
    Agnostic_baker_profiler.create_reset_block_section
      Agnostic_baker_profiler.agnostic_baker_profiler
end

module type AGNOSTIC_DAEMON = sig
  type t

  type command

  val run :
    keep_alive:bool ->
    command:command ->
    extra_nodes:Tezos_client_base.Client_context.full list ->
    Tezos_client_base.Client_context.full ->
    unit tzresult Lwt.t
end

module type AGENT = sig
  val name : string

  type command

  val run_command :
    (module Protocol_plugin_sig.S) ->
    Tezos_client_base.Client_context.full ->
    command ->
    unit tzresult Lwt.t

  val init_sapling_params : bool
end

type baker_command =
  | Run_with_local_node of {
      data_dir : string;
      args : Configuration.t;
      sources : Signature.public_key_hash trace;
    }
  | Run_remotely of {
      args : Configuration.t;
      sources : Signature.public_key_hash trace;
    }
  | Run_vdf of {pidfile : string option; keep_alive : bool}
  | Run_accuser of {
      pidfile : string option;
      preserved_levels : int;
      keep_alive : bool;
    }

module Baker_agent : AGENT with type command = baker_command = struct
  let name = "baker"

  type command = baker_command

  let run_command (module Plugin : Protocol_plugin_sig.S) cctxt command =
    match command with
    | Run_with_local_node {data_dir; args; sources} ->
        Command_run.run_baker (module Plugin) args (Some data_dir) sources cctxt
    | Run_remotely {args; sources} ->
        Command_run.run_baker (module Plugin) args None sources cctxt
    | Run_vdf {pidfile; keep_alive} ->
        Command_run.may_lock_pidfile pidfile @@ fun () ->
        Plugin.Baker_commands_helpers.run_vdf_daemon ~cctxt ~keep_alive
    | Run_accuser {pidfile; preserved_levels; keep_alive} ->
        Command_run.may_lock_pidfile pidfile @@ fun () ->
        Plugin.Accuser_commands_helpers.run ~cctxt ~preserved_levels ~keep_alive

  let init_sapling_params = true
end

type accuser_command =
  | Run_accuser of {
      pidfile : string option;
      preserved_levels : int;
      keep_alive : bool;
    }

module Accuser_agent : AGENT with type command = accuser_command = struct
  let name = "accuser"

  type command = accuser_command

  let run_command (module Plugin : Protocol_plugin_sig.S) cctxt command =
    match command with
    | Run_accuser {pidfile; preserved_levels; keep_alive} ->
        Command_run.may_lock_pidfile pidfile @@ fun () ->
        Plugin.Accuser_commands_helpers.run ~cctxt ~preserved_levels ~keep_alive

  let init_sapling_params = false
end

module Make_daemon (Agent : AGENT) :
  AGNOSTIC_DAEMON with type command = Agent.command = struct
  type process = {thread : unit tzresult Lwt.t; canceller : unit tzresult Lwt.u}

  type baker = {protocol_hash : Protocol_hash.t; process : process}

  type baker_to_kill = {baker : baker; level_to_kill : int}

  type command = Agent.command

  type state = {
    current_baker : baker;
    old_baker : baker_to_kill option;
    keep_alive : bool;
    command : command;
    cctxt : Tezos_client_base.Client_context.full;
    extra_nodes : Tezos_client_base.Client_context.full list;
    head_stream :
      (* we switch [head_stream] from [`Live] to [`Reconnecting] on
         disconnections, and from [`Reconnecting] to [`Live] on reconnections *)
      [ `Live of
        (Tezos_client_base.Client_context.full
        * (Block_hash.t * Block_header.t))
        Lwt_stream.t
      | `Reconnecting of
        (Tezos_client_base.Client_context.full
        * (Block_hash.t * Block_header.t))
        Lwt_stream.t
        tzresult
        Lwt.t ];
    last_seen_level : int32 option;
  }

  type t = state

  (* ---- Baker Process Management ---- *)

  let rec retry_on_disconnection node_addr f arg =
    let open Lwt_result_syntax in
    let*! result = f arg in
    match result with
    | Ok res -> return res
    | Error
        ( Tezos_rpc_http.RPC_client_errors.Request_failed _ :: _
        | Lost_node_connection :: _
        | Cannot_connect_to_node _ :: _ ) ->
        let* _level =
          Utils.retry
            ~emit:Events.(emit retry_on_disconnection)
            ~max_delay:10.
            ~delay:1.
            ~factor:1.5
            ~msg:(fun errs ->
              Format.asprintf
                "agnostic daemon failed to reach the node at %s.@.%a"
                node_addr
                pp_print_trace
                errs)
            ~is_error:(function
              | Tezos_rpc_http.RPC_client_errors.Request_failed _
              | Cannot_connect_to_node _ ->
                  true
              | _ -> false)
            (fun node_addr -> Rpc_services.get_level ~node_addr)
            node_addr
        in
        retry_on_disconnection node_addr f arg
    | Error trace -> fail trace

  (** [run_thread ~protocol_hash ~cancel_promise ~init_sapling_params cctxt
      command] returns the main running thread for the baker given its protocol
      [~protocol_hash] and cancellation [~cancel_promise]. It can optionally
      initialise sapling parameters, as requested by [~init_sapling_params]. *)
  let run_thread ~protocol_hash ~cancel_promise ~init_sapling_params cctxt
      command =
    let plugin =
      match
        Protocol_plugins.proto_plugin_for_protocol
          protocol_hash
        [@profiler.record_f {verbosity = Notice} "proto_plugin_for_protocol"]
      with
      | Error e ->
          Format.kasprintf
            Stdlib.failwith
            "Cannot start %s for protocol %a.\n%a"
            Agent.name
            Protocol_hash.pp
            protocol_hash
            pp_print_trace
            e
      | Ok plugin -> plugin
    in

    if init_sapling_params then
      (* This call is not strictly necessary as the parameters are initialized
         lazily the first time a Sapling operation (validation or forging) is
         done. This is what the client does.
         For a long running binary however it is important to make sure that the
         parameters files are there at the start and avoid failing much later while
         validating an operation. Plus paying this cost upfront means that the first
         validation will not be more expensive. *)
      Tezos_sapling.Core.Validator.init_params ()
    else () ;

    let agent_thread = Agent.run_command plugin cctxt command in
    Lwt.pick [agent_thread; cancel_promise]

  (** [spawn_baker cctxt command protocol_hash] spawns a new baker
      process for the given [protocol_hash]. *)
  let spawn_baker cctxt command protocol_hash =
    let open Lwt_result_syntax in
    let*! () = Events.(emit starting_agent) (Agent.name, protocol_hash) in
    let cancel_promise, canceller = Lwt.wait () in
    let thread =
      run_thread
        ~protocol_hash
        ~cancel_promise
        ~init_sapling_params:Agent.init_sapling_params
        cctxt
        command
    in
    let*! () = Events.(emit agent_running) (Agent.name, protocol_hash) in
    return {protocol_hash; process = {thread; canceller}}

  (** [hot_swap_baker ~state ~current_protocol_hash ~next_protocol_hash
    ~level_to_kill_old_baker] moves the current baker into the old baker slot
    (to be killed later) and spawns a new baker for [~next_protocol_hash] *)
  let hot_swap_baker ~state ~current_protocol_hash ~next_protocol_hash
      ~level_to_kill_old_baker =
    let open Lwt_result_syntax in
    let next_proto_status = Parameters.protocol_status next_protocol_hash in
    let*! () =
      Events.(emit protocol_encountered) (next_proto_status, next_protocol_hash)
    in
    let*! () =
      Events.(emit become_old_agent)
        (Agent.name, current_protocol_hash, level_to_kill_old_baker)
    in
    let old_baker =
      Some
        {baker = state.current_baker; level_to_kill = level_to_kill_old_baker}
    in
    let* new_baker = spawn_baker state.cctxt state.command next_protocol_hash in
    return {state with old_baker; current_baker = new_baker}

  (** [maybe_kill_old_baker state] checks whether the [old_baker] process
    from the [state] of the agnostic baker has surpassed its lifetime and it stops
    it if that is the case. *)
  let maybe_kill_old_baker state =
    let open Lwt_result_syntax in
    match state.old_baker with
    | None -> return state
    | Some {baker; level_to_kill} ->
        let node_addr = Uri.to_string state.cctxt#base in
        let* head_level =
          (Rpc_services.get_level
             ~node_addr [@profiler.record_s {verbosity = Notice} "get_level"])
        in
        if head_level >= level_to_kill then (
          let*! () =
            Events.(emit stopping_agent) (Agent.name, baker.protocol_hash)
          in
          Lwt.wakeup
            baker.process.canceller
            (Ok ()) [@profiler.record_f {verbosity = Notice} "kill old baker"] ;
          return {state with old_baker = None})
        else return state

  (* ---- Baker and Chain Monitoring ---- *)

  let monitor_heads cctxt =
    let open Lwt_result_syntax in
    let* stream, _stopper = Monitor_services.heads cctxt cctxt#chain in
    return stream

  (** [monitor_heads_with_cctxt cctxt] creates a head stream tagged with the
      source cctxt for RPC routing. *)
  let monitor_heads_with_cctxt cctxt =
    let open Lwt_result_syntax in
    let* stream = monitor_heads cctxt in
    return (Lwt_stream.map (fun head -> (cctxt, head)) stream)

  (** [merge_head_streams nodes] creates a merged stream
      from all node connections. Each stream element is tagged with its source
      cctxt for RPC routing. Automatically reconnects on stream closure. *)
  let merge_head_streams nodes =
    let open Lwt_syntax in
    let merged_stream, push_to_merged_stream = Lwt_stream.create () in
    List.iter
      (fun cctxt ->
        let node_uri = Uri.to_string cctxt#base in
        Lwt.async (fun () ->
            let rec monitor_stream stream =
              let* value = Lwt_stream.get stream in
              match value with
              | Some head ->
                  push_to_merged_stream (Some (cctxt, head)) ;
                  monitor_stream stream
              | None ->
                  (* Stream closed - retry connection *)
                  let* () = Events.(emit node_connection_lost) node_uri in
                  recreate_stream ()
            and recreate_stream () =
              let* () = Lwt_unix.sleep 5.0 in
              let* result = monitor_heads cctxt in
              match result with
              | Ok new_stream ->
                  let* () = Events.(emit node_connection_restored) node_uri in
                  monitor_stream new_stream
              | Error trace ->
                  let* () =
                    Events.(emit node_connection_retry_failed) (node_uri, trace)
                  in
                  recreate_stream ()
            in
            (* Start initial stream *)
            let* result = monitor_heads cctxt in
            match result with
            | Ok stream -> monitor_stream stream
            | Error trace ->
                let* () =
                  Events.(emit node_connection_retry_failed) (node_uri, trace)
                in
                recreate_stream ()))
      nodes ;
    return merged_stream

  (** [monitor_voting_periods ~state] continuously monitors chain data to detect
    protocol changes. It will:
    - Shut down an old baker it its time has come;
    - Spawn and "hot-swap" to a new baker if the next protocol hash is different.
    The voting period information is used for logging purposes. *)
  let monitor_voting_periods ~state =
    let open Lwt_result_syntax in
    let node_addr = Uri.to_string state.cctxt#base in
    let* block_hash =
      (Rpc_services.get_block_hash
         ~node_addr [@profiler.record_s {verbosity = Notice} "get_block_hash"])
    in
    () [@profiler.overwrite Profiler.reset_block_section (block_hash, [])] ;
    let* period_kind, remaining =
      (Rpc_services.get_current_period
         ~node_addr
       [@profiler.record_s {verbosity = Notice} "get_current_period"])
    in
    let*! () =
      Events.(emit period_status) (block_hash, period_kind, remaining)
    in
    let* state =
      (maybe_kill_old_baker
         state [@profiler.record_s {verbosity = Notice} "maybe_kill_old_baker"])
    in
    let* next_protocol_hash =
      (Rpc_services.get_next_protocol_hash
         ~node_addr
       [@profiler.record_s {verbosity = Notice} "get_next_protocol_hash"])
    in
    let current_protocol_hash = state.current_baker.protocol_hash in
    if not (Protocol_hash.equal current_protocol_hash next_protocol_hash) then
      let* head_level =
        (Rpc_services.get_level
           ~node_addr [@profiler.record_s {verbosity = Notice} "get_level"])
      in
      (hot_swap_baker
         ~state
         ~current_protocol_hash
         ~next_protocol_hash
         ~level_to_kill_old_baker:
           (head_level + Parameters.extra_levels_for_old_baker)
       [@profiler.record_s {verbosity = Notice} "hot_swap_baker"])
    else return state

  (* ---- Agnostic Baker Bootstrap ---- *)

  (** [may_start_initial_baker cctxt command ~node_addr] recursively waits
      for an [active] protocol and spawns a baker for it. If the protocol is
      [frozen] (not [active] anymore), it waits for a head with an [active]
      protocol. *)
  let may_start_initial_baker cctxt command ~node_addr =
    let open Lwt_result_syntax in
    let rec may_start ?last_known_proto ~head_stream () =
      let* protocol_hash = Rpc_services.get_next_protocol_hash ~node_addr in
      let proto_status = Parameters.protocol_status protocol_hash in
      let*! () =
        match last_known_proto with
        | None -> Lwt.return_unit
        | Some h ->
            if not (Protocol_hash.equal h protocol_hash) then
              Events.(emit protocol_encountered) (proto_status, protocol_hash)
            else Lwt.return_unit
      in
      match proto_status with
      | Active ->
          let* current_baker = spawn_baker cctxt command protocol_hash in
          return current_baker
      | Frozen -> (
          let* head_stream =
            match head_stream with
            | Some v -> return v
            | None ->
                let*! () =
                  Events.(emit protocol_encountered)
                    (proto_status, protocol_hash)
                in
                let*! () = Events.(emit waiting_for_active_protocol) () in
                monitor_heads cctxt
          in
          let*! v = Lwt_stream.get head_stream in
          match v with
          | Some _tick ->
              may_start
                ~last_known_proto:protocol_hash
                ~head_stream:(Some head_stream)
                ()
          | None -> tzfail Lost_node_connection)
    in
    may_start ~head_stream:None ()

  (* Main entrypoint for the agnostic baker binary.

     We distinguish two cases:
     1. If the binary is called against a `--help`, `--version` or `man` command, then
     there is no reason to connect to a node, find the current protocol etc.
     2. Otherwise, we run the agnostic baker daemon, which first obtains the
     current protocol from the connected node, and then it monitors the chain
     to determine when to switch to a new protocol baker process. *)

  let[@warning "-32"] may_start_profiler baking_dir =
    let profiling_config =
      Tezos_profiler_unix.Profiler_instance.read_profiling_config_from_base_dir
        baking_dir
    in
    match
      Tezos_profiler_unix.Profiler_instance.selected_backends ~profiling_config
    with
    | Some backends ->
        List.iter
          (fun Tezos_profiler_unix.Profiler_instance.{instance_maker; _} ->
            let profiler_maker = instance_maker ~directory:baking_dir in
            Agnostic_baker_profiler.init ~profiling_config profiler_maker)
          backends
    | None -> ()

  type event =
    | New_head of {
        source_cctxt : Tezos_client_base.Client_context.full;
        head : Block_hash.t * Block_header.t;
      }
    | Head_stream_ended
    | Head_stream_reconnected of
        (Tezos_client_base.Client_context.full
        * (Block_hash.t * Block_header.t))
        Lwt_stream.t
    | Old_baker_stopped
    | Current_baker_stopped

  let main_iteration state =
    let open Lwt_result_syntax in
    let current_baker =
      Lwt_result.map
        (fun () -> Current_baker_stopped)
        state.current_baker.process.thread
    in
    let old_baker =
      match state.old_baker with
      | Some old_baker ->
          Lwt_result.map
            (fun () -> Old_baker_stopped)
            old_baker.baker.process.thread
      | None ->
          (* If there is no [old_baker], this promise is not expected to resolve
           anytime. *)
          Lwt_utils.never_ending ()
    in
    let head_stream =
      match state.head_stream with
      | `Live stream ->
          Lwt.map
            (function
              | None -> Ok Head_stream_ended
              | Some (source_cctxt, head) -> Ok (New_head {source_cctxt; head}))
            (Lwt_stream.get stream)
      | `Reconnecting promise ->
          Lwt_result.map (fun stream -> Head_stream_reconnected stream) promise
    in
    let* pick = Lwt.choose [current_baker; old_baker; head_stream] in
    match pick with
    | New_head {source_cctxt; head = block_hash, block_header} ->
        let*! () =
          Events.(emit monitoring_head_from_node)
            ( Int32.to_int block_header.shell.level,
              Uri.to_string source_cctxt#base,
              block_hash )
        in
        let current_level = block_header.shell.level in
        let should_process_block =
          match state.last_seen_level with
          | None -> true
          | Some last_level -> Int32.compare current_level last_level > 0
        in
        if should_process_block then
          (* Update state.cctxt to the source of this head *)
          let state =
            {
              state with
              cctxt = source_cctxt;
              last_seen_level = Some current_level;
            }
          in
          let* state = monitor_voting_periods ~state in
          return (Some state)
        else
          let*! () =
            Events.(emit skipping_lower_or_equal_level)
              ( Int32.to_int current_level,
                Uri.to_string source_cctxt#base,
                block_hash )
          in
          return (Some state)
    | Head_stream_ended ->
        let*! () =
          Events.(emit head_stream_ended) (Uri.to_string state.cctxt#base)
        in
        let reconnect_promise =
          retry_on_disconnection
            (Uri.to_string state.cctxt#base)
            monitor_heads_with_cctxt
            state.cctxt
        in
        return (Some {state with head_stream = `Reconnecting reconnect_promise})
    | Head_stream_reconnected stream ->
        let*! () =
          Events.(emit head_stream_reconnected) (Uri.to_string state.cctxt#base)
        in
        return (Some {state with head_stream = `Live stream})
    | Old_baker_stopped -> (
        match state.old_baker with
        | None -> assert false
        | Some old_baker ->
            let*! () =
              Events.(emit old_baker_stopped old_baker.baker.protocol_hash)
            in
            let* head_level =
              (Rpc_services.get_level
                 ~node_addr:(Uri.to_string state.cctxt#base)
               [@profiler.record_s {verbosity = Notice} "get_level"])
            in
            if head_level >= old_baker.level_to_kill then
              (* The old baker is expired, it is expected. *)
              return (Some {state with old_baker = None})
            else failwith "Old baker was killed unexpectedly")
    | Current_baker_stopped ->
        (* It's not supposed to stop. *)
        failwith "Current baker stopped unexpectedly"

  let rec main_loop state =
    let open Lwt_result_syntax in
    let* result = main_iteration state in
    match result with
    | None -> return_unit
    | Some next_state -> main_loop next_state

  let run ~keep_alive ~command ~extra_nodes cctxt =
    let open Lwt_result_syntax in
    () [@profiler.overwrite may_start_profiler cctxt#get_base_dir] ;
    let node_addr = Uri.to_string cctxt#base in
    let*! () = Events.(emit starting_daemon) Agent.name in
    let _ccid =
      Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
          let*! () = Events.(emit stopping_daemon) Agent.name in
          Lwt.return_unit)
    in
    let* current_baker =
      if keep_alive then
        retry_on_disconnection
          node_addr
          (fun () -> may_start_initial_baker cctxt command ~node_addr)
          ()
      else may_start_initial_baker cctxt command ~node_addr
    in
    let* head_stream =
      if extra_nodes = [] then
        (* Useful if the baker is started before the node. *)
        retry_on_disconnection node_addr monitor_heads_with_cctxt cctxt
      else
        let*! stream = merge_head_streams (cctxt :: extra_nodes) in
        return stream
    in
    let state =
      {
        current_baker;
        old_baker = None;
        keep_alive;
        command;
        cctxt;
        extra_nodes;
        head_stream = `Live head_stream;
        last_seen_level = None;
      }
    in
    main_loop state
end

module Baker : AGNOSTIC_DAEMON with type command = Baker_agent.command =
  Make_daemon (Baker_agent)

module Accuser : AGNOSTIC_DAEMON with type command = Accuser_agent.command =
  Make_daemon (Accuser_agent)
