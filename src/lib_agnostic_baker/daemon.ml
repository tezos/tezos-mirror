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
        let baking_mode = Some data_dir in
        Plugin.Baker_commands_helpers.run_baker
          ~configuration:args
          ~baking_mode
          ~sources
          ~cctxt
    | Run_remotely {args; sources} ->
        let baking_mode = None in
        Plugin.Baker_commands_helpers.run_baker
          ~configuration:args
          ~baking_mode
          ~sources
          ~cctxt
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
    node_endpoint : string;
    mutable current_baker : baker option;
    mutable old_baker : baker_to_kill option;
    keep_alive : bool;
    command : command;
    cctxt : Tezos_client_base.Client_context.full;
  }

  type t = state

  (* ---- Baker Process Management ---- *)

  let rec retry_on_disconnection ~emit node_addr f =
    let open Lwt_result_syntax in
    let*! result = f () in
    match result with
    | Ok () -> return_unit
    | Error (Lost_node_connection :: _ | Cannot_connect_to_node _ :: _) ->
        let* _level =
          Utils.retry
            ~emit
            ~max_delay:10.
            ~delay:1.
            ~factor:1.5
            ~tries:max_int
            ~is_error:(function Cannot_connect_to_node _ -> true | _ -> false)
            (fun node_addr -> Rpc_services.get_level ~node_addr)
            node_addr
        in
        retry_on_disconnection ~emit node_addr f
    | Error trace -> fail trace

  (** [run_thread ~protocol_hash ~cancel_promise ~init_sapling_params state] returns the
      main running thread for the baker given its protocol [~protocol_hash] and
      cancellation [~cancel_promise]. It can optionally initialise sapling parameters,
      as requested by [~init_sapling_params]. *)
  let run_thread ~protocol_hash ~cancel_promise ~init_sapling_params state =
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

    let agent_thread = Agent.run_command plugin state.cctxt state.command in
    Lwt.pick [agent_thread; cancel_promise]

  (** [spawn_baker protocol_hash] spawns a new baker process for the given [protocol_hash]. *)
  let spawn_baker state protocol_hash =
    let open Lwt_result_syntax in
    let*! () = Events.(emit starting_agent) (Agent.name, protocol_hash) in
    let cancel_promise, canceller = Lwt.wait () in
    let thread =
      run_thread
        ~protocol_hash
        ~cancel_promise
        ~init_sapling_params:Agent.init_sapling_params
        state
    in
    let*! () = Events.(emit agent_running) (Agent.name, protocol_hash) in
    return {protocol_hash; process = {thread; canceller}}

  (** [hot_swap_baker ~state ~current_protocol_hash ~next_protocol_hash
    ~level_to_kill_old_baker] moves the current baker into the old baker slot
    (to be killed later) and spawns a new baker for [~next_protocol_hash] *)
  let hot_swap_baker ~state ~current_protocol_hash ~next_protocol_hash
      ~level_to_kill_old_baker =
    let open Lwt_result_syntax in
    let* current_baker =
      match state.current_baker with
      | Some baker -> return baker
      | None -> tzfail (Missing_current_agent Agent.name)
    in
    let next_proto_status = Parameters.protocol_status next_protocol_hash in
    let*! () =
      Events.(emit protocol_encountered) (next_proto_status, next_protocol_hash)
    in
    let*! () =
      Events.(emit become_old_agent)
        (Agent.name, current_protocol_hash, level_to_kill_old_baker)
    in
    state.old_baker <-
      Some {baker = current_baker; level_to_kill = level_to_kill_old_baker} ;
    state.current_baker <- None ;
    let* new_baker = spawn_baker state next_protocol_hash in
    state.current_baker <- Some new_baker ;
    return_unit

  (** [maybe_kill_old_baker state node_addr] checks whether the [old_baker] process
    from the [state] of the agnostic baker has surpassed its lifetime and it stops
    it if that is the case. *)
  let maybe_kill_old_baker state node_addr =
    let open Lwt_result_syntax in
    match state.old_baker with
    | None -> return_unit
    | Some {baker; level_to_kill} ->
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
          state.old_baker <- None ;
          return_unit)
        else return_unit

  (* ---- Baker and Chain Monitoring ---- *)

  (** [monitor_heads ~node_addr] creates a stream which returns the data
    of the heads of the current network; this information is received
    from the RPC calls at the endpoint given by [~node_addr]. *)
  let monitor_heads ~node_addr =
    let open Lwt_result_syntax in
    let uri = Format.sprintf "%s/monitor/heads/main" node_addr in
    let* _, body = Rpc_services.request_uri ~node_addr ~uri in
    let cohttp_stream = Cohttp_lwt.Body.to_stream body in
    let buffer = Buffer.create 2048 in
    let stream, push = Lwt_stream.create () in
    let on_chunk v = push (Some v) and on_close () = push None in
    let rec loop () =
      let*! v = Lwt_stream.get cohttp_stream in
      match v with
      | None ->
          on_close () ;
          Lwt.return_unit
      | Some chunk ->
          Buffer.add_string buffer chunk ;
          let data = Buffer.contents buffer in
          Buffer.reset buffer ;
          on_chunk data ;
          loop ()
    in
    ignore (loop () : unit Lwt.t) ;
    return stream

  (** [monitor_voting_periods ~state head_stream] continuously monitors [heads_stream]
    to detect protocol changes. It will:
    - Shut down an old baker it its time has come;
    - Spawn and "hot-swap" to a new baker if the next protocol hash is different.
    The voting period information is used for logging purposes. *)
  let monitor_voting_periods ~state head_stream =
    let open Lwt_result_syntax in
    let node_addr = state.node_endpoint in
    let rec loop () =
      let*! v = Lwt_stream.get head_stream in
      match v with
      | None -> tzfail Lost_node_connection
      | Some _tick ->
          let* block_hash =
            (Rpc_services.get_block_hash
               ~node_addr
             [@profiler.record_s {verbosity = Notice} "get_block_hash"])
          in
          ()
          [@profiler.reset_block_section
            {profiler_module = Profiler} block_hash] ;
          let* period_kind, remaining =
            (Rpc_services.get_current_period
               ~node_addr
             [@profiler.record_s {verbosity = Notice} "get_current_period"])
          in
          let*! () =
            Events.(emit period_status) (block_hash, period_kind, remaining)
          in
          let* () =
            (maybe_kill_old_baker
               state
               node_addr
             [@profiler.record_s {verbosity = Notice} "maybe_kill_old_baker"])
          in
          let* next_protocol_hash =
            (Rpc_services.get_next_protocol_hash
               ~node_addr
             [@profiler.record_s {verbosity = Notice} "get_next_protocol_hash"])
          in
          let* current_protocol_hash =
            match state.current_baker with
            | None -> tzfail (Missing_current_agent Agent.name)
            | Some baker -> return baker.protocol_hash
          in
          let* () =
            if
              not (Protocol_hash.equal current_protocol_hash next_protocol_hash)
            then
              let* head_level =
                (Rpc_services.get_level
                   ~node_addr
                 [@profiler.record_s {verbosity = Notice} "get_level"])
              in
              (hot_swap_baker
                 ~state
                 ~current_protocol_hash
                 ~next_protocol_hash
                 ~level_to_kill_old_baker:
                   (head_level + Parameters.extra_levels_for_old_baker)
               [@profiler.record_s {verbosity = Notice} "hot_swap_baker"])
            else return_unit
          in
          loop ()
    in
    loop ()

  (** [baker_thread ~state] monitors the current baker thread for any potential error, and
    it propagates any error that can appear. *)
  let baker_thread ~state =
    let open Lwt_result_syntax in
    let*! res =
      match state.current_baker with
      | Some baker -> baker.process.thread
      | None -> return_unit
    in
    match res with
    | Ok () -> return_unit
    | Error err -> fail (Agent_process_error Agent.name :: err)

  (* ---- Agnostic Baker Bootstrap ---- *)

  (** [may_start_initial_baker state] recursively waits for an [active] protocol
    and spawns a baker for it. If the protocol is [frozen] (not [active] anymore), it
    waits for a head with an [active] protocol. *)
  let may_start_initial_baker state =
    let open Lwt_result_syntax in
    let rec may_start ?last_known_proto ~head_stream () =
      let* protocol_hash =
        Rpc_services.get_next_protocol_hash ~node_addr:state.node_endpoint
      in
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
          let* current_baker = spawn_baker state protocol_hash in
          state.current_baker <- Some current_baker ;
          return_unit
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
                monitor_heads ~node_addr:state.node_endpoint
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
    match Tezos_profiler_unix.Profiler_instance.selected_backend () with
    | Some {instance_maker; _} ->
        let profiler_maker = instance_maker ~directory:baking_dir in
        Agnostic_baker_profiler.init profiler_maker
    | None -> ()

  let run ~keep_alive ~command cctxt =
    let open Lwt_result_syntax in
    let state =
      {
        node_endpoint = Uri.to_string cctxt#base;
        current_baker = None;
        old_baker = None;
        keep_alive;
        command;
        cctxt;
      }
    in
    () [@profiler.overwrite may_start_profiler cctxt#get_base_dir] ;
    let node_addr = state.node_endpoint in
    let*! () = Events.(emit starting_daemon) Agent.name in
    let _ccid =
      Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
          let*! () = Events.(emit stopping_daemon) Agent.name in
          Lwt.return_unit)
    in
    let* () =
      if state.keep_alive then
        retry_on_disconnection
          ~emit:(Events.emit Events.cannot_connect)
          node_addr
          (fun () -> may_start_initial_baker state)
      else may_start_initial_baker state
    in
    let monitor_voting_periods () =
      let* head_stream = monitor_heads ~node_addr in
      monitor_voting_periods ~state head_stream
    in
    (* Monitoring voting periods through heads monitoring to avoid
       missing UAUs. *)
    let* () =
      Lwt.pick
        [
          (* We do not care if --keep-alive is provided, if the baker thread doesn't
             have the argument it'll abort the process anyway. *)
          retry_on_disconnection
            ~emit:(fun _ -> Lwt.return_unit)
            node_addr
            monitor_voting_periods;
          baker_thread ~state;
        ]
    in
    let*! () = Lwt_utils.never_ending () in
    return_unit
end

module Baker : AGNOSTIC_DAEMON with type command = Baker_agent.command =
  Make_daemon (Baker_agent)

module Accuser : AGNOSTIC_DAEMON with type command = Accuser_agent.command =
  Make_daemon (Accuser_agent)
