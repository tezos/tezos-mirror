(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Profiler = struct
  include (val Profiler.wrap Agnostic_baker_profiler.agnostic_baker_profiler)

  let[@warning "-32"] reset_block_section =
    Agnostic_baker_profiler.create_reset_block_section
      Agnostic_baker_profiler.agnostic_baker_profiler
end

open Agnostic_baker_errors

(* Number of extra levels to keep the old baker alive before shutting it down.
   This extra time is used to avoid halting the chain in cases such as
   reorganization or high round migration blocks. *)
let extra_levels_for_old_baker = 3

type process = {thread : int Lwt.t; canceller : int Lwt.u}

type baker = {protocol_hash : Protocol_hash.t; process : process}

(** [run_thread ~protocol_hash ~baker_commands ~cancel_promise ~logs_path]
    returns the main running thread for the baker given its protocol [~procol_hash],
    corresponding commands [~baker_commands] and Lwt cancellation promise [~cancel_promise].

    The event logs are stored according to [~logs_path]. *)
let run_thread ~protocol_hash ~baker_commands ~cancel_promise ~logs_path =
  let () =
    Client_commands.register protocol_hash @@ fun _network -> baker_commands
  in

  let select_commands _ _ = Lwt_result_syntax.return baker_commands in

  (* This call is not strictly necessary as the parameters are initialized
     lazily the first time a Sapling operation (validation or forging) is
     done. This is what the client does.
     For a long running binary however it is important to make sure that the
     parameters files are there at the start and avoid failing much later while
     validating an operation. Plus paying this cost upfront means that the first
     validation will not be more expensive. *)
  let () = Tezos_sapling.Core.Validator.init_params () in

  let module Config = struct
    include Daemon_config

    let default_daily_logs_path = logs_path
  end in
  Lwt.pick
    [
      Client_main_run.lwt_run
        (module Config)
        ~select_commands
          (* The underlying logging from the baker must not be initialised, otherwise we double log. *)
        ~disable_logging:true
        ();
      cancel_promise;
    ]

(** [spawn_baker protocol_hash] spawns a new baker process for the given [protocol_hash]. *)
let spawn_baker protocol_hash =
  let open Lwt_result_syntax in
  let*! () = Agnostic_baker_events.(emit starting_baker) protocol_hash in
  let cancel_promise, canceller = Lwt.wait () in
  let* thread =
    let*? plugin =
      (Protocol_plugins.proto_plugin_for_protocol
         protocol_hash
       [@profiler.record_f {verbosity = Notice} "proto_plugin_for_protocol"])
    in
    let baker_commands = Commands.baker_commands ~plugin () in
    return
    @@ run_thread
         ~protocol_hash
         ~baker_commands
         ~cancel_promise
         ~logs_path:Parameters.default_daily_logs_path
  in
  let*! () = Agnostic_baker_events.(emit baker_running) protocol_hash in
  return {protocol_hash; process = {thread; canceller}}

type baker_to_kill = {baker : baker; level_to_kill : int}

type 'a state = {
  node_endpoint : string;
  mutable current_baker : baker option;
  mutable old_baker : baker_to_kill option;
}

type 'a t = 'a state

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

(** [hot_swap_baker ~state ~current_protocol_hash ~next_protocol_hash 
    ~level_to_kill_old_baker] moves the current baker into the old baker slot
    (to be killed later) and spawns a new baker for [~next_protocol_hash] *)
let hot_swap_baker ~state ~current_protocol_hash ~next_protocol_hash
    ~level_to_kill_old_baker =
  let open Lwt_result_syntax in
  let* current_baker =
    match state.current_baker with
    | Some baker -> return baker
    | None -> tzfail Missing_current_baker
  in
  let next_proto_status = Parameters.protocol_status next_protocol_hash in
  let*! () =
    Agnostic_baker_events.(emit protocol_encountered)
      (next_proto_status, next_protocol_hash)
  in
  let*! () =
    Agnostic_baker_events.(emit become_old_baker)
      (current_protocol_hash, level_to_kill_old_baker)
  in
  state.old_baker <-
    Some {baker = current_baker; level_to_kill = level_to_kill_old_baker} ;
  state.current_baker <- None ;
  let* new_baker = spawn_baker next_protocol_hash in
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
          Agnostic_baker_events.(emit stopping_baker) baker.protocol_hash
        in
        Lwt.wakeup
          baker.process.canceller
          0 [@profiler.record_f {verbosity = Notice} "kill old baker"] ;
        state.old_baker <- None ;
        return_unit)
      else return_unit

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
        [@profiler.reset_block_section {profiler_module = Profiler} block_hash] ;
        let* period_kind, remaining =
          (Rpc_services.get_current_period
             ~node_addr
           [@profiler.record_s {verbosity = Notice} "get_current_period"])
        in
        let*! () =
          Agnostic_baker_events.(emit period_status)
            (block_hash, period_kind, remaining)
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
          | None -> tzfail Missing_current_baker
          | Some baker -> return baker.protocol_hash
        in
        let* () =
          if not (Protocol_hash.equal current_protocol_hash next_protocol_hash)
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
               ~level_to_kill_old_baker:(head_level + extra_levels_for_old_baker)
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
  let*! retcode =
    match state.current_baker with
    | Some baker -> baker.process.thread
    | None -> Lwt.return 0
  in
  if retcode = 0 then return_unit else tzfail Baker_process_error

(** [may_start_initial_baker state] recursively waits for an [active] protocol
    and spawns a baker for it. If the protocol is [frozen] (not [active] anymore), it
    waits for a head with an [active] protocol. *)
let may_start_initial_baker state =
  let open Lwt_result_syntax in
  let*! () = Agnostic_baker_events.(emit experimental_binary) () in
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
            Agnostic_baker_events.(emit protocol_encountered)
              (proto_status, protocol_hash)
          else Lwt.return_unit
    in
    match proto_status with
    | Active ->
        let* current_baker = spawn_baker protocol_hash in
        state.current_baker <- Some current_baker ;
        return_unit
    | Frozen -> (
        let* head_stream =
          match head_stream with
          | Some v -> return v
          | None ->
              let*! () =
                Agnostic_baker_events.(emit protocol_encountered)
                  (proto_status, protocol_hash)
              in
              let*! () =
                Agnostic_baker_events.(emit waiting_for_active_protocol) ()
              in
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

let create ~node_endpoint =
  {node_endpoint; current_baker = None; old_baker = None}

let run state =
  let open Lwt_result_syntax in
  let node_addr = state.node_endpoint in
  let*! () = Agnostic_baker_events.(emit starting_daemon) () in
  let _ccid =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        let*! () = Agnostic_baker_events.(emit stopping_daemon) () in
        Lwt.return_unit)
  in
  let* () = may_start_initial_baker state in
  let* head_stream = monitor_heads ~node_addr in
  (* Monitoring voting periods through heads monitoring to avoid
     missing UAUs. *)
  Lwt.pick [monitor_voting_periods ~state head_stream; baker_thread ~state]
