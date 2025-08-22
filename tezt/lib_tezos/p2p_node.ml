(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let default_net_port = 19732

let default_net_addr =
  Constant.default_host ^ ":" ^ string_of_int default_net_port

let default_rpc_port = 18732

let default_rpc_addr =
  Constant.default_host ^ ":" ^ string_of_int default_rpc_port

module Parameters = struct
  type persistent_state = {
    net_addr : string;
    mutable net_port : int;
    peers : string list option;
    ping_interval : float option;
    discovery_addr : string option;
    rpc_host : string;
    rpc_port : int;
    mutable pending_ready : unit option Lwt.u list;
    runner : Runner.t option;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "p2p-node"

  let default_colors = Log.Color.[|FG.green|]
end

open Parameters
include Daemon.Make (Parameters)

let net_addr p2p_node = p2p_node.persistent_state.net_addr

let net_port p2p_node = p2p_node.persistent_state.net_port

let trigger_ready p2p_node value =
  let pending = p2p_node.persistent_state.pending_ready in
  p2p_node.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready p2p_node =
  (match p2p_node.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready p2p_node (Some ())

let handle_event p2p_node ({name; _} : event) =
  match name with "starting_p2p_node.v0" -> set_ready p2p_node | _ -> ()

let create ?(path = Uses.path Constant.octez_p2p_node) ?name ?color ?event_pipe
    ?runner ?peers ?ping_interval ?discovery_addr
    ?(net_addr = Constant.default_host) ?net_port
    ?(rpc_host = Constant.default_host) ?rpc_port () =
  let net_port = Option.value ~default:(Port.fresh ()) net_port in
  let rpc_port = Option.value ~default:(Port.fresh ()) rpc_port in
  let p2p_node =
    create
      ~path
      ?name
      ?color
      ?event_pipe
      ?runner
      {
        runner;
        pending_ready = [];
        net_addr;
        net_port;
        peers;
        ping_interval;
        discovery_addr;
        rpc_host;
        rpc_port;
      }
  in
  on_event p2p_node (handle_event p2p_node) ;
  p2p_node

let run_args p2p_node =
  let net_addr =
    p2p_node.persistent_state.net_addr ^ ":"
    ^ string_of_int p2p_node.persistent_state.net_port
  in
  let peers =
    Cli_arg.optional_arg
      "peers"
      (String.concat ",")
      p2p_node.persistent_state.peers
  in
  let ping_interval =
    Cli_arg.optional_arg
      "ping-interval"
      string_of_float
      p2p_node.persistent_state.ping_interval
  in
  let discovery_addr =
    Cli_arg.optional_arg
      "discovery-addr"
      Fun.id
      p2p_node.persistent_state.discovery_addr
  in
  let rpc_addr =
    p2p_node.persistent_state.rpc_host ^ ":"
    ^ string_of_int p2p_node.persistent_state.rpc_port
  in
  ["run"; "--net-addr"; net_addr; "--rpc-addr"; rpc_addr]
  @ peers @ ping_interval @ discovery_addr

let check_event ?where p2p_node name promise =
  let* result = promise in
  match result with
  | None ->
      raise
        (Terminated_before_event {daemon = p2p_node.name; event = name; where})
  | Some x -> return x

let wait_for_ready p2p_node =
  match p2p_node.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      p2p_node.persistent_state.pending_ready <-
        resolver :: p2p_node.persistent_state.pending_ready ;
      check_event p2p_node "start_p2p_node.v0" promise

let run ?env ?event_level p2p_node =
  (match p2p_node.status with
  | Not_running -> ()
  | Running _ -> Test.fail "p2p_node %s is already running" p2p_node.name) ;
  let on_terminate _ =
    (* Cancel all [Ready] event listeners. *)
    trigger_ready p2p_node None ;
    unit
  in
  run
    ?env
    ?event_level
    p2p_node
    {ready = false}
    (run_args p2p_node)
    ~on_terminate
    ?runner:p2p_node.persistent_state.runner

let init ?path ?name ?color ?event_pipe ?event_level ?runner ?peers
    ?ping_interval ?discovery_addr ?net_addr ?net_port ?rpc_host ?rpc_port () =
  let p2p_node =
    create
      ?path
      ?name
      ?color
      ?event_pipe
      ?runner
      ?peers
      ?ping_interval
      ?discovery_addr
      ?net_addr
      ?net_port
      ?rpc_host
      ?rpc_port
      ()
  in
  let* () = run ?event_level p2p_node in
  let* () = wait_for_ready p2p_node in
  return p2p_node

let send_raw_data p2p_node ~data =
  (* Extracted from Lwt_utils_unix. *)
  let write_string ?(pos = 0) ?len descr buf =
    let len = match len with None -> String.length buf - pos | Some l -> l in
    let rec inner pos len =
      if len = 0 then Lwt.return_unit
      else
        Lwt.bind (Lwt_unix.write_string descr buf pos len) (function
          | 0 ->
              Lwt.fail End_of_file
              (* other endpoint cleanly closed its connection *)
          | nb_written -> inner (pos + nb_written) (len - nb_written))
    in
    inner pos len
  in
  Log.debug "Write raw data to P2P node %s" p2p_node.name ;
  let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec socket ;
  let uaddr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, net_port p2p_node) in
  let* () = Lwt_unix.connect socket uaddr in
  write_string socket data

let as_rpc_endpoint ?(local = false) (t : t) =
  let state = t.persistent_state in
  let scheme = "http" in
  let host =
    if local || Option.is_none t.persistent_state.runner then state.rpc_host
    else Runner.address t.persistent_state.runner
  in
  Endpoint.make ~scheme ~host ~port:state.rpc_port ()
