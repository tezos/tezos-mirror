(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

module Parameters = struct
  type legacy_mode_settings = {
    threshold : int;
    committee_members : string list;
    committee_member_address_opt : string option;
  }

  type coordinator_mode_settings = {committee_members : string list}

  type committee_member_mode_settings = {
    address : string;
    coordinator_rpc_host : string;
    coordinator_rpc_port : int;
  }

  type observer_mode_settings = {
    coordinator_rpc_host : string;
    coordinator_rpc_port : int;
  }

  type mode_settings =
    | Legacy of legacy_mode_settings
    | Coordinator of coordinator_mode_settings
    | Committee_member of committee_member_mode_settings
    | Observer of observer_mode_settings

  type persistent_state = {
    data_dir : string;
    reveal_data_dir : string;
    rpc_host : string;
    rpc_port : int;
    mode : mode_settings;
    node : Node.t;
    client : Client.t;
    mutable pending_ready : unit option Lwt.u list;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "octez-dac-node"

  let default_colors = Log.Color.[|FG.gray; FG.magenta; FG.yellow; FG.green|]
end

open Parameters
include Daemon.Make (Parameters)

let wait dac_node =
  match dac_node.status with
  | Not_running ->
      Test.fail
        "DAC node %s is not running, cannot wait for it to terminate"
        (name dac_node)
  | Running {process; _} -> Process.wait process

let is_running_not_ready dac_node =
  match dac_node.status with
  | Running {session_state = {ready}; _} when not ready -> true
  | _ -> false

let name dac_node = dac_node.name

let mode dac_node =
  match dac_node.persistent_state.mode with
  | Legacy _ -> "Legacy"
  | Coordinator _ -> "Coordinator"
  | Committee_member _ -> "Committee_member"
  | Observer _ -> "Observer"

let rpc_host dac_node = dac_node.persistent_state.rpc_host

let rpc_port dac_node = dac_node.persistent_state.rpc_port

let layer1_addr dac_node = Node.rpc_host dac_node.persistent_state.node

let layer1_port dac_node = Node.rpc_port dac_node.persistent_state.node

let endpoint dac_node =
  Printf.sprintf "http://%s:%d" (rpc_host dac_node) (rpc_port dac_node)

let data_dir dac_node = dac_node.persistent_state.data_dir

let reveal_data_dir dac_node = dac_node.persistent_state.reveal_data_dir

let spawn_command dac_node =
  Process.spawn ~name:dac_node.name ~color:dac_node.color dac_node.path

let spawn_config_init dac_node =
  let arg_command =
    [
      "--data-dir";
      data_dir dac_node;
      "--rpc-port";
      string_of_int (rpc_port dac_node);
      "--rpc-addr";
      rpc_host dac_node;
      "--reveal-data-dir";
      reveal_data_dir dac_node;
    ]
  in
  let mode_command =
    match dac_node.persistent_state.mode with
    | Legacy legacy_params -> (
        [
          "configure";
          "as";
          "legacy";
          "with";
          "data";
          "availability";
          "committee";
          "members";
        ]
        @ legacy_params.committee_members
        @ ["and"; "threshold"; Int.to_string legacy_params.threshold]
        @
        match legacy_params.committee_member_address_opt with
        | None -> []
        | Some committee_member_address ->
            ["--committee-member-address"; committee_member_address])
    | Coordinator coordinator_params ->
        [
          "configure";
          "as";
          "coordinator";
          "with";
          "data";
          "availability";
          "committee";
          "members";
        ]
        @ coordinator_params.committee_members
    | Committee_member committee_member_params ->
        let coordinator_host =
          committee_member_params.coordinator_rpc_host ^ ":"
          ^ Int.to_string committee_member_params.coordinator_rpc_port
        in
        [
          "configure";
          "as";
          "committee";
          "member";
          "with";
          "coordinator";
          coordinator_host;
          "and";
          "signer";
          committee_member_params.address;
        ]
    | Observer observer_params ->
        let coordinator_host =
          observer_params.coordinator_rpc_host ^ ":"
          ^ Int.to_string observer_params.coordinator_rpc_port
        in
        ["configure"; "as"; "observer"; "with"; "coordinator"; coordinator_host]
  in
  spawn_command dac_node (mode_command @ arg_command)

let ls = "ls"

let ls_reveal_data_dir dac_node =
  let reveal_data_dir = dac_node.persistent_state.reveal_data_dir in
  let commands = [reveal_data_dir] in
  let process =
    Process.spawn ~name:dac_node.name ~color:dac_node.color ls commands
  in
  let* filenames = Process.check_and_read_stdout process in
  return @@ String.split_on_char '\n' filenames

let init_config dac_node =
  let process = spawn_config_init dac_node in
  let* output = Process.check_and_read_stdout process in
  match output =~* rex "DAC node configuration written in ([^\n]*)" with
  | None -> failwith "DAC node configuration initialization failed"
  | Some filename -> return filename

module Config_file = struct
  let filename dac_node = sf "%s/config.json" @@ data_dir dac_node

  let read dac_node = JSON.parse_file (filename dac_node)

  let write dac_node config = JSON.encode_to_file (filename dac_node) config

  let update dac_node update = read dac_node |> update |> write dac_node
end

let check_event ?timeout ?where dac_node name promise =
  let* result =
    match timeout with
    | None -> promise
    | Some timeout ->
        Lwt.pick
          [
            promise;
            (let* () = Lwt_unix.sleep timeout in
             Lwt.return None);
          ]
  in
  match result with
  | None ->
      raise
        (Terminated_before_event {daemon = dac_node.name; event = name; where})
  | Some x -> return x

let trigger_ready dac_node value =
  let pending = dac_node.persistent_state.pending_ready in
  dac_node.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready dac_node =
  (match dac_node.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready dac_node (Some ())

let wait_for_ready dac_node =
  match dac_node.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      dac_node.persistent_state.pending_ready <-
        resolver :: dac_node.persistent_state.pending_ready ;
      check_event dac_node "dac_node_is_ready.v0" promise

let handle_event dac_node {name; value = _; timestamp = _} =
  match name with "dac_node_is_ready.v0" -> set_ready dac_node | _ -> ()

let create ?(path = Constant.dac_node) ?name ?color ?data_dir ?event_pipe
    ?(rpc_host = "127.0.0.1") ?rpc_port ?reveal_data_dir ~mode ~node ~client ()
    =
  let name = match name with None -> fresh_name () | Some name -> name in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let reveal_data_dir =
    match reveal_data_dir with
    | None -> Temp.dir (name ^ "preimages")
    | Some dir -> dir
  in
  let rpc_port =
    match rpc_port with None -> Port.fresh () | Some port -> port
  in
  let dac_node =
    create
      ~path
      ~name
      ?color
      ?event_pipe
      {
        data_dir;
        reveal_data_dir;
        rpc_host;
        rpc_port;
        mode;
        pending_ready = [];
        node;
        client;
      }
  in
  on_event dac_node (handle_event dac_node) ;
  dac_node

let create_legacy ?(path = Constant.dac_node) ?name ?color ?data_dir ?event_pipe
    ?(rpc_host = "127.0.0.1") ?rpc_port ?reveal_data_dir ~threshold
    ~committee_members ?committee_member_address ~node ~client () =
  let mode =
    Legacy
      {
        threshold;
        committee_members;
        committee_member_address_opt = committee_member_address;
      }
  in
  create
    ~path
    ?name
    ?color
    ?data_dir
    ?event_pipe
    ~rpc_host
    ?rpc_port
    ?reveal_data_dir
    ~mode
    ~node
    ~client
    ()

let create_coordinator ?(path = Constant.dac_node) ?name ?color ?data_dir
    ?event_pipe ?(rpc_host = "127.0.0.1") ?rpc_port ?reveal_data_dir
    ~committee_members ~node ~client () =
  let mode = Coordinator {committee_members} in
  create
    ~path
    ?name
    ?color
    ?data_dir
    ?event_pipe
    ~rpc_host
    ?rpc_port
    ?reveal_data_dir
    ~mode
    ~node
    ~client
    ()

let create_committee_member ?(path = Constant.dac_node) ?name ?color ?data_dir
    ?event_pipe ?(rpc_host = "127.0.0.1") ?rpc_port ?reveal_data_dir
    ?(coordinator_rpc_host = "127.0.0.1") ?coordinator_rpc_port ~address ~node
    ~client () =
  let coordinator_rpc_port =
    match coordinator_rpc_port with None -> Port.fresh () | Some port -> port
  in
  let mode =
    Committee_member {address; coordinator_rpc_host; coordinator_rpc_port}
  in
  create
    ~path
    ?name
    ?color
    ?data_dir
    ?event_pipe
    ~rpc_host
    ?rpc_port
    ?reveal_data_dir
    ~mode
    ~node
    ~client
    ()

let create_observer ?(path = Constant.dac_node) ?name ?color ?data_dir
    ?event_pipe ?(rpc_host = "127.0.0.1") ?rpc_port ?reveal_data_dir
    ?(coordinator_rpc_host = "127.0.0.1") ?coordinator_rpc_port ~node ~client ()
    =
  let coordinator_rpc_port =
    match coordinator_rpc_port with None -> Port.fresh () | Some port -> port
  in
  let mode = Observer {coordinator_rpc_host; coordinator_rpc_port} in
  create
    ~path
    ?name
    ?color
    ?data_dir
    ?event_pipe
    ~rpc_host
    ?rpc_port
    ?reveal_data_dir
    ~mode
    ~node
    ~client
    ()

let make_arguments node =
  let base_dir_args =
    ["--base-dir"; Client.base_dir node.persistent_state.client]
  in
  let endpoint_args =
    [
      "--endpoint";
      Printf.sprintf "http://%s:%d" (layer1_addr node) (layer1_port node);
    ]
  in
  base_dir_args @ endpoint_args

let do_runlike_command ?env node arguments =
  if node.status <> Not_running then
    Test.fail "DAC node %s is already running" node.name ;
  let on_terminate _status =
    trigger_ready node None ;
    unit
  in
  let arguments = make_arguments node @ arguments in
  run ?env node {ready = false} arguments ~on_terminate

let run ?env node =
  do_runlike_command
    ?env
    node
    ["run"; "--data-dir"; node.persistent_state.data_dir]

let run ?(wait_ready = true) ?env node =
  let* () = run ?env node in
  let* () = if wait_ready then wait_for_ready node else Lwt.return_unit in
  return ()
