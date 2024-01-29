(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
  type persistent_state = {
    data_dir : string;
    rpc_host : string;
    rpc_port : int;
    listen_addr : string;
        (** The TCP address and port at which this instance can be reached. *)
    public_addr : string;
    metrics_addr : string;
    l1_node_endpoint : Client.endpoint;
    mutable pending_ready : unit option Lwt.u list;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "octez-dal-node"

  let default_colors = Log.Color.[|FG.gray; FG.magenta; FG.yellow; FG.green|]
end

open Parameters
include Daemon.Make (Parameters)

let wait dal_node =
  match dal_node.status with
  | Not_running ->
      Test.fail
        "DAL node %s is not running, cannot wait for it to terminate"
        (name dal_node)
  | Running {process; _} -> Process.wait process

let is_running_not_ready dal_node =
  match dal_node.status with
  | Running {session_state = {ready}; _} when not ready -> true
  | _ -> false

let name dal_node = dal_node.name

let rpc_host dal_node = dal_node.persistent_state.rpc_host

let rpc_port dal_node = dal_node.persistent_state.rpc_port

let rpc_endpoint dal_node =
  Printf.sprintf "http://%s:%d" (rpc_host dal_node) (rpc_port dal_node)

let listen_addr dal_node = dal_node.persistent_state.listen_addr

let public_addr dal_node = dal_node.persistent_state.public_addr

let metrics_addr dal_node = dal_node.persistent_state.metrics_addr

let data_dir dal_node = dal_node.persistent_state.data_dir

let spawn_command dal_node =
  Process.spawn ~name:dal_node.name ~color:dal_node.color dal_node.path

let spawn_config_init ?(expected_pow = 0.) ?(peers = [])
    ?(attester_profiles = []) ?(producer_profiles = [])
    ?(observer_profiles = []) ?(bootstrap_profile = false) dal_node =
  spawn_command dal_node
  @@ List.filter_map
       Fun.id
       [
         Some "config";
         Some "init";
         Some "--data-dir";
         Some (data_dir dal_node);
         Some "--rpc-addr";
         Some (Format.asprintf "%s:%d" (rpc_host dal_node) (rpc_port dal_node));
         Some "--net-addr";
         Some (listen_addr dal_node);
         Some "--public-addr";
         Some (public_addr dal_node);
         Some "--metrics-addr";
         Some (metrics_addr dal_node);
         Some "--expected-pow";
         Some (string_of_float expected_pow);
         Some "--peers";
         Some (String.concat "," peers);
         Some "--attester-profiles";
         Some (String.concat "," attester_profiles);
         Some "--producer-profiles";
         Some (String.concat "," (List.map string_of_int producer_profiles));
         Some "--observer-profiles";
         Some (String.concat "," (List.map string_of_int observer_profiles));
         (if bootstrap_profile then Some "--bootstrap-profile" else None);
       ]

module Config_file = struct
  let filename dal_node = sf "%s/config.json" @@ data_dir dal_node

  let read dal_node = JSON.parse_file (filename dal_node)

  let write dal_node config = JSON.encode_to_file (filename dal_node) config

  let update dal_node update = read dal_node |> update |> write dal_node
end

let init_config ?expected_pow ?peers ?attester_profiles ?producer_profiles
    ?observer_profiles ?bootstrap_profile dal_node =
  let process =
    spawn_config_init
      ?expected_pow
      ?peers
      ?attester_profiles
      ?producer_profiles
      ?observer_profiles
      ?bootstrap_profile
      dal_node
  in
  Process.check process

let read_identity dal_node =
  let filename = sf "%s/identity.json" @@ data_dir dal_node in
  JSON.parse_file filename

let check_event ?timeout ?where dal_node name promise =
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
        (Terminated_before_event {daemon = dal_node.name; event = name; where})
  | Some x -> return x

let trigger_ready dal_node value =
  let pending = dal_node.persistent_state.pending_ready in
  dal_node.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready dal_node =
  (match dal_node.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready dal_node (Some ())

let wait_for_ready dal_node =
  match dal_node.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      dal_node.persistent_state.pending_ready <-
        resolver :: dal_node.persistent_state.pending_ready ;
      check_event dal_node "dal_node_is_ready.v0" promise

let handle_event dal_node {name; value = _; timestamp = _} =
  match name with "dal_node_is_ready.v0" -> set_ready dal_node | _ -> ()

let create_from_endpoint ?(path = Uses.path Constant.octez_dal_node) ?name
    ?color ?data_dir ?event_pipe ?(rpc_host = Constant.default_host) ?rpc_port
    ?listen_addr ?public_addr ?metrics_addr ~l1_node_endpoint () =
  let name = match name with None -> fresh_name () | Some name -> name in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let rpc_port =
    match rpc_port with None -> Port.fresh () | Some port -> port
  in
  let listen_addr =
    match listen_addr with
    | None -> Format.sprintf "%s:%d" Constant.default_host @@ Port.fresh ()
    | Some addr -> addr
  in
  let public_addr =
    match public_addr with None -> listen_addr | Some addr -> addr
  in
  let metrics_addr =
    match metrics_addr with
    | None -> Format.sprintf "%s:%d" Constant.default_host @@ Port.fresh ()
    | Some addr -> addr
  in
  let dal_node =
    create
      ~path
      ~name
      ?color
      ?event_pipe
      {
        data_dir;
        rpc_host;
        rpc_port;
        listen_addr;
        public_addr;
        metrics_addr;
        pending_ready = [];
        l1_node_endpoint;
      }
  in
  on_event dal_node (handle_event dal_node) ;
  dal_node

(* TODO: have rpc_addr here, like for others. *)
let create ?(path = Uses.path Constant.octez_dal_node) ?name ?color ?data_dir
    ?event_pipe ?(rpc_host = Constant.default_host) ?rpc_port ?listen_addr
    ?public_addr ?metrics_addr ~node () =
  create_from_endpoint
    ~path
    ?name
    ?color
    ?data_dir
    ?event_pipe
    ~rpc_host
    ?rpc_port
    ?listen_addr
    ?public_addr
    ?metrics_addr
    ~l1_node_endpoint:(Client.Node node)
    ()

let make_arguments node =
  let l1_endpoint =
    Client.as_foreign_endpoint node.persistent_state.l1_node_endpoint
  in
  [
    "--endpoint";
    Endpoint.as_string l1_endpoint;
    "--rpc-addr";
    Format.asprintf "%s:%d" (rpc_host node) (rpc_port node);
    "--net-addr";
    listen_addr node;
    "--public-addr";
    public_addr node;
    "--metrics-addr";
    metrics_addr node;
  ]

let do_runlike_command ?env ?(event_level = `Debug) node arguments =
  if node.status <> Not_running then
    Test.fail "DAL node %s is already running" node.name ;
  let on_terminate _status =
    trigger_ready node None ;
    unit
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6164
     Improve handling of arguments:
     * unclear what should happen when two values for the same argument are given
     * [make_arguments] seems incomplete
     * refactoring possible in [spawn_config_init] *)
  let arguments = arguments @ make_arguments node in
  run ?env ~event_level node {ready = false} arguments ~on_terminate

let run ?env ?event_level node =
  do_runlike_command
    ?env
    ?event_level
    node
    ["run"; "--data-dir"; node.persistent_state.data_dir]

let run ?(wait_ready = true) ?env ?event_level node =
  let* () = run ?env ?event_level node in
  let* () = if wait_ready then wait_for_ready node else Lwt.return_unit in
  return ()

let as_rpc_endpoint (t : t) =
  let state = t.persistent_state in
  let scheme = "http" in
  Endpoint.{scheme; host = state.rpc_host; port = state.rpc_port}
