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
    public_addr : string option;
    metrics_addr : string;
    l1_node_endpoint : Endpoint.t;
    mutable pending_ready : unit option Lwt.u list;
    runner : Runner.t option;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "octez-dal-node"

  let default_colors = Log.Color.[|FG.gray; FG.magenta; FG.yellow; FG.green|]
end

type history_mode = Full | Auto | Custom of int

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

let rpc_endpoint ?(local = false) node =
  let host =
    if local then Constant.default_host
    else Runner.address node.persistent_state.runner
  in
  Printf.sprintf "http://%s:%d" host (rpc_port node)

let listen_addr dal_node = dal_node.persistent_state.listen_addr

let public_addr dal_node = dal_node.persistent_state.public_addr

let metrics_addr dal_node = dal_node.persistent_state.metrics_addr

let metrics_port dal_node =
  try
    dal_node.persistent_state.metrics_addr |> String.split_on_char ':'
    |> Fun.flip List.nth 1 |> int_of_string
  with _ -> 11733

let data_dir dal_node = dal_node.persistent_state.data_dir

let spawn_command dal_node =
  Process.spawn
    ?runner:dal_node.persistent_state.runner
    ~name:dal_node.name
    ~color:dal_node.color
    dal_node.path

let spawn_config_init ?(expected_pow = 0.) ?(peers = [])
    ?(attester_profiles = []) ?(producer_profiles = [])
    ?(observer_profiles = []) ?(bootstrap_profile = false) ?history_mode
    dal_node =
  spawn_command dal_node @@ List.filter_map Fun.id
  @@ [
       Some "config";
       Some "init";
       Some "--data-dir";
       Some (data_dir dal_node);
       Some "--rpc-addr";
       Some (Format.asprintf "%s:%d" (rpc_host dal_node) (rpc_port dal_node));
       Some "--net-addr";
       Some (listen_addr dal_node);
       Some "--metrics-addr";
       Some (metrics_addr dal_node);
       Some "--expected-pow";
       Some (string_of_float expected_pow);
     ]
  @ (match public_addr dal_node with
    | None -> []
    | Some addr -> [Some "--public-addr"; Some addr])
  @ (if peers = [] then [None]
     else [Some "--peers"; Some (String.concat "," peers)])
  @ (if attester_profiles = [] then [None]
     else
       [Some "--attester-profiles"; Some (String.concat "," attester_profiles)])
  @ (if observer_profiles = [] then [None]
     else
       [
         Some "--observer-profiles";
         Some (String.concat "," (List.map string_of_int observer_profiles));
       ])
  @ (if producer_profiles = [] then [None]
     else
       [
         Some "--producer-profiles";
         Some (String.concat "," (List.map string_of_int producer_profiles));
       ])
  @ (if bootstrap_profile then [Some "--bootstrap-profile"] else [None])
  @
  match history_mode with
  | None -> []
  | Some Full -> [Some "--history-mode"; Some "full"]
  | Some Auto -> [Some "--history-mode"; Some "auto"]
  | Some (Custom i) -> [Some "--history-mode"; Some (string_of_int i)]

module Config_file = struct
  let filename dal_node = sf "%s/config.json" @@ data_dir dal_node

  let read dal_node = JSON.parse_file (filename dal_node)

  let write dal_node config = JSON.encode_to_file (filename dal_node) config

  let update dal_node update = read dal_node |> update |> write dal_node
end

let init_config ?expected_pow ?peers ?attester_profiles ?producer_profiles
    ?observer_profiles ?bootstrap_profile ?history_mode dal_node =
  let process =
    spawn_config_init
      ?expected_pow
      ?peers
      ?attester_profiles
      ?producer_profiles
      ?observer_profiles
      ?bootstrap_profile
      ?history_mode
      dal_node
  in
  Process.check process

let read_identity dal_node =
  let filename = sf "%s/identity.json" @@ data_dir dal_node in
  match dal_node.persistent_state.runner with
  | None -> Lwt.return JSON.(parse_file filename |-> "peer_id" |> as_string)
  | Some runner ->
      let* content =
        Process.spawn ~runner "cat" [filename] |> Process.check_and_read_stdout
      in
      JSON.(
        parse ~origin:"Dal_node.read_identity" content
        |-> "peer_id" |> as_string)
      |> Lwt.return

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

let wait_for_connections node connections =
  let counter = ref 0 in
  let waiter, resolver = Lwt.task () in
  on_event node (fun {name; _} ->
      match name with
      | "new_connection.v0" ->
          incr counter ;
          if !counter = connections then Lwt.wakeup resolver ()
      | _ -> ()) ;
  let* () = wait_for_ready node in
  waiter

let wait_for_disconnection node ~peer_id =
  wait_for node "disconnected.v0" (fun event ->
      if JSON.(event |-> "peer" |> as_string) = peer_id then Some () else None)

let handle_event dal_node {name; value = _; timestamp = _} =
  match name with "dal_node_is_ready.v0" -> set_ready dal_node | _ -> ()

let create_from_endpoint ?runner ?(path = Uses.path Constant.octez_dal_node)
    ?name ?color ?data_dir ?event_pipe ?(rpc_host = Constant.default_host)
    ?rpc_port ?listen_addr ?public_addr ?metrics_addr ~l1_node_endpoint () =
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
  let metrics_addr =
    match metrics_addr with
    | None -> Format.sprintf "%s:%d" Constant.default_host @@ Port.fresh ()
    | Some addr -> addr
  in
  let dal_node =
    create
      ?runner
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
        runner;
      }
  in
  on_event dal_node (handle_event dal_node) ;
  dal_node

(* TODO: have rpc_addr here, like for others. *)
let create ?runner ?(path = Uses.path Constant.octez_dal_node) ?name ?color
    ?data_dir ?event_pipe ?(rpc_host = Constant.default_host) ?rpc_port
    ?listen_addr ?public_addr ?metrics_addr ~node () =
  create_from_endpoint
    ?runner
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
    ~l1_node_endpoint:(Node.as_rpc_endpoint node)
    ()

let make_arguments node =
  let rpc_host =
    match node.persistent_state.runner with
    | Some _ -> Unix.(string_of_inet_addr inet_addr_any)
    | None -> rpc_host node
  in
  [
    "--endpoint";
    Endpoint.as_string node.persistent_state.l1_node_endpoint;
    "--rpc-addr";
    Format.asprintf "%s:%d" rpc_host (rpc_port node);
    "--net-addr";
    listen_addr node;
    "--metrics-addr";
    metrics_addr node;
  ]
  @
  match public_addr node with
  | None -> []
  | Some addr -> ["--public-addr"; addr]

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
  run
    ?runner:node.persistent_state.runner
    ?env
    ~event_level
    node
    {ready = false}
    arguments
    ~on_terminate

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

let runner (t : t) = t.persistent_state.runner

let point node =
  let address = Runner.address node.persistent_state.runner in
  let net_port =
    String.split_on_char ':' node.persistent_state.listen_addr
    |> Fun.flip List.nth 1
  in
  (address, net_port)

let point_str node =
  let addr, port = point node in
  addr ^ ":" ^ port

let load_last_finalized_processed_level dal_node =
  let open Tezos_stdlib_unix in
  let aux () =
    let open Lwt_result.Syntax in
    let open Lwt_result in
    let last_processed_level_filename = "last_processed_level" in
    let root_dir = sf "%s/store" (data_dir dal_node) in
    let* kvs =
      Key_value_store.Internal_for_tests.init ~lru_size:1 ~root_dir ()
    in
    let file_layout ~root_dir () =
      let filepath = Filename.concat root_dir last_processed_level_filename in
      Key_value_store.layout
        ~encoding:Data_encoding.int32
        ~filepath
        ~eq:Stdlib.( = )
        ~index_of:(fun () -> 0)
        ~number_of_keys_per_file:1
        ()
    in
    let* value_res = Key_value_store.read_value kvs file_layout () () in
    let* () = Key_value_store.close kvs in
    Int32.to_int value_res |> return
  in
  let* v_res = aux () in
  match v_res with Ok v -> Lwt.return_some v | Error _ -> Lwt.return_none
