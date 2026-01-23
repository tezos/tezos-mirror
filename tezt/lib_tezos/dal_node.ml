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

type publish_slots_regularly = {
  frequency : int;
  slot_index : int;
  secret_key : Account.secret_key;
}

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
    disable_shard_validation : bool;
    disable_amplification : bool;
    publish_slots_regularly : publish_slots_regularly option;
    ignore_pkhs : string list option;
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

let disable_shard_validation_environment_variable =
  "TEZOS_DISABLE_SHARD_VALIDATION_I_KNOW_WHAT_I_AM_DOING"

let ignore_topics_environment_variable =
  "TEZOS_IGNORE_TOPICS_I_KNOW_WHAT_I_AM_DOING"

let allow_regular_publication_environment_variable =
  "TEZOS_DAL_PUBLISH_REGULARLY_I_KNOW_WHAT_I_AM_DOING"

let check_error ?exit_code ?msg dal_node =
  match dal_node.status with
  | Not_running ->
      Test.fail "DAL node %s is not running, it has no stderr" (name dal_node)
  | Running {process; _} -> Process.check_error ?exit_code ?msg process

let use_baker_to_start_dal_node =
  match Sys.getenv_opt "TZ_SCHEDULE_KIND" with
  | Some "EXTENDED_DAL_USE_BAKER" -> Some true
  | Some _ -> Some false
  | _ -> None

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

let mk_backup_uris_args sources =
  if List.is_empty sources then []
  else ["--slots-backup-uri"; String.concat "," sources]

let mk_trust_slots_backup_uris flag =
  if flag then ["--trust-slots-backup-uris"] else []

let spawn_config_init ?(expected_pow = 0.) ?(peers = [])
    ?(attester_profiles = []) ?(operator_profiles = [])
    ?(observer_profiles = []) ?(bootstrap_profile = false) ?history_mode
    ?(slots_backup_uris = []) ?(trust_slots_backup_uris = false)
    ?batching_time_interval dal_node =
  spawn_command dal_node @@ ["config"]
  @ (if use_baker_to_start_dal_node = Some true then ["dal"] else [])
  @ [
      "init";
      "--data-dir";
      data_dir dal_node;
      "--rpc-addr";
      Format.asprintf "%s:%d" (rpc_host dal_node) (rpc_port dal_node);
      "--net-addr";
      listen_addr dal_node;
      "--metrics-addr";
      metrics_addr dal_node;
      "--expected-pow";
      string_of_float expected_pow;
    ]
  @ (match public_addr dal_node with
    | None -> []
    | Some addr -> ["--public-addr"; addr])
  @ (if peers = [] then [] else ["--peers"; String.concat "," peers])
  @ (if attester_profiles = [] then []
     else ["--attester-profiles"; String.concat "," attester_profiles])
  @ (if observer_profiles = [] then []
     else
       [
         "--observer-profiles";
         String.concat "," (List.map string_of_int observer_profiles);
       ])
  @ (if operator_profiles = [] then []
     else
       [
         "--operator-profiles";
         String.concat "," (List.map string_of_int operator_profiles);
       ])
  @ (if bootstrap_profile then ["--bootstrap-profile"] else [])
  @ Option.fold
      ~none:[]
      ~some:(fun s -> ["--batching-time-interval"; s])
      batching_time_interval
  @ mk_backup_uris_args slots_backup_uris
  @ mk_trust_slots_backup_uris trust_slots_backup_uris
  @
  match history_mode with
  | None -> []
  | Some Full -> ["--history-mode"; "full"]
  | Some Auto -> ["--history-mode"; "auto"]
  | Some (Custom i) -> ["--history-mode"; string_of_int i]

let spawn_config_update ?(expected_pow = 0.) ?(peers = [])
    ?(attester_profiles = []) ?(operator_profiles = [])
    ?(observer_profiles = []) ?(bootstrap_profile = false) ?history_mode
    ?(slots_backup_uris = []) ?(trust_slots_backup_uris = false)
    ?batching_time_interval dal_node =
  spawn_command dal_node
  @@ [
       "config";
       "update";
       "--data-dir";
       data_dir dal_node;
       "--expected-pow";
       string_of_float expected_pow;
     ]
  @ (if peers = [] then [] else ["--peers"; String.concat "," peers])
  @ (if attester_profiles = [] then []
     else ["--attester-profiles"; String.concat "," attester_profiles])
  @ (if observer_profiles = [] then []
     else
       [
         "--observer-profiles";
         String.concat "," (List.map string_of_int observer_profiles);
       ])
  @ (if operator_profiles = [] then []
     else
       [
         "--operator-profiles";
         String.concat "," (List.map string_of_int operator_profiles);
       ])
  @ (if bootstrap_profile then ["--bootstrap-profile"] else [])
  @ Option.fold
      ~none:[]
      ~some:(fun s -> ["--batching-time-interval"; s])
      batching_time_interval
  @ mk_backup_uris_args slots_backup_uris
  @ mk_trust_slots_backup_uris trust_slots_backup_uris
  @
  match history_mode with
  | None -> []
  | Some Full -> ["--history-mode"; "full"]
  | Some Auto -> ["--history-mode"; "auto"]
  | Some (Custom i) -> ["--history-mode"; string_of_int i]

module Config_file = struct
  let filename dal_node = sf "%s/config.json" @@ data_dir dal_node

  let read dal_node = JSON.parse_file (filename dal_node)

  let write dal_node config = JSON.encode_to_file (filename dal_node) config

  let update dal_node update = read dal_node |> update |> write dal_node
end

let init_config ?expected_pow ?peers ?attester_profiles ?operator_profiles
    ?observer_profiles ?bootstrap_profile ?history_mode ?slots_backup_uris
    ?trust_slots_backup_uris ?batching_time_interval dal_node =
  let process =
    spawn_config_init
      ?expected_pow
      ?peers
      ?attester_profiles
      ?operator_profiles
      ?observer_profiles
      ?bootstrap_profile
      ?history_mode
      ?slots_backup_uris
      ?trust_slots_backup_uris
      ?batching_time_interval
      dal_node
  in
  Process.check process

let update_config ?expected_pow ?peers ?attester_profiles ?operator_profiles
    ?observer_profiles ?bootstrap_profile ?history_mode ?slots_backup_uris
    ?trust_slots_backup_uris ?batching_time_interval dal_node =
  let process =
    spawn_config_update
      ?expected_pow
      ?peers
      ?attester_profiles
      ?operator_profiles
      ?observer_profiles
      ?bootstrap_profile
      ?history_mode
      ?slots_backup_uris
      ?trust_slots_backup_uris
      ?batching_time_interval
      dal_node
  in
  Process.check process

let identity_file dal_node = Filename.concat (data_dir dal_node) "identity.json"

let read_identity dal_node =
  let filename = identity_file dal_node in
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
      check_event dal_node "dal_is_ready.v0" promise

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
  match name with "dal_is_ready.v0" -> set_ready dal_node | _ -> ()

let create_from_endpoint ?runner ?path ?name ?color ?data_dir ?event_pipe
    ?(rpc_host = Constant.default_host) ?rpc_port ?listen_addr ?public_addr
    ?metrics_addr ?(disable_shard_validation = false)
    ?(disable_amplification = false) ?ignore_pkhs ?publish_slots_regularly
    ~l1_node_endpoint () =
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
  let path =
    Option.value
      path
      ~default:
        (if use_baker_to_start_dal_node = Some true then
           Uses.path Constant.octez_agnostic_baker
         else Uses.path Constant.octez_dal_node)
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
        disable_shard_validation;
        disable_amplification;
        ignore_pkhs;
        l1_node_endpoint;
        runner;
        publish_slots_regularly;
      }
  in
  on_event dal_node (handle_event dal_node) ;
  dal_node

(* TODO: have rpc_addr here, like for others. *)
let create ?runner ?path ?name ?color ?data_dir ?event_pipe
    ?(rpc_host = Constant.default_host) ?rpc_port ?listen_addr ?public_addr
    ?metrics_addr ?disable_shard_validation ?disable_amplification ?ignore_pkhs
    ?publish_slots_regularly ~node () =
  let l1_node_endpoint =
    Node.as_rpc_endpoint ~local:(Node.runner node = runner) node
  in
  create_from_endpoint
    ?runner
    ?path
    ?name
    ?color
    ?data_dir
    ?event_pipe
    ~rpc_host
    ?rpc_port
    ?listen_addr
    ?public_addr
    ?metrics_addr
    ?disable_shard_validation
    ?disable_amplification
    ?ignore_pkhs
    ?publish_slots_regularly
    ~l1_node_endpoint
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
  @ (match public_addr node with
    | None -> []
    | Some addr -> ["--public-addr"; addr])
  @ (if node.persistent_state.disable_shard_validation then
       ["--disable-shard-validation"]
     else [])
  @ (if node.persistent_state.disable_amplification then
       ["--disable-amplification"]
     else [])
  @ Option.fold
      ~none:[]
      ~some:(fun pkhs -> "--ignore-topics" :: [String.concat "," pkhs])
      node.persistent_state.ignore_pkhs
  @ Option.fold
      ~none:[]
      ~some:(fun {frequency; slot_index; secret_key} ->
        match secret_key with
        | Unencrypted secret_key ->
            [
              "--publish-slots-regularly";
              Format.sprintf "%d-%d-%s" frequency slot_index secret_key;
            ]
        | _ ->
            failwith
              "Cannot publish slots regularly without unencrypted secret key")
      node.persistent_state.publish_slots_regularly

let do_runlike_command ?env ?(event_level = `Info) node arguments =
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
    (["run"]
    @ (if use_baker_to_start_dal_node = Some true then ["dal"] else [])
    @ ["--verbose"; "--data-dir"; node.persistent_state.data_dir])

let run ?(wait_ready = true) ?env ?event_level node =
  let* () = run ?env ?event_level node in
  let* () = if wait_ready then wait_for_ready node else Lwt.return_unit in
  return ()

let as_rpc_endpoint (t : t) =
  let state = t.persistent_state in
  let scheme = "http" in
  Endpoint.make ~scheme ~host:state.rpc_host ~port:state.rpc_port ()

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

let debug_print_store_schemas ?path ?hooks () =
  let args =
    ["debug"]
    @ (if use_baker_to_start_dal_node = Some true then ["dal"] else [])
    @ ["print"; "store"; "schemas"]
  in
  let path =
    Option.value
      path
      ~default:
        (if use_baker_to_start_dal_node = Some true then
           Uses.path Constant.octez_agnostic_baker
         else Uses.path Constant.octez_dal_node)
  in
  let process = Process.spawn ?hooks path @@ args in
  Process.check process

let snapshot_aux cmd dal_node ?(extra = []) ?endpoint ?min_published_level
    ?max_published_level ?slots output_file =
  let data_dir = data_dir dal_node in
  let endpoint_args =
    match endpoint with
    | None -> []
    | Some ep -> ["--endpoint"; Endpoint.as_string ep]
  in
  let min_level_args =
    match min_published_level with
    | None -> []
    | Some level -> ["--min-published-level"; Int32.to_string level]
  in
  let max_level_args =
    match max_published_level with
    | None -> []
    | Some level -> ["--max-published-level"; Int32.to_string level]
  in
  let slot_args =
    match slots with
    | None -> []
    | Some indices ->
        ["--slots"; String.concat "," (List.map string_of_int indices)]
  in
  let args =
    ["snapshot"; cmd] @ ["--data-dir"; data_dir] @ endpoint_args
    @ min_level_args @ max_level_args @ slot_args @ extra @ [output_file]
  in
  let path =
    if use_baker_to_start_dal_node = Some true then
      Uses.path Constant.octez_agnostic_baker
    else Uses.path Constant.octez_dal_node
  in
  let process = Process.spawn path args in
  Process.check process

let snapshot_export = snapshot_aux "export" ~extra:[]

let snapshot_import t ?(no_check = false) =
  let no_check_args = if no_check then ["--no-check"] else [] in
  snapshot_aux ~extra:no_check_args "import" t

module Proxy = struct
  type answer = [`Response of string | `Stream of Cohttp_lwt.Body.t]

  type route = {
    path : Re.Str.regexp;
    callback :
      path:string ->
      fetch_answer:(unit -> Ezjsonm.t Lwt.t) ->
      fetch_stream:(unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
      answer option Lwt.t;
  }

  type proxy = {
    name : string;
    routes : route list;
    shutdown : unit Lwt.t;
    trigger_shutdown : unit Lwt.u;
  }

  let make ~name ~routes =
    let shutdown, trigger_shutdown = Lwt.task () in
    {name; routes; shutdown; trigger_shutdown}

  let route ~path ~callback = {path; callback}

  let find_mocked_action t ~path =
    List.find_opt (fun act -> Re.Str.string_match act.path path 0) t.routes

  (** [rewrite_attestable_stream_line ~number_of_slots ~published_level line]
      rewrites a single JSON line from [/profiles/<FAULTY_TZ>/monitor/attestable_slots].
      The goal is to make the faulty delegate appear to have all slots attestable for
      [~published_level]. To do this, we need to add the missing slot indices for each level
      in the [Backfill] element that is always the first one sent in the monitoring stream.
      Note: this function heavily depends on the types of elements that are in the returned RPC
      stream, information which can be found at {!Tezos_dal_node_services.Types.Attestable_event.t}. *)
  let rewrite_attestable_stream_line ~number_of_slots ~published_level line =
    let open Ezjsonm in
    let make_slot_id slot_level slot_index =
      dict [("slot_level", int slot_level); ("slot_index", int slot_index)]
    in
    let parse_slot_id v =
      let slot_level = find v ["slot_level"] |> get_int in
      let slot_index = find v ["slot_index"] |> get_int in
      (slot_level, slot_index)
    in

    let rewrite_backfill v =
      let payload = find v ["backfill_payload"] in
      let slot_ids =
        match find_opt payload ["slot_ids"] with
        | None -> []
        | Some arr -> get_list parse_slot_id arr
      in
      (* Find which indices at published_level are already present *)
      let existing_indices =
        List.filter_map
          (fun (slot_level, slot_index) ->
            if slot_level = published_level then Some slot_index else None)
          slot_ids
      in
      (* Generate missing indices *)
      let all_indices = List.init number_of_slots Fun.id in
      let missing_indices =
        List.filter (fun i -> not (List.mem i existing_indices)) all_indices
      in
      (* Append missing slot_ids *)
      let new_slot_ids =
        slot_ids @ List.map (fun i -> (published_level, i)) missing_indices
      in
      (* Build updated JSON *)
      let new_slot_ids_json =
        list
          (fun (slot_level, slot_index) -> make_slot_id slot_level slot_index)
          new_slot_ids
      in
      update v ["backfill_payload"; "slot_ids"] (Some new_slot_ids_json)
      |> value_to_string
    in
    let result =
      try
        let v = from_string line in
        match find_opt v ["kind"] |> Option.map get_string with
        | Some "backfill" -> rewrite_backfill v
        | _ -> line
      with _ -> line
    in
    result ^ "\n"

  let json_backfill ~published_level ~number_of_slots =
    let open Ezjsonm in
    let slot_id_to_json (level, index) =
      dict [("slot_level", int level); ("slot_index", int index)]
    in
    let slot_ids = List.init number_of_slots (fun i -> (published_level, i)) in
    let backfill =
      dict
        [
          ("kind", string "backfill");
          ( "backfill_payload",
            dict
              [
                ("slot_ids", list slot_id_to_json slot_ids);
                ("trap_slot_ids", list slot_id_to_json []);
                ("no_shards_committee_levels", list int []);
              ] );
        ]
    in
    Ezjsonm.value_to_string backfill ^ "\n"

  let routes ~attestation_lag ~number_of_slots ~faulty_delegate
      ~target_attested_level =
    let open Ezjsonm in
    (* Converts a Cohttp body into an JSON line stream *)
    let json_lines_of_body body =
      let chunks = Cohttp_lwt.Body.to_stream body in
      let buf = Buffer.create 2048 in

      let chop s =
        let n = String.length s in
        if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s
      in

      let rec produce_line () =
        let contents = Buffer.contents buf in
        match String.index_opt contents '\n' with
        | Some idx ->
            (* We have a complete line *)
            let line = String.sub contents 0 idx in
            let rest =
              String.sub contents (idx + 1) (String.length contents - idx - 1)
            in
            Buffer.clear buf ;
            Buffer.add_string buf rest ;
            Lwt.return_some (chop line)
        | None -> (
            (* Need more data from the stream *)
            let* next_chunk = Lwt_stream.get chunks in
            match next_chunk with
            | None ->
                (* Stream ended - emit final partial line if any *)
                if Buffer.length buf = 0 then Lwt.return_none
                else
                  let line = Buffer.contents buf in
                  Buffer.clear buf ;
                  Lwt.return_some (chop line)
            | Some chunk ->
                Buffer.add_string buf chunk ;
                produce_line ())
      in

      Lwt_stream.from produce_line
    in
    [
      (let path =
         Re.Str.regexp
         @@ Format.sprintf
              "/profiles/%s/monitor/attestable_slots"
              faulty_delegate
       in
       route ~path ~callback:(fun ~path:_ ~fetch_answer:_ ~fetch_stream ->
           let published_level = target_attested_level - attestation_lag in
           let backfill_stream =
             let line = json_backfill ~published_level ~number_of_slots in
             Lwt_stream.of_list [line]
           in
           let* _resp, body = fetch_stream () in
           let lines = json_lines_of_body body in
           let rewritten_lines =
             Lwt_stream.map
               (rewrite_attestable_stream_line
                  ~number_of_slots
                  ~published_level)
               lines
           in
           let rewritten_stream =
             Lwt_stream.append backfill_stream rewritten_lines
           in
           let rewritten_body = Cohttp_lwt.Body.of_stream rewritten_stream in
           return (Some (`Stream rewritten_body))));
      (let path =
         Re.Str.regexp
         @@ Format.sprintf
              "/profiles/%s/attested_levels/%d/attestable_slots"
              faulty_delegate
              target_attested_level
       in
       route ~path ~callback:(fun ~path:_ ~fetch_answer ~fetch_stream:_ ->
           let* dal_node_answer = fetch_answer () in
           let v = value dal_node_answer in
           let kind = find v ["kind"] |> get_string in
           let new_v =
             if String.equal kind "attestable_slots_set" then
               let attestable_slots_set =
                 (* Declare each slot attestable. *)
                 find v ["attestable_slots_set"]
                 |> get_list get_bool
                 |> List.map (fun _ -> true)
                 |> list bool
               in
               update v ["attestable_slots_set"] (Some attestable_slots_set)
             else v
           in
           return (Some (`Response (value_to_string new_v)))));
    ]

  let make ~name ~attestation_lag ~number_of_slots ~faulty_delegate
      ~target_attested_level =
    let routes =
      routes
        ~attestation_lag
        ~number_of_slots
        ~faulty_delegate
        ~target_attested_level
    in
    make ~name ~routes

  let run t ~honest_dal_node ~faulty_dal_node =
    let dal_uri uri =
      Uri.make
        ~scheme:"http"
        ~host:(rpc_host honest_dal_node)
        ~port:(rpc_port honest_dal_node)
        ~path:(Uri.path uri)
        ~query:(Uri.query uri)
        ()
    in
    let callback _conn req body =
      let uri = Cohttp.Request.uri req in
      let uri_str = Uri.to_string uri in
      let method_ = Cohttp.Request.meth req in
      let path = Uri.path uri in
      match find_mocked_action t ~path with
      | Some action -> (
          Log.debug "[%s] mocking data for request: '%s'" t.name uri_str ;
          let headers = Cohttp.Request.headers req in
          let fetch_answer () =
            let* _resp, body =
              Cohttp_lwt_unix.Client.call ~headers ~body method_ (dal_uri uri)
            in
            let* body_str = Cohttp_lwt.Body.to_string body in
            return (Ezjsonm.from_string body_str)
          in
          let fetch_stream () =
            Cohttp_lwt_unix.Client.call ~headers ~body method_ (dal_uri uri)
          in
          let* res = action.callback ~path ~fetch_answer ~fetch_stream in
          match res with
          | None -> Cohttp_lwt_unix.Server.respond_not_found ()
          | Some (`Response body) ->
              Log.debug
                "[%s] mocking with custom response answer '%s'"
                t.name
                body ;
              Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
          | Some (`Stream stream_body) ->
              Log.debug "[%s] mocking with custom stream answer" t.name ;
              Cohttp_lwt_unix.Server.respond ~status:`OK ~body:stream_body ())
      | None ->
          Log.debug
            "[%s] forwarding the following request to the honest dal node: '%s'"
            t.name
            uri_str ;
          let headers = Cohttp.Request.headers req in
          let* resp, body =
            Cohttp_lwt_unix.Client.call ~headers ~body method_ (dal_uri uri)
          in
          Cohttp_lwt_unix.Server.respond
            ~status:(Cohttp.Response.status resp)
            ~headers:(Cohttp.Response.headers resp)
            ~body
            ()
    in
    let start () =
      Cohttp_lwt_unix.Server.create
        ~mode:(`TCP (`Port (rpc_port faulty_dal_node)))
        ~stop:t.shutdown
        (Cohttp_lwt_unix.Server.make ~callback ())
    in
    Lwt.async start

  let stop t = Lwt.wakeup t.trigger_shutdown ()
end

module Mockup = struct
  type answer = [`Response of string | `Stream of string Lwt_stream.t]

  type route = {
    path_pattern : Re.Pcre.regexp;
    callback : path:string -> answer option Lwt.t;
  }

  type t = {
    name : string;
    routes : route list;
    shutdown : unit Lwt.t;
    trigger_shutdown : unit Lwt.u;
  }

  let make ~name ~routes =
    let shutdown, trigger_shutdown = Lwt.task () in
    {name; routes; shutdown; trigger_shutdown}

  let route ~path_pattern ~callback =
    {path_pattern = Re.Pcre.regexp path_pattern; callback}

  let find_mocked_action t ~path =
    List.find_opt
      (fun act -> Re.Pcre.pmatch ~rex:act.path_pattern path)
      t.routes

  let run t ~port =
    let callback _conn req _body =
      let uri = Cohttp.Request.uri req in
      let uri_str = Uri.to_string uri in
      let path = Uri.path uri in
      match find_mocked_action t ~path with
      | Some action -> (
          Log.debug "[%s] mocking request: '%s'" t.name uri_str ;
          let* res = action.callback ~path in
          match res with
          | None -> Cohttp_lwt_unix.Server.respond_not_found ()
          | Some (`Response body) ->
              Log.debug "[%s] responding with mock: '%s'" t.name body ;
              Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
          | Some (`Stream stream) ->
              let headers =
                Cohttp.Header.init_with "content-type" "application/json"
              in
              let body = Cohttp_lwt.Body.of_stream stream in
              Cohttp_lwt_unix.Server.respond ~headers ~status:`OK ~body ())
      | None ->
          Log.warn "[%s] no mock route for: '%s', failing." t.name uri_str ;
          Cohttp_lwt_unix.Server.respond_error
            ~status:`Not_implemented
            ~body:"Unmocked RPC"
            ()
    in
    let start () =
      Cohttp_lwt_unix.Server.create
        ~mode:(`TCP (`Port port))
        ~stop:t.shutdown
        (Cohttp_lwt_unix.Server.make ~callback ())
    in
    Lwt.async start

  let stop t = Lwt.wakeup t.trigger_shutdown ()
end

module Mockup_for_baker = struct
  type t = Mockup.t

  let json_backfill_event ~slot_ids =
    let open Ezjsonm in
    let slot_id_to_json (level, index) =
      dict [("slot_level", int level); ("slot_index", int index)]
    in
    value_to_string
      (dict
         [
           ("kind", string "backfill");
           ( "backfill_payload",
             dict
               [
                 ("slot_ids", list slot_id_to_json slot_ids);
                 ("trap_slot_ids", list slot_id_to_json []);
                 ("no_shards_committee_levels", list int []);
               ] );
         ])

  (* Emit a single [Backfill] line that covers a range of published levels on
     slot_index=0 so the baker’s cache contains the bit we need for the test's
     attested_level. *)
  let monitor_route ~published_levels =
    let path_pattern =
      "^/profiles/(tz[1234][A-Za-z0-9]+)/monitor/attestable_slots/?$"
    in
    Mockup.route ~path_pattern ~callback:(fun ~path ->
        let re = Re.Pcre.regexp path_pattern in
        let _attester =
          match Re.exec_opt re path with
          | Some groups -> Re.Group.get groups 1
          | None -> Test.fail "failed to extract pkh from %s" path
        in
        let slot_ids = List.init published_levels (fun i -> (i + 1, 0)) in
        let body_line = json_backfill_event ~slot_ids ^ "\n" in
        let stream = Lwt_stream.of_list [body_line] in
        Lwt.return_some (`Stream stream))

  let routes ~attestation_lag ~attesters ~attestable_slots =
    [
      (* Mirrors the DAL node’s backfill window on subscribe, where (attestation_lag + 1)
         published levels are included. *)
      monitor_route ~published_levels:(attestation_lag + 1);
      (let path_pattern =
         Format.sprintf
           "/profiles/(tz[1234][a-zA-Z0-9]+)/attested_levels/([1-9][0-9]*)/attestable_slots"
       in
       Mockup.route ~path_pattern ~callback:(fun ~path ->
           let open Ezjsonm in
           let re = Re.Pcre.regexp path_pattern in
           let attester =
             match Re.exec_opt re path with
             | Some groups -> Re.Group.get groups 1
             | None -> Test.fail "failed to extract pkh from %s" path
           in
           let attested_level =
             match Re.exec_opt re path with
             | Some groups -> Re.Group.get groups 2 |> int_of_string
             | None -> Test.fail "failed to extract attested_level from %s" path
           in
           let published_level = attested_level - attestation_lag in
           let mocked_json =
             dict
               [
                 ("kind", string "attestable_slots_set");
                 ( "attestable_slots_set",
                   list bool @@ attestable_slots ~attester ~attested_level );
                 ("published_level", int published_level);
               ]
           in
           let body = value_to_string mocked_json in
           Lwt.return_some (`Response body)));
      (let path_pattern = "^/profiles/?$" in
       Mockup.route ~path_pattern ~callback:(fun ~path:_ ->
           let open Ezjsonm in
           let mocked_json =
             dict
               [
                 ("kind", string "controller");
                 ( "controller_profiles",
                   dict [("attesters", list string attesters)] );
               ]
           in
           let body = value_to_string mocked_json in
           Lwt.return_some (`Response body)));
      (let path_pattern = "^/health/?$" in
       Mockup.route ~path_pattern ~callback:(fun ~path:_ ->
           let open Ezjsonm in
           let mocked_json =
             dict
               [
                 ("status", string "up");
                 ( "checks",
                   list
                     (fun (name, status) ->
                       dict [("name", string name); ("status", string status)])
                     [("p2p", "up"); ("topics", "ok"); ("gossipsub", "up")] );
               ]
           in
           let body = value_to_string mocked_json in
           Lwt.return_some (`Response body)));
    ]

  let make ~name ~attestation_lag ~attesters ~attestable_slots =
    let routes = routes ~attestation_lag ~attesters ~attestable_slots in
    Mockup.make ~name ~routes

  let run t = Mockup.run t

  let stop t = Mockup.stop t
end
