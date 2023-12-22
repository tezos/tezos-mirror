(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

type 'a known = Unknown | Known of 'a

type purpose = Operating | Batching | Cementing | Recovering

type operation_kind =
  | Publish
  | Add_messages
  | Cement
  | Timeout
  | Refute
  | Recover
  | Execute_outbox_message

let operation_kind_of_string = function
  | "publish" -> Some Publish
  | "add_messages" -> Some Add_messages
  | "cement" -> Some Cement
  | "timeout" -> Some Timeout
  | "refute" -> Some Refute
  | "recover" -> Some Recover
  | "execute_outbox_message" -> Some Execute_outbox_message
  | _ -> None

let operation_kind_of_string_exn s =
  match operation_kind_of_string s with
  | Some p -> p
  | None -> invalid_arg ("operation_kind_of_string " ^ s)

let string_of_operation_kind = function
  | Publish -> "publish"
  | Add_messages -> "add_messages"
  | Cement -> "cement"
  | Timeout -> "timeout"
  | Refute -> "refute"
  | Recover -> "recover"
  | Execute_outbox_message -> "execute_outbox_message"

type mode =
  | Batcher
  | Custom of operation_kind list
  | Maintenance
  | Observer
  | Operator
  | Accuser
  | Bailout

let string_of_mode = function
  | Observer -> "observer"
  | Batcher -> "batcher"
  | Maintenance -> "maintenance"
  | Operator -> "operator"
  | Custom op_kinds ->
      if op_kinds = [] then "custom"
      else
        "custom:"
        ^ String.concat "," (List.map string_of_operation_kind op_kinds)
  | Accuser -> "accuser"
  | Bailout -> "bailout"

type history_mode = Archive | Full

let string_of_history_mode = function Archive -> "archive" | Full -> "full"

type argument =
  | Data_dir of string
  | Rpc_addr of string
  | Rpc_port of int
  | Log_kernel_debug
  | Log_kernel_debug_file of string
  | Metrics_addr of string
  | Injector_attempts of int
  | Boot_sector_file of string
  | Dac_observer of Dac_node.t
  | Loser_mode of string
  | No_degraded
  | Gc_frequency of int
  | History_mode of history_mode
  | Dal_node of Dal_node.t
  | Mode of mode
  | Rollup of string

let make_argument = function
  | Data_dir dir -> ["--data-dir"; dir]
  | Rpc_addr host -> ["--rpc-addr"; host]
  | Rpc_port port -> ["--rpc-port"; string_of_int port]
  | Log_kernel_debug -> ["--log-kernel-debug"]
  | Log_kernel_debug_file file -> ["--log-kernel-debug-file"; file]
  | Metrics_addr addr -> ["--metrics-addr"; addr]
  | Injector_attempts n -> ["--injector-attempts"; string_of_int n]
  | Boot_sector_file file -> ["--boot-sector-file"; file]
  | Dac_observer dac_node -> ["--dac-observer"; Dac_node.endpoint dac_node]
  | Loser_mode loser -> ["--loser-mode"; loser]
  | No_degraded -> ["--no-degraded"]
  | Gc_frequency freq -> ["--gc-frequency"; string_of_int freq]
  | History_mode mode -> ["--history-mode"; string_of_history_mode mode]
  | Dal_node dal_node -> ["--dal-node"; Dal_node.rpc_endpoint dal_node]
  | Mode mode -> ["--mode"; string_of_mode mode]
  | Rollup addr -> ["--rollup"; addr]

let is_redundant = function
  | Data_dir _, Data_dir _
  | Rpc_addr _, Rpc_addr _
  | Rpc_port _, Rpc_port _
  | Log_kernel_debug, Log_kernel_debug
  | Log_kernel_debug_file _, Log_kernel_debug_file _
  | Injector_attempts _, Injector_attempts _
  | Boot_sector_file _, Boot_sector_file _
  | Dac_observer _, Dac_observer _
  | Loser_mode _, Loser_mode _
  | No_degraded, No_degraded
  | Gc_frequency _, Gc_frequency _
  | History_mode _, History_mode _
  | Dal_node _, Dal_node _
  | Mode _, Mode _
  | Rollup _, Rollup _ ->
      true
  | Metrics_addr addr1, Metrics_addr addr2 -> addr1 = addr2
  | Metrics_addr _, _
  | Data_dir _, _
  | Rpc_addr _, _
  | Rpc_port _, _
  | Log_kernel_debug, _
  | Log_kernel_debug_file _, _
  | Injector_attempts _, _
  | Boot_sector_file _, _
  | Dac_observer _, _
  | Loser_mode _, _
  | No_degraded, _
  | Gc_frequency _, _
  | History_mode _, _
  | Dal_node _, _
  | Mode _, _
  | Rollup _, _ ->
      false

let make_arguments arguments = List.flatten (List.map make_argument arguments)

let add_missing_argument arguments argument =
  if List.exists (fun arg -> is_redundant (arg, argument)) arguments then
    arguments
  else argument :: arguments

(** join both list, if one argument exists in both list, then the one
    of [extra_arguments] is used. This is so arguments added in
    [~extra_arguments] for {!run} like function takes precedent from
    default arguments. *)
let add_missing_arguments ~extra_arguments ~arguments =
  List.fold_left add_missing_argument extra_arguments arguments

let optional_switch arg switch = if switch then [arg] else []

let optional_arg const = function None -> [] | Some x -> [const x]

module Parameters = struct
  type persistent_state = {
    data_dir : string;
    base_dir : string;
    operators : (purpose * string) list;
    default_operator : string option;
    metrics_addr : string option;
    metrics_port : int;
    rpc_host : string;
    rpc_port : int;
    mode : mode;
    dal_node : Dal_node.t option;
    loser_mode : string option;
    allow_degraded : bool;
    gc_frequency : int;
    history_mode : history_mode;
    password_file : string option;
    mutable endpoint : Client.endpoint;
    mutable pending_ready : unit option Lwt.u list;
    mutable pending_level : (int * int option Lwt.u) list;
    runner : Runner.t option;
  }

  type session_state = {mutable ready : bool; mutable level : int known}

  let base_default_name = "sc-rollup-node"

  let default_colors = Log.Color.[|FG.magenta; FG.green; FG.yellow; FG.cyan|]
end

open Parameters
include Daemon.Make (Parameters)

let mode_of_string s =
  match String.split_on_char ':' s with
  | ["custom"; kinds] ->
      let operation_kinds_strs = String.split_on_char ',' kinds in
      let operation_kinds =
        List.map operation_kind_of_string_exn operation_kinds_strs
      in
      Custom operation_kinds
  | _ -> (
      match String.lowercase_ascii s with
      | "observer" -> Observer
      | "accuser" -> Accuser
      | "bailout" -> Bailout
      | "batcher" -> Batcher
      | "maintenance" -> Maintenance
      | "operator" -> Operator
      | "custom" -> Custom []
      | _ -> invalid_arg (Format.sprintf "%S is not an existing mode" s))

let string_of_history_mode = function Archive -> "archive" | Full -> "full"

let check_error ?exit_code ?msg sc_node =
  match sc_node.status with
  | Not_running ->
      Test.fail
        "Smart rollup node %s is not running, it has no stderr"
        (name sc_node)
  | Running {process; _} -> Process.check_error ?exit_code ?msg process

let wait sc_node =
  match sc_node.status with
  | Not_running ->
      Test.fail
        "Smart rollup node %s is not running, cannot wait for it to terminate"
        (name sc_node)
  | Running {process; _} -> Process.wait process

let process node =
  match node.status with
  | Running {process; _} -> Some process
  | Not_running -> None

let write_in_stdin rollup_node str =
  match rollup_node.status with
  | Running {stdin; _} -> Lwt_io.write stdin Format.(sprintf "%s\n%!" str)
  | Not_running ->
      Test.fail
        "Smart rollup node %s is not running, cannot write in stdin"
        (name rollup_node)

let color sc_node = sc_node.color

let rpc_host sc_node = sc_node.persistent_state.rpc_host

let rpc_port sc_node = sc_node.persistent_state.rpc_port

let metrics node =
  ( Option.value ~default:"127.0.0.1" node.persistent_state.metrics_addr,
    node.persistent_state.metrics_port )

let endpoint sc_node =
  Printf.sprintf "http://%s:%d" (rpc_host sc_node) (rpc_port sc_node)

let data_dir sc_node = sc_node.persistent_state.data_dir

let base_dir sc_node = sc_node.persistent_state.base_dir

let string_of_purpose = function
  | Operating -> "operating"
  | Batching -> "batching"
  | Cementing -> "cementing"
  | Recovering -> "recovering"

(* Extracts operators from node state, handling custom mode, and
   formats them as "purpose:operator". Includes default operator if present. *)
let operators_params rollup_node =
  List.fold_left
    (fun acc (purpose, operator) ->
      (string_of_purpose purpose ^ ":" ^ operator) :: acc)
    (Option.to_list rollup_node.persistent_state.default_operator)
    rollup_node.persistent_state.operators

let make_command_arguments ?password_file node =
  [
    "--endpoint";
    Client.string_of_endpoint ~hostname:true node.persistent_state.endpoint;
    "--base-dir";
    base_dir node;
  ]
  @ Cli_arg.optional_arg "password-filename" Fun.id password_file

let spawn_command sc_node args =
  Process.spawn
    ?runner:sc_node.persistent_state.runner
    ~name:sc_node.name
    ~color:sc_node.color
    sc_node.path
  @@ make_command_arguments sc_node
  @ args

let runlike_argument rollup_node =
  let rollup_node_state = rollup_node.persistent_state in
  [
    Data_dir (data_dir rollup_node);
    (let metrics_addr, metrics_port = metrics rollup_node in
     Metrics_addr (metrics_addr ^ ":" ^ string_of_int metrics_port));
    Rpc_addr (rpc_host rollup_node);
    Rpc_port (rpc_port rollup_node);
    History_mode rollup_node_state.history_mode;
    Gc_frequency rollup_node_state.gc_frequency;
  ]
  @ optional_arg (fun s -> Loser_mode s) rollup_node_state.loser_mode
  @ optional_switch No_degraded (not rollup_node_state.allow_degraded)
  @ optional_arg (fun n -> Dal_node n) rollup_node_state.dal_node

let node_args rollup_node rollup_address =
  let mode = string_of_mode rollup_node.persistent_state.mode in
  ( mode,
    ["for"; rollup_address; "with"; "operators"] @ operators_params rollup_node,
    runlike_argument rollup_node )

let legacy_node_args rollup_node rollup_address =
  Mode rollup_node.persistent_state.mode :: Rollup rollup_address
  :: runlike_argument rollup_node

let spawn_config_init ?(force = false) sc_node rollup_address =
  let mode, cmd, args = node_args sc_node rollup_address in
  spawn_command sc_node @@ ["init"; mode; "config"] @ cmd @ make_arguments args
  @ Cli_arg.optional_switch "force" force

let config_init ?force sc_node rollup_address =
  let process = spawn_config_init ?force sc_node rollup_address in
  let* output = Process.check_and_read_stdout process in
  match
    output =~* rex "Smart rollup node configuration written in ([^\n]*)"
  with
  | None -> failwith "Smart rollup node configuration initialization failed"
  | Some filename -> return filename

module Config_file = struct
  let filename sc_node = sf "%s/config.json" @@ data_dir sc_node

  let read sc_node = JSON.parse_file (filename sc_node)

  let write sc_node config = JSON.encode_to_file (filename sc_node) config

  let update sc_node update = read sc_node |> update |> write sc_node
end

let trigger_ready sc_node value =
  let pending = sc_node.persistent_state.pending_ready in
  sc_node.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready sc_node =
  (match sc_node.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready sc_node (Some ())

let check_event ?timeout ?where sc_node name promise =
  let promise = Lwt.map Result.ok promise in
  let* result =
    match timeout with
    | None -> promise
    | Some timeout ->
        Lwt.pick
          [
            promise;
            (let* () = Lwt_unix.sleep timeout in
             Lwt.return_error ());
          ]
  in
  match result with
  | Ok (Some x) -> return x
  | Ok None ->
      raise
        (Terminated_before_event {daemon = sc_node.name; event = name; where})
  | Error () ->
      Format.ksprintf
        failwith
        "Timeout waiting for event %s of %s"
        name
        sc_node.name

let wait_for_ready sc_node =
  match sc_node.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      sc_node.persistent_state.pending_ready <-
        resolver :: sc_node.persistent_state.pending_ready ;
      check_event sc_node "sc_rollup_node_is_ready.v0" promise

let update_level sc_node current_level =
  (match sc_node.status with
  | Not_running -> ()
  | Running status -> (
      match status.session_state.level with
      | Unknown -> status.session_state.level <- Known current_level
      | Known old_level ->
          status.session_state.level <- Known (max old_level current_level))) ;
  let pending = sc_node.persistent_state.pending_level in
  sc_node.persistent_state.pending_level <- [] ;
  List.iter
    (fun ((level, resolver) as pending) ->
      if current_level >= level then
        Lwt.wakeup_later resolver (Some current_level)
      else
        sc_node.persistent_state.pending_level <-
          pending :: sc_node.persistent_state.pending_level)
    pending

let wait_for_level ?timeout sc_node level =
  match sc_node.status with
  | Running {session_state = {level = Known current_level; _}; _}
    when current_level >= level ->
      return current_level
  | Not_running | Running _ ->
      let promise, resolver = Lwt.task () in
      sc_node.persistent_state.pending_level <-
        (level, resolver) :: sc_node.persistent_state.pending_level ;
      check_event
        ?timeout
        sc_node
        "smart_rollup_node_daemon_new_head_processed.v0"
        ~where:("level >= " ^ string_of_int level)
        promise

let unsafe_wait_sync ?path_client ?timeout sc_node =
  let* node_level =
    match sc_node.persistent_state.endpoint with
    | Node node -> Node.get_level node
    | endpoint ->
        let* level =
          Client.RPC.call (Client.create ?path:path_client ~endpoint ())
          @@ RPC.get_chain_block_helper_current_level ()
        in
        return level.level
  in
  wait_for_level ?timeout sc_node node_level

let wait_sync ?path_client sc_node ~timeout =
  unsafe_wait_sync ?path_client sc_node ~timeout

let handle_event sc_node {name; value; timestamp = _} =
  match name with
  | "smart_rollup_node_is_ready.v0" -> set_ready sc_node
  | "smart_rollup_node_daemon_new_head_processed.v0" ->
      let level = JSON.(value |-> "level" |> as_int) in
      update_level sc_node level
  | "smart_rollup_node_daemon_new_heads_processed.v0" ->
      let level = JSON.(value |-> "to" |> as_int) in
      update_level sc_node level
  | _ -> ()

let create_with_endpoint ?runner ?path ?name ?color ?data_dir ~base_dir
    ?event_pipe ?metrics_addr ?metrics_port ?(rpc_host = "127.0.0.1") ?rpc_port
    ?(operators = []) ?default_operator ?(dal_node : Dal_node.t option)
    ?loser_mode ?(allow_degraded = false) ?(gc_frequency = 1)
    ?(history_mode = Full) ?password_file mode endpoint =
  let name = match name with None -> fresh_name () | Some name -> name in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let rpc_port =
    match rpc_port with None -> Port.fresh () | Some port -> port
  in
  let metrics_port =
    match metrics_port with None -> Port.fresh () | Some port -> port
  in
  let path =
    Option.value ~default:(Uses.path Constant.octez_smart_rollup_node) path
  in
  let sc_node =
    create
      ~path
      ~name
      ?runner
      ?color
      ?event_pipe
      {
        data_dir;
        base_dir;
        metrics_addr;
        metrics_port;
        rpc_host;
        rpc_port;
        operators;
        default_operator;
        mode;
        endpoint;
        dal_node;
        loser_mode;
        allow_degraded;
        gc_frequency;
        history_mode;
        password_file;
        pending_ready = [];
        pending_level = [];
        runner;
      }
  in
  on_event sc_node (handle_event sc_node) ;
  sc_node

let create ?runner ?path ?name ?color ?data_dir ~base_dir ?event_pipe
    ?metrics_addr ?metrics_port ?rpc_host ?rpc_port ?operators ?default_operator
    ?dal_node ?loser_mode ?allow_degraded ?gc_frequency ?history_mode
    ?password_file mode (node : Node.t) =
  create_with_endpoint
    ?runner
    ?path
    ?name
    ?color
    ?data_dir
    ~base_dir
    ?event_pipe
    ?metrics_addr
    ?metrics_port
    ?rpc_host
    ?rpc_port
    ?operators
    ?default_operator
    ?dal_node
    ?loser_mode
    ?allow_degraded
    ?gc_frequency
    ?history_mode
    ?password_file
    mode
    (Node node)

let do_runlike_command ?event_level ?event_sections_levels ?password_file node
    arguments =
  if node.status <> Not_running then
    Test.fail "Smart contract rollup node %s is already running" node.name ;
  let on_terminate _status =
    trigger_ready node None ;
    unit
  in
  let arguments = make_command_arguments ?password_file node @ arguments in
  run
    ?runner:node.persistent_state.runner
    ?event_level
    ?event_sections_levels
    node
    {ready = false; level = Unknown}
    arguments
    ~on_terminate

let run ?(legacy = false) ?(restart = false) ?mode ?event_level
    ?event_sections_levels node rollup_address extra_arguments =
  let* () = if restart then terminate node else return () in
  let cmd =
    if legacy then
      let arguments = legacy_node_args node rollup_address in
      let arguments = add_missing_arguments ~arguments ~extra_arguments in
      ["run"] @ make_arguments arguments
    else
      let default_mode, cmd, arguments = node_args node rollup_address in
      let arguments = add_missing_arguments ~arguments ~extra_arguments in
      let final_mode =
        match mode with Some m -> string_of_mode m | None -> default_mode
      in
      ["run"; final_mode] @ cmd @ make_arguments arguments
  in
  do_runlike_command ?event_level ?event_sections_levels node cmd

let run ?legacy ?restart ?mode ?event_level ?event_sections_levels
    ?(wait_ready = true) node rollup_address arguments =
  let* () =
    run
      ?legacy
      ?restart
      ?mode
      ?event_level
      ?event_sections_levels
      node
      rollup_address
      arguments
  in
  let* () = if wait_ready then wait_for_ready node else unit in
  return ()

let change_node_and_restart ?event_level sc_rollup_node rollup_address node =
  let* () = terminate sc_rollup_node in
  sc_rollup_node.persistent_state.endpoint <- Node node ;
  run ?event_level sc_rollup_node rollup_address []

let change_node_mode sc_rollup_node mode =
  {
    sc_rollup_node with
    persistent_state = {sc_rollup_node.persistent_state with mode};
  }

let dump_durable_storage ~sc_rollup_node ~dump ?(block = "head") () =
  let cmd =
    [
      "dump";
      "durable";
      "storage";
      "into";
      dump;
      "--data-dir";
      sc_rollup_node.persistent_state.data_dir;
      "--block";
      block;
    ]
  in
  let process = spawn_command sc_rollup_node cmd in
  Process.check process

let export_snapshot ?(compress_on_the_fly = false) sc_rollup_node dir =
  let process =
    spawn_command
      sc_rollup_node
      ([
         "snapshot";
         "export";
         "--dest";
         dir;
         "--data-dir";
         data_dir sc_rollup_node;
       ]
      @ Cli_arg.optional_switch "compress-on-the-fly" compress_on_the_fly)
  in
  let parse process =
    let* output = Process.check_and_read_stdout process in
    match output =~* rex "Snapshot exported to ([^\n]*)" with
    | None -> Test.fail "Snapshot export failed"
    | Some filename -> return filename
  in
  Runnable.{value = process; run = parse}

let import_snapshot sc_rollup_node ~snapshot_file =
  let process =
    spawn_command
      sc_rollup_node
      [
        "snapshot";
        "import";
        snapshot_file;
        "--data-dir";
        data_dir sc_rollup_node;
      ]
  in
  Runnable.{value = process; run = Process.check}

let as_rpc_endpoint (t : t) =
  let state = t.persistent_state in
  let scheme = "http" in
  Endpoint.{scheme; host = state.rpc_host; port = state.rpc_port}

module RPC = struct
  module RPC_callers : RPC_core.CALLERS with type uri_provider := t = struct
    let call ?rpc_hooks ?log_request ?log_response_status ?log_response_body
        node rpc =
      RPC_core.call
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
        (as_rpc_endpoint node)
        rpc

    let call_raw ?rpc_hooks ?log_request ?log_response_status ?log_response_body
        node rpc =
      RPC_core.call_raw
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
        (as_rpc_endpoint node)
        rpc

    let call_json ?rpc_hooks ?log_request ?log_response_status
        ?log_response_body node rpc =
      RPC_core.call_json
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
        (as_rpc_endpoint node)
        rpc
  end

  include RPC_callers
end
