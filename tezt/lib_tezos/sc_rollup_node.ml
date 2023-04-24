(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type mode = Batcher | Custom | Maintenance | Observer | Operator | Accuser

module Parameters = struct
  type persistent_state = {
    data_dir : string;
    base_dir : string;
    operators : (string * string) list;
    default_operator : string option;
    rpc_host : string;
    rpc_port : int;
    mode : mode;
    dal_node : Dal_node.t option;
    mutable node : Node.t;
    mutable pending_ready : unit option Lwt.u list;
    mutable pending_level : (int * int option Lwt.u) list;
    runner : Runner.t option;
  }

  type session_state = {mutable ready : bool; mutable level : int known}

  let base_default_name = "sc-rollup-node"

  let default_colors = Log.Color.[|FG.gray; FG.magenta; FG.yellow; FG.green|]
end

open Parameters
include Daemon.Make (Parameters)

let string_of_mode = function
  | Observer -> "observer"
  | Batcher -> "batcher"
  | Maintenance -> "maintenance"
  | Operator -> "operator"
  | Custom -> "custom"
  | Accuser -> "accuser"

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

let name sc_node = sc_node.name

let rpc_host sc_node = sc_node.persistent_state.rpc_host

let rpc_port sc_node = sc_node.persistent_state.rpc_port

let endpoint sc_node =
  Printf.sprintf "http://%s:%d" (rpc_host sc_node) (rpc_port sc_node)

let data_dir sc_node = sc_node.persistent_state.data_dir

let base_dir sc_node = sc_node.persistent_state.base_dir

let operators_params sc_node =
  let acc =
    match sc_node.persistent_state.default_operator with
    | None -> []
    | Some operator -> [operator]
  in
  List.fold_left
    (fun acc (purpose, operator) ->
      String.concat ":" [purpose; operator] :: acc)
    acc
    sc_node.persistent_state.operators

let layer1_addr sc_node = Node.rpc_host sc_node.persistent_state.node

let layer1_port sc_node = Node.rpc_port sc_node.persistent_state.node

let spawn_command sc_node args =
  Process.spawn
    ?runner:sc_node.persistent_state.runner
    ~name:sc_node.name
    ~color:sc_node.color
    sc_node.path
  @@ ["--base-dir"; base_dir sc_node]
  @ args

let common_node_args ?loser_mode sc_node =
  [
    "--data-dir";
    data_dir sc_node;
    "--rpc-addr";
    rpc_host sc_node;
    "--rpc-port";
    string_of_int @@ rpc_port sc_node;
  ]
  @ (match loser_mode with None -> [] | Some mode -> ["--loser-mode"; mode])
  @
  match sc_node.persistent_state.dal_node with
  | None -> []
  | Some dal_node ->
      let endpoint =
        sf
          "http://%s:%d"
          (Dal_node.rpc_host dal_node)
          (Dal_node.rpc_port dal_node)
      in
      ["--dal-node"; endpoint]

let node_args ?loser_mode sc_node rollup_address =
  let mode = string_of_mode sc_node.persistent_state.mode in
  ( mode,
    ["for"; rollup_address; "with"; "operators"]
    @ operators_params sc_node
    @ common_node_args ?loser_mode sc_node )

let legacy_node_args ?loser_mode sc_node rollup_address =
  let mode = string_of_mode sc_node.persistent_state.mode in
  ["--mode"; mode; "--rollup"; rollup_address]
  @ common_node_args ?loser_mode sc_node

let spawn_config_init sc_node ?(force = false) ?loser_mode rollup_address =
  let mode, args = node_args ?loser_mode sc_node rollup_address in
  spawn_command sc_node @@ ["init"; mode; "config"] @ args
  @ if force then ["--force"] else []

let config_init sc_node ?force ?loser_mode rollup_address =
  let process = spawn_config_init sc_node ?force ?loser_mode rollup_address in
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
      check_event sc_node "smart_rollup_node_is_ready.v0" promise

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
        "sc_rollup_node_layer_1_new_head_processed.v0"
        ~where:("level >= " ^ string_of_int level)
        promise

let wait_sync sc_node ~timeout =
  let node_level = Node.get_level sc_node.persistent_state.node in
  wait_for_level ~timeout sc_node node_level

let handle_event sc_node {name; value; timestamp = _} =
  match name with
  | "smart_rollup_node_is_ready.v0" -> set_ready sc_node
  | "sc_rollup_node_layer_1_new_head_processed.v0" ->
      let level = JSON.(value |-> "level" |> as_int) in
      update_level sc_node level
  | _ -> ()

let create ~protocol ?runner ?path ?name ?color ?data_dir ~base_dir ?event_pipe
    ?(rpc_host = "127.0.0.1") ?rpc_port ?(operators = []) ?default_operator
    ?(dal_node : Dal_node.t option) mode (node : Node.t) =
  let name = match name with None -> fresh_name () | Some name -> name in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let rpc_port =
    match rpc_port with None -> Port.fresh () | Some port -> port
  in
  let path = Option.value ~default:(Protocol.sc_rollup_node protocol) path in
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
        rpc_host;
        rpc_port;
        operators;
        default_operator;
        mode;
        node;
        dal_node;
        pending_ready = [];
        pending_level = [];
        runner;
      }
  in
  on_event sc_node (handle_event sc_node) ;
  sc_node

let make_arguments node =
  List.filter_map Fun.id
  @@ [
       Some "--endpoint";
       Some
         (Printf.sprintf "http://%s:%d" (layer1_addr node) (layer1_port node));
       Some "--base-dir";
       Some (base_dir node);
     ]

let do_runlike_command ?event_level ?event_sections_levels node arguments =
  if node.status <> Not_running then
    Test.fail "Smart contract rollup node %s is already running" node.name ;
  let on_terminate _status =
    trigger_ready node None ;
    unit
  in
  let arguments = make_arguments node @ arguments in
  run
    ?runner:node.persistent_state.runner
    ?event_level
    ?event_sections_levels
    node
    {ready = false; level = Unknown}
    arguments
    ~on_terminate

let run ?(legacy = false) ?event_level ?event_sections_levels ?loser_mode node
    rollup_address extra_arguments =
  let cmd =
    if legacy then
      let args = legacy_node_args ?loser_mode node rollup_address in
      ["run"] @ args @ extra_arguments
    else
      let mode, args = node_args ?loser_mode node rollup_address in
      ["run"; mode] @ args @ extra_arguments
  in
  do_runlike_command ?event_level ?event_sections_levels node cmd

let run ?legacy ?event_level ?event_sections_levels ?loser_mode node
    rollup_address arguments =
  let* () =
    run
      ?legacy
      ?event_level
      ?event_sections_levels
      ?loser_mode
      node
      rollup_address
      arguments
  in
  let* () = wait_for_ready node in
  return ()

let change_node_and_restart ?event_level sc_rollup_node rollup_address node =
  let* () = terminate sc_rollup_node in
  sc_rollup_node.persistent_state.node <- node ;
  run ?event_level sc_rollup_node rollup_address []
