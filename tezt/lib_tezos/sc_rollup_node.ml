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

module Parameters = struct
  type persistent_state = {
    data_dir : string;
    rpc_host : string;
    rpc_port : int;
    node : Node.t;
    mutable pending_ready : unit option Lwt.u list;
    mutable pending_level : (int * int option Lwt.u) list;
  }

  type session_state = {mutable ready : bool; mutable level : int known}

  let base_default_name = "sc-rollup-node"

  let default_colors = Log.Color.[|FG.gray; FG.magenta; FG.yellow; FG.green|]
end

open Parameters
include Daemon.Make (Parameters)

let check_error ?exit_code ?msg sc_node =
  match sc_node.status with
  | Not_running ->
      Test.fail
        "Smart-contract rollup node %s is not running, it has no stderr"
        (name sc_node)
  | Running {process; _} -> Process.check_error ?exit_code ?msg process

let wait sc_node =
  match sc_node.status with
  | Not_running ->
      Test.fail
        "Smart-contract rollup node %s is not running, cannot wait for it to \
         terminate"
        (name sc_node)
  | Running {process; _} -> Process.wait process

let name sc_node = sc_node.name

let rpc_host sc_node = sc_node.persistent_state.rpc_host

let rpc_port sc_node = sc_node.persistent_state.rpc_port

let endpoint sc_node =
  Printf.sprintf "http://%s:%d" (rpc_host sc_node) (rpc_port sc_node)

let data_dir sc_node = sc_node.persistent_state.data_dir

let spawn_command sc_node =
  Process.spawn ~name:sc_node.name ~color:sc_node.color sc_node.path

let spawn_config_init sc_node rollup_address =
  spawn_command
    sc_node
    [
      "config";
      "init";
      "on";
      rollup_address;
      "--data-dir";
      data_dir sc_node;
      "--rpc-addr";
      rpc_host sc_node;
      "--rpc-port";
      string_of_int @@ rpc_port sc_node;
    ]

let config_init sc_node rollup_address =
  let process = spawn_config_init sc_node rollup_address in
  let* output = Process.check_and_read_stdout process in
  match
    output
    =~* rex "Smart-contract rollup node configuration written in ([^\n]*)"
  with
  | None ->
      failwith "Smart-contract rollup node configuration initialization failed"
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

let check_event ?where sc_node name promise =
  let* result = promise in
  match result with
  | None ->
      raise
        (Terminated_before_event {daemon = sc_node.name; event = name; where})
  | Some x -> return x

let wait_for_ready sc_node =
  match sc_node.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let (promise, resolver) = Lwt.task () in
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

let wait_for_level sc_node level =
  match sc_node.status with
  | Running {session_state = {level = Known current_level; _}; _}
    when current_level >= level ->
      return current_level
  | Not_running | Running _ ->
      let (promise, resolver) = Lwt.task () in
      sc_node.persistent_state.pending_level <-
        (level, resolver) :: sc_node.persistent_state.pending_level ;
      check_event
        sc_node
        "sc_rollup_node_layer_1_new_head.v0"
        ~where:("level >= " ^ string_of_int level)
        promise

let handle_event sc_node {name; value} =
  match name with
  | "sc_rollup_node_is_ready.v0" -> set_ready sc_node
  | "sc_rollup_node_layer_1_new_head_processed.v0" ->
      let level = JSON.(value |-> "level" |> as_int) in
      update_level sc_node level
  | _ -> ()

let create ?(path = Constant.sc_rollup_node) ?name ?color ?data_dir ?event_pipe
    ?(rpc_host = "127.0.0.1") ?rpc_port (node : Node.t) =
  let name = match name with None -> fresh_name () | Some name -> name in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let rpc_port =
    match rpc_port with None -> Port.fresh () | Some port -> port
  in
  let sc_node =
    create
      ~path
      ~name
      ?color
      ?event_pipe
      {
        data_dir;
        rpc_host;
        rpc_port;
        node;
        pending_ready = [];
        pending_level = [];
      }
  in
  on_event sc_node (handle_event sc_node) ;
  sc_node

let make_arguments node =
  [
    "--endpoint";
    Printf.sprintf
      "http://%s:%d"
      (Node.rpc_host node.persistent_state.node)
      (Node.rpc_port node.persistent_state.node);
  ]

let do_runlike_command node arguments =
  if node.status <> Not_running then
    Test.fail "Smart contract rollup node %s is already running" node.name ;
  let on_terminate _status =
    trigger_ready node None ;
    unit
  in
  let arguments = make_arguments node @ arguments in
  run node {ready = false; level = Unknown} arguments ~on_terminate

let run node =
  do_runlike_command node ["run"; "--data-dir"; node.persistent_state.data_dir]

let run node =
  let* () = run node in
  let* () = wait_for_ready node in
  return ()
