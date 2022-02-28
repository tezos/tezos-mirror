(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

module Constant = struct
  let tx_rollup_node = "./tezos-tx-rollup-node-alpha"
end

module Parameters = struct
  type persistent_state = {
    tezos_node : Node.t;
    client : Client.t;
    data_dir : string;
    runner : Runner.t option;
    operator : string;
    rollup_id : string;
    rollup_genesis : string;
    rpc_host : string;
    rpc_port : int;
    dormant_mode : bool;
    mutable pending_ready : unit option Lwt.u list;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "tx-rollup-node"

  let default_colors = Log.Color.[|FG.magenta; FG.yellow; FG.green; FG.cyan|]
end

open Parameters
include Daemon.Make (Parameters)

let rpc_host node = node.persistent_state.rpc_host

let rpc_port node = node.persistent_state.rpc_port

let data_dir node = node.persistent_state.data_dir

let endpoint node =
  Printf.sprintf "http://%s:%d" (rpc_host node) (rpc_port node)

let operator node = node.persistent_state.operator

let spawn_command node =
  Process.spawn ~name:node.name ~color:node.color node.path

let spawn_config_init node rollup_id rollup_genesis =
  spawn_command
    node
    [
      "config";
      "init";
      "on";
      "--operator";
      operator node;
      "--data-dir";
      data_dir node;
      "--rollup-id";
      rollup_id;
      "--rollup-genesis";
      rollup_genesis;
      "--rpc-addr";
      rpc_host node;
      "--rpc-port";
      string_of_int @@ rpc_port node;
    ]

let config_init node rollup_id rollup_genesis =
  let process = spawn_config_init node rollup_id rollup_genesis in
  let* output = Process.check_and_read_stdout process in
  match output =~* rex "Configuration written in ([^\n]*)" with
  | None -> failwith "Configuration initialization failed"
  | Some filename -> return filename

let check_event ?where node name promise =
  let* result = promise in
  match result with
  | None ->
      raise (Terminated_before_event {daemon = node.name; event = name; where})
  | Some x -> return x

let trigger_ready node value =
  let pending = node.persistent_state.pending_ready in
  node.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready node =
  (match node.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready node (Some ())

let handle_event node {name; value = _} =
  match name with "tx_rollup_node_is_ready.v0" -> set_ready node | _ -> ()

let wait_for_ready node =
  match node.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let (promise, resolver) = Lwt.task () in
      node.persistent_state.pending_ready <-
        resolver :: node.persistent_state.pending_ready ;
      check_event node "tx_rollup_node_is_ready.v0" promise

let create ?(path = Constant.tx_rollup_node) ?runner ?data_dir
    ?(addr = "127.0.0.1") ?port ?(dormant_mode = false) ?color ?event_pipe ?name
    ~rollup_id ~rollup_genesis ~operator client tezos_node =
  let name = match name with None -> fresh_name () | Some name -> name in
  let rpc_port = Option.fold ~none:Port.fresh ~some:Fun.const port () in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let tx_node =
    create
      ?runner
      ~path
      ~name
      ?color
      ?event_pipe
      {
        tezos_node;
        data_dir;
        rollup_id;
        rpc_host = addr;
        rpc_port;
        rollup_genesis;
        runner;
        operator;
        client;
        pending_ready = [];
        dormant_mode;
      }
  in
  on_event tx_node (handle_event tx_node) ;
  tx_node

let make_arguments node =
  [
    "--endpoint";
    Printf.sprintf
      "http://%s:%d"
      (Node.rpc_host node.persistent_state.tezos_node)
      (Node.rpc_port node.persistent_state.tezos_node);
  ]

let do_runlike_command node arguments =
  if node.status <> Not_running then
    Test.fail "Transaction rollup node %s is already running" node.name ;
  let on_terminate _status =
    trigger_ready node None ;
    unit
  in
  let arguments = make_arguments node @ arguments in
  run node {ready = false} arguments ~on_terminate

let run node =
  do_runlike_command node ["run"; "--data-dir"; node.persistent_state.data_dir]
