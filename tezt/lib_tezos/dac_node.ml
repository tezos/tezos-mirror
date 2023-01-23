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

let rpc_host dac_node = dac_node.persistent_state.rpc_host

let rpc_port dac_node = dac_node.persistent_state.rpc_port

let layer1_addr dac_node = Node.rpc_host dac_node.persistent_state.node

let layer1_port dac_node = Node.rpc_port dac_node.persistent_state.node

let endpoint dac_node =
  Printf.sprintf "http://%s:%d" (rpc_host dac_node) (rpc_port dac_node)

let data_dir dac_node = dac_node.persistent_state.data_dir

let spawn_command dac_node =
  Process.spawn ~name:dac_node.name ~color:dac_node.color dac_node.path

let spawn_config_init dac_node =
  spawn_command dac_node
  @@ [
       "init-config";
       "--data-dir";
       data_dir dac_node;
       "--rpc-port";
       string_of_int (rpc_port dac_node);
       "--rpc-addr";
       rpc_host dac_node;
     ]

let init_config dac_node =
  let process = spawn_config_init dac_node in
  let* output = Process.check_and_read_stdout process in
  match output =~* rex "DAC node configuration written in ([^\n]*)" with
  | None -> failwith "DAC node configuration initialization failed"
  | Some filename -> return filename

module Dac = struct
  let spawn_set_parameters ?threshold ?reveal_data_dir dac_node =
    let threshold_arg =
      match threshold with
      | None -> []
      | Some threshold -> ["--threshold"; Int.to_string threshold]
    in
    let reveal_data_dir_arg =
      match reveal_data_dir with
      | None -> []
      | Some reveal_data_dir -> ["--reveal-data-dir"; reveal_data_dir]
    in
    let data_dir_arg = ["--data-dir"; data_dir dac_node] in
    spawn_command dac_node
    @@ ["set"; "dac"; "parameters"]
    @ threshold_arg @ reveal_data_dir_arg @ data_dir_arg

  let set_parameters ?threshold ?reveal_data_dir dac_node =
    spawn_set_parameters ?threshold ?reveal_data_dir dac_node |> Process.check

  let spawn_add_committee_member ~address dac_node =
    let base_dir_argument =
      ["--base-dir"; Client.base_dir dac_node.persistent_state.client]
    in
    spawn_command
      dac_node
      (base_dir_argument
      @ ["add"; "data"; "availability"; "committee"; "member"]
      @ [address]
      @ ["--data-dir"; dac_node.persistent_state.data_dir])

  let add_committee_member ~address dac_node =
    spawn_add_committee_member ~address dac_node |> Process.check
end

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
    ?(rpc_host = "127.0.0.1") ?rpc_port ~node ~client () =
  let name = match name with None -> fresh_name () | Some name -> name in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
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
      {data_dir; rpc_host; rpc_port; pending_ready = []; node; client}
  in
  on_event dac_node (handle_event dac_node) ;
  dac_node

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
