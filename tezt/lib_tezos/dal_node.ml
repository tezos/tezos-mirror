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
    node : Node.t;
    client : Client.t;
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

let listen_addr dal_node = dal_node.persistent_state.listen_addr

let layer1_addr dal_node = Node.rpc_host dal_node.persistent_state.node

let layer1_port dal_node = Node.rpc_port dal_node.persistent_state.node

let endpoint dal_node =
  Printf.sprintf "http://%s:%d" (rpc_host dal_node) (rpc_port dal_node)

let data_dir dal_node = dal_node.persistent_state.data_dir

let spawn_command dal_node =
  Process.spawn ~name:dal_node.name ~color:dal_node.color dal_node.path

let spawn_config_init ?(use_unsafe_srs = true) ?(expected_pow = 0.) dal_node =
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
         (if use_unsafe_srs then Some "--use-unsafe-srs-for-tests" else None);
         Some "--expected-pow";
         Some (string_of_float expected_pow);
       ]

module Config_file = struct
  let filename dal_node = sf "%s/config.json" @@ data_dir dal_node

  let read dal_node = JSON.parse_file (filename dal_node)

  let write dal_node config = JSON.encode_to_file (filename dal_node) config

  let update dal_node update = read dal_node |> update |> write dal_node
end

let init_config ?use_unsafe_srs ?expected_pow dal_node =
  let process = spawn_config_init ?use_unsafe_srs ?expected_pow dal_node in
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

let create ?(path = Constant.dal_node) ?name ?color ?data_dir ?event_pipe
    ?(rpc_host = "127.0.0.1") ?rpc_port ?listen_addr ~node ~client () =
  let name = match name with None -> fresh_name () | Some name -> name in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let rpc_port =
    match rpc_port with None -> Port.fresh () | Some port -> port
  in
  let listen_addr =
    match listen_addr with
    | None -> Format.sprintf "127.0.0.1:%d" @@ Port.fresh ()
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
        pending_ready = [];
        node;
        client;
      }
  in
  on_event dal_node (handle_event dal_node) ;
  dal_node

let make_arguments node =
  [
    "--endpoint";
    Printf.sprintf "http://%s:%d" (layer1_addr node) (layer1_port node);
  ]

let do_runlike_command ?env node arguments =
  if node.status <> Not_running then
    Test.fail "DAL node %s is already running" node.name ;
  let on_terminate _status =
    trigger_ready node None ;
    unit
  in
  let arguments = arguments @ make_arguments node in
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
