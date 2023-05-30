(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    arguments : string list;
    mutable pending_ready : unit option Lwt.u list;
    rpc_addr : string;
    rpc_port : int;
    rollup_node : Sc_rollup_node.t option;
    runner : Runner.t option;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "evm_proxy_server"

  let default_colors = Log.Color.[|FG.magenta|]
end

open Parameters
include Daemon.Make (Parameters)

let path = "./octez-evm-proxy-server"

let connection_arguments ?rpc_addr ?rpc_port rollup_node_endpoint =
  let open Cli_arg in
  let rpc_port =
    match rpc_port with None -> Port.fresh () | Some port -> port
  in
  ( [
      "--rollup-node-endpoint";
      rollup_node_endpoint;
      "--rpc-port";
      string_of_int rpc_port;
    ]
    @ optional_arg "--rpc-addr" Fun.id rpc_addr,
    Option.value ~default:"127.0.0.1" rpc_addr,
    rpc_port )

let trigger_ready sc_node value =
  let pending = sc_node.persistent_state.pending_ready in
  sc_node.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready proxy_server =
  (match proxy_server.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready proxy_server (Some ())

let event_ready_name = "evm_proxy_server_is_ready.v0"

let handle_event (proxy_server : t) {name; value = _; timestamp = _} =
  if name = event_ready_name then set_ready proxy_server else ()

let check_event proxy_server name promise =
  let* result = promise in
  match result with
  | None ->
      raise
        (Terminated_before_event
           {daemon = proxy_server.name; event = name; where = None})
  | Some x -> return x

let wait_for_ready proxy_server =
  match proxy_server.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      proxy_server.persistent_state.pending_ready <-
        resolver :: proxy_server.persistent_state.pending_ready ;
      check_event proxy_server event_ready_name promise

let create ?runner ?rpc_addr ?rpc_port rollup_node =
  let rollup_node_endpoint =
    match rollup_node with
    | None -> "mockup"
    | Some rollup_node -> Sc_rollup_node.endpoint rollup_node
  in
  let arguments, rpc_addr, rpc_port =
    connection_arguments ?rpc_addr ?rpc_port rollup_node_endpoint
  in
  let proxy_server =
    create
      ~path
      {arguments; pending_ready = []; rpc_addr; rpc_port; rollup_node; runner}
  in
  on_event proxy_server (handle_event proxy_server) ;
  proxy_server

let mockup ?runner ?rpc_addr ?rpc_port () =
  create ?runner ?rpc_addr ?rpc_port None

let create ?runner ?rpc_addr ?rpc_port rollup_node =
  create ?runner ?rpc_addr ?rpc_port (Some rollup_node)

let run proxy_server =
  let* () =
    run
      proxy_server
      {ready = false}
      (["run"] @ proxy_server.persistent_state.arguments)
  in
  let* () = wait_for_ready proxy_server in
  unit

let spawn_command proxy_server args =
  Process.spawn ?runner:proxy_server.persistent_state.runner path @@ args

let spawn_run proxy_server =
  spawn_command proxy_server (["run"] @ proxy_server.persistent_state.arguments)

let endpoint (proxy_server : t) =
  Format.sprintf
    "http://%s:%d"
    proxy_server.persistent_state.rpc_addr
    proxy_server.persistent_state.rpc_port

let init ?runner ?rpc_addr ?rpc_port rollup_node =
  let proxy_server = create ?runner ?rpc_addr ?rpc_port rollup_node in
  let* () = run proxy_server in
  return proxy_server

type request = {method_ : string; parameters : JSON.u}

let request_to_JSON {method_; parameters} : JSON.u =
  `O
    [
      ("jsonrpc", `String "2.0");
      ("method", `String method_);
      ("params", parameters);
      ("id", `String "0");
    ]

let build_request request =
  request_to_JSON request |> JSON.annotate ~origin:"evm_proxy_server"

let batch_requests requests =
  `A (List.map request_to_JSON requests)
  |> JSON.annotate ~origin:"evm_proxy_server"

(* We keep both encoding (with a single object or an array of objects) and both
   function on purpose, to ensure both encoding are supported by the server. *)
let call_evm_rpc proxy_server request =
  let endpoint = endpoint proxy_server in
  RPC.Curl.post endpoint (build_request request) |> Runnable.run

let batch_evm_rpc proxy_server requests =
  let endpoint = endpoint proxy_server in
  RPC.Curl.post endpoint (batch_requests requests) |> Runnable.run

let extract_result json = JSON.(json |-> "result")

let fetch_contract_code evm_proxy_server contract_address =
  let* code =
    call_evm_rpc
      evm_proxy_server
      {
        method_ = "eth_getCode";
        parameters = `A [`String contract_address; `String "latest"];
      }
  in
  return (extract_result code |> JSON.as_string)

type txpool_slot = {address : string; transactions : (int64 * JSON.t) list}

let txpool_content evm_proxy_server =
  let* txpool =
    call_evm_rpc
      evm_proxy_server
      {method_ = "txpool_content"; parameters = `A []}
  in
  Log.info "Result: %s" (JSON.encode txpool) ;
  let txpool = extract_result txpool in
  let parse field =
    let open JSON in
    let pool = txpool |-> field in
    (* `|->` returns `Null if the field does not exists, and `Null is
       interpreted as the empty list by `as_object`. As such, we must ensure the
       field exists. *)
    if is_null pool then Test.fail "%s must exists" field
    else
      pool |> as_object
      |> List.map (fun (address, transactions) ->
             let transactions =
               transactions |> as_object
               |> List.map (fun (nonce, tx) -> (Int64.of_string nonce, tx))
             in
             {address; transactions})
  in
  return (parse "pending", parse "queued")
