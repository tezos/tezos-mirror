(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
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

type time_between_blocks = Nothing | Time_between_blocks of float

type mode =
  | Observer of {initial_kernel : string; preimage_dir : string}
  | Sequencer of {
      initial_kernel : string;
      preimage_dir : string;
      private_rpc_port : int;
      time_between_blocks : time_between_blocks option;
      sequencer : string;
      genesis_timestamp : Client.timestamp option;
    }
  | Proxy of {devmode : bool}

module Per_level_map = Map.Make (Int)

module Parameters = struct
  type persistent_state = {
    arguments : string list;
    mutable pending_ready : unit option Lwt.u list;
    mutable last_injected_level : int;
    mutable pending_blueprint_injected : unit option Lwt.u list Per_level_map.t;
    mutable last_applied_level : int;
    mutable pending_blueprint_applied : unit option Lwt.u list Per_level_map.t;
    mode : mode;
    data_dir : string;
    rpc_addr : string;
    rpc_port : int;
    endpoint : string;
    runner : Runner.t option;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "evm_node"

  let default_colors = Log.Color.[|FG.green; FG.yellow; FG.cyan; FG.magenta|]
end

open Parameters
include Daemon.Make (Parameters)

let mode t = t.persistent_state.mode

let devmode t =
  match t.persistent_state.mode with
  | Observer _ | Sequencer _ -> true
  | Proxy {devmode; _} -> devmode

let is_sequencer t =
  match t.persistent_state.mode with
  | Sequencer _ -> true
  | Observer _ | Proxy _ -> false

let initial_kernel t =
  match t.persistent_state.mode with
  | Sequencer {initial_kernel; _} | Observer {initial_kernel; _} ->
      initial_kernel
  | Proxy _ ->
      Test.fail
        "Wrong argument: [initial_kernel] does not support the proxy node"

let can_apply_blueprint t =
  match t.persistent_state.mode with
  | Sequencer _ | Observer _ -> true
  | Proxy _ -> false

let connection_arguments ?rpc_addr ?rpc_port () =
  let open Cli_arg in
  let rpc_port =
    match rpc_port with None -> Port.fresh () | Some port -> port
  in
  ( ["--rpc-port"; string_of_int rpc_port]
    @ optional_arg "rpc-addr" Fun.id rpc_addr,
    Option.value ~default:Constant.default_host rpc_addr,
    rpc_port )

let trigger_ready sc_node value =
  let pending = sc_node.persistent_state.pending_ready in
  sc_node.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let trigger_blueprint_injected evm_node level =
  let pending = evm_node.persistent_state.pending_blueprint_injected in
  let pending_for_level = Per_level_map.find_opt level pending in
  evm_node.persistent_state.last_injected_level <- level ;
  evm_node.persistent_state.pending_blueprint_injected <-
    Per_level_map.remove level pending ;
  List.iter (fun pending -> Lwt.wakeup_later pending (Some ()))
  @@ Option.value ~default:[] pending_for_level

let trigger_blueprint_applied evm_node level =
  let pending = evm_node.persistent_state.pending_blueprint_applied in
  let pending_for_level = Per_level_map.find_opt level pending in
  evm_node.persistent_state.last_applied_level <- level ;
  evm_node.persistent_state.pending_blueprint_applied <-
    Per_level_map.remove level pending ;
  List.iter (fun pending -> Lwt.wakeup_later pending (Some ()))
  @@ Option.value ~default:[] pending_for_level

let set_ready evm_node =
  (match evm_node.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready evm_node (Some ())

let event_ready_name = "evm_node_is_ready.v0"

let mode_prefix = function true -> "evm_node_dev" | false -> "evm_node_prod"

let event_blueprint_injected_name devmode =
  mode_prefix devmode ^ "_blueprint_injection.v0"

let event_blueprint_applied_name devmode =
  mode_prefix devmode ^ "_blueprint_application.v0"

let handle_is_ready_event (evm_node : t) {name; value = _; timestamp = _} =
  if name = event_ready_name then set_ready evm_node else ()

let handle_blueprint_injected_event (evm_node : t) {name; value; timestamp = _}
    =
  if name = event_blueprint_injected_name (devmode evm_node) then
    trigger_blueprint_injected
      evm_node
      JSON.(value |> as_string |> int_of_string)
  else ()

let handle_blueprint_applied_event (evm_node : t) {name; value; timestamp = _} =
  if name = event_blueprint_applied_name (devmode evm_node) then
    trigger_blueprint_applied
      evm_node
      JSON.(value |> as_string |> int_of_string)
  else ()

let check_event ?timeout evm_node name promise =
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
        (Terminated_before_event
           {daemon = evm_node.name; event = name; where = None})
  | Error () ->
      Format.ksprintf
        failwith
        "Timeout waiting for event %s of %s"
        name
        evm_node.name

let wait_for_ready evm_node =
  match evm_node.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      evm_node.persistent_state.pending_ready <-
        resolver :: evm_node.persistent_state.pending_ready ;
      check_event evm_node event_ready_name promise

let wait_for_blueprint_injected ~timeout evm_node level =
  match evm_node.status with
  | Running {session_state = {ready = true; _}; _} when is_sequencer evm_node ->
      let current_level = evm_node.persistent_state.last_injected_level in
      if level <= current_level then unit
      else
        let promise, resolver = Lwt.task () in
        evm_node.persistent_state.pending_blueprint_injected <-
          Per_level_map.update
            level
            (fun pending -> Some (resolver :: Option.value ~default:[] pending))
            evm_node.persistent_state.pending_blueprint_injected ;
        check_event
          ~timeout
          evm_node
          (event_blueprint_injected_name (devmode evm_node))
          promise
  | Running {session_state = {ready = true; _}; _} ->
      failwith "EVM node is not a sequencer"
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      failwith "EVM node is not ready"

let wait_for_blueprint_applied ~timeout evm_node level =
  match evm_node.status with
  | Running {session_state = {ready = true; _}; _}
    when can_apply_blueprint evm_node ->
      let current_level = evm_node.persistent_state.last_applied_level in
      if level <= current_level then unit
      else
        let promise, resolver = Lwt.task () in
        evm_node.persistent_state.pending_blueprint_applied <-
          Per_level_map.update
            level
            (fun pending -> Some (resolver :: Option.value ~default:[] pending))
            evm_node.persistent_state.pending_blueprint_applied ;
        check_event
          ~timeout
          evm_node
          (event_blueprint_applied_name (devmode evm_node))
          promise
  | Running {session_state = {ready = true; _}; _} ->
      failwith "EVM node is not a sequencer"
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      failwith "EVM node is not ready"

let create ?runner ?(mode = Proxy {devmode = false}) ?data_dir ?rpc_addr
    ?rpc_port endpoint =
  let arguments, rpc_addr, rpc_port =
    connection_arguments ?rpc_addr ?rpc_port ()
  in
  let name = fresh_name () in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let evm_node =
    create
      ~path:(Uses.path Constant.octez_evm_node)
      ~name
      {
        arguments;
        pending_ready = [];
        last_injected_level = 0;
        pending_blueprint_injected = Per_level_map.empty;
        last_applied_level = 0;
        pending_blueprint_applied = Per_level_map.empty;
        mode;
        data_dir;
        rpc_addr;
        rpc_port;
        endpoint;
        runner;
      }
  in
  on_event evm_node (handle_is_ready_event evm_node) ;
  on_event evm_node (handle_blueprint_injected_event evm_node) ;
  on_event evm_node (handle_blueprint_applied_event evm_node) ;
  evm_node

let create ?runner ?mode ?data_dir ?rpc_addr ?rpc_port rollup_node =
  create ?mode ?runner ?data_dir ?rpc_addr ?rpc_port rollup_node

let data_dir evm_node = ["--data-dir"; evm_node.persistent_state.data_dir]

let run_args evm_node =
  let shared_args = data_dir evm_node @ evm_node.persistent_state.arguments in
  let mode_args =
    match evm_node.persistent_state.mode with
    | Proxy {devmode} ->
        ["run"; "proxy"; "with"; "endpoint"; evm_node.persistent_state.endpoint]
        @ Cli_arg.optional_switch "devmode" devmode
    | Sequencer
        {
          initial_kernel;
          preimage_dir;
          private_rpc_port;
          time_between_blocks;
          sequencer;
          genesis_timestamp;
        } ->
        [
          "run";
          "sequencer";
          "with";
          "endpoint";
          evm_node.persistent_state.endpoint;
          "signing";
          "with";
          sequencer;
          "--initial-kernel";
          initial_kernel;
          "--preimage-dir";
          preimage_dir;
          "--private-rpc-port";
          string_of_int private_rpc_port;
        ]
        @ Cli_arg.optional_arg
            "time-between-blocks"
            (function
              | Nothing -> "none"
              | Time_between_blocks f -> Format.sprintf "%.3f" f)
            time_between_blocks
        @ Cli_arg.optional_arg
            "genesis-timestamp"
            (fun timestamp ->
              Client.time_of_timestamp timestamp |> Client.Time.to_notation)
            genesis_timestamp
    | Observer {preimage_dir; initial_kernel} ->
        [
          "run";
          "observer";
          "with";
          "endpoint";
          evm_node.persistent_state.endpoint;
          "--preimage-dir";
          preimage_dir;
          "--initial-kernel";
          initial_kernel;
        ]
  in
  mode_args @ shared_args

let run ?(wait = true) ?(extra_arguments = []) evm_node =
  let* () =
    run
      ~event_level:`Debug
      evm_node
      {ready = false}
      (run_args evm_node @ extra_arguments)
  in
  let* () = if wait then wait_for_ready evm_node else unit in
  unit

let spawn_command evm_node args =
  Process.spawn
    ?runner:evm_node.persistent_state.runner
    (Uses.path Constant.octez_evm_node)
  @@ args

let spawn_run ?(extra_arguments = []) evm_node =
  spawn_command evm_node (run_args evm_node @ extra_arguments)

let endpoint ?(private_ = false) (evm_node : t) =
  let addr, port, path =
    if private_ then
      match evm_node.persistent_state.mode with
      | Sequencer {private_rpc_port; _} ->
          (Constant.default_host, private_rpc_port, "/private")
      | Proxy _ -> Test.fail "Proxy doesn't have a private RPC server"
      | Observer _ -> Test.fail "Observer doesn't have a private RPC server"
    else
      ( evm_node.persistent_state.rpc_addr,
        evm_node.persistent_state.rpc_port,
        "" )
  in
  Format.sprintf "http://%s:%d%s" addr port path

let init ?runner ?mode ?data_dir ?rpc_addr ?rpc_port rollup_node =
  let evm_node =
    create ?runner ?mode ?data_dir ?rpc_addr ?rpc_port rollup_node
  in
  let* () = run evm_node in
  return evm_node

let init_from_rollup_node_data_dir evm_node rollup_node =
  let rollup_node_data_dir = Sc_rollup_node.data_dir rollup_node in
  let process =
    spawn_command
      evm_node
      (["init"; "from"; "rollup"; "node"; rollup_node_data_dir]
      @ data_dir evm_node)
  in
  Process.check process

type request = {method_ : string; parameters : JSON.u}

let request_to_JSON {method_; parameters} : JSON.u =
  `O
    ([
       ("jsonrpc", `String "2.0");
       ("method", `String method_);
       ("id", `String "0");
     ]
    @ if parameters == `Null then [] else [("params", parameters)])

let build_request request =
  request_to_JSON request |> JSON.annotate ~origin:"evm_node"

let batch_requests requests =
  `A (List.map request_to_JSON requests) |> JSON.annotate ~origin:"evm_node"

(* We keep both encoding (with a single object or an array of objects) and both
   function on purpose, to ensure both encoding are supported by the server. *)
let call_evm_rpc ?(private_ = false) evm_node request =
  let endpoint = endpoint ~private_ evm_node in
  Curl.post endpoint (build_request request) |> Runnable.run

let batch_evm_rpc ?(private_ = false) evm_node requests =
  let endpoint = endpoint ~private_ evm_node in
  Curl.post endpoint (batch_requests requests) |> Runnable.run

let extract_result json = JSON.(json |-> "result")

let extract_error_message json = JSON.(json |-> "error" |-> "message")

let fetch_contract_code evm_node contract_address =
  let* code =
    call_evm_rpc
      evm_node
      {
        method_ = "eth_getCode";
        parameters = `A [`String contract_address; `String "latest"];
      }
  in
  return (extract_result code |> JSON.as_string)

type txpool_slot = {address : string; transactions : (int64 * JSON.t) list}

let txpool_content evm_node =
  let* txpool =
    call_evm_rpc evm_node {method_ = "txpool_content"; parameters = `A []}
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

let upgrade_payload ~root_hash ~activation_timestamp =
  let args =
    [
      "make";
      "upgrade";
      "payload";
      "with";
      "root";
      "hash";
      root_hash;
      "at";
      "activation";
      "timestamp";
      activation_timestamp;
    ]
  in
  let process = Process.spawn (Uses.path Constant.octez_evm_node) @@ args in
  Process.check_and_read_stdout process
