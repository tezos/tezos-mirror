(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Parameters = struct
  type persistent_state = {
    runner : Runner.t option;
    uri : Uri.t;
    mutable pending_ready : unit option Lwt.u list;
    data_dir : string;
    node : Node.t;
    client : Client.t;
    endpoint : Client.endpoint;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "injector"

  let default_uri () =
    Uri.make ~scheme:"http" ~host:Constant.default_host ~port:(Port.fresh ()) ()

  let default_colors =
    Log.Color.[|BG.green ++ FG.blue; BG.green ++ FG.gray; BG.green ++ FG.blue|]
end

open Parameters
include Daemon.Make (Parameters)

let trigger_ready injector value =
  let pending = injector.persistent_state.pending_ready in
  injector.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready injector =
  (match injector.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready injector (Some ())

let handle_readiness injector (event : event) =
  if event.name = "injector_listening.v0" then set_ready injector

let rpc_host injector =
  Uri.host_with_default
    ~default:Constant.default_host
    injector.persistent_state.uri

let rpc_port injector = Option.get @@ Uri.port injector.persistent_state.uri

let as_rpc_endpoint (t : t) =
  let host = rpc_host t in
  let port = rpc_port t in
  Endpoint.{scheme = "http"; host; port}

let data_dir injector = injector.persistent_state.data_dir

let spawn_command injector =
  Process.spawn ~name:injector.name ~color:injector.color injector.path

let spawn_config_init injector signer =
  let signer = Account.(signer.public_key_hash) in
  let host_args = ["--address"; rpc_host injector] in
  let port_args =
    match Uri.port injector.persistent_state.uri with
    | None -> []
    | Some port -> ["--port"; Int.to_string port]
  in
  let block_delay_args = ["--block-delay"; Float.to_string 0.1] in
  let data_dir_args = ["--data-dir"; data_dir injector] in
  let base_dir_args =
    ["--base-dir"; Client.base_dir injector.persistent_state.client]
  in
  let arguments =
    base_dir_args @ ["init-config"] @ [signer] @ host_args @ port_args
    @ block_delay_args @ data_dir_args
  in
  spawn_command injector arguments

let init_config injector (signer : Account.key) =
  let process = spawn_config_init injector signer in
  let* output = Process.check_and_read_stdout process in
  match output =~* rex "Injector server configuration written in ([^\n]*)" with
  | None -> failwith "Injector configuration initialization failed"
  | Some filename -> return filename

let create ?name ?color ?data_dir ?event_pipe ?uri ?runner node client =
  let name = match name with None -> fresh_name () | Some name -> name in
  let uri =
    match uri with None -> Parameters.default_uri () | Some uri -> uri
  in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let endpoint = Client.Node node in
  let injector =
    create
      ~path:(Uses.path Constant.octez_injector_server)
      ?name:(Some name)
      ?color
      ?event_pipe
      ?runner
      {runner; uri; pending_ready = []; data_dir; endpoint; node; client}
  in
  on_event injector (handle_readiness injector) ;
  injector

let run injector =
  (match injector.status with
  | Not_running -> ()
  | Running _ -> Test.fail "injector %s is already running" injector.name) ;
  let runner = injector.persistent_state.runner in
  let base_dir_args =
    ["--base-dir"; Client.base_dir injector.persistent_state.client]
  in
  let data_dir = injector.persistent_state.data_dir in
  let endpoint_args =
    [
      "--endpoint";
      Client.(string_of_endpoint injector.persistent_state.endpoint);
    ]
  in
  let arguments =
    base_dir_args @ endpoint_args @ ["run"; "--data-dir"; data_dir]
  in
  let on_terminate _ =
    (* Cancel all [Ready] event listeners. *)
    trigger_ready injector None ;
    unit
  in
  run injector {ready = false} arguments ~on_terminate ?runner

module RPC = struct
  let call ?rpc_hooks ?log_request ?log_response_status ?log_response_body node
      rpc =
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

  let call_json ?rpc_hooks ?log_request ?log_response_status ?log_response_body
      node rpc =
    RPC_core.call_json
      ?rpc_hooks
      ?log_request
      ?log_response_status
      ?log_response_body
      (as_rpc_endpoint node)
      rpc

  type status =
    | Pending
    | Injected of {injected_oph : string; injected_op_index : int}
    | Included of {
        included_oph : string;
        included_op_index : int;
        block : string;
        level : int;
      }

  let make ?data ?query_string = RPC_core.make ?data ?query_string

  let add_pending_transaction ?parameters amount destination =
    let operation =
      `O
        ([
           ("kind", `String "transaction");
           ("amount", `String (Int64.to_string amount));
           ("destination", `String destination);
         ]
        @
        match parameters with
        | Some (entrypoint, value) ->
            [
              ( "parameters",
                `O
                  [("entrypoint", `String entrypoint); ("value", `String value)]
              );
            ]
        | None -> [])
    in
    let data : RPC_core.data = Data operation in
    make ~data POST ["add_pending_transaction"] JSON.as_string

  let operation_status op_hash =
    let query_string = [("op_hash", op_hash)] in
    make ~query_string GET ["operation_status"] (fun json ->
        Option.map
          (fun status ->
            let open JSON in
            match as_object status with
            | [("pending", _)] -> Pending
            | [("injected_oph", oph); ("injected_op_index", op_index)] ->
                Injected
                  {
                    injected_oph = oph |> as_string;
                    injected_op_index = op_index |> as_int;
                  }
            | [
             ("included_oph", oph);
             ("included_op_index", op_index);
             ("block", block);
             ("level", level);
            ] ->
                Included
                  {
                    included_oph = oph |> as_string;
                    included_op_index = op_index |> as_int;
                    block = block |> as_string;
                    level = level |> as_int;
                  }
            | _ -> assert false)
          (JSON.as_opt json))

  let inject () = make GET ["inject"] (fun _ -> ())
end
