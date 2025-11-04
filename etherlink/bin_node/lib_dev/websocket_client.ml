(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Rpc_encodings

type response =
  | Response of JSONRPC.response
  | Notification of Subscription.notification

module Request_table = Hashtbl.Make (struct
  type t = JSONRPC.id

  let equal = ( = )

  let hash = Hashtbl.hash
end)

module Subscription_table = Hashtbl.Make (struct
  type t = Ethereum_types.Subscription.id

  let equal (Ethereum_types.Subscription.Id (Hex id1))
      (Ethereum_types.Subscription.Id (Hex id2)) =
    String.equal id1 id2

  let hash = Hashtbl.hash
end)

type monitoring = {ping_timeout : float; ping_interval : float}

type conn = {
  media : Media_type.t;
  conn : Websocket_lwt_unix.conn;
  pending_requests :
    (((JSONRPC.value, exn) result -> unit) * Opentelemetry.Scope.t)
    Request_table.t;
  subscriptions : (Data_encoding.json option -> unit) Subscription_table.t;
  binary : bool;
  process : unit Lwt.t;
}

type t = {
  media : Media_type.t;
  monitoring : monitoring option;
  uri : Uri.t;
  keep_alive : bool;
  client_id : int;
  mutable id : int;
  mutable current : conn Lwt.t option;
}

type 'a subscription = {
  stream : 'a Lwt_stream.t;
  unsubscribe : unit -> bool tzresult Lwt.t;
}

type timeout = {
  timeout : float;
  on_timeout : [`Retry of int | `Retry_forever | `Fail];
}

exception Timeout of float

exception Connection_closed

module Event = struct
  include Internal_event.Simple

  let section = ["evm"; "client"; "websocket"]

  let exn =
    let open Data_encoding in
    conv Printexc.to_string (fun s -> Failure s) string

  let pp_exn fmt e = Format.pp_print_string fmt (Printexc.to_string e)

  let connected =
    declare_0
      ~section
      ~name:"websocket_client_connected"
      ~msg:"websocket client connected"
      ~level:Info
      ()

  let disconnecting =
    declare_0
      ~section
      ~name:"websocket_client_disconnecting"
      ~msg:"disconnecting websocket client"
      ~level:Debug
      ()

  let disconnected =
    declare_0
      ~section
      ~name:"websocket_client_disconnected"
      ~msg:"websocket client disconnected"
      ~level:Debug
      ()

  let disconnection_error =
    declare_1
      ~section
      ~name:"websocket_client_disconnection_error"
      ~msg:"websocket client disconnection error {exn}"
      ~level:Debug
      ("exn", exn)
      ~pp1:pp_exn

  let decoding_error =
    declare_2
      ~section
      ~name:"websocket_client_decoding_error"
      ~msg:"websocket client decoding error {error1}, {error2}"
      ~level:Error
      ("error1", Data_encoding.string)
      ("error2", Data_encoding.string)
      ~pp1:Format.pp_print_string
      ~pp2:Format.pp_print_string

  let connection_closed =
    declare_1
      ~section
      ~name:"websocket_client_connection_closed"
      ~msg:"websocket client connection was closed because {exn}"
      ~level:Notice
      ("exn", exn)
      ~pp1:pp_exn

  let received_frame =
    declare_1
      ~section
      ~name:"websocket_client_received_frame"
      ~msg:"websocket client received {frame}"
      ~level:Debug
      ("frame", Websocket_encodings.frame_encoding)
      ~pp1:Websocket.Frame.pp

  let send_frame =
    declare_1
      ~section
      ~name:"websocket_client_send_frame"
      ~msg:"websocket client sending {frame}"
      ~level:Debug
      ("frame", Websocket_encodings.frame_encoding)
      ~pp1:Websocket.Frame.pp

  let ignored_message =
    declare_1
      ~section
      ~name:"websocket_client_ignored_message"
      ~msg:"websocket client ignored message {message}"
      ~level:Warning
      ("message", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let timeout_send_recv =
    declare_2
      ~section
      ~name:"websocket_client_timeout_send_recv"
      ~msg:
        "websocket client did not receive a response after {timeout}s for \
         request {message}"
      ~level:Warning
      ("message", Data_encoding.string)
      ("timeout", Data_encoding.float)
      ~pp1:Format.pp_print_string

  let timeout_retry =
    declare_1
      ~section
      ~name:"websocket_client_timeout_retry"
      ~msg:"websocket client will retry sending request {remaining_attempts}"
      ~level:Warning
      ( "remaining_attempts",
        Data_encoding.(
          conv
            (function `Retry n -> Some n | `Retry_forever -> None)
            (function None -> `Retry_forever | Some n -> `Retry n)
            (option int31)) )
      ~pp1:(fun ppf -> function
        | `Retry n -> Format.fprintf ppf "with %d remaining attempts" n
        | `Retry_forever -> ())
end

type error +=
  | No_response of JSONRPC.request
  | Request_failed of JSONRPC.request * JSONRPC.error
  | Cannot_destruct of
      Ethereum_types.Subscription.kind * Ethereum_types.Subscription.id * string

let () =
  register_error_kind
    `Temporary
    ~id:"websocket_client.no_repsonse"
    ~title:"No response for the JSONRPC request"
    ~description:"No response for the JSONRPC request."
    ~pp:(fun ppf r ->
      Format.fprintf
        ppf
        "No response on websocket for request %a"
        Data_encoding.Json.pp
        (Data_encoding.Json.construct JSONRPC.request_encoding r))
    Data_encoding.(obj1 (req "request" JSONRPC.request_encoding))
    (function No_response r -> Some r | _ -> None)
    (fun r -> No_response r) ;
  register_error_kind
    `Temporary
    ~id:"websocket_client.request_failed"
    ~title:"JSONRPC request failed"
    ~description:"JSONRPC request failed."
    ~pp:(fun ppf (r, e) ->
      Format.fprintf
        ppf
        "JSONRPC request %a on websocket failed for %a"
        Data_encoding.Json.pp
        (Data_encoding.Json.construct JSONRPC.request_encoding r)
        Data_encoding.Json.pp
        (Data_encoding.Json.construct JSONRPC.error_encoding e))
    Data_encoding.(
      obj2
        (req "request" JSONRPC.request_encoding)
        (req "error" JSONRPC.error_encoding))
    (function Request_failed (r, e) -> Some (r, e) | _ -> None)
    (fun (r, e) -> Request_failed (r, e)) ;
  register_error_kind
    `Temporary
    ~id:"websocket_client.cannot_destruct"
    ~title:"Cannot destruct subscription notification"
    ~description:"Cannot destruct subscription notification."
    ~pp:(fun ppf (k, id, err) ->
      let (Ethereum_types.Subscription.Id (Hex id)) = id in
      Format.fprintf
        ppf
        "Cannot destruct notification for subscription %s of id 0x%s: %s"
        (Ezjsonm.value_to_string
           ~minify:true
           (Data_encoding.Json.construct
              Ethereum_types.Subscription.kind_encoding
              k))
        id
        err)
    Data_encoding.(
      obj3
        (req "kind" Ethereum_types.Subscription.kind_encoding)
        (req "id" Ethereum_types.Subscription.id_encoding)
        (req "err" string))
    (function Cannot_destruct (k, id, e) -> Some (k, id, e) | _ -> None)
    (fun (k, id, e) -> Cannot_destruct (k, id, e))

module Websocket_lwt_unix = struct
  include Websocket_lwt_unix

  let write conn fr =
    let open Lwt_syntax in
    let* () = Event.(emit send_frame) fr in
    Websocket_lwt_unix.write conn fr

  let read conn =
    let open Lwt_syntax in
    let* fr = Websocket_lwt_unix.read conn in
    let* () = Event.(emit received_frame) fr in
    return fr
end

let is_binary media =
  match Media_type.name media with
  | "application/octet-stream" -> true
  | _ -> false

let new_id client =
  let id = client.id in
  client.id <- client.id + 1 ;
  JSONRPC.Id_float (float_of_int id)

let parse_message (media : Media_type.t) message =
  let open Lwt_syntax in
  match media.destruct JSONRPC.response_encoding message with
  | Ok resp -> return_some (Response resp)
  | Error e1 -> (
      match media.destruct Subscription.notification_encoding message with
      | Ok notif -> return_some (Notification notif)
      | Error e2 ->
          let* () = Event.(emit decoding_error) (e1, e2) in
          return_none)

let encrich_response_scope scope (value : JSONRPC.value) =
  Opentelemetry.Scope.add_event scope (fun () ->
      Opentelemetry_lwt.Event.make "received_response") ;
  Opentelemetry.Scope.add_attrs scope (fun () ->
      match value with
      | Ok _ -> []
      | Error {code; message; _} ->
          Opentelemetry.Scope.set_status
            scope
            {message; code = Status_code_error} ;
          [
            ("rpc.jsonrpc.error_code", `Int code);
            ("rpc.jsonrpc.error_message", `String message);
          ])

let handle_response pending_requests subscriptions message =
  let open Lwt_syntax in
  function
  | Response {id; value} -> (
      match Request_table.find pending_requests id with
      | None -> Event.(emit ignored_message) message
      | Some (callback, scope) ->
          encrich_response_scope scope value ;
          callback (Ok value) ;
          return_unit)
  | Notification {params = {result; subscription}} -> (
      match Subscription_table.find subscriptions subscription with
      | None -> Event.(emit ignored_message) message
      | Some callback ->
          callback (Some result) ;
          return_unit)

let handle_message media pending_requests subscriptions message =
  let open Lwt_syntax in
  let* response = parse_message media message in
  Option.iter_s
    (handle_response pending_requests subscriptions message)
    response

let disconnect ?(status = Websocket_encodings.Normal_closure) conn =
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let* () = Event.(emit disconnecting) () in
      let* () =
        Websocket_lwt_unix.write
          conn
          (Websocket.Frame.close
             (Websocket_encodings.code_of_close_status status))
      in
      let* () = Websocket_lwt_unix.close_transport conn in
      let* () = Event.(emit disconnected) () in
      return_unit)
    (fun e ->
      let* () = Event.(emit disconnection_error) e in
      return_unit)

let monitor_connection ~ping_timeout ~ping_interval conn monitor_mbox =
  let open Lwt_syntax in
  let rec loop ~push ping_counter =
    let* () =
      if push then
        Websocket_lwt_unix.write
          conn
          (Websocket.Frame.create
             ~opcode:Ping
             ~content:(string_of_int ping_counter)
             ())
      else return_unit
    in
    let* res =
      Lwt.pick
        [
          (let+ c = Lwt_mvar.take monitor_mbox in
           `Pong c);
          (let+ () = Lwt_unix.sleep ping_timeout in
           `Timeout);
        ]
    in
    match res with
    | `Pong c ->
        if c = ping_counter then
          let* () = Lwt_unix.sleep ping_interval in
          loop ~push:true (ping_counter + 1)
        else (* ignore *)
          loop ~push:false ping_counter
    | `Timeout -> raise (Timeout ping_timeout)
  in
  loop ~push:true 0

let is_tls_service = function "wss" | "https" | "imaps" -> true | _ -> false

let system_service name =
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let* s =
        (* WebSocket connections (ws:// and wss://) use HTTP ports (80
           and 443 respectively) but aren't registered as separate TCP
           services in the system service database. *)
        let name' =
          match name with "ws" -> "http" | "wss" -> "https" | _ -> name
        in
        Lwt_unix.getservbyname name' "tcp"
      in
      let tls = is_tls_service name in
      let svc = {Resolver.name; port = s.Lwt_unix.s_port; tls} in
      Lwt.return (Some svc))
    (function Not_found -> Lwt.return_none | e -> Lwt.reraise e)

let resolver =
  let service = system_service in
  let rewrites = [("", Resolver_lwt_unix.system_resolver)] in
  Resolver_lwt.init ~service ~rewrites ()

let rec endp_peer_port (endp : [< Conduit.endp]) =
  match endp with
  | `TCP (addr, port) -> (Ipaddr.to_string addr, Some (`Int port))
  | `TLS (_tunnel, endp) -> endp_peer_port endp
  | `Vchan_domain_socket (name, port) -> (name, Some (`String port))
  | `Unix_domain_socket socket -> (socket, None)
  | `Unknown name -> (name, None)
  | `Vchan_direct (dom_id, port) ->
      (Format.sprintf "Domain(%d)" dom_id, Some (`String port))

let network_attributes endp =
  let peer_addr, peer_port = endp_peer_port endp in
  [("network.peer.address", `String peer_addr)]
  @ Option.(to_list (map (fun port -> ("network.peer.port", port)) peer_port))

let connect ?monitoring ?(on_close = Fun.id) media uri =
  let open Lwt_syntax in
  Opentelemetry_lwt.Trace.with_ ~service_name:"Websocket_client" "connect"
  @@ fun scope ->
  let* endp = Resolver_lwt.resolve_uri ~uri resolver in
  Opentelemetry.Scope.add_attrs scope (fun () ->
      Octez_telemetry.HTTP_client.span_attributes [media] `GET uri
      @ network_attributes endp) ;
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let* client = Conduit_lwt_unix.endp_to_client ~ctx endp in
  let accept_header = Tezos_rpc_http.Media_type.accept_header [media] in
  let content_header = Tezos_rpc_http.Media_type.acceptable_encoding [media] in
  let extra_headers =
    Cohttp.Header.of_list
      [("Accept", accept_header); ("Content-type", content_header)]
  in
  let* conn = Websocket_lwt_unix.connect ~ctx client uri ~extra_headers in
  let* () = Event.(emit connected) () in
  let message_buffer = Buffer.create 256 in
  let monitor =
    match monitoring with
    | None -> None
    | Some {ping_timeout; ping_interval} ->
        let monitor_mbox = Lwt_mvar.create_empty () in
        let monitor =
          monitor_connection ~ping_timeout ~ping_interval conn monitor_mbox
        in
        Some (monitor_mbox, monitor)
  in
  let get_frame () =
    Lwt.catch
      (fun () ->
        (* We need to pause explicitly here to allow for the subscription to
           register its callback after the initial request and before we process
           the next frame. *)
        let* () = Lwt.pause () in
        let read = Websocket_lwt_unix.read conn in
        let* frame =
          match monitor with
          | None -> read
          | Some (_, timeout) -> Lwt.choose [read; timeout]
        in
        match frame.opcode with
        | Close ->
            let* () = disconnect conn in
            return_none
        | _ -> return_some frame)
      (fun e ->
        let* () = Event.(emit connection_closed) e in
        let* () = disconnect ~status:Going_away conn in
        return_none)
  in
  let subscriptions = Subscription_table.create 7 in
  let pending_requests = Request_table.create 7 in
  let process_frame = function
    | {Websocket.Frame.opcode = Ping; content; _} ->
        (* We must answer ping frames from the server with a pong frame
           with the same content. *)
        let* () =
          Websocket_lwt_unix.write
            conn
            (Websocket.Frame.create ~opcode:Pong ~content ())
        in
        return_unit
    | {opcode = Pong; content; _} ->
        let* () =
          match monitor with
          | None -> return_unit
          | Some (mbox, _) -> (
              match int_of_string_opt content with
              | None -> return_unit
              | Some i -> Lwt_mvar.put mbox i)
        in
        return_unit
    | {opcode = Close; _} ->
        (* Cannot happen because frame_stream is closed when we receive
           this opcode. *)
        return_unit
    | {opcode = Text | Binary; content; final = false; _} ->
        (* New fragmented message *)
        Buffer.clear message_buffer ;
        Buffer.add_string message_buffer content ;
        return_unit
    | {opcode = Continuation; content; final = false; _} ->
        (* Non final fragment of message, we add the content to the
           buffer *)
        Buffer.add_string message_buffer content ;
        return_unit
    | {opcode = Text | Binary; content; final = true; _} ->
        (* Complete message in frame *)
        handle_message media pending_requests subscriptions content
    | {opcode = Continuation; content; final = true; _} ->
        (* Final data frame of fragmented message, the complete message is
           the buffer + new content *)
        let message = Buffer.contents message_buffer ^ content in
        handle_message media pending_requests subscriptions message
    | {opcode = Ctrl _ | Nonctrl _; _} ->
        (* Ignore other frames *)
        return_unit
  in
  let process =
    let rec loop () =
      let* frame = get_frame () in
      match frame with
      | None ->
          on_close () ;
          return_unit
      | Some frame ->
          let* () = process_frame frame in
          loop ()
    in
    let* () = loop () in
    Request_table.to_seq_values pending_requests
    |> List.of_seq
    |> List.iter (fun (callback, _) -> callback (Error Connection_closed)) ;
    Subscription_table.to_seq_values subscriptions
    |> List.of_seq
    |> List.iter (fun push -> push None) ;
    Request_table.clear pending_requests ;
    Subscription_table.clear subscriptions ;
    return_unit
  in
  return
    {
      media;
      conn;
      process;
      binary = is_binary media;
      pending_requests;
      subscriptions;
    }

let disconnect client =
  let open Lwt_syntax in
  match client.current with
  | None -> return_unit
  | Some current ->
      let* current in
      disconnect current.conn

let get_conn ({media; monitoring; uri; current; _} as client) =
  match current with
  | Some conn -> conn
  | None ->
      let conn =
        connect ?monitoring media uri ~on_close:(fun () ->
            client.current <- None)
      in
      client.current <- Some conn ;
      conn

let client_id {client_id; _} = client_id

let next_client_id = ref 0

let create ?monitoring ?(keep_alive = true) media uri =
  incr next_client_id ;
  let client =
    {
      media;
      monitoring;
      uri;
      keep_alive;
      client_id = !next_client_id;
      id = 0;
      current = None;
    }
  in
  let conn =
    connect ?monitoring media uri ~on_close:(fun () -> client.current <- None)
  in
  client.current <- Some conn ;
  client

let connect client =
  let open Lwt_result_syntax in
  protect @@ fun () ->
  let*! (_ : conn) = get_conn client in
  return_unit

let simple_write conn scope opcode content =
  Opentelemetry_lwt.Trace.with_
    ~service_name:"Websocket_client"
    ~kind:Span_kind_client
    ~scope
    "write"
  @@ fun scope ->
  Opentelemetry.Scope.add_attrs scope (fun () ->
      [
        ("message.size", `Int (String.length content));
        ( "websocket.frame.opcode",
          `String (Websocket.Frame.Opcode.to_string opcode) );
      ]) ;
  Websocket_lwt_unix.write conn (Websocket.Frame.create ~opcode ~content ())

let retry_write_until ~timeout ~on_timeout response conn scope opcode content =
  let open Lwt_syntax in
  let rec retry on_timeout =
    let* () = simple_write conn scope opcode content in
    let timeout_p =
      let+ () = Lwt_unix.sleep timeout in
      Error `Timeout
    in
    let response =
      let+ res = response in
      Ok res
    in
    let* res = Lwt.choose [response; timeout_p] in
    match res with
    | Ok res ->
        Lwt.cancel timeout_p ;
        return res
    | Error `Timeout -> (
        let* () = Event.(emit timeout_send_recv) (content, timeout) in
        match on_timeout with
        | `Fail -> raise (Timeout timeout)
        | `Retry n when n <= 0 -> raise (Timeout timeout)
        | (`Retry _ | `Retry_forever) as r ->
            let on_timeout =
              match r with
              | `Retry_forever -> `Retry_forever
              | `Retry n -> `Retry (n - 1)
            in
            let* () = Event.(emit timeout_retry) on_timeout in
            retry on_timeout)
  in
  retry on_timeout

let with_reconnect : type a. t -> (conn -> a Lwt.t) -> a Lwt.t =
 fun client f ->
  let open Lwt_syntax in
  let rec retry delay =
    Lwt.catch
      (fun () ->
        let* conn = get_conn client in
        f conn)
      (function
        | Unix.(Unix_error (ECONNREFUSED, _, _))
        | Connection_closed | Lwt_io.Channel_closed _
          when client.keep_alive ->
            client.current <- None ;
            (* We don't use a custom event here because some tests explicitly
               look for it. *)
            let* () = Events.retrying_connect ~endpoint:client.uri ~delay in
            let* () = Lwt_unix.sleep delay in
            let delay = delay *. 2. |> min 30. in
            retry delay
        | exn ->
            client.current <- None ;
            Lwt.reraise exn)
  in
  retry 0.5

let send_jsonrpc_request_conn (conn : conn) ?timeout (request : JSONRPC.request)
    =
  let open Lwt_syntax in
  Opentelemetry_lwt.Trace.with_
    ~service_name:"Websocket_client"
    ~kind:Span_kind_client
    "send_jsonrpc"
  @@ fun scope ->
  Opentelemetry.Scope.add_attrs scope (fun () ->
      let id_attrs =
        match request.id with
        | None -> []
        | Some id ->
            let v =
              match id with
              | Id_string s -> s
              | Id_float f ->
                  let ft = truncate f in
                  if float_of_int ft = f then string_of_int ft
                  else string_of_float f
            in
            [("rpc.jsonrpc.request_id", `String v)]
      in
      [
        ("rpc.system", `String "jsonrpc");
        ("rpc.method", `String request.method_);
        ("rpc.jsonrpc.version", `String "2.0");
      ]
      @ id_attrs) ;
  let message = conn.media.construct JSONRPC.request_encoding request in
  let opcode = if conn.binary then Websocket.Frame.Opcode.Binary else Text in
  let response, resolver = Lwt.task () in
  Request_table.replace
    conn.pending_requests
    request.id
    ((fun v -> try Lwt.wakeup_later_result resolver v with _ -> ()), scope) ;
  let* response =
    match timeout with
    | None ->
        let* () = simple_write conn.conn scope opcode message in
        response
    | Some {timeout; on_timeout} ->
        retry_write_until
          ~timeout
          ~on_timeout
          response
          conn.conn
          scope
          opcode
          message
  in
  Request_table.remove conn.pending_requests request.id ;
  return response

let send_jsonrpc_request client ?timeout request =
  with_reconnect client @@ fun conn ->
  send_jsonrpc_request_conn conn ?timeout request

type (_, _) call =
  | Call :
      (module METHOD with type input = 'input and type output = 'output)
      * 'input
      -> ('input, 'output) call

let send_jsonrpc' : type input output.
    t ->
    ?timeout:timeout ->
    (input, output) call ->
    (output * conn) tzresult Lwt.t =
 fun client ?timeout (Call ((module M), input)) ->
  let open Lwt_result_syntax in
  let id = new_id client in
  let request =
    JSONRPC.
      {
        method_ = M.method_;
        parameters = Some (Data_encoding.Json.construct M.input_encoding input);
        id = Some id;
      }
  in
  with_reconnect client @@ fun conn ->
  let*! response = send_jsonrpc_request_conn conn ?timeout request in
  match response with
  | Error e -> tzfail (Request_failed (request, e))
  | Ok response ->
      return (Data_encoding.Json.destruct M.output_encoding response, conn)

let send_jsonrpc client ?timeout call =
  let open Lwt_result_syntax in
  let+ res, _conn = send_jsonrpc' client ?timeout call in
  res

let subscribe' client ?timeout ~output_encoding
    (kind : Ethereum_types.Subscription.kind) =
  let open Lwt_result_syntax in
  let* subscription_id, conn =
    send_jsonrpc' client ?timeout (Call ((module Subscribe), kind))
  in
  let stream, push = Lwt_stream.create () in
  let push x =
    let v =
      match x with
      | None ->
          Subscription_table.remove conn.subscriptions subscription_id ;
          None
      | Some x -> (
          try
            Data_encoding.Json.destruct output_encoding x
            |> Result.ok |> Option.some
          with e ->
            let err =
              Format.asprintf
                "%a"
                (Json_encoding.print_error ?print_unknown:None)
                e
            in
            Some
              (Result_syntax.tzfail
                 (Cannot_destruct (kind, subscription_id, err))))
    in
    try push v with _ -> ()
  in
  Subscription_table.replace conn.subscriptions subscription_id push ;
  let unsubscribe () =
    let* r =
      send_jsonrpc client (Call ((module Unsubscribe), subscription_id))
    in
    push None ;
    return r
  in
  return {stream; unsubscribe}

let subscribe =
  subscribe'
    ~output_encoding:
      (Ethereum_types.Subscription.output_encoding
         Transaction_object.encoding
         Transaction_receipt.encoding)

let subscribe_filter client ?timeout kind filter =
  let open Lwt_result_syntax in
  let+ {stream; unsubscribe} = subscribe ?timeout client kind in
  let stream =
    Lwt_stream.filter_map
      (function
        | Ok x -> ( match filter x with Some x -> Some (Ok x) | None -> None)
        | Error e -> Some (Error e))
      stream
  in
  {stream; unsubscribe}

let subscribe_newHeads ?timeout client =
  subscribe_filter ?timeout client NewHeads @@ function
  | Ethereum_types.Subscription.NewHeads h -> Some h
  | _ -> None

let block_just_number_encoding =
  let open Data_encoding in
  conv (fun n -> (n, ())) (fun (n, ()) -> n)
  @@ merge_objs (obj1 (req "number" Ethereum_types.quantity_encoding)) unit

let subscribe_newHeadNumbers ?timeout client =
  subscribe'
    ?timeout
    client
    NewHeads
    ~output_encoding:block_just_number_encoding

let subscribe_newPendingTransactions ?timeout client =
  subscribe_filter ?timeout client NewPendingTransactions @@ function
  | Ethereum_types.Subscription.NewPendingTransactions tx -> Some tx
  | _ -> None

let subscribe_syncing ?timeout client =
  subscribe_filter ?timeout client Syncing @@ function
  | Ethereum_types.Subscription.Syncing s -> Some s
  | _ -> None

let subscribe_logs ?address ?topics ?timeout client =
  subscribe_filter ?timeout client (Logs {address; topics}) @@ function
  | Ethereum_types.Subscription.Logs log -> Some log
  | _ -> None

let subscribe_l1_l2_levels ?start_l1_level ?timeout client =
  subscribe_filter ?timeout client (Etherlink (L1_L2_levels start_l1_level))
  @@ function
  | Ethereum_types.Subscription.(Etherlink (L1_l2_levels l)) -> Some l
  | _ -> None
