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

type t = {
  media : Media_type.t;
  conn : Websocket_lwt_unix.conn;
  pending_requests : ((JSONRPC.value, exn) result -> unit) Request_table.t;
  subscriptions : (Data_encoding.json option -> unit) Subscription_table.t;
  binary : bool;
  mutable id : int;
  process : unit Lwt.t;
}

type 'a subscription = {
  stream : 'a Lwt_stream.t;
  unsubscribe : unit -> bool tzresult Lwt.t;
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
end

type error +=
  | No_response of JSONRPC.request
  | Request_failed of JSONRPC.request * JSONRPC.error

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
    (fun (r, e) -> Request_failed (r, e))

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

let handle_response pending_requests subscriptions message =
  let open Lwt_syntax in
  function
  | Response {id; value} -> (
      match Request_table.find pending_requests id with
      | None -> Event.(emit ignored_message) message
      | Some callback ->
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

type monitoring = {ping_timeout : float; ping_interval : float}

let connect ?monitoring media uri =
  let open Lwt_syntax in
  let* endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
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
      | None -> return_unit
      | Some frame ->
          let* () = process_frame frame in
          loop ()
    in
    let+ () = loop () in
    Request_table.iter
      (fun _ callback -> callback (Error Connection_closed))
      pending_requests ;
    Subscription_table.iter (fun _ callback -> callback None) subscriptions
  in
  return
    {
      media;
      conn;
      process;
      id = 0;
      binary = is_binary media;
      pending_requests;
      subscriptions;
    }

let disconnect {conn; _} = disconnect conn

let send_jsonrpc_request client (request : JSONRPC.request) =
  let open Lwt_result_syntax in
  let message = client.media.construct JSONRPC.request_encoding request in
  let opcode = if client.binary then Websocket.Frame.Opcode.Binary else Text in
  let response, resolver = Lwt.task () in
  Request_table.replace
    client.pending_requests
    request.id
    (Lwt.wakeup_later_result resolver) ;
  let*! () =
    Websocket_lwt_unix.write
      client.conn
      (Websocket.Frame.create ~opcode ~content:message ())
  in
  let*! response in
  Request_table.remove client.pending_requests request.id ;
  match response with
  | Error e -> tzfail (Request_failed (request, e))
  | Ok resp -> return resp

type (_, _) call =
  | Call :
      (module METHOD with type input = 'input and type output = 'output)
      * 'input
      -> ('input, 'output) call

let send_jsonrpc :
    type input output. t -> (input, output) call -> output tzresult Lwt.t =
 fun client (Call ((module M), input)) ->
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
  let+ response = send_jsonrpc_request client request in
  Data_encoding.Json.destruct M.output_encoding response

let subscribe client (kind : Ethereum_types.Subscription.kind) =
  let open Lwt_result_syntax in
  let* subscription_id =
    send_jsonrpc client (Call ((module Subscribe), kind))
  in
  let stream, push = Lwt_stream.create () in
  let push x =
    push
      (Option.map
         (Data_encoding.Json.destruct
            (Ethereum_types.Subscription.output_encoding
               Transaction_object.encoding))
         x)
  in
  Subscription_table.replace client.subscriptions subscription_id push ;
  let unsubscribe () =
    let* r =
      send_jsonrpc client (Call ((module Unsubscribe), subscription_id))
    in
    push None ;
    return r
  in
  return {stream; unsubscribe}

let subscribe_filter client kind filter =
  let open Lwt_result_syntax in
  let+ {stream; unsubscribe} = subscribe client kind in
  let stream = Lwt_stream.filter_map filter stream in
  {stream; unsubscribe}

let subscribe_newHeads client =
  subscribe_filter client NewHeads @@ function
  | Ethereum_types.Subscription.NewHeads h -> Some h
  | _ -> None

let subscribe_newPendingTransactions client =
  subscribe_filter client NewPendingTransactions @@ function
  | Ethereum_types.Subscription.NewPendingTransactions tx -> Some tx
  | _ -> None

let subscribe_syncing client =
  subscribe_filter client Syncing @@ function
  | Ethereum_types.Subscription.Syncing s -> Some s
  | _ -> None

let subscribe_logs ?address ?topics client =
  subscribe_filter client (Logs {address; topics}) @@ function
  | Ethereum_types.Subscription.Logs log -> Some log
  | _ -> None

let subscribe_l1_l2_levels ?start_l1_level client =
  subscribe_filter client (Etherlink (L1_L2_levels start_l1_level)) @@ function
  | Ethereum_types.Subscription.(Etherlink (L1_l2_levels l)) -> Some l
  | _ -> None
