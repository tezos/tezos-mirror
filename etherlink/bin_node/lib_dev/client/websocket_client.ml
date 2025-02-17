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

type t = {
  media : Media_type.t;
  conn : Websocket_lwt_unix.conn;
  response_stream : response Lwt_stream.t;
  binary : bool;
  mutable id : int;
}

type 'a subscription = {
  stream : 'a Lwt_stream.t;
  unsubscribe : unit -> bool tzresult Lwt.t;
}

module Event = struct
  include Internal_event.Simple

  let section = ["evm"; "client"; "websocket"]

  let exn =
    let open Data_encoding in
    conv Printexc.to_string (fun s -> Failure s) string

  let pp_exn fmt e = Format.pp_print_string fmt (Printexc.to_string e)

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

let handle_message (media : Media_type.t) message =
  let open Lwt_syntax in
  match media.destruct JSONRPC.response_encoding message with
  | Ok resp -> return_some (Response resp)
  | Error e1 -> (
      match media.destruct Subscription.notification_encoding message with
      | Ok notif -> return_some (Notification notif)
      | Error e2 ->
          let* () = Event.(emit decoding_error) (e1, e2) in
          return_none)

let disconnect conn =
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let* () = Event.(emit disconnecting) () in
      let* () = Websocket_lwt_unix.write conn (Websocket.Frame.close 1000) in
      let* () = Websocket_lwt_unix.close_transport conn in
      let* () = Event.(emit disconnected) () in
      return_unit)
    (fun e ->
      let* () = Event.(emit disconnection_error) e in
      return_unit)

let connect media uri =
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
  let message_buffer = Buffer.create 256 in
  let frame_stream =
    Lwt_stream.from (fun () ->
        Lwt.catch
          (fun () ->
            let* frame = Websocket_lwt_unix.read conn in
            match frame.opcode with
            | Close ->
                let* () = disconnect conn in
                return_none
            | _ -> return_some frame)
          (fun e ->
            let* () = Event.(emit connection_closed) e in
            let* () = disconnect conn in
            return_none))
  in
  let response_stream =
    Lwt_stream.filter_map_s
      (function
        | {Websocket.Frame.opcode = Ping; content; _} ->
            (* We must answer ping frames from the server with a pong frame
               with the same content. *)
            let* () =
              Websocket_lwt_unix.write
                conn
                (Websocket.Frame.create ~opcode:Pong ~content ())
            in
            return_none
        | {opcode = Pong; _} -> return_none
        | {opcode = Close; _} ->
            (* Cannot happen because frame_stream is closed when we receive this
               opcode. *)
            return_none
        | {opcode = Text | Binary; content; final = false; _} ->
            (* New fragmented message *)
            Buffer.clear message_buffer ;
            Buffer.add_string message_buffer content ;
            return_none
        | {opcode = Continuation; content; final = false; _} ->
            (* Non final fragment of message, we add the content to the
               buffer *)
            Buffer.add_string message_buffer content ;
            return_none
        | {opcode = Text | Binary; content; final = true; _} ->
            (* Complete message in frame *)
            handle_message media content
        | {opcode = Continuation; content; final = true; _} ->
            (* Final data frame of fragmented message, the complete message is
               the buffer + new content *)
            let message = Buffer.contents message_buffer ^ content in
            handle_message media message
        | {opcode = Ctrl _ | Nonctrl _; _} ->
            (* Ignore other frames *)
            return_none)
      frame_stream
  in
  return {media; conn; response_stream; id = 0; binary = is_binary media}

let disconnect {conn; _} = disconnect conn

let send_jsonrpc_request_helper client (request : JSONRPC.request) =
  let open Lwt_result_syntax in
  let message = client.media.construct JSONRPC.request_encoding request in
  let opcode = if client.binary then Websocket.Frame.Opcode.Binary else Text in
  let*! () =
    Websocket_lwt_unix.write
      client.conn
      (Websocket.Frame.create ~opcode ~content:message ())
  in
  let stream = Lwt_stream.clone client.response_stream in
  let*! response =
    Lwt_stream.find_map
      (function
        | Response {id; value} when id = request.id -> Some value | _ -> None)
      stream
  in
  match response with
  | None -> tzfail (No_response request)
  | Some (Error e) -> tzfail (Request_failed (request, e))
  | Some (Ok resp) -> return resp

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
  let+ response = send_jsonrpc_request_helper client request in
  Data_encoding.Json.destruct M.output_encoding response

let subscribe client (kind : Ethereum_types.Subscription.kind) =
  let open Lwt_result_syntax in
  let id = new_id client in
  let request =
    JSONRPC.
      {
        method_ = Subscribe.method_;
        parameters =
          Some (Data_encoding.Json.construct Subscribe.input_encoding kind);
        id = Some id;
      }
  in
  let message = client.media.construct JSONRPC.request_encoding request in
  let opcode = if client.binary then Websocket.Frame.Opcode.Binary else Text in
  let*! () =
    Websocket_lwt_unix.write
      client.conn
      (Websocket.Frame.create ~opcode ~content:message ())
  in
  let stream = Lwt_stream.clone client.response_stream in
  let stop, stop_resolver = Lwt.task () in
  let stream =
    Lwt_stream.from (fun () -> Lwt.choose [stop; Lwt_stream.get stream])
  in
  let*! subscription_repsonse =
    Lwt_stream.find_map
      (function
        | Response {id; value} when id = request.id -> (
            match value with
            | Ok json ->
                Some
                  (Ok
                     (Data_encoding.Json.destruct
                        Subscribe.output_encoding
                        json))
            | Error e -> Some (Error e))
        | _ -> None)
      stream
  in
  let+ (subscription_id : Ethereum_types.Subscription.id) =
    match subscription_repsonse with
    | None -> tzfail (No_response request)
    | Some (Error e) -> tzfail (Request_failed (request, e))
    | Some (Ok id) -> return id
  in
  let unsubscribe () =
    let* r =
      send_jsonrpc client (Call ((module Unsubscribe), subscription_id))
    in
    Lwt.wakeup_later stop_resolver None ;
    return r
  in
  let stream =
    Lwt_stream.filter_map
      (function
        | Notification notif when notif.params.subscription = subscription_id ->
            let event =
              Data_encoding.Json.destruct
                (Ethereum_types.Subscription.output_encoding
                   Transaction_object.encoding)
                notif.params.result
            in
            Some event
        | _ -> None)
      stream
  in
  {stream; unsubscribe}

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
