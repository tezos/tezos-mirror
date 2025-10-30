(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Rpc_encodings

type 'strategy rate_limit = {
  max : int;
  interval : Time.System.Span.t;
  strategy : 'strategy;
}
  constraint 'strategy = [< `Wait | `Error | `Close]

type parameters = {
  push_frame : Websocket.Frame.t option -> unit;
  http_request : Cohttp.Request.t;
  conn : Cohttp_lwt_unix.Server.conn;
  medias : Media_type.t list;
  max_message_length : int;
  handler : websocket_handler;
  monitor : Configuration.monitor_websocket_heartbeat option;
}

type ip_rate_limiter = {mutable count : int; mutable last : Time.System.t}

module Ip_table = Hashtbl.Make (struct
  type t = Ipaddr.t

  let equal ip1 ip2 = Ipaddr.compare ip1 ip2 = 0

  let hash ip = Hashtbl.hash ip
end)

type 'a rate_limiter = {
  kind : string;
  mutable limit : 'a rate_limit option;
  table : ip_rate_limiter Ip_table.t;
}

type messages_rate_limiter = [`Wait | `Error | `Close] rate_limiter

type frames_rate_limiter = [`Close] rate_limiter

let messages_rate_limiter : messages_rate_limiter =
  {kind = "messages"; limit = None; table = Ip_table.create 31}

let frames_rate_limiter : frames_rate_limiter =
  {kind = "frames"; limit = None; table = Ip_table.create 31}

module Types = struct
  type unsubscriber = unit -> bool tzresult Lwt.t

  type subscriptions_table =
    (Ethereum_types.Subscription.id, unsubscriber) Stdlib.Hashtbl.t

  type close_info = {reason : string; status : Websocket_encodings.close_status}

  type monitor_state = {
    params : Configuration.monitor_websocket_heartbeat;
    mbox : int Lwt_mvar.t;
  }

  type state = {
    push_frame : Websocket.Frame.t option -> unit;
    conn_descr : string;
    input_media_type : Media_type.t;
    output_media_type : Media_type.t;
    max_message_length : int;
    handler : websocket_handler;
    message_buffer : Buffer.t;
    subscriptions : subscriptions_table;
    mutable close_info : close_info option;
    monitor : monitor_state option;
    rate_limited_ip : Ipaddr.t option;
  }

  type nonrec parameters = parameters
end

module Name = struct
  type t = string

  let encoding = Data_encoding.string

  let base = ["evm_node_worker"; "websocket"]

  let pp = Format.pp_print_string

  let equal = String.equal
end

module Request = struct
  type ('a, 'b) t =
    | Frame : Websocket.Frame.t * Time.System.t -> (unit, error trace) t
        (** The only possible request is receiving a new frame on the
            websocket. *)

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding =
    let open Data_encoding in
    conv
      (fun (View (Frame (r, ts))) -> (r, ts))
      (fun (r, ts) -> View (Frame (r, ts)))
    @@ obj2
         (req "frame" Websocket_encodings.frame_encoding)
         (req "timestamp" Time.System.rfc_encoding)

  let pp ppf (View (Frame (r, ts))) =
    Format.fprintf ppf "%a: %a" Time.System.pp_hum ts Websocket.Frame.pp r
end

module Event = struct
  include Internal_event.Simple

  let section = ["evm_node"; "websocket"]

  let shutdown =
    declare_3
      ~section
      ~name:"websocket_shutdown"
      ~msg:
        "shutting down websocket worker for connection {conn}: {reason} \
         ({subscriptions} subscriptions)"
      ~level:Notice
      ("conn", Data_encoding.string)
      ("reason", Data_encoding.string)
      ("subscriptions", Data_encoding.int31)
      ~pp1:Format.pp_print_string
      ~pp2:Format.pp_print_string

  let starting =
    declare_1
      ~section
      ~name:"websocket_starting"
      ~msg:"starting worker for websocket connection {conn}"
      ~level:Notice
      ("conn", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let missing_worker =
    declare_1
      ~section
      ~name:"websocket_missing_worker"
      ~msg:"no worker for websocket connection {conn}"
      ~level:Error
      ("conn", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let request_failed =
    declare_3
      ~section
      ~name:"request_failed"
      ~msg:"request {view} failed ({worker_status}): {errors}"
      ~level:Warning
      ("view", Request.encoding)
      ~pp1:Request.pp
      ("worker_status", Tezos_base.Worker_types.request_status_encoding)
      ~pp2:Tezos_base.Worker_types.pp_status
      ("errors", Events.trace_encoding)
      ~pp3:Error_monad.pp_print_trace

  let request_completed_debug =
    declare_2
      ~section
      ~name:"request_completed_debug"
      ~msg:"{view} {worker_status}"
      ~level:Debug
      ("view", Request.encoding)
      ("worker_status", Tezos_base.Worker_types.request_status_encoding)
      ~pp1:Request.pp
      ~pp2:Tezos_base.Worker_types.pp_status

  let unsubscribe =
    declare_3
      ~section
      ~name:"websocket_unsubscribe"
      ~msg:"unsubscribe {id} for connection {conn}: {reason}"
      ~level:Info
      ("conn", Data_encoding.string)
      ("id", Ethereum_types.Subscription.id_encoding)
      ("reason", Data_encoding.string)
      ~pp1:Format.pp_print_string
      ~pp2:(fun ppf Ethereum_types.(Subscription.Id (Hex id)) ->
        Format.fprintf ppf "0x%s" id)
      ~pp3:Format.pp_print_string

  let monitoring_exception =
    declare_2
      ~section
      ~name:"websocket_monitoring_exception"
      ~msg:"monitoring for websocket {conn} raised exception {exception}"
      ~level:Warning
      ("conn", Data_encoding.string)
      ("exception", Data_encoding.string)
      ~pp1:Format.pp_print_string
      ~pp2:Format.pp_print_string
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let table = Worker.create_table Queue

let default_close_info = {Types.reason = ""; status = Normal_closure}

let shutdown_worker ~reason ?(status = default_close_info.status) w =
  let st = Worker.state w in
  (match st.close_info with
  | Some _ -> ()
  | None -> st.close_info <- Some {status; reason}) ;
  Worker.trigger_shutdown w

let shutdown ~reason ?status conn =
  match Worker.find_opt table conn with
  | None -> ()
  | Some w -> shutdown_worker ~reason ?status w

let handle_subscription
    {Types.push_frame; conn_descr; output_media_type; subscriptions; _} opcode
    {id; stream; stopper} =
  let open Lwt_syntax in
  Stdlib.Hashtbl.add subscriptions id stopper ;
  let iter () =
    let* () =
      Lwt_stream.iter
        (fun elt ->
          let content =
            match elt with
            | Ok elt ->
                output_media_type.construct
                  Subscription.notification_encoding
                  elt
            | Error exn ->
                let msg =
                  match exn with
                  | Failure msg -> msg
                  | _ -> Printexc.to_string exn
                in
                Rpc_errors.internal_error msg
                |> output_media_type.construct JSONRPC.error_encoding
          in
          push_frame (Some (Websocket.Frame.create ~opcode ~content ())))
        (Lwt_stream.wrap_exn stream)
    in
    let* () = Event.(emit unsubscribe) (conn_descr, id, "Stream terminated") in
    let* (_ : bool tzresult) = stopper () in
    Stdlib.Hashtbl.remove subscriptions id ;
    return_unit
  in
  (* Pushing subscription responses on the websocket is run asynchronously to
     allow for multiple requests/subscriptions on the same websocket. (In
     particular receiving unsubscribe requests). *)
  Lwt.dont_wait iter (fun exn ->
      Event.(emit__dont_wait__use_with_care unsubscribe)
        ( conn_descr,
          id,
          "Asynchronous exception in stream processing: "
          ^ Printexc.to_string exn ) ;
      Lwt.dont_wait
        (fun () ->
          let* (_ : bool tzresult) = stopper () in
          return_unit)
        ignore ;
      Stdlib.Hashtbl.remove subscriptions id ;
      ())

let opcode_of_media media =
  match Media_type.name media with
  | "application/octet-stream" -> Websocket.Frame.Opcode.Binary
  | _ -> Websocket.Frame.Opcode.Text

let mk_error_response (output_media_type : Media_type.t) id error =
  output_media_type.construct
    JSONRPC.response_encoding
    {value = Error error; id}

let rate_limit_reason rate_limiter =
  match rate_limiter.limit with
  | None -> assert false
  | Some limit ->
      Format.asprintf
        "Rate limited by %s on websocket: %d/%a"
        rate_limiter.kind
        limit.max
        Ptime.Span.pp
        limit.interval

let rate_limit worker frame_ts (rate_limiter : 'a rate_limiter) =
  let state = Worker.state worker in
  let {Types.rate_limited_ip; _} = state in
  match (rate_limiter.limit, rate_limited_ip) with
  | None, _ | _, None -> `Ok
  | Some limit, Some ip -> (
      match Ip_table.find rate_limiter.table ip with
      | None | Some {count = 0; _} ->
          Ip_table.replace rate_limiter.table ip {count = 1; last = frame_ts} ;
          `Ok
      | Some l ->
          if Ptime.Span.compare (Ptime.diff frame_ts l.last) limit.interval >= 0
          then (
            (* More than [interval] time has passed since last first call, reset
               timer and counter. *)
            l.count <- 1 ;
            l.last <- frame_ts ;
            `Ok)
          else if l.count < limit.max then (
            (* Still below limit but within rate interval, increment counter
               only. *)
            l.count <- l.count + 1 ;
            `Ok)
          else (* We have exceeded the limit *)
            `Limit_reached (limit, l))

let rate_limit_messages worker frame_ts
    (decode_message : unit -> (JSONRPC.request, string) result) f =
  let open Lwt_syntax in
  let state = Worker.state worker in
  let {Types.push_frame; output_media_type; _} = state in
  match rate_limit worker frame_ts messages_rate_limiter with
  | `Ok -> f ()
  | `Limit_reached (limit, l) -> (
      match limit.strategy with
      | `Wait ->
          let time_until_limit_lifted =
            Ptime.Span.sub limit.interval (Ptime.diff frame_ts l.last)
            |> Ptime.Span.to_float_s
          in
          let* () = Lwt_unix.sleep time_until_limit_lifted in
          l.count <- 1 ;
          l.last <- frame_ts ;
          f ()
      | `Error ->
          let id =
            match decode_message () with Error _ -> None | Ok {id; _} -> id
          in
          let content =
            mk_error_response output_media_type id
            @@ Rpc_errors.limit_exceeded
                 (rate_limit_reason messages_rate_limiter)
                 None
          in
          let opcode = opcode_of_media output_media_type in
          (* Instead of handling the message, we respond with a JSONRPC error on
             the websocket indicating that the call was rate limited. *)
          push_frame (Some (Websocket.Frame.create ~opcode ~content ())) ;
          (* Ignore frame *)
          return_unit
      | `Close ->
          let reason = rate_limit_reason messages_rate_limiter in
          shutdown_worker ~reason ~status:Policy worker ;
          return_unit)

let rate_limit_frame worker frame_ts f =
  match rate_limit worker frame_ts frames_rate_limiter with
  | `Ok -> f ()
  | `Limit_reached ({strategy = `Close; _}, _) ->
      let reason = rate_limit_reason frames_rate_limiter in
      shutdown_worker ~reason ~status:Policy worker

let cleanup_rate_limiters_on_close worker =
  let state = Worker.state worker in
  match state.rate_limited_ip with
  | None ->
      (* Not rate limited *)
      ()
  | Some ip ->
      (* Find if there are other connections/workers for the same IP *)
      let ip_has_other_workers =
        List.exists
          (fun (_conn, w) ->
            w != worker
            &&
            let state = Worker.state w in
            match state.rate_limited_ip with
            | None -> false
            | Some ip' -> Ipaddr.compare ip ip' = 0)
          (Worker.list table)
      in
      if not ip_has_other_workers then (
        (* [worker] is being closed and is the last connection for [ip], we can
           safely remove the rate limiters for it. *)
        Ip_table.remove messages_rate_limiter.table ip ;
        Ip_table.remove frames_rate_limiter.table ip)

let on_frame worker fr frame_ts =
  let open Lwt_syntax in
  let state = Worker.state worker in
  let {
    Types.push_frame;
    input_media_type;
    output_media_type;
    max_message_length;
    handler;
    monitor;
    _;
  } =
    state
  in
  let push_frame f = push_frame (Some f) in
  let handle_message message =
    (* We clear the current message buffer for future frames *)
    Buffer.clear state.message_buffer ;
    let decode_message () =
      input_media_type.destruct JSONRPC.request_encoding message
    in
    rate_limit_messages worker frame_ts decode_message @@ fun () ->
    let* response_content, subscription =
      match decode_message () with
      | Error err ->
          let response =
            mk_error_response output_media_type None
            @@ Rpc_errors.parse_error err
          in
          Lwt.return (response, None)
      | Ok request ->
          let* ws_response = handler request in
          let response =
            output_media_type.construct
              JSONRPC.response_encoding
              ws_response.response
          in
          Lwt.return (response, ws_response.subscription)
    in
    let opcode = opcode_of_media output_media_type in
    push_frame (Websocket.Frame.create ~opcode ~content:response_content ()) ;
    Option.iter (handle_subscription state opcode) subscription ;
    return_unit
  in
  match fr with
  | {Websocket.Frame.opcode = Ping; content; _} ->
      (* We must answer ping frames from the client with a pong frame with the
         same content. *)
      push_frame (Websocket.Frame.create ~opcode:Pong ~content ()) ;
      return_unit
  | {opcode = Pong; content; _} -> (
      match monitor with
      | None ->
          (* We don't expect to receive pong frames because we don't send any
             ping frames *)
          return_unit
      | Some {mbox; _} -> (
          match int_of_string_opt content with
          | None -> return_unit
          | Some i -> Lwt_mvar.put mbox i))
  | {opcode = Close; _} ->
      (* Client has sent a close frame, we shut everything down for this
         worker *)
      shutdown_worker ~reason:"Received close frame" worker ;
      return_unit
  | {opcode = Text | Binary; content; _}
    when String.length content > max_message_length ->
      (* We are receiving a message too big for the server *)
      shutdown_worker ~reason:"Message too big" ~status:Message_too_big worker ;
      return_unit
  | {opcode = Continuation; content; _}
    when Buffer.length state.message_buffer + String.length content
         > max_message_length ->
      (* We are receiving a message too big for the server *)
      shutdown_worker
        ~reason:"Fragmented message too big"
        ~status:Message_too_big
        worker ;
      return_unit
  | {opcode = Text | Binary; content; final = false; _} ->
      (* New fragmented message *)
      Buffer.clear state.message_buffer ;
      Buffer.add_string state.message_buffer content ;
      return_unit
  | {opcode = Continuation; content; final = false; _} ->
      (* Non final fragment of message, we add the content to the buffer *)
      Buffer.add_string state.message_buffer content ;
      return_unit
  | {opcode = Text | Binary; content; final = true; _} ->
      (* Complete message in frame *)
      handle_message content
  | {opcode = Continuation; content; final = true; _} ->
      (* Final data frame of fragmented message, the complete message is the
         buffer + new content *)
      let message = Buffer.contents state.message_buffer ^ content in
      handle_message message
  | {opcode = Ctrl _ | Nonctrl _; _} ->
      (* Ignore other frames *)
      return_unit

let monitor_websocket_aux worker
    Types.{mbox; params = Configuration.{ping_timeout; ping_interval}} () =
  let open Lwt_syntax in
  let Types.{push_frame; _} = Worker.state worker in
  let rec loop ~push ping_counter =
    if push then
      push_frame
        (Some
           (Websocket.Frame.create
              ~opcode:Ping
              ~content:(string_of_int ping_counter)
              ())) ;
    let* res =
      Lwt.pick
        [
          (let+ c = Lwt_mvar.take mbox in
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
    | `Timeout ->
        shutdown_worker
          worker
          ~status:Going_away
          ~reason:"Timeout in ping/pong heartbeat" ;
        return_unit
  in
  loop ~push:true 0

let monitor_websocket worker =
  let state = Worker.state worker in
  match state.monitor with
  | None -> ()
  | Some monitor ->
      Lwt.dont_wait (monitor_websocket_aux worker monitor) (fun exn ->
          Event.(emit__dont_wait__use_with_care monitoring_exception)
            (state.conn_descr, Printexc.to_string exn))

module Handlers = struct
  type self = worker

  let on_request : type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun worker request ->
    match request with
    | Request.Frame (fr, ts) ->
        protect @@ fun () -> on_frame worker fr ts |> Lwt_result.ok

  type launch_error =
    [ `Not_acceptable
    | `Unsupported_media_type of string
    | `Rate_limit_on_non_tcp ]

  let on_launch _w _name
      ({
         push_frame;
         http_request;
         conn;
         medias;
         max_message_length;
         handler;
         monitor;
       } :
        Types.parameters) =
    let open Lwt_result_syntax in
    let headers = Cohttp.Request.headers http_request in
    let medias =
      Tezos_rpc_http_server.RPC_server.Media.
        {media_types = medias; default_media_type = default_media_type medias}
    in
    let*? input_media_type =
      Tezos_rpc_http_server.RPC_server.Media.input_media_type ~headers medias
    in
    let*? _output_content_type, output_media_type =
      Tezos_rpc_http_server.RPC_server.Media.output_content_media_type
        ~headers
        medias
    in
    let endp, conn = conn in
    let conn_descr =
      Format.sprintf
        "%s[%s]"
        Conduit_lwt_unix.(
          endp |> endp_of_flow |> sexp_of_endp |> Sexplib0.Sexp.to_string)
        (Cohttp.Connection.to_string conn)
    in
    let*! () = Event.(emit starting) conn_descr in
    let monitor =
      Option.map
        (fun params -> {Types.mbox = Lwt_mvar.create_empty (); params})
        monitor
    in
    let*? rate_limited_ip =
      match (messages_rate_limiter.limit, frames_rate_limiter.limit) with
      | None, None -> Ok None
      | _ -> (
          match endp with
          | Conduit_lwt_unix.TCP {ip; _} -> Ok (Some ip)
          | _ -> Error `Rate_limit_on_non_tcp)
    in
    let state =
      Types.
        {
          push_frame;
          conn_descr;
          input_media_type;
          output_media_type;
          max_message_length;
          handler;
          message_buffer = Buffer.create 256;
          subscriptions = Stdlib.Hashtbl.create 3;
          close_info = None;
          monitor;
          rate_limited_ip;
        }
    in
    return state

  let on_error (type a b) _w st (r : (a, b) Request.t) (errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    let open Lwt_result_syntax in
    let request_view = Request.view r in
    let emit_and_continue errs =
      let*! () = Event.(emit request_failed) (request_view, st, errs) in
      return `Continue
    in
    match r with Request.Frame _ -> emit_and_continue errs

  let on_completion _w r _ st =
    match Request.view r with
    | Request.View (Frame _) ->
        Event.(emit request_completed_debug) (Request.view r, st)

  let on_no_request _ = Lwt.return_unit

  let on_close w =
    let open Lwt_syntax in
    let {Types.subscriptions; conn_descr; push_frame; close_info; _} =
      Worker.state w
    in
    let nb_sub = Stdlib.Hashtbl.length subscriptions in
    let {Types.reason; status} =
      Option.value close_info ~default:default_close_info
    in
    let* () = Event.(emit shutdown) (conn_descr, reason, nb_sub) in
    let () =
      try
        push_frame
          (Some
             (Websocket.Frame.close
                (Websocket_encodings.code_of_close_status status))) ;
        push_frame None
      with _ -> (* Websocket already closed *) ()
    in
    Stdlib.Hashtbl.iter
      (fun _id stopper ->
        Lwt.dont_wait
          (fun () ->
            let* (_ : bool tzresult) = stopper () in
            return_unit)
          ignore)
      subscriptions ;
    Stdlib.Hashtbl.clear subscriptions ;
    cleanup_rate_limiters_on_close w ;
    return_unit
end

let start (conn : Cohttp_lwt_unix.Server.conn) (http_request : Cohttp.Request.t)
    ?monitor medias ~max_message_length handler push_frame =
  let open Lwt_result_syntax in
  let name = Cohttp.Connection.to_string (snd conn) in
  let* (worker : _ Worker.t) =
    Worker.launch
      table
      name
      {
        push_frame;
        http_request;
        conn;
        medias;
        max_message_length;
        handler;
        monitor;
      }
      (module Handlers)
  in
  monitor_websocket worker ;
  return_unit

let new_frame conn fr =
  let frame_ts = Time.System.now () in
  match Worker.find_opt table conn with
  | None -> Event.(emit__dont_wait__use_with_care missing_worker) conn
  | Some w ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/7660
         The worker can still be alive but its queue closed in some degenerated
         cases. In this case we could lose frames. *)
      rate_limit_frame w frame_ts @@ fun () ->
      Worker.Queue.push_request_now w (Request.Frame (fr, frame_ts))

let cohttp_callback ?monitor ~max_message_length handler
    (conn : Cohttp_lwt_unix.Server.conn) req _body =
  let open Lwt_syntax in
  let media_types = Supported_media_types.all in
  let conn_name = conn |> snd |> Cohttp.Connection.to_string in
  let* response_action, push_frame =
    Websocket_cohttp_lwt.upgrade_connection
      req
      ~max_frame_length:max_message_length
      (new_frame conn_name)
      (fun io_exn ->
        let reason =
          "Websocket asynchronous IO exception: " ^ Printexc.to_string io_exn
        in
        shutdown ~reason conn_name ;
        return_unit)
  in
  let+ res =
    start ?monitor conn req media_types ~max_message_length handler push_frame
  in
  match res with
  | Ok () -> response_action
  | Error err ->
      let status, expl =
        match err with
        | `Unsupported_media_type ct ->
            (* HTTP error 415 *)
            (`Unsupported_media_type, ": Unsupported content type " ^ ct)
        | `Not_acceptable ->
            (* HTTP error 406 *)
            (`Not_acceptable, "")
        | `Rate_limit_on_non_tcp ->
            (* HTTP error 503 *)
            ( `Service_unavailable,
              ": Can only start rate limited websocket connection on TCP \
               connection" )
      in
      let body =
        Cohttp_lwt.Body.of_string ("Cannot accept websocket connection" ^ expl)
      in
      `Response (Cohttp.Response.make ~status (), body)

let on_conn_closed (conn : Cohttp_lwt_unix.Server.conn) =
  let conn_str = Cohttp.Connection.to_string (snd conn) in
  shutdown ~reason:"Connection closed" conn_str

let setup_rate_limiter rate_limiter limit =
  let open Result_syntax in
  let+ () =
    if limit.max <= 0 then
      error_with
        "Max %s in websocket rate limiter must be strictly positive"
        rate_limiter.kind
    else return_unit
  in
  rate_limiter.limit <- Some limit

let setup_rate_limiters ?messages_limit ?frames_limit () =
  let open Result_syntax in
  let* () =
    Option.iter_e (setup_rate_limiter messages_rate_limiter) messages_limit
  in
  Option.iter_e (setup_rate_limiter frames_rate_limiter) frames_limit
