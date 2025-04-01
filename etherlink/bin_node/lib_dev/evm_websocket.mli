(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Callback to use for a websocket endpoint in a Cohttp server.
    [cohttp_callback ?monitor ~max_message_length handler conn req body]
    upgrades the connection for request [req] to the websocket protocol and
    starts worker that processes incoming frames and writes in return in the
    websocket. [max_message_length] specifies the maximum size of message
    accepted by the server, over which the connection will be closed. If
    [monitor] is provided, the websocket connection is monitored with the given
    parameters and the connection closed if the client fails to answer.*)
val cohttp_callback :
  ?monitor:Configuration.monitor_websocket_heartbeat ->
  max_message_length:int ->
  Rpc_encodings.websocket_handler ->
  Cohttp_lwt_unix.Server.conn ->
  Cohttp.Request.t ->
  'body ->
  Cohttp_lwt_unix.Server.response_action Lwt.t

(** Callback to be called by Cohttp when it detects a closed connection before
    any read/write happens. [on_conn_closed conn] stops the websocket worker and
    cleans resources associated to connection [conn], if it exists. *)
val on_conn_closed : Cohttp_lwt_unix.Server.conn -> unit

type 'strategy rate_limit = {
  max : int;
      (** Max allowed websocket frames or messages in the below interval. *)
  interval : Time.System.Span.t;  (** Interval for the rate limit. *)
  strategy : 'strategy;
      (** Strategy to adopt when a client sends messages which exceed the
          defined rate limit. *)
}
  constraint
    'strategy =
    [< `Wait  (** The call is blocking for [interval]. *)
    | `Error
      (** The server responds with a JSONRPC error indicating the rate limiting as
        the reason and the limit. *)
    | `Close
      (** The server closes the websocket connection with a close frame
          indicating the rate limiting as the reason and the limit. *)
    ]

(** Setup the rate limiters parameters for the websocket server (for all
    connections). *)
val setup_rate_limiters :
  ?messages_limit:[`Wait | `Close | `Error] rate_limit ->
  ?frames_limit:[`Close] rate_limit ->
  unit ->
  unit tzresult
