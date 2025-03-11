(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** {1 Directories depending on backends} *)

module EndpointMap : Map.S with type key = Cohttp.Code.meth * string

type resto_dir = {
  dir : unit Tezos_rpc.Directory.t;
  extra :
    (Cohttp_lwt_unix.Server.conn ->
    Cohttp.Request.t ->
    Cohttp_lwt.Body.t ->
    Cohttp_lwt_unix.Server.response_action Lwt.t)
    EndpointMap.t;
}

(** The type of RPC directory for EVM node depending on the chosen RPC server
    backend. *)
type t = private
  | Resto of resto_dir  (** A Resto directory *)
  | Dream of Dream.route trace  (** A list of Dream routes *)

(** An empty directory depending on the RPC server backend. *)
val empty : Configuration.rpc_server -> t

(** A directory initialised with a resto directory. Will produce a [Resto]
    value, so not compatible with Dream. *)
val init_from_resto_directory : unit Tezos_rpc.Directory.t -> t

(** {1 Registering services} *)

(** {2 Generic functions} *)

(** Register a new service with it's handler. *)
val register :
  t ->
  ([< Resto.meth], unit, 'params, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  ('params -> 'query -> 'input -> 'output tzresult Lwt.t) ->
  t

(** Register a new service with it's handler. The server answers with 404
    Not_Found if the handler returns [None]. *)
val opt_register :
  t ->
  ([< Resto.meth], unit, 'params, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  ('params -> 'query -> 'input -> 'output option tzresult Lwt.t) ->
  t

(** Register a new service with it's handler. *)
val lwt_register :
  t ->
  ([< Resto.meth], unit, 'params, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  ('params -> 'query -> 'input -> 'output Lwt.t) ->
  t

(** Register a new streamed service. The handler should produce output elements
    in a stream. *)
val streamed_register :
  t ->
  ([< Resto.meth], unit, 'params, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  ('params -> 'query -> 'input -> ('output Lwt_stream.t * (unit -> unit)) Lwt.t) ->
  t

(** Register a new endpoint for collecting metrics. *)
val register_metrics : string -> t -> t

(** Register a new websocket service. The handler should return an initial
    JSONRPC response and optionally produce output elements in a stream for
    subscription services. Requests above [max_message_length] will be rejected
    and the connection closed (only for {!Resto} directories). If [monitor] is
    provided, the websocket connection is monitored with the given parameters
    (only for {!Resto} directories).
*)
val jsonrpc_websocket_register :
  ?monitor:Configuration.monitor_websocket_heartbeat ->
  max_message_length:int ->
  t ->
  string ->
  Rpc_encodings.websocket_handler ->
  t

(** Registers a {!/describe} service for a Resto directory. No effect for a
    Dream directory. *)
val register_describe : t -> t

(** {2 Curried functions with respect to service parameters} *)

val register0 :
  t ->
  ([< Resto.meth], unit, unit, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  ('query -> 'input -> 'output tzresult Lwt.t) ->
  t

val register1 :
  t ->
  ([< Resto.meth], unit, unit * 'a, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  ('a -> 'query -> 'input -> 'output tzresult Lwt.t) ->
  t

val register2 :
  t ->
  ( [< Resto.meth],
    unit,
    (unit * 'a) * 'b,
    'query,
    'input,
    'output )
  Tezos_rpc.Service.t ->
  ('a -> 'b -> 'query -> 'input -> 'output tzresult Lwt.t) ->
  t

val opt_register0 :
  t ->
  ([< Resto.meth], unit, unit, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  ('query -> 'input -> 'output option tzresult Lwt.t) ->
  t

val opt_register1 :
  t ->
  ([< Resto.meth], unit, unit * 'a, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  ('a -> 'query -> 'input -> 'output option tzresult Lwt.t) ->
  t

val opt_register2 :
  t ->
  ( [< Resto.meth],
    unit,
    (unit * 'a) * 'b,
    'query,
    'input,
    'output )
  Tezos_rpc.Service.t ->
  ('a -> 'b -> 'query -> 'input -> 'output option tzresult Lwt.t) ->
  t

val lwt_register0 :
  t ->
  ([< Resto.meth], unit, unit, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  ('query -> 'input -> 'output Lwt.t) ->
  t

val lwt_register1 :
  t ->
  ([< Resto.meth], unit, unit * 'a, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  ('a -> 'query -> 'input -> 'output Lwt.t) ->
  t

val lwt_register2 :
  t ->
  ( [< Resto.meth],
    unit,
    (unit * 'a) * 'b,
    'query,
    'input,
    'output )
  Tezos_rpc.Service.t ->
  ('a -> 'b -> 'query -> 'input -> 'output Lwt.t) ->
  t

val streamed_register0 :
  t ->
  ([< Resto.meth], unit, unit, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  ('query -> 'input -> ('output Lwt_stream.t * (unit -> unit)) Lwt.t) ->
  t

val streamed_register1 :
  t ->
  ([< Resto.meth], unit, unit * 'a, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  ('a -> 'query -> 'input -> ('output Lwt_stream.t * (unit -> unit)) Lwt.t) ->
  t

val streamed_register2 :
  t ->
  ( [< Resto.meth],
    unit,
    (unit * 'a) * 'b,
    'query,
    'input,
    'output )
  Tezos_rpc.Service.t ->
  ('a ->
  'b ->
  'query ->
  'input ->
  ('output Lwt_stream.t * (unit -> unit)) Lwt.t) ->
  t
