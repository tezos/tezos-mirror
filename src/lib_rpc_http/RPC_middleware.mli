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

(** This module provides middlewares that is used by the RPC servers to
    forward unsupported RPCs to a full node. *)

(** Callbacks to add side-effects to {!proxy_server_query_forwarder},
    typically used to emit events. *)
type forwarder_events = {
  on_forwarding : Cohttp.Request.t -> unit Lwt.t;
      (** Called right before an unsupported RPC gets forwarded to the node. *)
  on_locally_handled : Cohttp.Request.t -> unit Lwt.t;
      (** Called after an RPC has been handled by the server itself
          (without any forwarding). *)
}

(** A Resto middleware that transforms any callback to an other that rewrites
    queries that the proxy server cannot handle and forwards them to the full
    node at the given [Uri.t]. If [acl] parameter is provided, the forwarding
    happens only if the ACL rules are satisfied.*)
val proxy_server_query_forwarder :
  ?acl:RPC_server.Acl.t ->
  ?ctx:Cohttp_lwt_unix.Net.ctx ->
  ?forwarder_events:forwarder_events ->
  Uri.t ->
  RPC_server.callback ->
  RPC_server.callback

(** A Resto middleware that transforms any server callback to an other
    that handles RPC metrics *)
val rpc_metrics_transform_callback :
  update_metrics:
    (string ->
    string ->
    (unit -> Cohttp_lwt_unix.Server.response_action Lwt.t) ->
    Cohttp_lwt_unix.Server.response_action Lwt.t) ->
  unit Tezos_rpc.Directory.t ->
  RPC_server.callback ->
  RPC_server.callback
