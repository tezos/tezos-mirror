(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

let socket_forwarding_dns = "octez-node-unix-socket"

let socket_forwarding_uri = Format.sprintf "http://%s" socket_forwarding_dns

let build_socket_redirection_ctx socket_path =
  let resolver =
    let h = Stdlib.Hashtbl.create 1 in
    Stdlib.Hashtbl.add h socket_forwarding_dns (`Unix_domain_socket socket_path) ;
    Resolver_lwt_unix.static h
  in
  Cohttp_lwt_unix.Client.custom_ctx ~resolver ()

let callback ~acl server socket_path =
  let callback (conn : Cohttp_lwt_unix.Server.conn) req body =
    Tezos_rpc_http_server.RPC_server.resto_callback server conn req body
  in
  let forwarding_endpoint = Uri.of_string socket_forwarding_uri in
  let on_forwarding req =
    Rpc_process_event.(emit forwarding_rpc (Cohttp.Request.resource req))
  in
  let on_locally_handled req =
    Rpc_process_event.(emit locally_handled_rpc (Cohttp.Request.resource req))
  in
  let ctx = build_socket_redirection_ctx socket_path in
  RPC_middleware.proxy_server_query_forwarder
    ~acl
    ~ctx
    ~forwarder_events:{on_forwarding; on_locally_handled}
    forwarding_endpoint
    callback
