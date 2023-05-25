(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** EVM proxy server state. *)
type t

(** [create ?runner ?rpc_addr ?rpc_port rollup_node] creates an EVM proxy server.

    The server listens to requests at address [rpc_addr] and the port
    [rpc_port]. [rpc_addr] defaults to ["127.0.0.1"] and a fresh port is
    chosen if [rpc_port] is not set.

    The server communicates with a rollup-node and sets its endpoint via
    [rollup_node].
*)
val create :
  ?runner:Runner.t -> ?rpc_addr:string -> ?rpc_port:int -> Sc_rollup_node.t -> t

(** [mockup ?runner ?rpc_addr ?rpc_port ()] is like [create] but doesn't
    communicate with a [rollup_node] and serves mockup values. *)
val mockup : ?runner:Runner.t -> ?rpc_addr:string -> ?rpc_port:int -> unit -> t

(** [run proxy_server] launches the EVM proxy server with the arguments
    given during {!create}. *)
val run : t -> unit Lwt.t

(** [init ?runner ?rpc_addr ?rpc_port rollup_node] creates an EVM proxy server
    with {!create} and runs it with {!run}. *)
val init :
  ?runner:Runner.t ->
  ?rpc_addr:string ->
  ?rpc_port:int ->
  Sc_rollup_node.t ->
  t Lwt.t

(** [spawn_run proxy_server] same as {!run} but spawns a process. *)
val spawn_run : t -> Process.t

(** [endpoint proxy_server] returns the endpoint to communicate with the
    [proxy_server]. *)
val endpoint : t -> string

(** JSON-RPC request. *)
type request = {method_ : string; parameters : JSON.u}

(** [call_evm_rpc proxy_server ~request] sends a JSON-RPC request to the
    [proxy_server], for the given [request]. *)
val call_evm_rpc : t -> request -> JSON.t Lwt.t

(** [batch_evm_rpc proxy_server ~requests] sends multiple JSON-RPC requests to the
    [proxy_server], for the given [requests]. *)
val batch_evm_rpc : t -> request list -> JSON.t Lwt.t

(** [fetch_contract_code proxy_server contract] returns the code associated to
    the given contract in the rollup. *)
val fetch_contract_code : t -> string -> string Lwt.t
