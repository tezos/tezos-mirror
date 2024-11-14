(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_shell_services

(** The module type of a proxy environment. Modules of this type should be
    prepared protocol-side and registered here to become available to the
    proxy facility. *)
module type Proxy_sig = sig
  val protocol_hash : Protocol_hash.t

  (** RPCs provided by the protocol *)
  val directory : Tezos_protocol_environment.rpc_context Tezos_rpc.Directory.t

  (** How to build the context to execute RPCs on. Arguments are:

      - A printer (for logging)
      - An instance of [Tezos_rpc.Context.generic], to perform RPCs
      - Whether [octez-client] is running
      - The chain for which the context is required
      - The block for which the context is required
    *)
  val initial_context :
    Proxy_getter.rpc_context_args ->
    Context_hash.t ->
    Tezos_protocol_environment.Context.t tzresult Lwt.t

  (** The [time_between_blocks] constant for the given block, if any. *)
  val time_between_blocks :
    Tezos_rpc.Context.generic ->
    Block_services.chain ->
    Block_services.block ->
    int64 option tzresult Lwt.t

  (** Functions used to implement the light mode *)
  include Light_proto.PROTO_RPCS
end

type proxy_environment = (module Proxy_sig)

(** [register_proxy_context env] adds a proxy environment to the list
    of known environments. Raise {!Invalid_argument} if an environment
    for the given protocol exists already. *)
val register_proxy_context : proxy_environment -> unit

(** [get_all_registered ()] returns all proxy environments that have been
    registered so far with {!register_proxy_context} *)
val get_all_registered : unit -> proxy_environment list

(** Returns a proxy environment for the given protocol. *)
val get_registered_proxy :
  Tezos_client_base.Client_context.printer ->
  Protocol_hash.t ->
  proxy_environment tzresult Lwt.t
