(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Code that is used both by protocol-dependent code
    and by other code. Note that we don't want this code
    in proxy_proto.ml because it's independent from the protocol
    and we neither want this code in proxy_getter.ml, because
    it would create a cyclic dependency between proxy_proto.ml
    and proxy_getter.ml *)

module Local = Local_context

(** Whether [tezos-client] or [tezos-proxy-server] is running. *)
type mode =
  | Client  (** Mode when [tezos-client] executes *)
  | Server  (** Mode when [tezos-proxy-server] executes *)

type proxy_getter_input = {
  rpc_context : RPC_context.simple;
  mode : mode;
  chain : Tezos_shell_services.Block_services.chain;
  block : Tezos_shell_services.Block_services.block;
}

(** The result of setting a leaf. A mutation if done in place, otherwise
    a fresh value. We need this type because the proxy implementation
    returns a value whereas the light mode's implementation
    performs a mutation (because of Irmin under the hood). *)
type 'a update = Mutation | Value of 'a

module type TREE = sig
  type t

  type key

  val empty : t

  val get : t -> key -> Proxy_context.M.tree option Lwt.t

  val set_leaf :
    t ->
    key ->
    Tezos_shell_services.Block_services.raw_context ->
    t update Lwt.t
end

(** Module used by implementations of [Proxy_getter.M]. *)
module type CORE = sig
  (* Get the data associated to the given key *)
  val get : Local.key -> Local.tree option Lwt.t

  (* Retrieves the data for the given key *)
  val do_rpc : proxy_getter_input -> Local.key -> unit tzresult Lwt.t
end
