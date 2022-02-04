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

(** A dumb container, used to perform RPC calls concerning a specific
    chain and block. In other words this container is used to perform
    RPC calls of the form [/chains/<chain>/blocks/<block>] where the <...>
    received the value of the corresponding field of this record. *)
type proxy_getter_input = {
  rpc_context : RPC_context.simple;  (** How to perform RPC calls *)
  mode : mode;  (** Whether [tezos-client] or [tezos-proxy-server] is running *)
  chain : Tezos_shell_services.Block_services.chain;
      (** The chain involved in the RPC call *)
  block : Tezos_shell_services.Block_services.block;
      (** The block involved in the RPC call *)
}

(** The result of setting a leaf. A mutation if done in place, otherwise
    a fresh value. We need this type because the proxy implementation
    returns a value whereas the light mode's implementation
    performs a mutation (because of Irmin under the hood). *)
type 'a update = Mutation | Value of 'a

(** An ad-hoc module type used by implementations of the proxy mode
    when it uses the [../raw/bytes] RPC to query its distant endpoint.
    It is ad-hoc because its [get] function has the concrete {!Proxy_context.M.tree}
    as a return type and because [add_leaf] has the concrete
    {!Tezos_shell_services.Block_services.raw_context} as a parameter
    (this type is inherited from the return type of the [../raw/bytes] RPC). *)
module type TREE = sig
  (** The abstract type that implementors of this module type provide.
      Obtain an instance with {!empty}. Think of [t] as a tree type. *)
  type t

  (** An abstract type of key. *)
  type key

  (** [empty] returns a pristine value *)
  val empty : t

  (** [get t key] returns the tree of data mapped by [key], if any. *)
  val get : t -> key -> Proxy_context.M.tree option Lwt.t

  (** [add_leaf t key raw_ctxt] returns a variant of [t] where [key] is
      mapped to [raw_ctxt]. When this function is called, it transforms
      [raw_ctxt], under the hood, into an instance of {!Proxy_context.M.tree},
      as the latter is the type internally stored in {!t} (it needs to be,
      as it's the return type of {!get}).

      This function is called [add_leaf], because the proxy mode iteratively
      builds its local copy of the endpoint's data. This function is only called
      when adding a new leaf in the tree of data, never to replace existing
      data. In other words, it's not a general purpose setter. *)
  val add_leaf :
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
