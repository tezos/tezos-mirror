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

module Local := Local_context

(** The size of a tree, for logging *)
val raw_context_size : Tezos_shell_services.Block_services.raw_context -> int

module StringMap = String.Map

module type REQUESTS_TREE = sig
  (** The point of this data structure is as follows:

      Suppose [Make.cache] is like this, because key A has been
      requested already and the tree at A has two nodes B and C:

      A-B
       \
        C

      If proxy_getter receives a request for A-D, there's no point doing
      a request, even if it's not there; because as A has been requested
      already; if A-D was available, it would be there already.

      This is a crucial optimisation that reduces the number of .../raw/bytes
      RPC requests by 90% when executing baking_rights&?all=true locally,
      after the chain starts having more than a few cycles.
      More specifically, in baking_rights, the client keeps looking for
      (missing) keys of the form rolls;owner;snapshot;10;1;74;5;1354
      while the parent key rolls;owner;snapshot;10;1 has been obtained before.

      This structure has the invariant that all leaves are [All] nodes.
      If requests A;B and A;C have been done, the tree is as follows:

      A[Partial] -> B[All]
         \
          \-------> C[All]

      If then request A is done, the tree becomes:

      A[All]
    *)

  type tree = Partial of tree StringMap.t | All

  val empty : tree

  val add : tree -> string list -> tree

  val find_opt : tree -> string list -> tree option
end

module RequestsTree : REQUESTS_TREE

module type M = sig
  (** Whether the key is mapped to a directory *)
  val proxy_dir_mem :
    Proxy.proxy_getter_input -> Local.key -> bool tzresult Lwt.t

  (** The value to which a key maps *)
  val proxy_get :
    Proxy.proxy_getter_input -> Local.key -> Local.tree option tzresult Lwt.t

  (** Whether the key is mapped to a value *)
  val proxy_mem : Proxy.proxy_getter_input -> Local.key -> bool tzresult Lwt.t
end

type proxy_m = (module M)

(** The different ways to obtain data from the node. The two functions
    being wrapped are ultimately used to build {!Proxy_delegate.t} values,
    that are passed to {!Proxy_context.empty}. *)
type proxy_builder =
  | Of_rpc of (Proxy_proto.proto_rpc -> proxy_m Lwt.t)
      (** Build a proxy that uses network requests for all data. *)
  | Of_data_dir of (Context_hash.t -> Proxy_delegate.t tzresult Lwt.t)
      (** Build a proxy that looks up data in a running node's data dir. *)

(** Input data required by the proxy mode to build a
    {!Tezos_protocol_environment.rpc_context}. *)
type rpc_context_args = {
  printer : Tezos_client_base.Client_context.printer option;
      (** Optional printer to display information in some custom format. *)
  proxy_builder : proxy_builder;
      (** Given the protocol implementation of the RPCs required by the proxy mode,
          how to build an instance of {!proxy_m} that will then make it possible
          to build a {!Tezos_protocol_environment.Proxy_context}. *)
  rpc_context : RPC_context.generic;
      (** How to perform RPC calls. We need such a value, because the proxy mode
          performs RPCs to initialize itself (by requesting the header) and
          also to fill {!Tezos_protocol_environment.Proxy_context} on-demand. *)
  mode : Proxy.mode;  (** Whether the client or the proxy server is running. *)
  chain : Tezos_shell_services.Block_services.chain;
      (** The chain to provide RPC calls for. *)
  block : Tezos_shell_services.Block_services.block;
      (** The block to provide RPC calls for. *)
}

(** Builds a proxy delegate in the way specified by the proxy_builder field of
    the {!rpc_context_args} argument. *)
val make_delegate :
  rpc_context_args ->
  (module Proxy_proto.PROTO_RPC) ->
  Context_hash.t ->
  Proxy_delegate.t tzresult Lwt.t

(** Functor to obtain the implementation of [M] for the proxy
    mode (as opposed to the light mode implementation) *)
module MakeProxy (X : Proxy_proto.PROTO_RPC) : M

(** Functor to obtain a generic implementation. Used by the light mode *)
module Make (C : Proxy.CORE) (X : Proxy_proto.PROTO_RPC) : M

(** Exposed for testing purpose only, you should not use it directly *)
module Internal : sig
  module Tree : Proxy.TREE with type t = Local.tree with type key = Local.key

  val raw_context_to_tree :
    Tezos_shell_services.Block_services.raw_context -> Local.tree option Lwt.t
end
