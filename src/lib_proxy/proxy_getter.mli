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

module Local := Tezos_context_memory.Context

(** The size of a tree, for logging *)
val raw_context_size : Tezos_shell_services.Block_services.raw_context -> int

module StringMap = TzString.Map

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

      This structure has the invariant that all leafs are [All] nodes.
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

(** Functor to obtain the implementation of [M] for the proxy
    mode (as opposed to the light mode implementation) *)
module MakeProxy (X : Proxy_proto.PROTO_RPC) : M

(** Exposed for testing purpose only, you should not use it directly *)
module Internal : sig
  module Tree : Proxy.TREE with type t = Local.tree with type key = Local.key
end
