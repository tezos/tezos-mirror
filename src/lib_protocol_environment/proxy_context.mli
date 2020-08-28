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

(** This module is the location where the proxy tweaks the behavior of a
    vanilla client. A regular mockup client uses a [Memory_context] in
    place of this implementation. Compared to [Memory_context], this
    instance features a [M.ProxyDelegate]
    which under the hood relies on [Proxy_getter].

    Whereas [Memory_context] is a regular recursive map, [Proxy_context]
    obtains values by delegating to endpoints when receiving requests.
    Hence, after right after having created an [empty] value with an instance
    of [M.ProxyDelegate], this value is as filled as the distant endpoint
    it delegates to. *)

open Tezos_protocol_environment

module M : sig
  type key = string list (* as in environment_context.mli *)

  type value = Bytes.t (* as in environment_context.mli *)

  type tree = Dir of tree TzString.Map.t | Key of value

  val tree_size : tree -> int

  val empty : tree

  type t
end

type rpc = M.key -> M.tree tzresult Lwt.t

type _ Context.kind += Proxy : M.t Context.kind

(** Constructs an empty context, possibly giving the rpc right-away.
    Otherwise set it later with [set rpc] *)
val empty : rpc option -> Context.t

(** Sets the rpc in the context. Doesn't take an option, because it's never
    required to put [None]. *)
val set_rpc : rpc -> Context.t -> Context.t
