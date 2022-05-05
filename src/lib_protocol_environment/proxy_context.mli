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

(** This module is the location where the proxy tweaks the behavior of a vanilla
    client. A regular mockup client uses a {!Memory_context} in place of this
    implementation. Compared to [Memory_context], this instance features a
    {!Proxy_Delegate.T} which under the hood relies on [Proxy_getter].

    Other [*_context] modules of {!Tezos_protocol_environment}, i.e.
    siblings of this file, are backed by different type of values coming
    from {!Tezos_context}. This file is backed by {!M.t} below, which
    is a thin layer over {!Tezos_memory_context.Context}. Because of that,
    this instance of {!Tezos_protocol_environment} is close to
    the {!Memory_context} one.

    Whereas [Memory_context] is a regular recursive map, [Proxy_context] obtains
    values by delegating to endpoints when receiving requests. Hence, right
    after having created an {!empty} value with an instance of [Proxy_Delegate.T],
    this value behaves as the distant endpoint it delegates to. *)

open Tezos_protocol_environment

(** The module by which to parameterize
    {!Tezos_protocol_environment.Context.kind} below.

    [Proxy_Delegate.T] is packed as the type [M.proxy_delegate],
    because it is used for obtaining pristine instances of [Proxy_context]
    with {!empty} below. *)
module M : sig
  type key = string list (* as in environment_context.mli *)

  type value = Bytes.t (* as in environment_context.mli *)

  type tree = Tezos_context_memory.Context.tree

  val empty : tree

  type t
end

(** The additional kind identifying {!Proxy_context} values. Used to
    detect at runtime when a proxy context is expected, to disambiguate
    from other kinds. *)
type _ Context.kind += Context : M.t Context.kind

(** Constructs an empty context, possibly giving the delegate (the function
    querying the endpoint) right away.
    Otherwise set it later with [set delegate] *)
val empty : Proxy_delegate.t option -> Context.t
