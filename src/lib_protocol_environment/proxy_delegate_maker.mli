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

(** [of_memory_tree t] creates a delegate that is backed by the tree [t].
    This is an alternative to delegating to a distant endpoint by doing RPCs
    (as done in the client, see e.g. [proto_alpha/lib_client/proxy.ml]).
    As delegates created by [of_memory_tree] are backed by an in-memory
    tree, they are as fast as they can be.

    This constructor is easier to use than {!of_memory_context}, because
    it requires only a tree of data; not a context. This constructor
    is typically useful for tests. *)
val of_memory_tree :
  Tezos_context_memory.Context.tree -> Proxy_context.M.proxy_delegate

(** [of_memory_context m] creates a delegate that is backed by the tree underlying [m].
    This is an alternative to delegating to a distant endpoint by doing RPCs
    (as done in the client, see e.g. [proto_alpha/lib_client/proxy.ml]).
    As delegates created by [of_memory_context] are backed by an in-memory
    tree, they are as fast as they can be.

    This constructor is slightly harder to use than {!of_memory_tree}, because
    it requires a full-fledged context instead of a tree (a context contains
    a tree, so a context is harder to obtain). *)
val of_memory_context :
  Tezos_context_memory.Context.t -> Proxy_context.M.proxy_delegate
