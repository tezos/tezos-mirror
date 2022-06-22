(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** [Shell_context] is a persisting recursive map backed by an on-disk Irmin tree,
    i.e. {!Tezos_context.Context} (in [src/lib_context]), used
    by nodes. It is the "main" kind of context. It abstracts away
    {!Tezos_context.Context}, by hiding the internals, and making it
    an instance of the generic {!Protocol_environment.Context.t}.
    Other [*_context] modules of {!Tezos_protocol_environment}, i.e.
    siblings of this file, are backed by different type of values coming
    from {!Tezos_context}.

    [Shell_context] is one of the instances of {!Environment_context} along
    {!Memory_context} and {!Proxy_context}. All these 3 instances
    can implement the same API (i.e. {!Environment_context}), because
    this API is highly polymorphic, thanks to {!Environment_context.Context.ops}.

    As such, a {!Shell_context} value is an instance of {!Environment_context}
    whose [kind] is the one declared in this file.

    Instances of {!Shell_context} are harder to obtain than {!Memory_context} ones, because
    they require a dedicated folder on disk (the subdirectory [context]
    of the node's [--data-dir] when it runs). As such, instances of
    {!Shell_context} are harder to initialize and use than {!Memory_context};
    making them less amenable to testing for example. *)

open Tezos_protocol_environment
open Tezos_crypto

(** The additional kind identifying {!Shell_context} values. Used to
    detect at runtime when a shell context is expected, to disambiguate
    from other kinds. *)
type _ Context.kind += Context : Tezos_context.Context.t Context.kind

(** [checkout index ctxt_hash] checks whether the underlying data on disk
    contain an entry for [ctxt_hash]. If so, it is returned; otherwise
    [Nothing] is returned. *)
val checkout :
  Tezos_context.Context.index -> Context_hash.t -> Context.t option Lwt.t

(** [checkout_exn index ctxt_hash] checks whether the underlying data on disk
    contain an entry for [ctxt_hash]. If so, the data are loaded and returned;
    otherwise the exception thrown by {!Tezos_context.Context.checkout_exn}
    is forwarded. Prefer using {!checkout}. *)
val checkout_exn :
  Tezos_context.Context.index -> Context_hash.t -> Context.t Lwt.t

(** [wrap_disk_context t] creates a shell context from an Irmin on-disk folder
    (i.e. {!Tezos_context.Context.t}). This function hereby abstracts away
    a value, from the low-level [lib_context] to the higher-level [lib_protocol_environment]. *)
val wrap_disk_context : Tezos_context.Context.t -> Context.t

(** [unwrap_disk_context t] gives access to the lower-level {!Tezos_context.Context.t}
    value underlying a {!Shell_context}. *)
val unwrap_disk_context : Context.t -> Tezos_context.Context.t
