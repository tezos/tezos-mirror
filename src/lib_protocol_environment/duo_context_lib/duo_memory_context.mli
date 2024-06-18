(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [Memory_context] is a recursive map backed by an in-memory Brassaia tree,
    i.e. {!Tezos_context_memory.Context} (in [src/lib_context]), used
    by the mockup mode and by proof verifiers (i.e. clients
    of {!Tezos_context_helpers.Context.Make_tree.Proof}).
    It abstracts away {!Tezos_context_memory.Context.t}.

    It is one of the instances of {!Environment_context} along
    {!Shell_context} and {!Proxy_context}. All these 3 instances
    can implement the same API (i.e. {!Environment_context}), because
    this API is highly polymorphic, thanks to {!Environment_context.Context.ops}.

    As such, a {!Memory_context} value is an instance of {!Environment_context}
    whose {!Environment_context.Context.kind} is the one declared below in this
    file, i.e. {!extension-Context}.

    Instances of {!t} are easier to obtain than {!Shell_context} (which
    are used by nodes), because they don't require access to a disk: they
    live completely in memory. That is why they are ideal for testing. *)

open Tezos_protocol_environment

(** The additional kind identifying {!Memory_context} values. Used to
    detect at runtime when a memory context is expected, to disambiguate
    from other kinds. *)
type _ Context.kind += Context : Context_wrapper.Memory_context.t Context.kind

(** [checkout index ctxt_hash] checks whether the underlying data on disk
    contain an entry for [ctxt_hash]. If so, it is returned; otherwise
    [Nothing] is returned. *)
val checkout :
  Context_wrapper.Memory_context.index ->
  Context_hash.t ->
  Context.t option Lwt.t

(** [checkout_exn index ctxt_hash] checks whether the underlying data on disk
    contain an entry for [ctxt_hash]. If so, the data are loaded and returned;
    otherwise the exception thrown by {!Tezos_context_brassaia.Context.checkout_exn}
    is forwarded. Prefer using {!checkout}. *)
val checkout_exn :
  Context_wrapper.Memory_context.index -> Context_hash.t -> Context.t Lwt.t

(** [wrap_disk_context t] creates a brassaia context from an Brassaia on-disk folder
    (i.e. {!Tezos_context.Context.t}). This function hereby abstracts away
    a value, from the low-level [lib_context] to the higher-level [lib_protocol_environment]. *)
val wrap_memory_context : Context_wrapper.Memory_context.t -> Context.t

(** [unwrap_disk_context t] gives access to the lower-level {!Tezos_context.Context.t}
    value underlying a {!Brassaia_context}. *)
val unwrap_memory_context : Context.t -> Context_wrapper.Memory_context.t
