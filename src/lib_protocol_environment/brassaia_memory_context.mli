(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
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
open Tezos_context_brassaia_memory

open Environment_context

(** The type of the context backing {!Memory_context}. Main use is
    the parameterization of {!Environment_context.Context.kind} below,
    as well as the instantiation of the {!S} signature below (the module type
    of {!M}). *)
type t = Tezos_context_memory.Context.t

(** The additional kind identifying {!Memory_context} values. Used to
    detect at runtime when a memory context is expected, to disambiguate
    from other kinds. *)
type _ Context.kind += Context : t Context.kind

(** [empty] creates a pristine memory context: a {!Protocol_environment.Context.t}
    value whose kind indicates that it is a memory context. See {!wrap_memory_context}
    for an alternative constructor. *)
val empty : Context.t

(** [encoding] is an appropriate encoding for {!Environment_context.Context.t} values
    whose kind is [Memory_context]. This is used by the mockup for storing
    its state on disk. This is unique to [Memory_context]:

    - [Shell_context] is backed by an Irmin on-disk storage and so doesn't need
      its own serialization mechanism.
    - [Proxy_context] doesn't need to be serializable. *)
val encoding : Context.t Data_encoding.t

(** [M] is the [Memory_context] specific instance of {!Environment_context_intf.S} *)
module M : S with type t = t

(** [wrap_memory_context t] creates a memory context from an Irmin in-memory tree:
    in this signature, [t] comes from {!Tezos_context_memory.Context}
    (in [src/lib_context]. See {!empty} for an alternative constructor. *)
val wrap_memory_context : t -> Context.t

(** [unwrap_memory_context ctxt] returns the {!Tezos_context_memory.Context.t}
    underlying [ctxt]. *)
val unwrap_memory_context : Context.t -> t
