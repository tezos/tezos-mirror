(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2025 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Stripped down signature of Irmin_context *)

type repo

(** The type of trees stored in the context, i.e. the actual data. *)
type tree

(** The type of indexed repository for contexts. The parameter indicates if the
    index can be written or only read. *)
type 'a index = ('a, repo) Context_sigs.index

(** Read/write {!type:index}. *)
type rw_index = [`Read | `Write] index

(** Read only {!type:index}. *)
type ro_index = [`Read] index

(** The type of context with its content. *)
type 'a t = ('a, repo, tree) Context_sigs.t

(** Read/write context {!t}. *)
type rw = [`Read | `Write] t

(** A context hash is the hash produced when the data of the context is
    committed to disk, i.e. the {!type:commit} hash. *)
type hash

(** [load cache_size path] initializes from disk a context from [path].
    [cache_size] allows to change the LRU cache size of Irmin
    (100_000 by default at irmin-pack/config.ml *)
val load :
  cache_size:int ->
  ?async_domain:bool ->
  'a Access_mode.t ->
  string ->
  'a index tzresult Lwt.t

val reload : ro_index -> unit

(** [commit ?message context] commits content of the context [context] on disk,
    and return the commit hash. *)
val commit : ?message:string -> [> `Write] t -> hash Lwt.t

val checkout_exn : 'a index -> hash -> 'a t Lwt.t

(** [empty ctxt] is the context with an empty content for the repository [ctxt]. *)
val empty : 'a index -> 'a t

(** [split ctxt] creates a new suffix file, also called "chunk", into the
    Irmin's file hierarchy. This split function is expected to be called after
    committing a commit that will be a future candidate for a GC target.  *)
val split : _ index -> unit

(** [gc index ?callback hash] removes all data older than [hash] from disk.
    If passed, [callback] will be executed when garbage collection finishes. *)
val gc :
  [> `Write] index -> ?callback:(unit -> unit Lwt.t) -> hash -> unit Lwt.t

(** [wait_gc_completion index] will return a blocking thread if a
    GC run is currently ongoing. *)
val wait_gc_completion : [> `Write] index -> unit Lwt.t

val context_hash_of_hash : hash -> Smart_rollup_context_hash.t

val hash_of_context_hash : Smart_rollup_context_hash.t -> hash

(** State of the PVM that this rollup node deals with *)
module PVMState : sig
  (** The value of a PVM state *)
  type value = tree

  (** [empty ()] is the empty PVM state. *)
  val empty : unit -> value

  val get : _ t -> value Lwt.t

  (** [set context state] saves the PVM state [state] in the context and returns
      the updated context. Note: [set] does not perform any write on disk, this
      information must be committed using {!val:commit}. *)
  val set : 'a t -> value -> 'a t Lwt.t
end

(** Access to underlying Irmin tree for components which cannot be abstract
   over the PVM durable storage backend type. *)
module Wasm_internal : sig
  val to_irmin : tree -> Irmin_context.tree

  val of_irmin : Irmin_context.tree -> tree
end
