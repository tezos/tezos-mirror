(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
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

type repo

(** The type of trees stored in the context, i.e. the actual data. *)
type tree

type state = tree

type mut_state = state ref

type 'a raw_index = ('a, repo) Context_sigs.raw_index

(** The type of indexed repository for contexts. The parameter indicates if the
    index can be written or only read. *)
type 'a index = ('a, repo) Context_sigs.index

(** Read/write {!type:index}. *)
type rw_index = [`Read | `Write] index

(** Read only {!type:index}. *)
type ro_index = [`Read] index

(** The type of context with its content. *)
type 'a t = ('a, repo, mut_state) Context_sigs.t

(** Read/write context {!t}. *)
type rw = [`Read | `Write] t

(** Read-only context {!t}. *)
type ro = [`Read] t

module Tree :
  Tezos_context_sigs.Context.TREE
    with type t := unit
     and type key = string list
     and type value = bytes
     and type tree = tree

(** A context hash is the hash produced when the data of the context is
    committed to disk, i.e. the {!type:commit} hash. *)
type hash

(** The type of commits for the context. *)
type commit

val impl_name : string

val equality_witness : (repo, state, mut_state) Context_sigs.equality_witness

val from_imm : state -> mut_state

val to_imm : mut_state -> state

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

(** [index context] is the repository of the context [context]. *)
val index : 'a t -> 'a index

(** [close ctxt] closes the context index [ctxt]. *)
val close : _ index -> unit Lwt.t

(** [readonly index] returns a read-only version of the index. *)
val readonly : [> `Read] index -> [`Read] index

(** [raw_commit ?message ctxt tree] commits the [tree] in the context repository
    [ctxt] on disk, and return the commit. *)
val raw_commit : ?message:string -> [> `Write] index -> tree -> commit Lwt.t

(** [commit ?message context] commits content of the context [context] on disk,
    and return the commit hash. *)
val commit : ?message:string -> [> `Write] t -> hash Lwt.t

(** [checkout ctxt hash] checkouts the content that corresponds to the commit
    hash [hash] in the repository [ctxt] and returns the corresponding
    context. If there is no commit that corresponds to [hash], it returns
    [None].  *)
val checkout : 'a index -> hash -> 'a t option Lwt.t

val checkout_exn : 'a index -> hash -> 'a t Lwt.t

(** [empty ctxt] is the context with an empty content for the repository [ctxt]. *)
val empty : 'a index -> 'a t

(** [is_empty context] returns [true] iff the context content of [context] is
    empty. *)
val is_empty : _ t -> bool

(** [split ctxt] creates a new suffix file, also called "chunk", into the
    Irmin's file hierarchy. This split function is expected to be called after
    committing a commit that will be a future candidate for a GC target.  *)
val split : _ index -> unit

(** [gc index ?callback hash] removes all data older than [hash] from disk.
    If passed, [callback] will be executed when garbage collection finishes. *)
val gc :
  [> `Write] index -> ?callback:(unit -> unit Lwt.t) -> hash -> unit Lwt.t

(** [is_gc_finished index] returns true if a GC is finished (or idle) and false
    if a GC is running for [index]. *)
val is_gc_finished : [> `Write] index -> bool

(** [cancel_gc index] stops the Irmin GC if it is currently running for
    [index]. It returns [true] if a GC was canceled. *)
val cancel_gc : [> `Write] index -> bool

(** [wait_gc_completion index] will return a blocking thread if a
    GC run is currently ongoing. *)
val wait_gc_completion : [> `Write] index -> unit Lwt.t

(** [export_snapshot index context_hash ~path] exports the context corresponding
    to [context_hash], if found in [index], into the given folder path. As the
    export uses the GC's behaviour to extract a single commit into a standalone
    fresh store, it is not possible to export a snapshot while a GC is
    running. This call will hang until the GC has finished.

    Note: there is no associated [import_snapshot] function as the import
    consist in copying the exported Irmin store. *)
val export_snapshot : _ index -> hash -> path:string -> unit tzresult Lwt.t

val context_hash_of_hash : hash -> Smart_rollup_context_hash.t

val hash_of_context_hash : Smart_rollup_context_hash.t -> hash

(** Module for generating and verifying proofs for a context *)
module Proof (Hash : sig
  type t

  val of_context_hash : Context_hash.t -> t
end) (Proof_encoding : sig
  val proof_encoding :
    Tezos_context_sigs.Context.Proof_types.tree
    Tezos_context_sigs.Context.Proof_types.t
    Data_encoding.t
end) : sig
  (** Tree representation for proof generation.

      NOTE: The index needs to be accessed with write permissions because we
      need to commit on disk to generate the proofs (e.g. in
      {!Inbox.produce_proof}, {!PVM.produce_proof}. or
      {!PVM.produce_output_proof}). *)
  module Tree :
    Tezos_context_sigs.Context.TREE
      with type key = string list
       and type value = bytes
       and type t = rw_index
       and type tree = tree

  type tree = Tree.tree

  (** See {!Sc_rollup_PVM_sem.proof} *)
  type proof

  val hash_tree : tree -> Hash.t

  (** See {!Sc_rollup_PVM_sem.proof_encoding} *)
  val proof_encoding : proof Data_encoding.t

  (** [proof_before proof] is the hash of the state before the step that
      generated [rpoof].  *)
  val proof_before : proof -> Hash.t

  (** [proof_after proof] is the hash of the state after the step that generated
      [rpoof].  *)
  val proof_after : proof -> Hash.t

  (** [cast_read_only] replaces [proof.after] with [proof.before]. *)
  val cast_read_only : proof -> proof

  (** [produce_proof ctxt tree f] produces and returns a proof for the execution
      of [f] on the state [tree]. *)
  val produce_proof :
    rw_index -> tree -> (tree -> (tree * 'a) Lwt.t) -> (proof * 'a) option Lwt.t

  (** [verify_proof proof f] verifies that [f] produces the proof [proof] and
      returns the resulting [tree], or [None] if the proof cannot be
      verified. *)
  val verify_proof :
    proof -> (tree -> (tree * 'a) Lwt.t) -> (tree * 'a) option Lwt.t
end

(** State of the PVM that this rollup node deals with *)
module PVMState : sig
  (** The value of a PVM state *)
  type value = mut_state

  (** [empty ()] is the empty PVM state. *)
  val empty : unit -> value

  (** [find context] returns the PVM state stored in the [context], if any. *)
  val find : _ t -> value option Lwt.t

  val get : _ t -> value Lwt.t

  (** [lookup state path] returns the data stored for the path [path] in the PVM
      state [state].  *)
  val lookup : value -> string list -> bytes option Lwt.t

  (** [set context state] saves the PVM state [state] in the context. Note:
      [set] does not perform any write on disk, this information must be
      committed using {!val:commit}. *)
  val set : 'a t -> value -> unit Lwt.t
end

module Internal_for_tests : sig
  (** [get_a_tree key] provides a value of internal type [tree] which can be
      used as a state to be set in the context directly. *)
  val get_a_tree : string -> tree Lwt.t
end
