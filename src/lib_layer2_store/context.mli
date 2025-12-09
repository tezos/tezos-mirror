(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module is largely inspired from
    {!module:Tezos_protocol_environment.Environement_context} *)

open Context_sigs

(** This module dipatches context calls to contexts/pvm_states
    corresponding to the used pvm *)

(** See {!module:Tezos_protocol_environment.Environement_context.ops} *)
type ('repo, 'tree) pvm_context_impl =
  (module Context_sigs.S with type repo = 'repo and type tree = 'tree)

(* Context existential that embeds the context_module associated to
   pvm protocol_plugins *)

(** See {!module:Tezos_protocol_environment.Environement_context.t} *)
type ('a, 'repo, 'tree, 'loaded_tree) container = private {
  index : ('a, 'repo) index;
  pvm_context_impl : ('repo, 'tree) pvm_context_impl;
  impl_name : string;
  tree : 'loaded_tree;
  equality_witness : ('repo, 'tree) equality_witness;
}

type 'a index = private Index : ('a, 'repo, 'tree, unit) container -> 'a index

type 'a t = private Context : ('a, 'repo, 'tree, 'tree) container -> 'a t

(** Read/write context {!t}. *)
type rw = [`Read | `Write] t

(** Read-only context {!t}. *)
type ro = [`Read] t

(** Read/write {!index}. *)
type rw_index = [`Read | `Write] index

(** Read-only {!index}. *)
type ro_index = [`Read] index

val make_index :
  index:('a, 'b) Context_sigs.index ->
  pvm_context_impl:('b, 'c) pvm_context_impl ->
  equality_witness:('b, 'c) equality_witness ->
  impl_name:string ->
  'a index

val equiv :
  'a Equality_witness.t * 'b Equality_witness.t ->
  'c Equality_witness.t * 'd Equality_witness.t ->
  ('a, 'c) Equality_witness.eq option * ('b, 'd) Equality_witness.eq option

module Hash = Smart_rollup_context_hash

type hash = Hash.t

(** [load cache_size path] initializes from disk a context from
    [path]. [cache_size] allows to change size of the Context Backend
    in use (for instance, the LRU cache size of Irmin (100_000 by
    default at irmin-pack/config.ml) *)
val load :
  ('repo, 'tree) pvm_context_impl ->
  cache_size:int ->
  ([< `Read | `Write > `Read] as 'a) Access_mode.t ->
  string ->
  'a index tzresult Lwt.t

(** [index context] is the repository of the context [context]. *)
val index : 'a t -> 'a index

(** [close ctxt] closes the context index [ctxt]. *)
val close : 'a index -> unit Lwt.t

(** [readonly index] returns a read-only version of the index. *)
val readonly : _ index -> [`Read] index

(** [checkout ctxt hash] checkouts the content that corresponds to the commit
    hash [hash] in the repository [ctxt] and returns the corresponding
    context. If there is no commit that corresponds to [hash], it returns
    [None].  *)
val checkout : 'a index -> hash -> 'a t option Lwt.t

(** [empty ctxt] is the context with an empty content for the repository [ctxt]. *)
val empty : 'a index -> 'a t

(** [commit ?message context] commits content of the context [context] on disk,
    and return the commit hash. *)
val commit : ?message:string -> [`Read | `Write] t -> hash Lwt.t

(** [is_gc_finished index] returns true if a GC is finished (or idle) and false
    if a GC is running for [index]. *)
val is_gc_finished : [`Read | `Write] index -> bool

(** [cancel_gc index] stops the Irmin GC if it is currently running for
    [index]. It returns [true] if a GC was canceled. *)
val cancel_gc : [`Read | `Write] index -> bool

(** [split ctxt] creates a new suffix file, also called "chunk", into the context's
    file hierarchy. This split function is expected to be called after
    committing a commit that will be a future candidate for a GC target.  *)
val split : _ index -> unit

(** [gc index ?callback hash] removes all data older than [hash] from disk.
    If passed, [callback] will be executed when garbage collection finishes. *)
val gc :
  [`Read | `Write] index -> ?callback:(unit -> unit Lwt.t) -> hash -> unit Lwt.t

(** [wait_gc_completion index] will return a blocking thread if a
    GC run is currently ongoing. *)
val wait_gc_completion : [`Read | `Write] index -> unit Lwt.t

(** [export_snapshot index context_hash ~path] exports the context corresponding
    to [context_hash], if found in [index], into the given folder path. *)
val export_snapshot : _ index -> hash -> path:string -> unit tzresult Lwt.t

(* Pvm_state that embeds the context_module embedded associated to pvm
   protocol_plugins *)
type pvmstate =
  | PVMState : {
      pvm_context_impl : ('repo, 'tree) pvm_context_impl;
      impl_name : string;
      pvmstate : 'tree;
      equality_witness : ('repo, 'tree) equality_witness;
    }
      -> pvmstate

val make_pvmstate :
  pvm_context_impl:('a, 'b) pvm_context_impl ->
  equality_witness:('a, 'b) equality_witness ->
  impl_name:string ->
  pvmstate:'b ->
  pvmstate

(** State of the PVM that this rollup node deals with *)
module PVMState : sig
  (** The value of a PVM state *)
  type value = pvmstate

  (** [empty ()] is the empty PVM state. *)
  val empty : 'a index -> value

  (** [find context] returns the PVM state stored in the [context], if any. *)
  val find : 'a t -> value option Lwt.t

  (** [get context] is the same as {!find} but fails if there is no PVM state
      stored in the context. *)
  val get : 'a t -> value tzresult Lwt.t

  (** [lookup state path] returns the data stored for the path [path] in the PVM
      state [state].  *)
  val lookup : value -> string list -> bytes option Lwt.t

  (** [set context state] saves the PVM state [state] in the context and returns
      the updated context. Note: [set] does not perform any write on disk, this
      information must be committed using {!val:commit}. *)
  val set : 'a t -> value -> 'a t Lwt.t
end

module Version : sig
  type t

  (** The current and expected version of the context. *)
  val version : t

  (** The encoding for the context version. *)
  val encoding : t Data_encoding.t

  (** [check v] fails if [v] is different from the expected version of the
      context. *)
  val check : t -> unit tzresult

  (** String representation of context version. *)
  val to_string : t -> string
end

module Internal_for_tests : sig
  (** [get_a_tree key] provides a value of internal type [tree] which can be
      used as a state to be set in the context directly. *)
  val get_a_tree : (module Context_sigs.S) -> string -> pvmstate Lwt.t
end

module Wrapper : sig
  (** Context wrappers translate from/to node-context and node-pvmstate PVMs
      internal representation to those used in the PVM.  Also provides
      conversion functions from/to mutable and immutable PVM types.  Each
      different PVM context will imply a dedicated wrapper.*)
  module type S = sig
    type repo

    type tree

    (** Type used by the mutable API for PVMs *)
    type mut_state

    val of_node_context : 'a index -> ('a, repo) Context_sigs.index

    val to_node_context : ('a, repo) Context_sigs.index -> 'a index

    val of_node_pvmstate : pvmstate -> tree

    val to_node_pvmstate : tree -> pvmstate

    val from_imm : tree -> mut_state

    val to_imm : mut_state -> tree
  end

  (** Specialized module to handle translation to/from a specific context
      backend implementation *)
  module Make (C : sig
    include Context_sigs.S

    type mut_state

    val from_imm : tree -> mut_state

    val to_imm : mut_state -> tree
  end) :
    S
      with type repo = C.repo
       and type tree = C.tree
       and type mut_state = C.mut_state
end
