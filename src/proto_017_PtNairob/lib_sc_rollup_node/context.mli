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

open Store_sigs

(** The type of indexed repository for contexts. The parameter indicates if the
    index can be written or only read. *)
type 'a index constraint 'a = [< `Read | `Write > `Read]

(** Read/write {!index}. *)
type rw_index = [`Read | `Write] index

(** Read only {!index}. *)
type ro_index = [`Read] index

(** The type of trees stored in the context, i.e. the actual data. *)
type tree

(** The type of context with its content. *)
type 'a t constraint 'a = [< `Read | `Write > `Read]

(** Read/write context {!t}. *)
type rw = [`Read | `Write] t

(** Read-only context {!t}. *)
type ro = [`Read] t

(** A context hash is the hash produced when the data of the context is
    committed to disk, i.e. the {!commit} hash. *)
type hash = Smart_rollup_context_hash.t

(** The type of commits for the context. *)
type commit

(** [load path] initializes from disk a context from [path]. *)
val load : 'a mode -> string -> 'a index tzresult Lwt.t

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

(** [empty ctxt] is the context with an empty content for the repository [ctxt]. *)
val empty : 'a index -> 'a t

(** [is_empty context] returns [true] iff the context content of [context] is
    empty. *)
val is_empty : _ t -> bool

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
  type value = tree

  (** [empty ()] is the empty PVM state. *)
  val empty : unit -> value

  (** [find context] returns the PVM state stored in the [context], if any. *)
  val find : _ t -> value option Lwt.t

  (** [lookup state path] returns the data stored for the path [path] in the PVM
      state [state].  *)
  val lookup : value -> string list -> bytes option Lwt.t

  (** [set context state] saves the PVM state [state] in the context and returns
      the updated context. Note: [set] does not perform any write on disk, this
      information must be committed using {!commit}. *)
  val set : 'a t -> value -> 'a t Lwt.t
end

(** Static information about the rollup. *)
module Rollup : sig
  val get_address :
    _ index -> Octez_smart_rollup.Address.t option tzresult Lwt.t

  val check_or_set_address :
    'a mode -> 'a index -> Octez_smart_rollup.Address.t -> unit tzresult Lwt.t
end
