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

open Protocol
open Alpha_context

(** The type of indexed repository for contexts  *)
type index

(** The type of trees stored in the context, i.e. the actual data. *)
type tree

(** The type of context with its content *)
type t

(** A context hash is the hash produced when the data of the context is
    committed to disk, i.e. the {!commit} hash. *)
type hash

(** The type of commits for the context. *)
type commit

(** [hash_encoding] is the encoding for context hashes, of type {!hash}. *)
val hash_encoding : hash Data_encoding.t

(** [hash_to_raw_string h] is the raw string representation for the hash [h]. *)
val hash_to_raw_string : hash -> string

(** [pp_hash fmt h] prints the hash [h] in hexadecimal notation on the formatter
    [fmt]. *)
val pp_hash : Format.formatter -> hash -> unit

(** [load config] initializes from disk a context using the [data_dir]
    information contained in the configuration [config]. *)
val load : Configuration.t -> index Lwt.t

(** [close ctxt] closes the context index [ctxt]. *)
val close : index -> unit Lwt.t

(** [raw_commit ?message ctxt tree] commits the [tree] in the context repository
    [ctxt] on disk, and return the commit. *)
val raw_commit : ?message:string -> index -> tree -> commit Lwt.t

(** [commit ?message context] commits content of the context [context] on disk,
    and return the commit hash. *)
val commit : ?message:string -> t -> hash Lwt.t

(** [checkout ctxt hash] checkouts the content that corresponds to the commit
    hash [hash] in the repository [ctxt] and returns the corresponding
    context. If there is no commit that corresponds to [hash], it returns
    [None].  *)
val checkout : index -> hash -> t option Lwt.t

(** [empty ctxt] is the context with an empty content for the repository [ctxt]. *)
val empty : index -> t

(** [is_empty context] returns [true] iff the context content of [context] is
    empty. *)
val is_empty : t -> bool

(** Module for generating and verifying proofs for a context *)
module Proof (Hash : sig
  type t

  val of_context_hash : Context_hash.t -> t
end) : sig
  module Tree :
    Tezos_context_sigs.Context.TREE
      with type key = string list
       and type value = bytes
       and type t = index
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
    index -> tree -> (tree -> (tree * 'a) Lwt.t) -> (proof * 'a) option Lwt.t

  (** [verify_proof proof f] verifies that [f] produces the proof [proof] and
      returns the resulting [tree], or [None] if the proof cannot be
      verified. *)
  val verify_proof :
    proof -> (tree -> (tree * 'a) Lwt.t) -> (tree * 'a) option Lwt.t
end

(** Aggregated collection of messages from the L1 inbox. *)
module MessageTrees : sig
  (** The value of a messages tree  *)
  type value

  (** [find context] returns the messages tree stored in the [context], if any. *)
  val find : t -> value option Lwt.t

  (** [set context msg_tree] saves the messages tree [msg_tree] in the context
      and returns the updated context. Note: [set] does not perform any write on
      disk, this information must be committed using {!commit}. *)
  val set : t -> value -> t Lwt.t
end

(** L1 inboxes representation in the rollup node. This is a version of the
    protocols inboxes specialized to message trees ({!MessageTrees}) and which
    can produce proofs for inboxes stored in the context. *)
module Inbox : sig
  type t = Sc_rollup.Inbox.t

  module Message : module type of Sc_rollup.Inbox.Message

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val inbox_level : t -> Raw_level.t

  type history_proof = Sc_rollup.Inbox.history_proof

  include
    Sc_rollup.Inbox.Merkelized_operations
      with type tree = MessageTrees.value
       and type inbox_context = index
end

(** State of the PVM that this rollup node deals with *)
module PVMState : sig
  (** The value of a PVM state *)
  type value = tree

  (** [find context] returns the PVM state stored in the [context], if any. *)
  val find : t -> value option Lwt.t

  (** [lookup state path] returns the data stored for the path [path] in the PVM
      state [state].  *)
  val lookup : value -> string list -> bytes option Lwt.t

  (** [set context state] saves the PVM state [state] in the context and returns
      the updated context. Note: [set] does not perform any write on disk, this
      information must be committed using {!commit}. *)
  val set : t -> value -> t Lwt.t
end
