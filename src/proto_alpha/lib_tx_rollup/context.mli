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

(** A block-indexed (key x value) store directory. *)
type index

(** Type of persitant context. *)
type context

include
  Protocol.Tx_rollup_l2_context_sig.CONTEXT
    with type t = context
     and type 'a m = 'a tzresult Lwt.t

val index : context -> index

(** Open or initialize a versioned store at a given path. *)
val init :
  ?patch_context:(context -> context tzresult Lwt.t) ->
  ?readonly:bool ->
  ?indexing_strategy:[`Always | `Minimal] ->
  ?index_log_size:int ->
  string ->
  index Lwt.t

(** Initialize an "empty" context from an index. It is not really empty in the
    sense that the underlying tree is not empty, it is then committed. *)
val init_context : index -> t Lwt.t

(** Close the index. Does not fail when the context is already closed. *)
val close : index -> unit Lwt.t

(** Sync the context with disk. Only useful for read-only instances.
    Does not fail when the context is not in read-only mode. *)
val sync : index -> unit Lwt.t

(** {2 Accessing and Updating Versions} *)

(** Returns true if there is a commit with this context hash *)
val exists : index -> Protocol.Tx_rollup_l2_context_hash.t -> bool Lwt.t

(** Checkout the context associated to a context hash. The context must have
    been committed (with {!commit}). Resolves with [None] if there is no such
    commit. *)
val checkout_opt :
  index -> Protocol.Tx_rollup_l2_context_hash.t -> context option Lwt.t

(** Same as {!checkout_opt} but resolves with an exception if there is no such
    commit. *)
val checkout_exn :
  index -> Protocol.Tx_rollup_l2_context_hash.t -> context Lwt.t

(** Same as {!checkout_opt} but resolves with an error if there is no such
    commit. *)
val checkout :
  index -> Protocol.Tx_rollup_l2_context_hash.t -> context tzresult Lwt.t

(** Hash a context. The hash can be done with an additional [message]. *)
val hash : ?message:string -> t -> Protocol.Tx_rollup_l2_context_hash.t

(** Create a commit and return the context hash. The hash can be done with an
    additional [message]. *)
val commit :
  ?message:string -> context -> Protocol.Tx_rollup_l2_context_hash.t Lwt.t

(** {2 Prover Context} *)

(** The prover context is a subset of the context. It uses the internal
    context tree to produce proofs. *)

type tree

module Prover_context :
  Protocol.Tx_rollup_l2_context_sig.CONTEXT
    with type t = tree
     and type 'a m = 'a Lwt.t

(** ['a produce_proof_result] is the result type needed for the {!produce_proof}
    callback function. *)
type 'a produce_proof_result = {
  tree : tree;  (** the tree modified by the callback function *)
  result : 'a;  (** the callback result. *)
}

(** [produce_proof ctxt f] applies [f] in the {!tree} inside [ctxt].

    It returns a proof that is produced by applying [f], the proof is
    constructed using low-levels accesses to the three, that is, it needs the
    modified tree to be included in the [f]'s result to calculate the proof.

    Beside the proof production, this function can be used to perform semantical
    changes in the {!Prover_context}. Thus, we give the possibility to return a
    result in {!'a produce_proof_result} to observe [f]'s results.
*)
val produce_proof :
  context ->
  (tree -> 'a produce_proof_result Lwt.t) ->
  (Protocol.Tx_rollup_l2_proof.t * 'a produce_proof_result) tzresult Lwt.t

val hash_tree : tree -> Context_hash.t

(** [add_tree ctxt tree] adds [tree] in the [ctxt]. In order to perform
    actions on the tree (e.g. proof production), it needs to be persistent. Thus,
    the context is committed on disk after we added the tree, that is, after
    every modification on the tree such as a message interpretation.

    FIXME: https://gitlab.com/tezos/tezos/-/issues/2780
    We would like to avoid the commit in this function for performance
    matters.
*)
val add_tree :
  context -> tree -> (context * Protocol.Tx_rollup_l2_context_hash.t) Lwt.t

val tree_hash_of_context : context -> Context_hash.t tzresult Lwt.t

(** {2 Sub-context for tickets } *)

(** Adds a new association [ticket index -> ticket] in the context. *)
val register_ticket :
  context ->
  Protocol.Tx_rollup_l2_context_sig.ticket_index ->
  Ticket.t ->
  context Lwt.t

(** [get_ticket ctxt ticket_index] retrieves the ticket associated to
    [ticket_index] in the context [ctxt]. Resolves with [None] if there is no
    ticket associated to [ticket_hash]. *)
val get_ticket :
  context ->
  Protocol.Tx_rollup_l2_context_sig.ticket_index ->
  Ticket.t option Lwt.t

(** {2 Sub-context for address indexes } *)

(** Adds a new association [address index -> address] in the context. *)
val register_address :
  context ->
  Protocol.Tx_rollup_l2_context_sig.address_index ->
  Protocol.Tx_rollup_l2_address.t ->
  context Lwt.t

(** [get_address ctxt address_index] retrieves the address associated to
    [address_index] in the context [ctxt]. Resolves with [None] if there is no
    address associated to [address_index]. *)
val get_address :
  context ->
  Protocol.Tx_rollup_l2_context_sig.address_index ->
  Protocol.Tx_rollup_l2_address.t option Lwt.t
