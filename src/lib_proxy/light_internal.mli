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

(** Prints a key *)
val key_to_string : string list -> string

(** Translates a [raw_context], i.e. a tree sent by the [raw_bytes] RPC,
    to an Irmin tree that can be integrated in the light mode's store. *)
val raw_context_to_irmin_tree :
  Tezos_shell_services.Block_services.raw_context ->
  Tezos_context_memory.Context.tree Lwt.t

(** Module containing operations of Merkle proofs used by the light mode *)
module Merkle : sig
  (** Transform a Merkle tree into an Irmin tree. Parameters are:

      * The repo where the result should live
      * The Merkle tree to translate

      Returns: The Irmin tree obtained or an error message *)
  val merkle_tree_to_irmin_tree :
    Tezos_context_memory.Context.Tree.repo ->
    Tezos_shell_services.Block_services.merkle_tree ->
    (Tezos_context_memory.Context.tree, string) result Lwt.t

  (** Whether an Irmin tree contains a Merkle tree (in particular
      whether they agree on hashes). Returns unit if the Irmin tree
      contains the Merkle tree, otherwise a message explaining why it is
      not the case. *)
  val contains_merkle_tree :
    Tezos_context_memory.Context.tree ->
    Tezos_shell_services.Block_services.merkle_tree ->
    (unit, string) result Lwt.t

  (** Union an Irmin tree and a Merkle tree. Parameters are:

      * The repo where the Irmin tree is stored (necessary to create shallow
        trees from Merkle hashes)
      * The Irmin tree
      * The Merkle tree

      Returns: an augmented variant of the input Irmin tree or an error
               message. *)
  val union_irmin_tree_merkle_tree :
    Tezos_context_memory.Context.Tree.repo ->
    Tezos_context_memory.Context.tree ->
    Tezos_shell_services.Block_services.merkle_tree ->
    (Tezos_context_memory.Context.tree, string) result Lwt.t

  (** [trees_shape_match path t1 t2] returns [Ok ()] if [t1] and [t2] have the same shape
      (tree of keys), otherwise an [Error] explaining how the shapes differ.
      The shape check tolerates a shape difference for nodes along the key's
      path (because at the end of this path, the first endpoint returns data; whereas
      validating endpoints return a hash). *)
  val trees_shape_match :
    string list ->
    Tezos_shell_services.Block_services.merkle_tree ->
    Tezos_shell_services.Block_services.merkle_tree ->
    (unit, string list) result
end
