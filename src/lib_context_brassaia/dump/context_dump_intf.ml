(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <nomadic@tezcore.com>               *)
(* Copyright (c) 2018-2020 Tarides <contact@tarides.com>                     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

type error +=
  | System_write_error of string
  | Bad_hash of string * Bytes.t * Bytes.t
  | Context_not_found of Bytes.t
  | System_read_error of string
  | Inconsistent_snapshot_file
  | Inconsistent_snapshot_data
  | Missing_snapshot_data
  | Invalid_snapshot_version of string * string list
  | Restore_context_failure

(* A hash that has been tagged with the kind of the value that it references
   (either a node or a blob). *)
module type Kinded_hash = sig
  type t

  type hash

  val encoding : t Data_encoding.t
end

module type Dump_interface = sig
  type index

  type context

  type tree

  type hash

  type contents := bytes

  type step := string

  type commit_info

  type batch

  val batch : index -> (batch -> 'a Lwt.t) -> 'a Lwt.t

  val commit_info_encoding : commit_info Data_encoding.t

  val hash_equal : hash -> hash -> bool

  module Block_header : sig
    type t = Block_header.t

    val to_bytes : t -> Bytes.t

    val of_bytes : Bytes.t -> t option

    val equal : t -> t -> bool

    val encoding : t Data_encoding.t
  end

  module Commit_hash : sig
    type t

    val to_bytes : t -> Bytes.t

    val of_bytes : Bytes.t -> t tzresult

    val encoding : t Data_encoding.t
  end

  module Kinded_hash : Kinded_hash with type hash := hash

  (* Commit manipulation (for parents) *)
  val context_parents : context -> Commit_hash.t list

  (* Commit info *)
  val context_info : context -> commit_info

  (* Retrieve context *)
  val checkout : index -> Commit_hash.t -> context option Lwt.t

  val set_context :
    info:commit_info ->
    parents:Commit_hash.t list ->
    context ->
    Commit_hash.t ->
    bool Lwt.t

  (* for dumping *)
  val context_tree : context -> tree

  (* for restoring *)
  val make_context : index -> context

  val update_context : context -> tree -> context

  val add_bytes : batch -> bytes -> tree Lwt.t

  val add_dir :
    batch ->
    (step * Kinded_hash.t, error trace) Seq_es.t ->
    (tree option, error trace) result Lwt.t

  (** This type exposes the internal nodes used in brassaia in order to use them
      during snapshot import and export. *)
  module Snapshot : sig
    type kinded_hash = Contents of hash * unit | Node of hash

    type entry = {step : string; hash : kinded_hash}

    type inode_tree = {depth : int; length : int; pointers : (int * hash) list}

    type v = Inode_tree of inode_tree | Inode_value of entry list

    type inode = {v : v; root : bool}

    type t = Inode of inode | Blob of contents

    val encoding : inode Data_encoding.t
  end

  (** [tree_iteri_unique ?on_disk index f tree] traverses
     [tree], applying [f] to all inodes and contents.

     [f] is called in post-order, that is [f] is first called on the
     leaves, and the last call to [f] is on the root of the tree.

     The traversal order is stable.

     The traversal skips objects that are structurally equal to
     objects that were already traversed. In other words,
     [tree_iteri_unique] internally uses a hash set in order to
     guarantee that all the objects passed to [f] don't hash the same
     way.

     If [on_disk] is true (by default its false), it uses an on_disk index.

     Returns the total number of elements visited. *)
  val tree_iteri_unique :
    ?on_disk:bool -> index -> (Snapshot.t -> unit Lwt.t) -> tree -> int Lwt.t

  type import

  (** [v_import ?in_memory index] creates an [importer] instance. If
     [in_memory] is true, the import will be fully in memory.
     [in_memory] is set to false by default. *)
  val v_import : ?in_memory:bool -> index -> import

  (** [save_inode index importer elt] saves [elt] to the store. *)
  val save_inode : index -> import -> Snapshot.t -> tree option Lwt.t

  (** [close importer] close the [importer] instance.*)
  val close_import : import -> index -> unit
end

module type S = sig
  type index

  type context

  type block_header

  type context_hash

  val restore_context_fd :
    index ->
    expected_context_hash:context_hash ->
    fd:Lwt_unix.file_descr ->
    nb_context_elements:int ->
    in_memory:bool ->
    progress_display_mode:Animation.progress_display_mode ->
    unit tzresult Lwt.t
end

module type Context_dump = sig
  module type Dump_interface = Dump_interface

  module type S = S

  module Make (I : Dump_interface) :
    S
      with type index := I.index
       and type context := I.context
       and type block_header := I.Block_header.t
       and type context_hash := I.Commit_hash.t
end
