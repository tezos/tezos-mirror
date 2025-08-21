(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import

(** [Brassaia-pack-unix]-specific extensions to the [Store] module type. *)

module type S = sig
  (** An [brassaia-pack-unix] store. This provides the common {!Brassaia} interface
      with [brassaia-pack-unix] specific extensions. *)

  (** @inline *)
  include Brassaia.Generic_key.S

  (** {1 Integrity Check} *)

  (** Checks the integrity of the repository. if [auto_repair] is [true], will
      also try to fix the issues. [ppf] is a formatter for progressive
      reporting. [`Fixed] and [`Corrupted] report the number of fixed/corrupted
      entries. *)
  val integrity_check :
    ?ppf:Format.formatter ->
    ?heads:commit list ->
    auto_repair:bool ->
    repo ->
    ( [> `Fixed of int | `No_error],
      [> `Cannot_fix of string | `Corrupted of int] )
    result

  val integrity_check_inodes :
    ?heads:commit list ->
    repo ->
    ([> `No_error], [> `Cannot_fix of string]) result

  val traverse_pack_file :
    [ `Reconstruct_index of [`In_place | `Output of string]
    | `Check_index
    | `Check_and_fix_index ] ->
    Brassaia.config ->
    unit

  val test_traverse_pack_file :
    [ `Reconstruct_index of [`In_place | `Output of string]
    | `Check_index
    | `Check_and_fix_index ] ->
    Brassaia.config ->
    unit

  (** {1 Chunking} *)

  (** [split t] starts a fresh chunk file for appending data. Only allowed on
      stores that allow gc.

      Raises [RO_Not_Allowed] if called by a readonly instance.

      Raises [Split_forbidden_during_batch] if called inside a batch operation.

      Raises [Multiple_empty_chunks] if no data was added to the currently-used
      chunk file.

      Raises [Split_disallowed] if {!is_split_allowed} is false.

      TODO: Detail exceptions raised. *)
  val split : repo -> unit

  (** [is_split_allowed repo] returns if split is supported. Currently returns
      the same value as {!Gc.is_allowed}. *)
  val is_split_allowed : repo -> bool

  (** {1 Lower layer} *)

  (** [add_volume t] creates a new empty volume in the lower layer.

      Raises [RO_Not_Allowed] if called by a readonly instance.

      Raises [Add_volume_forbidden_during_gc] if called while a GC is running.

      Raises [Multiple_empty_volumes] if there is already an empty volume. *)
  val add_volume : repo -> unit

  (** {1 On-disk} *)

  (** [reload t] reloads a readonly pack with the files on disk. Raises
      [invalid_argument] if called by a read-write pack.*)
  val reload : repo -> unit

  (** [flush t] flush read-write pack on disk. Raises [RO_Not_Allowed] if called
      by a readonly instance.*)
  val flush : repo -> unit

  (** [create_one_commit_store t key path] creates a new store at [path] from
      the existing one, containing only one commit, specified by the [key]. Note
      that this operation is blocking.

      It requires that the files existing on disk when the operation is
      launched, remain on disk until the operation completes. In particular, a
      Gc running in a different process could remove files from disk. *)
  val create_one_commit_store : repo -> commit_key -> string -> unit

  (** {1 Garbage Collection} *)

  module Gc : sig
    (** The state of the GC process after calling {!finalise_exn} *)
    type process_state = [`Idle | `Running | `Finalised of Stats.Latest_gc.stats]

    (** {1 Low-level API} *)

    (** [start_exn] tries to start the GC process and returns true if the GC is
        launched. If a GC is already running, a new one is not started.

        The GC process will not be automatically finalised. The caller is
        responsible for calling {!finalise_exn}.

        If [unlink] is false then temporary files and files from the previous
        generation will be kept on disk after the GC finished. This option is
        useful for debugging. The default is [true].

        TODO: Detail exceptions raised. *)
    val start_exn : ?unlink:bool -> repo -> commit_key -> bool

    (** [finalise_exn ?wait repo] waits for the GC process to finish in order to
        finalise it. It returns the state of the GC process from the point of
        view of the function call; subsequent calls of [finalise_exn] after a
        return of [`Finalised] will return [`Idle].

        Finalising consists of mutating [repo] so that it points to the new file
        and to flush the internal caches that could be referencing GCed objects.
        What is done with the discarded data depends on {!behaviour}.

        If [wait = true] (the default), the call blocks until the GC process
        finishes. If [wait = false], finalisation will occur if the process has
        ended.

        If there are no running GCs, the call is a no-op and it returns [`Idle].

        TODO: Detail exceptions raised. *)
    val finalise_exn : ?wait:bool -> repo -> process_state

    (** {1 High-level API} *)

    (** Pretty-print error messages meant for informational purposes, like
        logging *)
    type msg = [`Msg of string]

    (** [run repo commit_key] attempts to start a GC process for a [repo] by
        discarding or archiving all data prior to [commit_key] (depending on
        {!behaviour}. If a GC process is already running, a new one will not be
        started.

        [run] will also finalise the GC process automatically. For more detailed
        control, see {!start_exn} and {!finalise_exn}.

        When the GC process is finalised, [finished] is called with the result
        of finalisation.

        To monitor progress of GC, see {!wait} or {!is_finished}.

        Returns whether a GC process successfully started or not.

        All exceptions that [Brassaia_pack] knows how to handle are caught and
        returned as pretty-print error messages; others are re-raised. The error
        messages should be used only for informational purposes, like logging. *)
    val run :
      ?finished:((Stats.Latest_gc.stats, msg) result -> unit) ->
      repo ->
      commit_key ->
      (bool, msg) result

    (** [wait repo] blocks until GC is finished or is idle.

        If a GC finalises, its stats are returned.

        All exceptions that [Brassaia_pack] knows how to handle are caught and
        returned as pretty-print error messages; others are re-raised. The error
        messages should be used only for informational purposes, like logging. *)
    val wait : repo -> (Stats.Latest_gc.stats option, msg) result

    (** [cancel repo] aborts the current GC and returns [true], or returns
        [false] if no GC was running. *)
    val cancel : repo -> bool

    (** [is_finished repo] is [true] if a GC is finished (or idle) and [false]
        if a GC is running for the given [repo]. *)
    val is_finished : repo -> bool

    (** [behaviour repo] returns the behaviour that the GC will have during
        finalization.

        This depends on the presence of a lower layer in the store: if a lower
        layer is present, the GC will archive old data into that lower layer.
        Else, it will delete that data. *)
    val behaviour : repo -> [`Archive | `Delete]

    (** [is_allowed repo] returns true if a gc can be run on the store. *)
    val is_allowed : repo -> bool

    (** [latest_gc_target] returns the commit key on which the latest, finished
        gc was called on. *)
    val latest_gc_target : repo -> commit_key option
  end

  (** {1 Snapshots} *)

  module Snapshot : sig
    type kinded_hash = Contents of hash | Node of hash [@@deriving brassaia]

    type entry = {step : string; hash : kinded_hash} [@@deriving brassaia]

    type inode_tree = {depth : int; length : int; pointers : (int * hash) list}
    [@@deriving brassaia]

    type v = Inode_tree of inode_tree | Inode_value of entry list
    [@@deriving brassaia]

    type inode = {v : v; root : bool} [@@deriving brassaia]

    type t = Inode of inode | Blob of Backend.Contents.Val.t
    [@@deriving brassaia]

    (** [export ?on_disk repo f ~root_key] applies [f] to all inodes and
        contents in a rooted tree, with root specified by [root_key].

        The traversal requires an index to keep track of visited elements.

        - if [on_disk] is not specified, the index is in memory.
        - if [on_disk] is [`Path path], a temporary index is created at path.

        The traversal order is stable. In [Inode_tree], it is lexicographic on
        the [index] function (see {!Conf.inode_child_order}). In [Inode_value],
        it is lexicographic on the steps.

        [f] is called in post-order, that is [f] is first called on the leaves,
        and the last call to [f] is on the root designated by [root_key].

        The traversal skips objects that are structurally equal to objects that
        were already traversed. In other words, [export] internally uses a hash
        set in order to guarantee that all the objects passed to [f] don't hash
        the same way.

        Returns the total number of elements visited. *)
    val export :
      ?on_disk:[`Path of string] ->
      repo ->
      (t -> unit) ->
      root_key:Tree.kinded_key ->
      int

    module Import : sig
      type process

      (** [init ?on_disk repo] create a [snaphot] instance. The traversal requires
          an index to keep track of visited elements.

          - if [on_disk] is not specified, the index is in memory.
          - if [on_disk] is [`Path path], a temporary index is created at path.
          - if [on_disk] is [`Reuse] the store's index is reused. *)
      val init : ?on_disk:[`Path of string | `Reuse] -> repo -> process

      (** [save_elt snapshot elt] saves [elt] to the store. *)
      val save_elt : process -> t -> node_key

      (** [close snapshot] close the [snaphot] instance.*)
      val close : process -> repo -> unit
    end
  end

  (** {1 Statistics} *)

  val stats : dump_blob_paths_to:string option -> commit:commit -> repo -> unit

  (** {1 Internals} *)

  module Internal : sig
    (** Unstable internal API agnostic about the underlying storage. Use it only
        to implement or test inodes. *)

    module Io = Io.Unix

    module Index : Pack_index.S with type key = hash

    module File_manager : File_manager.S with module Index = Index

    val file_manager : repo -> File_manager.t

    module Dict = Dict

    val dict : repo -> Dict.t

    module Dispatcher : Dispatcher.S

    val dispatcher : repo -> Dispatcher.t

    module XKey : Pack_key.S with type hash = Schema.Hash.t

    val suffix_commit_mem : repo -> XKey.t -> bool

    val suffix_node_mem : repo -> XKey.t -> bool

    val suffix_contents_mem : repo -> XKey.t -> bool

    val kill_gc : repo -> bool
  end
end

module S_is_a_store (X : S) : Brassaia.Generic_key.S = X

module type Maker = sig
  type endpoint = unit

  include Brassaia.Key.Store_spec.S

  module Make (Schema : Brassaia.Schema.Extended) :
    S
    (* We can't have `with module Schema = Schema` here, since the Schema
       on the RHS contains more information than the one on the LHS. We _want_
       to do something like `with module Schema = (Schema : Brassaia.Schema.S)`,
       but this isn't supported.

       TODO: extract these extensions as a separate functor argument instead. *)
      with type Schema.Hash.t = Schema.Hash.t
       and type Schema.Branch.t = Schema.Branch.t
       and type Schema.Contents.t = Schema.Contents.t
       and type Schema.Info.t = Schema.Info.t
       and type contents_key = (Schema.Hash.t, Schema.Contents.t) contents_key
       and type node_key = Schema.Hash.t node_key
       and type commit_key = Schema.Hash.t commit_key
       and type Backend.Remote.endpoint = endpoint
end
with type ('h, _) contents_key = 'h Pack_key.t
 and type 'h node_key = 'h Pack_key.t
 and type 'h commit_key = 'h Pack_key.t

module type KV = sig
  type endpoint = unit

  type hash = Brassaia.Schema.default_hash

  include Pack_key.Store_spec

  module Make (C : Brassaia.Contents.S) :
    S
      with module Schema.Contents = C
       and type Backend.Remote.endpoint = endpoint
       and type Schema.Hash.t = hash
       and type contents_key = (hash, C.t) contents_key
       and type node_key = hash node_key
       and type commit_key = hash commit_key
       and type Schema.Branch.t = string
end

module type Sigs = sig
  module type S = S

  module type Maker = Maker

  module type KV = KV
end
