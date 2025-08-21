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

module Brassaia = Brassaia_eio.Brassaia

type length_header = [`Varint] option

type inode_child_order =
  [ `Seeded_hash  (** use a non-crypto seeded-hash of the step *)
  | `Hash_bits  (** crypto hash the step and extract the relevant bits. *)
  | `Custom of depth:int -> bytes -> int  (** use a custom index *) ]

module type S = sig
  (** The branching factor of the inode tree. 32 is a good choice for general
      applications. *)
  val nb_entries : int

  (** This offers a way to conditional base hashing on node entries instead of
      inodes. It is available for some backwards compatibility applications, but
      for most applications, this should be set to 0. *)
  val stable_hash : int

  (** Describes the length header of the user's contents values when
      binary-encoded. Supported modes are:

      - [Some `Varint]: the length header is a LEB128-encoded integer at the
        very beginning of the encoded value.

      - [None]: there is no length header, and values have unknown size. NOTE:
        when using [brassaia-pack] in this mode, the selected indexing strategy
        {i must} index all contents values (as recovering contents values from
        the store will require referring to the index for their length
        information). *)
  val contents_length_header : length_header

  (** The strategy used for sorting entries. [`Hash_bits] is the recommended
      choice. [`Seeded_hash] is vunerable to attacks if storing user-generated
      keys. *)
  val inode_child_order : inode_child_order

  (** If [true], brassaia-pack raises [Failure] if it is asked to save the empty
      inode. This default is [false]. It should be set to [true] if the [Schema]
      of the store allows a hash collision between the empty inode and this
      string of length 1: ["\000"].

      See https://github.com/mirage/brassaia/issues/1304 *)
  val forbid_empty_dir_persistence : bool
end

val spec : Brassaia.Backend.Conf.Spec.t

(** Strategy for when attempting to write when the index log is full and waiting
    for an in-progress merge to complete.

    - [`Block_writes] will block writes
    - [`Overcommit_memory] will allow writes by growing the in-memory cache
      indefinitely *)
type merge_throttle = [`Block_writes | `Overcommit_memory] [@@deriving brassaia]

module Key : sig
  val fresh : bool Brassaia.Backend.Conf.key

  val lru_size : int Brassaia.Backend.Conf.key

  val lru_max_memory : int option Brassaia.Backend.Conf.key

  val index_log_size : int Brassaia.Backend.Conf.key

  val readonly : bool Brassaia.Backend.Conf.key

  val root : string Brassaia.Backend.Conf.key

  val lower_root : string option Brassaia.Backend.Conf.key

  val merge_throttle : merge_throttle Brassaia.Backend.Conf.key

  val indexing_strategy : Indexing_strategy.t Brassaia.Backend.Conf.key

  val use_fsync : bool Brassaia.Backend.Conf.key

  val no_migrate : bool Brassaia.Backend.Conf.key
end

(** Flag to indicate that the store will start with fresh data on disk. Warning:
    setting this to [true] will delete existing data. Default is [false]. *)
val fresh : Brassaia.Backend.Conf.t -> bool

(** Maximum size, in number of entries, of LRU cache. Default [100_000]. Unused
    if {!lru_max_memory} is set. *)
val lru_size : Brassaia.Backend.Conf.t -> int

(** Maximum memory, in bytes, for the LRU cache to use. Default [None], which
    falls back to {!lru_size} for LRU limit. *)
val lru_max_memory : Brassaia.Backend.Conf.t -> int option

(** Size, in number of entries, of index log. Default [2_500_000]. *)
val index_log_size : Brassaia.Backend.Conf.t -> int

(** Flag for opening data in read-only mode. Default [false]. *)
val readonly : Brassaia.Backend.Conf.t -> bool

(** Strategy for how to handle writes when index log is full and a merge is
    in-progress. Default [`Block_writes]. *)
val merge_throttle : Brassaia.Backend.Conf.t -> merge_throttle

(** Location of directory for saving data on disk.

    Note: The path before the root directory must exist. Only the final
    directory in the path will be created if it is missing. *)
val root : Brassaia.Backend.Conf.t -> string

(** Optional path for lower layer directory. Default [None].

    The presence or not of a lower layer has implications on the behaviour of
    the GC: if a lower layer is present, the GC will archive data instead of
    deleting it.*)
val lower_root : Brassaia.Backend.Conf.t -> string option

(** Strategy for choosing which objects to index. See {!Indexing_strategy.t} for
    more discussion. Default {!Indexing_strategy.default} *)
val indexing_strategy : Brassaia.Backend.Conf.t -> Indexing_strategy.t

(** Flag to indicate that fsync should be used to enforce durability when
    flushing data to disk. Default [false]. *)
val use_fsync : Brassaia.Backend.Conf.t -> bool

(** Flag to prevent migration of data. Default [false]. *)
val no_migrate : Brassaia.Backend.Conf.t -> bool

(** [init root] creates a backend configuration for storing data with default
    configuration parameters and stored at [root]. Flags are documented above. *)
val init :
  ?fresh:bool ->
  ?readonly:bool ->
  ?lru_size:int ->
  ?lru_max_memory:int option ->
  ?index_log_size:int ->
  ?merge_throttle:merge_throttle ->
  ?indexing_strategy:Indexing_strategy.t ->
  ?use_fsync:bool ->
  ?no_migrate:bool ->
  ?lower_root:string option ->
  string ->
  Brassaia.config
