(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Persistent block store with linear history

    The cemented block store is a store where blocks are stored
    linearly (by level) in chunks. Blocks in this store should not be
    reorganized anymore and are thus *cemented*. As these blocks
    should not be accessed regularly and especially their optionally
    stored metadata, the later are compressed using a zip format to
    save disk space. For each chunk of blocks, a dedicated file is
    used. Moreover, to enable easy access and to prevent too much
    on-disk reading, two indexed maps are used to retrieve blocks hash
    from their level and their level from the block hash.

    The cemented block store contains a set of files updated each time
    a new chunk is added to the store. These files indicate which
    interval of blocks (w.r.t. their levels) are stored in it.

    {1 Invariants}

    This store is expected to respect the following invariants:

    - A key/value present in an index is present as well in the other
      as value/key.

    - Every block stored is correctly referenced through its
      associated indexes.

    - A cemented chunk of blocks that is represented by the interval
      [ i ; j ] (with i <= j) contains | j - i + 1 | blocks and are
      ordered from i to j in the file.

    - The set F of cemented chunks is always ordered by block level.

    - The cemented store does not contain holes: let F be the cemented
      chunks, if |F| > 1 then:

{v
      ∀f_x=(i,j) ∈ F ∧ x < |F|, ∃f_y =(i', j'), x = y - 1 ∧ j + 1 = j'
v}

      meaning the concatenation of every chunk must be continuous.

    - A metadata zip file is indexed by the same interval as the
      chunks and, when it is the lowest chunk of metadata stored, is
      not assured to contain every block's metadata of the chunk.

    {1 Files format}

    The cemented block store is composed of the following files:

    - file: /<i_j>, a chunk of blocks from level i to level j. The
      format of this file is:

    | <n> × <offset> | <n> × <block> |

    where n is (j - i + 1), <offset> is a 4 bytes integer representing
    the absolute offset of a block where the k-th (with 0 <= k < n)
    offset stands for the absolute offset of the k-th block in the
    file and with <block>, a {!Block_repr.t} value encoded using
    {!Block_repr.encoding} (thus prefixed by the its size).

    - dir: /cemented_block_index_level, the Hash -> Level key/value
      index ;

    - dir: /cemented_block_index_hash, the Level -> Hash key/value
      index.

    - dir: /metadata, the directory containing chunks of compressed
      metadata (present if relevant).

    - files: /metadata/<i_j>.zip, the compressed metadata where every
      chunk of block's metadata is indexed by their level encoded as
      string (present if relevant). *)

(** On-disk index of block's hash to level. *)
module Cemented_block_level_index :
  Index.S with type key = Block_key.t and type value = Block_level.t

(** On-disk index of block's level to hash. *)
module Cemented_block_hash_index :
  Index.S with type key = Block_level.t and type value = Block_key.t

(** The type of the cemented block store *)
type t

type cemented_metadata_file = {
  start_level : int32;
  end_level : int32;
  metadata_file : [`Cemented_blocks_metadata] Naming.file;
}

(** The type for cemented block chunks file description *)
type cemented_blocks_file = {
  start_level : int32;
  end_level : int32;
  file : [`Cemented_blocks_file] Naming.file;
}

(** [init ?log_size ~cemented_blocks_dir ~readonly] creates or loads
    an existing cemented block store at path
    [cemented_blocks_dir]. [cemented_blocks_dir] will be created if it
    does not exists. If [readonly] is true, cementing blocks will
    result in an error. [log_size] determines the index cache size. *)
val init :
  ?log_size:int ->
  [< `Chain_dir] Naming.directory ->
  readonly:bool ->
  t tzresult Lwt.t

(** [close cemented_store] closes the [cemented_store] opened files:
    its indexes. *)
val close : t -> unit

(** [cemented_blocks_files cemented_store] returns the {b current}
   array of cemented blocks chunks files. The returned array is sorted
   in ascending order such that the first element of the array is the
   lowest known cycle of the store. *)
val cemented_blocks_files : t -> cemented_blocks_file array option

(** [cemented_metadata_files cemented_store] returns the {b current}
   array of cemented metadata files. The returned array is sorted in
   ascending order such that the first element of the array is the
   lowest known cycle of the store. *)
val cemented_metadata_files :
  t -> cemented_metadata_file array option tzresult Lwt.t

(** [cemented_block_level_index block_store] returns the hash to level
    index. *)
val cemented_block_level_index : t -> Cemented_block_level_index.t

(** [cemented_block_hash_index block_store] returns the level to hash
    index. *)
val cemented_block_hash_index : t -> Cemented_block_hash_index.t

(** [load_table ~cemented_blocks_dir] reads the [cemented_blocks_dir]
    directory and instantiate the cemented blocks chunks files. *)
val load_table :
  [`Cemented_blocks_dir] Naming.directory ->
  cemented_blocks_file array option tzresult Lwt.t

(** [load_metadata_table ~cemented_blocks_dir] similar to
    [load_table], but for the cemented metadata files. *)
val load_metadata_table :
  [`Cemented_blocks_dir] Naming.directory ->
  cemented_metadata_file array option tzresult Lwt.t

(** [find_block_file cemented_store block_level] lookups the
   [cemented_store] to find the cemented block chunk file that
   contains the block at level [block_level]. Returns [None] if the
   block cannot be found.*)
val find_block_file : t -> int32 -> cemented_blocks_file option

(** [is_cemented cemented_store block_hash] checks if the [block_hash]
    is stored in the [cemented_store]. *)
val is_cemented : t -> Block_hash.t -> bool

(** [get_cemented_block_level cemented_store block_hash] returns the
    level of the [block_hash] if present in [cemented_store]. Returns
    [None] otherwise. *)
val get_cemented_block_level : t -> Block_hash.t -> int32 option

(** [get_cemented_block_hash cemented_store block_level] returns the
    hash of the block at [block_level] if present in
    [cemented_store]. Returns [None] otherwise. *)
val get_cemented_block_hash : t -> int32 -> Block_hash.t option

(** [read_block_metadata cemented_store block_level] returns the
    metadata of the block at [block_level] if present in
    [cemented_store]. Returns [None] otherwise. *)
val read_block_metadata :
  t -> int32 -> Block_repr.metadata option tzresult Lwt.t

(** [cement_blocks_metadata cemented_store chunk] compresses and
    stores the metadata of blocks present in [chunk]. If no block of
    the given [chunk] contains metadata, nothing is done. Otherwise,
    for every block containing metadata, an entry is written in the
    dedicated .zip metadata file.

    We assume that the blocks containing metadata are contiguous and
    if at least one block has metadata, then the blocks from that
    block with metadata to the last block of [chunk] must have
    metadata. However, we do not check the validity of this assumption.*)
val cement_blocks_metadata : t -> Block_repr.t list -> unit tzresult Lwt.t

(** [get_lowest_cemented_level cemented_store] returns the lowest
    cemented block in [cemented_store], if it exists.*)
val get_lowest_cemented_level : t -> int32 option

(** [get_highest_cemented_level cemented_store] returns the highest
    cemented block in [cemented_store] if it exists. *)
val get_highest_cemented_level : t -> int32 option

(** [get_cemented_block_by_level cemented_store ~read_metadata level]
    reads the cemented block at [level] in [cemented_store], if it
    exists. It also tries to retrieves the metadata depending on
    [read_metadata] but do not fail if no metadata is available. *)
val get_cemented_block_by_level :
  t -> read_metadata:bool -> int32 -> Block_repr.block option tzresult Lwt.t

(** [get_cemented_block_by_hash cemented_store hash] reads the cemented
    block of [hash] in [cemented_store], if it exists. It also
    retrieves the metadata depending on [read_metadata]. *)
val get_cemented_block_by_hash :
  read_metadata:bool ->
  t ->
  Block_hash.t ->
  Block_repr.block option tzresult Lwt.t

(** [cemented_blocks ?check_consistency cemented_store ~write_metadata
    chunk] stores the [chunk] of blocks and write their metadata if
    the flag [write_metadata] is set. [check_consistency] (default is
    true) ensures that the blocks in the given [chunk] are contiguous. *)
val cement_blocks :
  ?check_consistency:bool ->
  t ->
  write_metadata:bool ->
  Block_repr.t list ->
  unit tzresult Lwt.t

(** [trigger_gc cemented_store history_mode] garbage collects metadata
    chunks and/or chunks from the [cemented_store] depending on the
    {!History_mode.t}:

    - in [Archive] mode, nothing is done;

    - in [Full offset] mode, only [offset] chunks of {b metadata} are
      kept;

    - in [Rolling offset] mode, only [offset] chunks of {b metadata
      and chunks} are kept.

      {b Important:} when purging chunks of blocks, it is necessary to
      rewrite the index to remove garbage collected blocks. Therefore,
      the higher the offset is, the longest the GC phase will last. *)
val trigger_gc : t -> History_mode.t -> unit Lwt.t

(** [iter_cemented_file ~cemented_block_dir f block_file] reads from
    the cemented [block_file] located in [cemented_block_dir] and
    applies [f] on every block. *)
val iter_cemented_file :
  (Block_repr.block -> unit Lwt.t) ->
  cemented_blocks_file ->
  unit tzresult Lwt.t

(** [check_indexes_consistency ?post_step ?genesis_hash cemented_store
    history_mode] iterates over a partially initialized
    [cemented_store] that contains both chunks of blocks and indexes
    then check the consistency of each block: (hashes, predecessors and
    levels). The hash is not checked for [genesis_hash] and
    [post_step] is called after each treated chunk. This is used for
    snapshot imports. *)
val check_indexes_consistency :
  ?post_step:(unit -> unit Lwt.t) ->
  ?genesis_hash:Block_hash.t ->
  t ->
  unit tzresult Lwt.t
