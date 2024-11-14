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

(** Persistent block store with arborescent history

    The floating block store is an append-only store where blocks are
    stored arbitrarily. This structure possess an indexed map
    {!Block_hash.t} -> (offset × predecessors x
    resulting_context_hash) which points to its offset in the
    associated file along with an exponential list of predecessors
    block hashes (as implemented in
    {!Block_store.compute_predecessors}) and the block's resulting
    context hash. The structure access/modification is protected by a
    mutex ({!Lwt_idle_waiter}) and thus can be manipulated
    concurrently. Stored blocks may or may not contain metadata. The
    instance maintains an opened file descriptor. Therefore it must be
    properly closed or it might lead to a file descriptor leak.

    Four different kind of instances are allowed to co-exist for an
    identical path: - RO, a read-only instance; - RW, a read-write
    instance - RO_TMP, RW_TMP, read-write instances; - Restore is a
    generic instance used to wrap leftover instances while fixing a
    crashed storage. See {!Block_store}.

    {1 Invariants}

    This store is expected to respect the following invariant:

    - Every block stored is correctly indexed.

    {1 Files format}

    The floating block store is composed of the following files:

    - file: /<kind>_floating_block_store, a list of {!Block_repr.t}:

    | <block> * |

    where <kind> is RO(_TMP), RW(_TMP) (see {!Naming}) and <block>, a
   {!Block_repr.t} value encoded using {!Block_repr.encoding} (thus
   prefixed by the its size).  *)

(** The type of the floating store. *)
type t

(** The type for the kind of floating store opened. *)
type floating_kind = Naming.floating_kind =
  | RO
  | RW
  | RW_TMP
  | RO_TMP
  | Restore of floating_kind

(** The type for informations stored in floating store indexes. *)
type info = {
  predecessors : Block_hash.t list;
  resulting_context_hash : Context_hash.t;
}

(** The default value of the floating blocks cache size used by index
    library. *)
val default_floating_blocks_log_size : int

(** [kind floating_store] returns the floating store's kind. *)
val kind : t -> floating_kind

(** [mem floating_store hash] tests whether [hash] is stored in
    [floating_store]. *)
val mem : t -> Block_hash.t -> bool Lwt.t

(** [may_sync floating_store] updates a RO index instance to allow
    concurrent access to the value added by a RW instance. This
    operation is expected to be cheap (~10us) and has no effect on RW
    instances. *)
val may_sync : t -> unit

(** [find_info floating_store block_hash] reads from the index the
    info of [block_hash] if the block is stored in [floating_store],
    returns [None] otherwise. *)
val find_info : t -> Block_hash.t -> info option Lwt.t

(** [find_predecessors floating_store block_hash] reads from the index
    the list of [block_hash]'s predecessors if the block is stored in
    [floating_store], returns [None] otherwise. *)
val find_predecessors : t -> Block_hash.t -> Block_hash.t list option Lwt.t

(** [find_resulting_context_hash floating_store block_hash] reads from
   the index the resulting context hash if the block is stored in
   [floating_store], returns [None] otherwise. *)
val find_resulting_context_hash :
  t -> Block_hash.t -> Context_hash.t option Lwt.t

(** [read_block floating_store hash] reads from the file the block of
    [hash] if the block is stored in [floating_store], returns [None]
    otherwise. *)
val read_block : t -> Block_hash.t -> Block_repr.t option Lwt.t

(** [read_block_and_info floating_store hash] same as
    [read_block] but also returns the block's info. Returns
    [None] if it fails to resolve the given [hash].*)
val read_block_and_info :
  t -> Block_hash.t -> (Block_repr.t * info) option Lwt.t

(** [append_block floating_store ?flush ?log_metrics info block]
    stores the [block] in [floating_store] updating its index with the
    given [info] and flushing if [flush] is set to [true] (defaults to
    [true]). [log_metrics] enables logs of the amount of written bytes
    using the node's metrics. *)
val append_block :
  t ->
  ?flush:bool ->
  ?log_metrics:bool ->
  info ->
  Block_repr.block ->
  unit tzresult Lwt.t

(** [append_all floating_store chunk] stores the [chunk] of (blocks x
    info) in [floating_store] updating its index accordingly. *)
val append_all : t -> (Block_repr.t * info) Seq.t -> unit tzresult Lwt.t

(** [iter_s_raw_fd f fd] unsafe sequential iterator on a file descriptor
    [fd]. Applies [f] on every block encountered.

    {b Warning}: should be used for internal use only (e.g. snapshots). *)
val iter_s_raw_fd :
  (Block_repr.t -> unit tzresult Lwt.t) ->
  Lwt_unix.file_descr ->
  unit tzresult Lwt.t

(** [fold_left_s f e floating_store] sequential fold left on the
    [floating_store] using [f] on every block and [e] as initial
    element. The function [f] is given the last read block. *)
val fold_left_s :
  ('a -> Block_repr.block -> 'a tzresult Lwt.t) -> 'a -> t -> 'a tzresult Lwt.t

(** [fold_left_with_info_s f e floating_store] sequential fold left on the
    [floating_store] using [f] on every block and [e] as initial
    element. The function [f] is given the last read block along with
    its info. *)
val fold_left_with_info_s :
  ('a -> Block_repr.block * info -> 'a tzresult Lwt.t) ->
  'a ->
  t ->
  'a tzresult Lwt.t

(** [iter_s f floating_store] sequential iterator on the
    [floating_store]. Applies [f] on every block read. *)
val iter_s : (Block_repr.t -> unit tzresult Lwt.t) -> t -> unit tzresult Lwt.t

(** [iter_with_info_s f floating_store] sequential iterator on the
    [floating_store]. Applies [f] on every block (and its info)
    read. *)
val iter_with_info_s :
  (Block_repr.t * info -> unit tzresult Lwt.t) -> t -> unit tzresult Lwt.t

(** [init ~chain_dir ~readonly kind] creates or load an existing
   floating store at path [chain_dir] with [kind].

    {b Warning} If [readonly] is not set, a [RO] instance is
    writable. *)
val init :
  [`Chain_dir] Naming.directory -> readonly:bool -> floating_kind -> t Lwt.t

(** [close floating_store] closes [floating_store] by closing the
    index and the associated opened file descriptor. *)
val close : t -> unit Lwt.t

(** [delete_files store] closes the [store] then deletes its content. *)
val delete_files : t -> unit Lwt.t

(** [swap ~src ~dst] closes [src] and [dst] then overwrites [dst]
    files with [src]'s ones.

    {b Warning} Non-atomic swap. *)
val swap : src:t -> dst:t -> unit Lwt.t

(** [append_floating_store ~from ~into] takes two opened floating block stores
    and appends all blocks contained in [from] to [into]. *)
val append_floating_store : from:t -> into:t -> unit tzresult Lwt.t

(** Integrity checks *)

(** [all_files_exists ~chain_dir kind] checks that the files to
    the associated [kind] of floating store are present in the
    [chain_dir] directory. *)
val all_files_exists :
  [`Chain_dir] Naming.directory -> floating_kind -> bool Lwt.t

(** [fix_integrity ~chain_dir kind error] fixes the integrity of the
    floating block stores by removing corrupted data and restoring a
    consistent state. *)
val fix_integrity :
  [`Chain_dir] Naming.directory -> floating_kind -> unit tzresult Lwt.t

(**/**)

(** Unsafe set of functions intended for merging optimizations. *)

(** [raw_append dst_store (hash, buffer, total_length, predecessors,
   resulting_context_hash)] appends a block with its [hash] in
   [dst_store] contained in the [buffer]. *)
val raw_append :
  t ->
  Block_hash.t * bytes * int * Block_hash.t list * Context_hash.t ->
  unit tzresult Lwt.t

(** [raw_copy_all src_stores block_hashes dst_store] retrieves
    [block_hashes] from [src_stores] and copy them (without decoding)
    to [dst_store] with a buffering mechanism. *)
val raw_copy_all :
  src_floating_stores:t list ->
  block_hashes:Block_hash.t list ->
  dst_floating_store:t ->
  unit tzresult Lwt.t

(** [raw_retrieve_blocks_seq src_stores block_hashes] retrieves
    [block_hashes] from [src_stores] and provide a buffered lazy
    sequence to access those blocks. Blocks are effectively read
    when the sequence items are consumed. The sequence elements are:
    a [block_hash], its [total_block_length] and a [buffer] such that
    [(decode Block_repr.encoding (Bytes.sub buffer 0 total_block_length))]
    is consistent.

    {b Warning}: the reading sequence result must only be used once as
    the given bytes are shared between elements and will be modified
    during the iteration. *)
val raw_retrieve_blocks_seq :
  src_floating_stores:t list ->
  block_hashes:Block_hash.t list ->
  (Block_hash.t * int * bytes) tzresult Lwt.t Seq.t

(** [raw_iterate f store] iterate over all blocks in a store and
    calling [f] providing it with a [buffer] and the
    [total_block_length] such that
    [(decode Block_repr.encoding (Bytes.sub buffer 0 total_block_length))]
    is consistent.

    {b Warning}: the [buffer] is modified through the iteration and
    therefore must not be used outside of the definition of [f].

    For internal use only! *)
val raw_iterate :
  (bytes * int -> unit tzresult Lwt.t) -> t -> unit tzresult Lwt.t

(** [raw_iterate_fd f fd] iterate over all blocks in a floating store
    block file and calling [f] providing it with a [buffer] and the
    [total_block_length] such that [(decode Block_repr.encoding
    (Bytes.sub buffer 0 total_block_length))] is consistent.

    {b Warning}: the [buffer] is modified through the iteration and
    therefore must not be used outside of the definition of [f].

    For internal use only! *)
val raw_iterate_fd :
  (bytes * int -> unit tzresult Lwt.t) ->
  Lwt_unix.file_descr ->
  unit tzresult Lwt.t
