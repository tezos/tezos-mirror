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

(** Persistent and cached generic block store

    The store instantiate a cemented block store and multiple floating
    block stores. The floating stores serve as buffers until enough
    blocks have arrived. Then it performs a "cementing" (also
    called a "merge"). Under normal circumstances, there are two
    different kinds ({!Floating_block_store.floating_kind}) of
    floating stores instances: a [RO](read-only) and a
    [RW](read-write). Newly arrived blocks are {b always} pushed in
    the [RW] instance. The block lookup is first tried in [RW], then
    [RO] and finally in the cement blocks.

    This store also instantiates a LRU block cache to reduce the number
    of I/O operations. This cache is updated whenever a block is read
    or stored.

    When a merge occurs, the [RW] instance is promoted as another
    [RO'] and a new [RW'] instance replaces it. This allows retrieving
    the new cycle to be cemented from [RO] and [RO'] (former [RW]) {b
    asynchronously} and thus allowing new blocks to be stored in the
    newly instantiated [RW] store without pausing. This asynchronous
    merging thread, while retrieving the cycle to cement, also
    combines [RO] and [RO'] into a {b new} [RO''] without the cemented
    cycle. When the merging thread is done, the former [RO] and [RO']
    instances are deleted from the disk and the new [RO''] replaces
    them. A merging thread has to wait for the previous one to finish.

    Retrieving the new cycle from [RO] and [RO'] from blocks [B_start]
    and [B_end] means that we must retrieve the set of blocks between
    them but also trim potential branches that have roots in this set.
    To achieve that, we iterate over [RO] and [RO'] {b linearly}. This
    means that {b every block's predecessor in floating stores must be
    previously known}. Either we previously encountered it in the same
    floating store file, either in [RO] if the block is in [RO'] or in
    the cemented store (see invariants below). This invariant is
    required to ensure minimal memory usage. The iterations done to
    retrieve the cycle and merge the floating stores works similarly
    to a {i stop and copy} GC algorithm. It works as follows:

    - We retrieve the blocks from [B_end] to [B_start] by sequentially
      reading predecessors.

    - We instantiate a set of encountered block hashes with [B_end]'s
      hash as initial value.

    - We iterate sequentially over [RO] and [RO'] blocks and copy them
      only if their predecessors is present in the visited set, adding
      their hash in the process.

    The result is a correct, in order and trimmed new [RO] floating
    store. A visual example of merging is given below.

    The merging thread will also trigger a garbage-collection of the
    cemented block store w.r.t. the given history mode.


    {1 Invariants}

    This store is expected to respect the following invariants:

    - If no merging thread is pending, two floating stores are
      present: a [RO] and a [RW].

    - If a merging thread is pending, there are three floating stores
      present: a [RO], a [RO'] and a [RW].

    - Blocks may be stored twice in {b floating stores} but only when
      a merging occurs (as [RO''] is a subset of [RO]+[RO']). However,
      blocks can be present in both the cemented and the floating
      store (after importing a storage snapshot as the floating block
      store consist in a checkpoint associated with its `max_op_ttl`
      blocks which were already cemented).

    - For every stored block in floating stores, its predecessor is
      either already stored in the same floating store file, in a
      previous floating store file or in the cemented store.

    - A merging thread does not start until the previous one has
      completed.
*)

(** {1 Merging example}
{v
           RO          RW

                 | C' - D' - E'   G'
              /  |               /
         A - B - | C - D - E - F - G
                 |  \
                 |    D'' - E''
                 |     \
                 |       E'''
v}

    For instance, a merging from [A] to [C] will first retrieve blocks
    [C], [B] and [A]. Then, iterate over all the blocks in both the
    [RO] and [RW] files: reading first in [RO], then in [RW]. By
    construction, blocks are stored after their predecessors. For
    example, \[ A ; B ; C ; C'; D'' ; D'; D ; E ; E'' ; F ; E''' ; G'
    ; G \] is a valid storing sequence.

    The algorithm starts iterating over this sequence and will only
    copy blocks for which predecessors are present in the set S of
    hash (initially S = \{ hash([C]) \}). Thus, for the given sequence,
    [D''] will first be considered, S will be updated to \{ hash([C]),
    hash([D]) \} and so on, until [RO] and [RW] are fully read.

    The new RO will then be:
{v
                G'
               /
    - D - E - F - G

    - D'' - E''
       \
        E'''
v}
    where its storing order will be correct with regards to the
    invariant.
*)

open Store_types

(** The type of the block store *)
type block_store

type t = block_store

(** The type of the block's key to be accessed: a hash and an offset.

    - Block (h, 0) represents the block h itself ;

    - Block (h, n) represents the block's [n]th predecessor.

    A block key may represent an invalid block (wrong hash and/or
    offset) as it is not ensured to be valid by construction.*)
type key = Block of (Block_hash.t * int)

(** The status of the merging thread *)
type merge_status = Not_running | Running | Merge_failed of tztrace

(** The status of the store *)
type status = Idle | Merging

val status_encoding : status Data_encoding.t

(** [cemented_block_store block_store] returns the instance of the
    cemented block store for [block_store]. *)
val cemented_block_store : block_store -> Cemented_block_store.t

(** [floating_block_stores block_store] returns all running floating
    block store instances for [block_store]. It will always return two
    or three ordered floating stores:

     - [ [RO] ; [RW] ] if a merge is not occurring;

     - [ [RO] ; [RO'] ; [RW] ] if a merge is occurring.

    {b Warning} These stores should only be accessed when the store is
    not active. *)
val floating_block_stores : block_store -> Floating_block_store.t list

val savepoint : block_store -> block_descriptor Lwt.t

val write_savepoint : block_store -> block_descriptor -> unit tzresult Lwt.t

val caboose : block_store -> block_descriptor Lwt.t

val write_caboose : block_store -> block_descriptor -> unit tzresult Lwt.t

val status : block_store -> status Lwt.t

val write_status : block_store -> status -> unit tzresult Lwt.t

val genesis_block : block_store -> Block_repr.t

(** [mem block_store key] tests the existence of the block [key] in
    [block_store]. *)
val mem : block_store -> key -> bool tzresult Lwt.t

(** [get_hash block_store key] retrieves the hash corresponding to the
   given [key] in [block_store]. Return [None] if the block is
   unknown. *)
val get_hash : block_store -> key -> Block_hash.t option tzresult Lwt.t

(** [resulting_context_hash block_store ~fetch_expect_predecessor_context key]
    retrieves the resulting context hash from the block application,
    corresponding to the given [key]. *)
val resulting_context_hash :
  block_store ->
  fetch_expect_predecessor_context:(unit -> bool tzresult Lwt.t) ->
  key ->
  Context_hash.t option tzresult Lwt.t

(** [read_block block_store ~read_metadata key] reads the block [key]
    in [block_store] if present. Return [None] if the block is
    unknown. If [read_metadata] is set to [true] it tries to retreive
    the metadata but do not fail if it is not available. *)
val read_block :
  block_store -> read_metadata:bool -> key -> Block_repr.t option tzresult Lwt.t

(** [read_block_metadata block_store key] reads the metadata for the
   block [key] in [block_store] if present. Return [None] if the block
   is unknown or if the metadata are not present. *)
val read_block_metadata :
  block_store -> key -> Block_repr.metadata option tzresult Lwt.t

(** [store_block block_store block resulting_context_hash] stores the
    [block] in the current [RW] floating store with its associated
    [resulting_context_hash]. *)
val store_block :
  block_store -> Block_repr.t -> Context_hash.t -> unit tzresult Lwt.t

(** [cement_blocks ?check_consistency ~write_metadata block_store
    chunk_iterator ~cycle_range]

    Wrapper of {!Cemented_block_store.cement_blocks}. *)
val cement_blocks :
  ?check_consistency:bool ->
  write_metadata:bool ->
  block_store ->
  Cemented_block_store.chunk_iterator ->
  cycle_range:int32 * int32 ->
  unit tzresult Lwt.t

(** [move_floating_store block_store ~src ~dst_kind] closes the
    floating store [src], tests the existence of a [dst_kind] store
    opened in [block_store] and tries to close it if it is the case. It
    then proceeds to replace the files from [src] to [dst].

    This function is unsafe and should only be called in very specific
    cases.

    {b Warning} [block_store] remains unchanged meaning that the
    potential deleted floating store is referenced in the structure.

    Fails if both [src] and [dst] (if it exists) have the same
    {!Floating_block_store.floating_kind}. *)
val move_floating_store :
  block_store ->
  src:Floating_block_store.t ->
  dst_kind:Floating_block_store.floating_kind ->
  unit tzresult Lwt.t

(** [await_merging block_store] waits for the current merging thread
    in [block_store] to finish if any. *)
val await_merging : block_store -> unit Lwt.t

(**
   (* TODO UPDATE MERGE DOC *)
   [merge_stores block_store ?finalizer ~nb_blocks_to_preserve
    ~history_mode ~from_block ~to_block] triggers a merge as described
    in the above description. This will result, {b asynchronously},
    in:

    - the cementing (if needs be) of a cycle from [from_block] to
      [to_block] (included)

    - trimming the floating stores and preserves [to_block] -
      [nb_blocks_to_preserve] blocks (iff these blocks are present or
      the longest suffix otherwise) along with their metadata in the
      floating store. It may potentially have duplicates in the
      cemented block store.

    After the cementing, {!Cemented_block_store.trigger_gc} will be
    called with the given [history_mode]. When the merging thread
    succeeds, the callback [finalizer] will be called.

    If a merge thread is already occurring, this function will first
    wait for the previous merge to be done.

    {b Warning} For a given [block_store], the caller must wait for
    this function termination before calling it again or it may result
    in concurrent intertwining causing the cementing to be out of
    order. *)
val merge_stores :
  block_store ->
  on_error:(tztrace -> unit tzresult Lwt.t) ->
  finalizer:(int32 -> unit tzresult Lwt.t) ->
  history_mode:History_mode.t ->
  new_head:Block_repr.t ->
  new_head_metadata:Block_repr.metadata ->
  cementing_highwatermark:int32 ->
  unit tzresult Lwt.t

val get_merge_status : t -> merge_status

val pp_merge_status : Format.formatter -> merge_status -> unit

(** [switch_history_mode block_store ~current_head
   ~previous_history_mode ~new_history_mode] switches the store from
   the [previous_history_mode] to the given [new_history_mode]. To do
   so, it infers and updates both the caboose and savepoint. If
   needed, a garbage collection of unnecessary cycles is performed. *)
val switch_history_mode :
  block_store ->
  current_head:Block_repr.t ->
  previous_history_mode:History_mode.t ->
  new_history_mode:History_mode.t ->
  unit tzresult Lwt.t

(** [create ?block_cache_limit ~chain_dir ~genesis_block] instantiates
   a fresh [block_store] in directory [chain_dir] and stores the
   [genesis_block] in it. It fails if the given [chain_dir] is already
   populated. Setting the [block_cache_limit] allows to override the
   default block cache size. *)
val create :
  ?block_cache_limit:int ->
  [`Chain_dir] Naming.directory ->
  genesis_block:Block_repr.t ->
  block_store tzresult Lwt.t

(** [load ?block_cache_limit chain_dir ~genesis_block ~readonly] loads
   an existing block_store from directory [chain_dir]. Setting
   [readonly] will prevent new blocks from being stored. Setting the
   [block_cache_limit] allows to override the default block cache
   size. *)
val load :
  ?block_cache_limit:int ->
  [`Chain_dir] Naming.directory ->
  genesis_block:Block_repr.t ->
  readonly:bool ->
  block_store tzresult Lwt.t

(** [register_gc_callback block_store callback] installs a [callback]
   that may be triggered during a block store merge in order to
   garbage-collect old contexts. *)
val register_gc_callback :
  block_store -> (Block_hash.t -> unit tzresult Lwt.t) option -> unit

(** [register_split_callback chain_store callback] installs a
    [callback] that may be triggered during a [set_head] in order to
    split the context into a new chunk. *)
val register_split_callback :
  block_store -> (unit -> unit tzresult Lwt.t) option -> unit

(** [split_context block_store new_head_lpbl] calls the callback
    registered by [register_split_callback] if any. *)
val split_context : t -> Int32.t -> unit tzresult Lwt.t

(** [close block_store] closes the [block_store] and every underlying
    opened stores.

    {b Warning} If a merging thread is occurring, it will wait up to
    5s for its termination before effectively closing the store. *)
val close : block_store -> unit Lwt.t

(** [may_recover_merge block_store] recovers, if needed, from a
   [block_store] where the merge procedure was interrupted. *)
val may_recover_merge : block_store -> unit tzresult Lwt.t

(** Upgrade a v_2 to v_3 block store by retrieving
    [resulting_context_hash] of all blocks present in the floating
    stores and updating their index.

    {b Warning} Not backward-compatible. *)
val v_3_0_upgrade :
  [`Chain_dir] Naming.directory ->
  cleanups:(unit -> unit Lwt.t) list ref ->
  finalizers:(unit -> unit Lwt.t) list ref ->
  unit tzresult Lwt.t
