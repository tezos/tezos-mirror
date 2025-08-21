(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

module type S = sig
  type +'a store

  (** Type of store. The parameter indicates if the store can be written or only
      read. *)
  type 'a t = ([< `Read | `Write > `Read] as 'a) store

  (** Read/write store {!t}. *)
  type rw = Access_mode.rw t

  (** Read only store {!t}. *)
  type ro = Access_mode.ro t

  (** Version supported by this code.  *)
  val version : Store_version.t

  (** [close store] closes the store. *)
  val close : _ t -> unit tzresult Lwt.t

  (** [load mode ~index_buffer_size ~l2_blocks_cache_size directory] loads a
      store from the data persisted in [directory]. If [mode] is {!Store_sigs.Read_only},
      then the indexes and irmin store will be opened in readonly mode and only read
      operations will be permitted. This allows to open a store for read access
      that is already opened in {!Store_sigs.Read_write} mode in another
      process. [index_buffer_size] is the maximum cache size in memory before it is flushed
      to disk. [l2_blocks_cache_size] is the number of L2 blocks the rollup node
      will keep in memory. *)
  val load :
    'a Access_mode.t ->
    index_buffer_size:int ->
    l2_blocks_cache_size:int ->
    string ->
    'a store tzresult Lwt.t

  (** [readonly store] returns a read-only version of [store]. *)
  val readonly : _ t -> ro

  (** [iter_l2_blocks ?progress metadata store f] iterates [f] on all L2 blocks
      reachable from the head, from newest to oldest. If [progress] is provided,
      a progress bar is displayed with the progress messages during the
      iteration. *)
  val iter_l2_blocks :
    ?progress:string ->
    Metadata.t ->
    _ t ->
    (Sc_rollup_block.t -> unit tzresult Lwt.t) ->
    unit tzresult Lwt.t

  (** [gc store ~level] asynchronously garbage collects everything from the
      store that concerns blocks for levels below [level]. *)
  val gc : rw -> level:int32 -> unit tzresult Lwt.t

  (** [wait_gc_completion store] returns a blocking thread if a GC run is
      currently ongoing. *)
  val wait_gc_completion : 'a t -> unit Lwt.t
end
