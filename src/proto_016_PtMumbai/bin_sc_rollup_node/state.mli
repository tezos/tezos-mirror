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

open Protocol.Alpha_context

(** [is_processed store hash] returns [true] if the block with [hash] has
    already been processed by the daemon. *)
val is_processed : _ Store.t -> Block_hash.t -> bool Lwt.t

(** [mark_processed_head store head] remembers that the [head] is processed. The
    system should not have to come back to it. *)
val save_l2_block : Store.rw -> Sc_rollup_block.t -> unit Lwt.t

(** [last_processed_head_opt store] returns the last processed head if it
    exists. *)
val last_processed_head_opt : _ Store.t -> Sc_rollup_block.t option Lwt.t

(** [mark_finalized_head store head] remembers that the [head] is finalized. By
    construction, every block whose level is smaller than [head]'s is also
    finalized. *)
val mark_finalized_head : Store.rw -> Layer1.head -> unit Lwt.t

(** [last_finalized_head_opt store] returns the last finalized head if it exists. *)
val get_finalized_head_opt : _ Store.t -> Layer1.head option Lwt.t

(** [hash_of_level node_ctxt level] returns the current block hash for a given
    [level]. *)
val hash_of_level : _ Node_context.t -> int32 -> Block_hash.t tzresult Lwt.t

(** [hash_of_level_opt] is like {!hash_of_level} but returns [None] if the
    [level] is not known. *)
val hash_of_level_opt : _ Node_context.t -> int32 -> Block_hash.t option Lwt.t

(** [save_level store head] registers the correspondences [head.level |->
    head.hash] in the store. *)
val save_level : Store.rw -> Layer1.head -> unit Lwt.t

(** [level_of_hash node_ctxt hash] returns the level for Tezos block hash [hash]
    if it is known by the Tezos Layer 1 node. *)
val level_of_hash : _ Node_context.t -> Block_hash.t -> int32 tzresult Lwt.t

(** [block_before store tick] returns the last layer 2 block whose initial tick
    is before [tick]. *)
val block_before :
  [> `Read] Store.store ->
  Sc_rollup.Tick.t ->
  Sc_rollup_block.t option tzresult Lwt.t
