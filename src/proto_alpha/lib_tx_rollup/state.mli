(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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
open Protocol_client_context
open Common

(** The RPC server and the Daemon main loop are sharing a variable of the
    type stored in the Irmin store. The [State] module allows access to this stored
    data. *)

module Tezos_blocks_cache :
  Ringo_lwt.Sigs.CACHE_MAP_OPT with type key = Block_hash.t

(** Information about the rollup that is kept in the state. *)
type rollup_info = Stores.rollup_info = {
  rollup_id : Tx_rollup.t;
  origination_level : int32 option;
}

type t = private {
  stores : Stores.t;
  cctxt : Protocol_client_context.full;
  context_index : Context.index;
  mutable head : L2block.t option;
  rollup_info : rollup_info;
  tezos_blocks_cache : Alpha_block_services.block_info Tezos_blocks_cache.t;
  constants : Constants.t;
  signers : Node_config.signers;
  caps : Node_config.caps;
}

(** [init cctxt config] creates a new state for the rollup node with a new store
   and context.  If the [rollup_genesis] block hash is provided in [config],
   checks that the rollup [rollup_id] is created inside the block identified by
   the hash. Otherwise, the genesis information is read from the disk. Note that
   if a [rollup_genesis] is provided, it must also match the one on disk. L2
   block are cached (controlled by l2_blocks_cache_size) for performance
   improvements w.r.t. access to the store. *)
val init :
  #Protocol_client_context.full ->
  ?readonly:bool ->
  Node_config.t ->
  t tzresult Lwt.t

(** {2 Reading the state from disk}  *)

(** Retrieve the current head of the rollup. Note that the current head can go
    in the past or change in case of reorganisations at the L1 layer.  *)
val get_head : t -> L2block.t option

(** Retrieve an L2 block by its hash *)
val get_block : t -> L2block.hash -> L2block.t option Lwt.t

(** Retrieve the block hash associated to a given level in the current
    chain. Note that levels can be reaffected in case of reorganisation at the L1
    layer. *)
val get_level : t -> L2block.level -> L2block.hash option Lwt.t

(** Retrieve an inbox associated to an L2 block *)
val get_inbox : t -> L2block.hash -> Inbox.t option Lwt.t

(** Retrieve the header of an L2 block *)
val get_header : t -> L2block.hash -> L2block.header option Lwt.t

(** Retrieve the L2 block hash corresponding to the given Tezos block. When
    there is no inbox for an L1 block, we associate to it the L2 block of its
    predecessor. So [get_tezos_l2_block_hash state h] returns L2 block hash at
    which the rollup was when the Tezos node was at block [h]. *)
val get_tezos_l2_block_hash : t -> Block_hash.t -> L2block.hash option Lwt.t

(** Same as {!get_tezos_block} but retrieves the associated L2 block at the same time. *)
val get_tezos_l2_block : t -> Block_hash.t -> L2block.t option Lwt.t

(** Same as {!get_level} but retrieves the associated header at the same time. *)
val get_level_l2_block_header :
  t -> L2block.level -> L2block.header option Lwt.t

(** Same as {!get_level} but retrieves the associated L2 block at the same time. *)
val get_level_l2_block : t -> L2block.level -> L2block.t option Lwt.t

(** Returns [true] if the Tezos block was already processed by the rollup node. *)
val tezos_block_already_processed : t -> Block_hash.t -> bool Lwt.t

(** Returns the inclusion info for a commitment. *)
val get_included_commitment :
  t ->
  Tx_rollup_commitment_hash.t ->
  L2block.commitment_included_info option Lwt.t

(** Returns the last finalized (on L1) rollup level. *)
val get_finalized_level : t -> Tx_rollup_level.t option Lwt.t

(** Retrieve an L2 block metadata from its header *)
val get_block_metadata : t -> L2block.header -> L2block.metadata Lwt.t

(** Retrieve an L2 block and associated metadata by its hash *)
val get_block_and_metadata :
  t -> L2block.hash -> (L2block.t * L2block.metadata) option Lwt.t

(** {2 Saving the state to disk}  *)

(** Set the current head of the rollup and return the blocks (hashes) that were
    reorganized. *)
val set_head : t -> L2block.t -> L2block.t reorg tzresult Lwt.t

(** Set the Tezos head and returns the reorganization of L1 blocks. *)
val set_tezos_head :
  t -> Block_hash.t -> Alpha_block_services.block_info reorg tzresult Lwt.t

(** Save an L2 block to disk:
    - Save both the header and the inbox
    - Make the level point to this block
 *)
val save_block : t -> L2block.t -> unit Lwt.t

(** Make a level point to a given L2 block. If the level already points to a
    block, it is changed. *)
val save_level : t -> L2block.level -> L2block.hash -> unit Lwt.t

(** Associate an L2 block to a Tezos block, and register its level and
    predecessor as well. *)
val save_tezos_block_info :
  t ->
  Block_hash.t ->
  L2block.hash ->
  level:int32 ->
  predecessor:Block_hash.t ->
  unit Lwt.t

(** Register a commitment as included on L1. *)
val set_commitment_included :
  t ->
  Tx_rollup_commitment_hash.t ->
  Block_hash.t ->
  Operation_hash.t ->
  unit Lwt.t

(** Register a commitment as not included on L1. *)
val unset_commitment_included : t -> Tx_rollup_commitment_hash.t -> unit Lwt.t

(** Register the last finalized (on L1) rollup level. *)
val set_finalized_level : t -> Tx_rollup_level.t -> unit tzresult Lwt.t

(** Delete the last finalized (on L1) rollup level. *)
val delete_finalized_level : t -> unit Lwt.t

(** Register the origination level of the rollup for this node. *)
val set_rollup_info :
  t -> Tx_rollup.t -> origination_level:int32 -> unit tzresult Lwt.t

(** {2 Misc}  *)

(** [rollup_operation_index] returns the index in which the rollup operation are
    stored into a [Block_info.t]. Currently, the manager operation validation
    pass is used. *)
val rollup_operation_index : int

(** Fetch a Tezos block from the cache or the node *)
val fetch_tezos_block :
  t -> Block_hash.t -> Alpha_block_services.block_info tzresult Lwt.t

(** Compute the reorganization of L2 blocks from the chain whose head is
    [old_head_hash] and the chain whose head [new_head_hash]. *)
val rollup_reorg :
  t -> old_head:L2block.t -> new_head:L2block.t -> L2block.t reorg Lwt.t
