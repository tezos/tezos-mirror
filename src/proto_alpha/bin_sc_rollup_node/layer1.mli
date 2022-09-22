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

(** This module maintains information about the layer 1 chain.

   This module follows the evolution of the layer 1 chain by
   subscribing to the head monitoring RPC offered by the Tezos node.
*)
type head = Head of {hash : Block_hash.t; level : int32}

val head_encoding : head Data_encoding.t

(** Type of cache holding the last 32 blocks, with their operations. *)
type blocks_cache

type t = private {
  blocks_cache : blocks_cache;
  heads : head Lwt_stream.t;
  cctxt : Protocol_client_context.full;
  stopper : RPC_context.stopper;
  genesis_info : Protocol.Alpha_context.Sc_rollup.Commitment.genesis_info;
}

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3311
   Allow to retrieve L1 blocks through Tezos node storage locally. *)

(** [start configuration cctxt store] returns a stream of [chain_event] obtained
    from the monitoring of the Tezos node set up by the client [cctxt]. The
    layer 1 state is stored in the data directory declared in
    [configuration]. *)
val start :
  Configuration.t ->
  Protocol_client_context.full ->
  Store.t ->
  (t * Protocol.Alpha_context.Sc_rollup.Kind.t) tzresult Lwt.t

(** [reconnect cfg l1_ctxt store] reconnects (and retries with delay) to the
    Tezos node. The delay for each reconnection is increased with a randomized
    exponential backoff (capped to 1.5h) . *)
val reconnect : Configuration.t -> t -> Store.t -> t tzresult Lwt.t

(** [level_of_hash l1_ctxt hash] returns the level for a given block
   [hash]. Raise [Invalid_argument] if [hash] does not belong to
   [store]. *)
val level_of_hash : t -> Block_hash.t -> int32 tzresult Lwt.t

(** [get_predecessor_opt state head] returns the predecessor, if any, of block
    of [head]. *)
val get_predecessor_opt : t -> head -> head option Lwt.t

(** [get_predecessor state head] returns the predecessor block of [head]. *)
val get_predecessor : t -> head -> head tzresult Lwt.t

(** [genesis_hash] is the hash of the genesis block of the chain. *)
val genesis_hash : Block_hash.t

(** [shutdown store] properly shut the layer 1 down. *)
val shutdown : t -> unit Lwt.t

(** [fetch_tezos_block l1_ctxt hash] returns a block info given a block hash.
    Looks for the block in the blocks cache first, and fetches it from the L1
    node otherwise. *)
val fetch_tezos_block :
  t ->
  Block_hash.t ->
  Protocol_client_context.Alpha_block_services.block_info tzresult Lwt.t

(** [get_tezos_reorg_for_new_head l1_ctxt old_head new_head] returns the
    reorganization of L1 blocks (if any) between [old_head] and [new_head]. *)
val get_tezos_reorg_for_new_head :
  t ->
  Block_hash.t option ->
  Block_hash.t ->
  Protocol_client_context.Alpha_block_services.block_info Injector_common.reorg
  tzresult
  Lwt.t
