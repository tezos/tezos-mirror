(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module maintains information about the layer 1 chain.

   This module follows the evolution of the layer 1 chain by
   subscribing to the head monitoring RPC offered by the Tezos node.
*)

type error += Cannot_find_block of Block_hash.t

type header = {
  hash : Block_hash.t;
  level : int32;
  header : Block_header.shell_header;
}

type head = {hash : Block_hash.t; level : int32}

val header_encoding : header Data_encoding.t

val head_encoding : head Data_encoding.t

val head_of_header : header -> head

type t

(** An extensible type for the protocol specific full blocks. This allows to
    have a single cache for blocks from all protocols. *)
type block = ..

(** Type of protocol specific functions for fetching protocol specific blocks. *)
type fetch_block_rpc =
  Client_context.full ->
  ?metadata:[`Always | `Never] ->
  ?chain:Tezos_shell_services.Block_services.chain ->
  ?block:Tezos_shell_services.Block_services.block ->
  unit ->
  block tzresult Lwt.t

(** Returns the raw L1 connection to allow for monitoring by others. *)
val raw_l1_connection : t -> Layer_1.t

(** [start ~name ~reconnection_delay ~l1_blocks_cache_size ?protocols cctxt]
    connects to a Tezos node and starts monitoring new heads. One can iterate on
    the heads by calling {!iter_heads} on its result. [reconnection_delay] gives
    an initial delay for the reconnection which is used in an exponential
    backoff. The [name] is used to differentiate events. [l1_blocks_cache_size]
    is the size of the caches for the blocks and headers. If [protocols] is
    provided, only heads of these protocols will be monitored. *)
val start :
  name:string ->
  reconnection_delay:float ->
  l1_blocks_cache_size:int ->
  ?protocols:Protocol_hash.t list ->
  ?prefetch_blocks:int ->
  #Client_context.full ->
  t tzresult Lwt.t

(** Same as {!start} but does not start monitoring L1 blocks. *)
val create :
  name:string ->
  reconnection_delay:float ->
  l1_blocks_cache_size:int ->
  ?protocols:Protocol_hash.t trace ->
  ?prefetch_blocks:int ->
  #Client_context.full ->
  t tzresult

(** [shutdown t] properly shuts the layer 1 down. *)
val shutdown : t -> unit Lwt.t

(** [iter_heads ?only_new t f] calls [f] on all new heads appearing in the layer
    1 chain. In case of a disconnection with the layer 1 node, it reconnects
    automatically. If [f] returns an error (other than a disconnection) it,
    [iter_heads] terminates and returns the error. If [only_new] is [true], [f]
    will be called only on heads that are produced after the call is made. *)
val iter_heads :
  ?only_new:bool -> t -> (header -> unit tzresult Lwt.t) -> unit tzresult Lwt.t

(** [wait_first t] waits for the first head to appear in the stream and
    returns it. *)
val wait_first : t -> header Lwt.t

val get_predecessor_opt :
  ?max_read:int ->
  t ->
  Block_hash.t * int32 ->
  (Block_hash.t * int32) option tzresult Lwt.t

val get_predecessor :
  ?max_read:int ->
  t ->
  Block_hash.t * int32 ->
  (Block_hash.t * int32) tzresult Lwt.t

val get_tezos_reorg_for_new_head :
  t ->
  ?get_old_predecessor:
    (Block_hash.t * int32 -> (Block_hash.t * int32) tzresult Lwt.t) ->
  [`Head of Block_hash.t * int32 | `Level of int32] ->
  Block_hash.t * int32 ->
  (Block_hash.t * int32) Reorg.t tzresult Lwt.t

(** {2 Helpers } *)

(** Register a block header in the cache. *)
val cache_shell_header : t -> Block_hash.t -> Block_header.shell_header -> unit

(** Returns the client context used by the L1 monitor. *)
val client_context : t -> Client_context.full

(** [fetch_tezos_shell_header cctxt hash] returns the block shell header of
    [hash]. Looks for the block in the blocks cache first, and fetches it from
    the L1 node otherwise. *)
val fetch_tezos_shell_header :
  t -> Block_hash.t -> Block_header.shell_header tzresult Lwt.t

(** [fetch_tezos_block fetch extract_header cctxt hash] returns a block info
    given a block hash. Looks for the block in the blocks cache first, and
    fetches it from the L1 node otherwise. *)
val fetch_tezos_block :
  fetch_block_rpc ->
  (block -> Block_header.shell_header) ->
  t ->
  Block_hash.t ->
  block tzresult Lwt.t

(** [make_prefetching_schedule l1_ctxt blocks] returns the list [blocks] with
    each element associated to a list of blocks to prefetch of at most
    [l1_ctxt.prefetch_blocks]. If [blocks = [b1; ...; bn]] and [prefetch_blocks
    = 3] then the result will be [(b1, [b1;b2;b3]); (b2, []); (b3, []); (b4,
    [b4;b5;b6]); ...]. *)
val make_prefetching_schedule : t -> 'block trace -> ('block * 'block list) list

(** [prefetch_tezos_blocks fetch extract_header l1_ctxt  blocks] prefetches the
    blocks asynchronously. NOTE: the number of blocks to prefetch must not be
    greater than the size of the blocks cache otherwise they will be lost. *)
val prefetch_tezos_blocks :
  fetch_block_rpc ->
  (block -> Block_header.shell_header) ->
  t ->
  head list ->
  unit

(**/**)

module Internal_for_tests : sig
  (** Create a dummy Layer 1 object that does not follow any L1 chain. This
      function is only to be used as a placeholder for unit tests (that do not
      exercise the Layer 1 connection). *)
  val dummy : #Client_context.full -> t
end
