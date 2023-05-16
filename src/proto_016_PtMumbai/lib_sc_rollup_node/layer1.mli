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
  #Client_context.full ->
  t Lwt.t

(** [shutdown t] properly shuts the layer 1 down. *)
val shutdown : t -> unit Lwt.t

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3311
   Allow to retrieve L1 blocks through Tezos node storage locally. *)

(** [iter_heads t f] calls [f] on all new heads appearing in the layer 1
    chain. In case of a disconnection with the layer 1 node, it reconnects
    automatically. If [f] returns an error (other than a disconnection) it,
    [iter_heads] terminates and returns the error.  *)
val iter_heads : t -> (header -> unit tzresult Lwt.t) -> unit tzresult Lwt.t

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

(** [fetch_tezos_shell_header cctxt hash] returns the block shell header of
    [hash]. Looks for the block in the blocks cache first, and fetches it from
    the L1 node otherwise. *)
val fetch_tezos_shell_header :
  t -> Block_hash.t -> Block_header.shell_header tzresult Lwt.t

(** [fetch_tezos_block cctxt hash] returns a block info given a block hash.
    Looks for the block in the blocks cache first, and fetches it from the L1
    node otherwise. *)
val fetch_tezos_block :
  t ->
  Block_hash.t ->
  Protocol_client_context.Alpha_block_services.block_info tzresult Lwt.t

(**/**)

module Internal_for_tests : sig
  (** Create a dummy Layer 1 object that does not follow any L1 chain. This
      function is only to be used as a placeholder for unit tests (that do not
      exercise the Layer 1 connection). *)
  val dummy : #Client_context.full -> t
end
