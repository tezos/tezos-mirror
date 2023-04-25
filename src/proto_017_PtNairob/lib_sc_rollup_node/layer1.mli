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

include module type of Octez_crawler.Layer_1

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3311
   Allow to retrieve L1 blocks through Tezos node storage locally. *)

(** [iter_heads t f] calls [f] on all new heads appearing in the layer 1
    chain. In case of a disconnection with the layer 1 node, it reconnects
    automatically. If [f] returns an error (other than a disconnection) it,
    [iter_heads] terminates and returns the error.  *)
val iter_heads : t -> (header -> unit tzresult Lwt.t) -> unit tzresult Lwt.t

(** {2 Helpers } *)

(** [cache_shell_header hash header] saves in the local cache the shell [header]
    for the block [hash]. *)
val cache_shell_header : Block_hash.t -> Block_header.shell_header -> unit

(** [fetch_tezos_shell_header cctxt hash] returns the block shell header of
    [hash]. Looks for the block in the blocks cache first, and fetches it from
    the L1 node otherwise. *)
val fetch_tezos_shell_header :
  #Tezos_rpc.Context.simple ->
  Block_hash.t ->
  Block_header.shell_header tzresult Lwt.t

(** [fetch_tezos_block cctxt hash] returns a block info given a block hash.
    Looks for the block in the blocks cache first, and fetches it from the L1
    node otherwise. *)
val fetch_tezos_block :
  #Tezos_rpc.Context.simple ->
  Block_hash.t ->
  Protocol_client_context.Alpha_block_services.block_info tzresult Lwt.t
