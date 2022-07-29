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

open Protocol_client_context

(** The type of signers for operations injected by the injector *)
type signer = {
  alias : string;
  pkh : Signature.public_key_hash;
  pk : Signature.public_key;
  sk : Client_keys.sk_uri;
}

(** Type of chain reorganizations. *)
type 'block reorg = {
  old_chain : 'block list;
      (** The blocks that were in the old chain and which are not in the new one. *)
  new_chain : 'block list;
      (** The blocks that are now in the new chain. The length of [old_chain] and
      [new_chain] may be different. *)
}

(** Retrieve a signer from the client wallet. *)
val get_signer :
  #Client_context.wallet -> Signature.public_key_hash -> signer tzresult Lwt.t

val no_reorg : 'a reorg

val reorg_encoding : 'a Data_encoding.t -> 'a reorg Data_encoding.t

type block_info := Alpha_block_services.block_info

(** [fetch_tezos_block ~find_in_cache cctxt hash] returns a block info given a
    block hash. Looks for the block using [find_in_cache] first, and fetches
    it from the L1 node otherwise. *)
val fetch_tezos_block :
  find_in_cache:
    (Block_hash.t ->
    (Block_hash.t -> block_info tzresult Lwt.t) ->
    block_info tzresult Lwt.t) ->
  #full ->
  Block_hash.t ->
  block_info tzresult Lwt.t

(** [tezos_reorg fetch ~old_head_hash ~new_head_hash] computes the
    reorganization of L1 blocks from the chain whose head is [old_head_hash] and
    the chain whose head [new_head_hash]. *)
val tezos_reorg :
  (Block_hash.t -> block_info tzresult Lwt.t) ->
  old_head_hash:Block_hash.t ->
  new_head_hash:Block_hash.t ->
  block_info reorg tzresult Lwt.t
