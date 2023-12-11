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

open Octez_smart_rollup_node.Layer1

(** [fetch_tezos_block cctxt hash] returns a block info given a block hash.
    Looks for the block in the blocks cache first, and fetches it from the L1
    node otherwise. *)
val fetch_tezos_block :
  t ->
  Block_hash.t ->
  Protocol_client_context.Alpha_block_services.block_info tzresult Lwt.t

(** [prefetch_tezos_blocks l1_ctxt blocks] prefetches the blocks
    asynchronously. NOTE: the number of blocks to prefetch must not be greater
    than the size of the blocks cache otherwise they will be lost. *)
val prefetch_tezos_blocks : t -> head list -> unit

val get_last_cemented_commitment :
  #Client_context.full -> Address.t -> Node_context.lcc tzresult Lwt.t

val get_last_published_commitment :
  ?allow_unstake:bool ->
  #Client_context.full ->
  Address.t ->
  Signature.public_key_hash ->
  Commitment.t option tzresult Lwt.t

val get_kind : #Client_context.full -> Address.t -> Kind.t tzresult Lwt.t

val genesis_inbox :
  #Client_context.full ->
  genesis_level:int32 ->
  Octez_smart_rollup.Inbox.t tzresult Lwt.t

(** Convert protocol constants to their protocol agnostic representation. *)
val constants_of_parametric :
  Protocol.Alpha_context.Constants.Parametric.t ->
  Rollup_constants.protocol_constants

(** Retrieve protocol agnotic constants for the head of the chain. *)
val retrieve_constants :
  ?block:Block_services.block ->
  #Client_context.full ->
  Rollup_constants.protocol_constants tzresult Lwt.t

val retrieve_genesis_info :
  #Client_context.full -> Address.t -> Node_context.genesis_info tzresult Lwt.t

(** [get_boot_sector block_hash node_ctxt] retrieves the boot sector from the
    rollup origination operation in block [block_hash]. Precondition:
    [block_hash] has to be the block where the rollup was originated. *)
val get_boot_sector : Block_hash.t -> _ Node_context.t -> string tzresult Lwt.t

(** Find and retrieve the whitelist the rollup. *)
val find_whitelist :
  #Client_context.full ->
  Address.t ->
  Signature.public_key_hash list option tzresult Lwt.t

(** Find and retrieve information about the last whitelist update. *)
val find_last_whitelist_update :
  #Client_context.full -> Address.t -> (Z.t * Int32.t) option tzresult Lwt.t

(** Retrieve a commitment published on L1. *)
val get_commitment :
  #Client_context.full ->
  Address.t ->
  Commitment.Hash.t ->
  Commitment.t tzresult Lwt.t
