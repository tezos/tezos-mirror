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

open Protocol
open Alpha_context

module Hash =
  Blake2B.Make
    (Base58)
    (struct
      let name = "tx_rollup_l2_block_hash"

      let title = "An tx_rollup L2 block identifier"

      let b58check_prefix = "\016\006\254" (* BTx(53) *)

      let size = None
    end)

let () = Base58.check_encoded_prefix Hash.b58check_encoding "BTx" 53

type hash = Hash.t

type level = Tx_rollup_level.t

type header = {
  level : level;
  tezos_block : Block_hash.t;
  predecessor : hash option;
  context : Tx_rollup_l2_context_hash.t;
  commitment : Tx_rollup_commitment_hash.t;
}

type t = {
  hash : hash;
  header : header;
  inbox : Inbox.t;
  commitment : Tx_rollup_commitment.Full.t;
}

type commitment_included_info = {
  block : Block_hash.t;
  operation : Operation_hash.t;
}

type metadata = {
  commitment_included : commitment_included_info option;
  finalized : bool;
}

let level_encoding = Tx_rollup_level.encoding

let level_to_string l = Int32.to_string (Tx_rollup_level.to_int32 l)

let header_encoding =
  let open Data_encoding in
  conv
    (fun {level; tezos_block; predecessor; context; commitment} ->
      (level, tezos_block, predecessor, context, commitment))
    (fun (level, tezos_block, predecessor, context, commitment) ->
      {level; tezos_block; predecessor; context; commitment})
    (obj5
       (req "level" Tx_rollup_level.encoding)
       (req "tezos_block" Block_hash.encoding)
       (opt "predecessor" Hash.encoding)
       (req "context" Tx_rollup_l2_context_hash.encoding)
       (req "commitment" Tx_rollup_commitment_hash.encoding))

let encoding =
  let open Data_encoding in
  conv
    (fun {hash; header; inbox; commitment} -> (hash, header, inbox, commitment))
    (fun (hash, header, inbox, commitment) -> {hash; header; inbox; commitment})
    (obj4
       (req "hash" Hash.encoding)
       (req "header" header_encoding)
       (req "inbox" Inbox.encoding)
       (req "commitment" Tx_rollup_commitment.Full.encoding))

let commitment_included_info_encoding =
  let open Data_encoding in
  conv
    (fun {block; operation} -> (block, operation))
    (fun (block, operation) -> {block; operation})
    (obj2
       (req "block" Block_hash.encoding)
       (req "operation" Operation_hash.encoding))

let metadata_encoding =
  let open Data_encoding in
  conv
    (fun {commitment_included; finalized} -> (commitment_included, finalized))
    (fun (commitment_included, finalized) -> {commitment_included; finalized})
    (obj2
       (opt "commitment_included" commitment_included_info_encoding)
       (req "finalized" bool))

let hash_header h =
  Hash.hash_bytes [Data_encoding.Binary.to_bytes_exn header_encoding h]
