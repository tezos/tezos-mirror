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

type level = Genesis | Rollup_level of Tx_rollup_level.t

type header = {
  level : level;
  tezos_block : Block_hash.t;
  predecessor : hash;
  context : Tx_rollup_l2_context_hash.t;
  commitment : Tx_rollup_commitment_hash.t option;
}

type t = {
  hash : hash;
  header : header;
  inbox : Inbox.t;
  commitment : Tx_rollup_commitment.Full.t option;
}

type commitment_included_info = {
  block : Block_hash.t;
  operation : Operation_hash.t;
}

type metadata = {
  commitment_included : commitment_included_info option;
  finalized : bool;
}

let level_encoding =
  let open Data_encoding in
  splitted
    ~json:
      (union
         [
           case
             ~title:"genesis"
             Json_only
             (constant "genesis")
             (function Genesis -> Some () | _ -> None)
             (fun () -> Genesis);
           case
             ~title:"rollup_level"
             Json_only
             Tx_rollup_level.encoding
             (function Rollup_level l -> Some l | _ -> None)
             (fun l -> Rollup_level l);
         ])
    ~binary:
      (conv
         (function
           | Genesis -> -1l | Rollup_level l -> Tx_rollup_level.to_int32 l)
         (function
           | -1l -> Genesis
           | l ->
               let l =
                 WithExceptions.Result.get_ok ~loc:__LOC__
                 @@ Tx_rollup_level.of_int32 l
               in
               Rollup_level l)
         int32)

let level_to_string = function
  | Genesis -> "genesis"
  | Rollup_level l -> Int32.to_string (Tx_rollup_level.to_int32 l)

let header_encoding =
  let open Data_encoding in
  conv
    (fun {level; tezos_block; predecessor; context; commitment} ->
      (level, tezos_block, predecessor, context, commitment))
    (fun (level, tezos_block, predecessor, context, commitment) ->
      {level; tezos_block; predecessor; context; commitment})
    (obj5
       (req "level" level_encoding)
       (req "tezos_block" Block_hash.encoding)
       (req "predecessor" Hash.encoding)
       (req "context" Tx_rollup_l2_context_hash.encoding)
       (opt "commitment" Tx_rollup_commitment_hash.encoding))

let encoding =
  let open Data_encoding in
  conv
    (fun {hash; header; inbox; commitment} -> (hash, header, inbox, commitment))
    (fun (hash, header, inbox, commitment) -> {hash; header; inbox; commitment})
    (obj4
       (req "hash" Hash.encoding)
       (req "header" header_encoding)
       (req "inbox" Inbox.encoding)
       (opt "commitment" Tx_rollup_commitment.Full.encoding))

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

let genesis_hash rollup = Hash.hash_string [Tx_rollup.to_b58check rollup]

let hash_header h =
  match h.level with
  | Genesis ->
      (* The genesis block is its own predecessor, so its "hash" can be found in
         the predecessor field (we don't use the real hash of the genesis
         header).. *)
      h.predecessor
  | Rollup_level _ ->
      Hash.hash_bytes [Data_encoding.Binary.to_bytes_exn header_encoding h]

let genesis_block ctxt rollup tezos_block =
  let open Lwt_syntax in
  let* context_hash = Context.commit ctxt in
  let hash = genesis_hash rollup in
  let header =
    {
      level = Genesis;
      tezos_block;
      predecessor = hash;
      (* Genesis block is its own predecessor *)
      context = context_hash;
      commitment = None;
    }
  in
  let inbox : Inbox.t = {contents = []; cumulated_size = 0} in
  return {hash; header; inbox; commitment = None}
