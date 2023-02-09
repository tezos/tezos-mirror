(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Ethereum address (20 bytes) *)
type address = Address of string [@@ocaml.unboxed]

let address_of_string s = Address s

let address_encoding =
  Data_encoding.(conv (fun (Address a) -> a) (fun a -> Address a) string)

(** Ethereum generic quantity, always encoded in hexadecimal. *)
type quantity = Qty of Z.t [@@ocaml.unboxed]

let quantity_of_z z = Qty z

let z_to_hexa = Z.format "#x"

let quantity_encoding =
  Data_encoding.conv
    (fun (Qty q) -> z_to_hexa q)
    (fun q -> Qty (Z.of_string q))
    Data_encoding.string

(** Ethereum block level. *)
type block_height = Block_height of Z.t [@@ocaml.unboxed]

let block_height_of_z z = Block_height z

let block_height_encoding =
  Data_encoding.conv
    (fun (Block_height h) -> z_to_hexa h)
    (fun h -> Block_height (Z.of_string h))
    Data_encoding.string

(** Ethereum block params in RPCs. *)
type block_param = Hash_param of block_height | Earliest | Latest | Pending

let block_param_encoding =
  let open Data_encoding in
  union
    [
      (let tag = "hex" in
       case
         ~title:tag
         (Tag 0)
         block_height_encoding
         (function Hash_param h -> Some h | _ -> None)
         (fun h -> Hash_param h));
      (let tag = "earliest" in
       case
         ~title:tag
         (Tag 1)
         (constant tag)
         (function Earliest -> Some () | _ -> None)
         (fun () -> Earliest));
      (let tag = "latest" in
       case
         ~title:tag
         (Tag 2)
         (constant tag)
         (function Latest -> Some () | _ -> None)
         (fun () -> Latest));
      (let tag = "pending" in
       case
         ~title:tag
         (Tag 3)
         (constant tag)
         (function Pending -> Some () | _ -> None)
         (fun () -> Pending));
    ]

(** Ethereum block hash (32 bytes) *)
type block_hash = Block_hash of string [@@ocaml.unboxed]

let block_hash_of_string s = Block_hash s

let block_hash_encoding =
  Data_encoding.(conv (fun (Block_hash h) -> h) (fun h -> Block_hash h) string)

(** Ethereum any hash *)
type hash = Hash of string [@@ocaml.unboxed]

let hash_of_string h = Hash h

let hash_encoding =
  Data_encoding.(conv (fun (Hash h) -> h) (fun h -> Hash h) string)

(** Ethereum block hash representation from RPCs. *)
type block = {
  number : block_height option;
  hash : block_hash option;
  parent : block_hash;
  nonce : hash;
  sha3Uncles : hash;
  logsBloom : hash option;
  transactionRoot : hash;
  stateRoot : hash;
  receiptRoot : hash;
  miner : hash;
  difficulty : quantity;
  totalDifficulty : quantity;
  extraData : string;
  size : quantity;
  gasLimit : quantity;
  gasUsed : quantity;
  timestamp : quantity;
  transaction : hash list;
  uncles : hash list;
}

let block_encoding =
  let open Data_encoding in
  conv
    (fun {
           number;
           hash;
           parent;
           nonce;
           sha3Uncles;
           logsBloom;
           transactionRoot;
           stateRoot;
           receiptRoot;
           miner;
           difficulty;
           totalDifficulty;
           extraData;
           size;
           gasLimit;
           gasUsed;
           timestamp;
           transaction;
           uncles;
         } ->
      ( ( number,
          hash,
          parent,
          nonce,
          sha3Uncles,
          logsBloom,
          transactionRoot,
          stateRoot,
          receiptRoot,
          miner ),
        ( difficulty,
          totalDifficulty,
          extraData,
          size,
          gasLimit,
          gasUsed,
          timestamp,
          transaction,
          uncles ) ))
    (fun ( ( number,
             hash,
             parent,
             nonce,
             sha3Uncles,
             logsBloom,
             transactionRoot,
             stateRoot,
             receiptRoot,
             miner ),
           ( difficulty,
             totalDifficulty,
             extraData,
             size,
             gasLimit,
             gasUsed,
             timestamp,
             transaction,
             uncles ) ) ->
      {
        number;
        hash;
        parent;
        nonce;
        sha3Uncles;
        logsBloom;
        transactionRoot;
        stateRoot;
        receiptRoot;
        miner;
        difficulty;
        totalDifficulty;
        extraData;
        size;
        gasLimit;
        gasUsed;
        timestamp;
        transaction;
        uncles;
      })
    (merge_objs
       (obj10
          (req "number" (option block_height_encoding))
          (req "hash" (option block_hash_encoding))
          (req "parent" block_hash_encoding)
          (req "nonce" hash_encoding)
          (req "sha3Uncles" hash_encoding)
          (req "logsBloom" (option hash_encoding))
          (req "transactionRoot" hash_encoding)
          (req "stateRoot" hash_encoding)
          (req "receiptRoot" hash_encoding)
          (req "miner" hash_encoding))
       (obj9
          (req "difficulty" quantity_encoding)
          (req "totalDifficulty" quantity_encoding)
          (req "extraData" string)
          (req "size" quantity_encoding)
          (req "gasLimit" quantity_encoding)
          (req "gasUsed" quantity_encoding)
          (req "timestamp" quantity_encoding)
          (req "transaction" (list hash_encoding))
          (req "uncles" (list hash_encoding))))
