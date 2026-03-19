(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
open Script_typed_ir

[@@@ocaml.warning "-32"]

let ( @$ ) x y = Item_t (x, y)

let bot = Bot_t

let unit = unit_t

(* the type of integers *)
let int = int_t

(* the type of naturals *)
let nat = nat_t

(* the type of strings *)
let string = string_t

(* the type of bytes *)
let bytes = bytes_t

(* the type of booleans *)
let bool = bool_t

(* the type of mutez *)
let mutez = mutez_t

(* the type of public key *)
let public_key = key_t

(* the type of key hashes *)
let key_hash = key_hash_t

(* the type of signatures *)
let signature = signature_t

(* the type of addresses *)
let address = address_t

(* the type of chain ids *)
let chain_id = chain_id_t

(* the type of timestamps *)
let timestamp = timestamp_t

(* list type constructor *)
let list x = match list_t (-1) x with Error _ -> assert false | Ok t -> t

(* option type constructor *)
let option x = match option_t (-1) x with Error _ -> assert false | Ok t -> t

(* map type constructor*)
let map k v = match map_t (-1) k v with Error _ -> assert false | Ok t -> t

(* map type constructor*)
let big_map k v =
  match big_map_t (-1) k v with Error _ -> assert false | Ok t -> t

(* set type constructor*)
let set k = match set_t (-1) k with Error _ -> assert false | Ok t -> t

(* pair type constructor*)
let pair k1 k2 =
  match pair_t (-1) k1 k2 with Error _ -> assert false | Ok t -> t

(* comparable pair type constructor *)
let cpair k1 k2 =
  match comparable_pair_t (-1) k1 k2 with Error _ -> assert false | Ok t -> t

(* or type constructor*)
let or_ k1 k2 = match or_t (-1) k1 k2 with Error _ -> assert false | Ok t -> t

(* comparable or type constructor *)
let cor k1 k2 =
  match comparable_or_t (-1) k1 k2 with Error _ -> assert false | Ok t -> t

let lambda x y =
  match lambda_t (-1) x y with Error _ -> assert false | Ok t -> t

let contract arg_ty =
  match contract_t (-1) arg_ty with Error _ -> assert false | Ok t -> t

let operation = operation_t

let sapling_state memo_size = sapling_state_t ~memo_size

let sapling_transaction memo_size = sapling_transaction_t ~memo_size

let sapling_transaction_deprecated memo_size =
  sapling_transaction_deprecated_t ~memo_size

let bls12_381_g1 = bls12_381_g1_t

let bls12_381_g2 = bls12_381_g2_t

let bls12_381_fr = bls12_381_fr_t

let ticket ty =
  match ticket_t (-1) ty with Error _ -> assert false | Ok t -> t

let chest_key = chest_key_t

let chest = chest_t
