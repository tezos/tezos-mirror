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

open Protocol
open Script_typed_ir

[@@@ocaml.warning "-32"]

let ( @$ ) x y = Item_t (x, y, None)

let bot = Bot_t

let unit = Unit_t None

let unit_cmp = Unit_key None

let int_cmp = Int_key None

let string_cmp = String_key None

(* the type of integers *)
let int = Int_t None

(* the type of naturals *)
let nat = Nat_t None

(* the type of strings *)
let string = String_t None

(* the type of bytes *)
let bytes = Bytes_t None

(* the type of booleans *)
let bool = Bool_t None

(* the type of mutez *)
let mutez = Mutez_t None

(* the type of public key *)
let public_key = Key_t None

(* the type of key hashes *)
let key_hash = Key_hash_t None

(* the type of signatures *)
let signature = Signature_t None

(* the type of addresses *)
let address = Address_t None

(* the type of chain ids *)
let chain_id = Chain_id_t None

(* the type of timestamps *)
let timestamp = Timestamp_t None

(* list type constructor *)
let list x = List_t (x, None)

(* option type constructor *)
let option x = Option_t (x, None)

(* map type constructor*)
let map k v = Map_t (k, v, None)

(* map type constructor*)
let big_map k v = Big_map_t (k, v, None)

(* set type constructor*)
let set k = Set_t (k, None)

(* pair type constructor*)
let pair k1 k2 = Pair_t ((k1, None, None), (k2, None, None), None)

(* union type constructor*)
let union k1 k2 = Union_t ((k1, None), (k2, None), None)

let lambda x y = Lambda_t (x, y, None)

let contract arg_ty = Contract_t (arg_ty, None)

let operation = Operation_t None

let sapling_state memo_size = Sapling_state_t (memo_size, None)

let sapling_transaction memo_size = Sapling_transaction_t (memo_size, None)

let bls12_381_g1 = Bls12_381_g1_t None

let bls12_381_g2 = Bls12_381_g2_t None

let bls12_381_fr = Bls12_381_fr_t None

let ticket ty = Ticket_t (ty, None)
