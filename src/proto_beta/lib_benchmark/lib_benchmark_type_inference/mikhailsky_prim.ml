(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024 Marigold, <contact@marigold.dev>                       *)
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

(** Mikhailsky primitives correspond to Michelson primitives plus special
    "holes" for instructions and data. *)

type prim =
  | K_parameter
  | K_storage
  | K_code
  | D_False
  | D_Elt
  | D_Left
  | D_None
  | D_Pair
  | D_Right
  | D_Some
  | D_True
  | D_Unit
  | D_Ticket
  | I_PACK
  | I_UNPACK
  | I_BLAKE2B
  | I_SHA256
  | I_SHA512
  | I_ABS
  | I_ADD
  | I_AMOUNT
  | I_AND
  | I_BALANCE
  | I_CAR
  | I_CDR
  | I_CHAIN_ID
  | I_CHECK_SIGNATURE
  | I_COMPARE
  | I_CONCAT
  | I_CONS
  | I_CREATE_ACCOUNT
  | I_CREATE_CONTRACT
  | I_IMPLICIT_ACCOUNT
  | I_DIP
  | I_DROP
  | I_DUP
  | I_EDIV
  | I_EMPTY_BIG_MAP
  | I_EMPTY_MAP
  | I_EMPTY_SET
  | I_EQ
  | I_EXEC
  | I_APPLY
  | I_FAILWITH
  | I_GE
  | I_GET_MAP
  | I_GET_AND_UPDATE_MAP
  | I_GT
  | I_HASH_KEY
  | I_IF
  | I_IF_CONS
  | I_IF_LEFT
  | I_IF_NONE
  | I_INT
  | I_LAMBDA
  | I_LE
  | I_LEFT
  | I_LOOP
  | I_LSL
  | I_LSR
  | I_LT
  | I_MAP_MAP
  | I_MAP_LIST
  | I_MEM_SET
  | I_MEM_MAP
  | I_MUL
  | I_NEG
  | I_NEQ
  | I_NIL
  | I_NONE
  | I_NOT
  | I_NOW
  | I_OR
  | I_PAIR
  | I_UNPAIR
  | I_PUSH
  | I_RIGHT
  | I_SIZE_SET
  | I_SIZE_MAP
  | I_SIZE_LIST
  | I_SIZE_STRING
  | I_SIZE_BYTES
  | I_SOME
  | I_SOURCE
  | I_SENDER
  | I_SELF
  | I_SLICE
  | I_STEPS_TO_QUOTA
  | I_SUB
  | I_SWAP
  | I_TRANSFER_TOKENS
  | I_SET_DELEGATE
  | I_UNIT
  | I_UPDATE_SET
  | I_UPDATE_MAP
  | I_XOR
  | I_ITER_MAP
  | I_ITER_LIST
  | I_ITER_SET
  | I_LOOP_LEFT
  | I_ADDRESS
  | I_CONTRACT
  | I_ISNAT
  | I_CAST
  | I_RENAME
  | I_DIG
  | I_DUG
  | I_LEVEL
  | I_SELF_ADDRESS
  | I_NEVER
  | I_SAPLING_EMPTY_STATE
  | I_SAPLING_VERIFY_UPDATE
  | I_VOTING_POWER
  | I_TOTAL_VOTING_POWER
  | I_KECCAK
  | I_SHA3
  | I_PAIRING_CHECK
  | I_TICKET
  | I_READ_TICKET
  | I_SPLIT_TICKET
  | I_JOIN_TICKETS
  | T_bool
  | T_contract
  | T_int
  | T_key
  | T_key_hash
  | T_lambda
  | T_list
  | T_map
  | T_big_map
  | T_nat
  | T_option
  | T_or
  | T_pair
  | T_set
  | T_signature
  | T_string
  | T_bytes
  | T_mutez
  | T_timestamp
  | T_unit
  | T_operation
  | T_address
  | T_chain_id
  | T_never
  | T_sapling_state
  | T_sapling_transaction_deprecated
  | T_bls12_381_g1
  | T_bls12_381_g2
  | T_bls12_381_fr
  | T_ticket
  (* Holes in programs and data. *)
  | I_Hole
  | D_Hole
  (* Annotations. *)
  | A_Int
  | A_Nat
  | A_Timestamp
  | A_Mutez
  | A_Key_hash
  | A_Key
  | A_List
  | A_Set
  | A_Map
  | A_Lambda

let relation =
  [
    (K_parameter, Michelson_v1_primitives.K_parameter);
    (K_storage, Michelson_v1_primitives.K_storage);
    (K_code, Michelson_v1_primitives.K_code);
    (D_False, Michelson_v1_primitives.D_False);
    (D_Elt, Michelson_v1_primitives.D_Elt);
    (D_Left, Michelson_v1_primitives.D_Left);
    (D_None, Michelson_v1_primitives.D_None);
    (D_Pair, Michelson_v1_primitives.D_Pair);
    (D_Right, Michelson_v1_primitives.D_Right);
    (D_Some, Michelson_v1_primitives.D_Some);
    (D_True, Michelson_v1_primitives.D_True);
    (D_Unit, Michelson_v1_primitives.D_Unit);
    (I_PACK, Michelson_v1_primitives.I_PACK);
    (I_UNPACK, Michelson_v1_primitives.I_UNPACK);
    (I_BLAKE2B, Michelson_v1_primitives.I_BLAKE2B);
    (I_SHA256, Michelson_v1_primitives.I_SHA256);
    (I_SHA512, Michelson_v1_primitives.I_SHA512);
    (I_ABS, Michelson_v1_primitives.I_ABS);
    (I_ADD, Michelson_v1_primitives.I_ADD);
    (I_AMOUNT, Michelson_v1_primitives.I_AMOUNT);
    (I_AND, Michelson_v1_primitives.I_AND);
    (I_BALANCE, Michelson_v1_primitives.I_BALANCE);
    (I_CAR, Michelson_v1_primitives.I_CAR);
    (I_CDR, Michelson_v1_primitives.I_CDR);
    (I_CHAIN_ID, Michelson_v1_primitives.I_CHAIN_ID);
    (I_CHECK_SIGNATURE, Michelson_v1_primitives.I_CHECK_SIGNATURE);
    (I_COMPARE, Michelson_v1_primitives.I_COMPARE);
    (I_CONCAT, Michelson_v1_primitives.I_CONCAT);
    (I_CONS, Michelson_v1_primitives.I_CONS);
    (I_CREATE_ACCOUNT, Michelson_v1_primitives.I_CREATE_ACCOUNT);
    (I_CREATE_CONTRACT, Michelson_v1_primitives.I_CREATE_CONTRACT);
    (I_IMPLICIT_ACCOUNT, Michelson_v1_primitives.I_IMPLICIT_ACCOUNT);
    (I_DIP, Michelson_v1_primitives.I_DIP);
    (I_DROP, Michelson_v1_primitives.I_DROP);
    (I_DUP, Michelson_v1_primitives.I_DUP);
    (I_EDIV, Michelson_v1_primitives.I_EDIV);
    (I_EMPTY_BIG_MAP, Michelson_v1_primitives.I_EMPTY_BIG_MAP);
    (I_EMPTY_MAP, Michelson_v1_primitives.I_EMPTY_MAP);
    (I_EMPTY_SET, Michelson_v1_primitives.I_EMPTY_SET);
    (I_EQ, Michelson_v1_primitives.I_EQ);
    (I_EXEC, Michelson_v1_primitives.I_EXEC);
    (I_APPLY, Michelson_v1_primitives.I_APPLY);
    (I_FAILWITH, Michelson_v1_primitives.I_FAILWITH);
    (I_GE, Michelson_v1_primitives.I_GE);
    (I_GET_MAP, Michelson_v1_primitives.I_GET);
    (I_GET_AND_UPDATE_MAP, Michelson_v1_primitives.I_GET_AND_UPDATE);
    (I_GT, Michelson_v1_primitives.I_GT);
    (I_HASH_KEY, Michelson_v1_primitives.I_HASH_KEY);
    (I_IF, Michelson_v1_primitives.I_IF);
    (I_IF_CONS, Michelson_v1_primitives.I_IF_CONS);
    (I_IF_LEFT, Michelson_v1_primitives.I_IF_LEFT);
    (I_IF_NONE, Michelson_v1_primitives.I_IF_NONE);
    (I_INT, Michelson_v1_primitives.I_INT);
    (I_LAMBDA, Michelson_v1_primitives.I_LAMBDA);
    (I_LE, Michelson_v1_primitives.I_LE);
    (I_LEFT, Michelson_v1_primitives.I_LEFT);
    (I_LEVEL, Michelson_v1_primitives.I_LEVEL);
    (I_LOOP, Michelson_v1_primitives.I_LOOP);
    (I_LSL, Michelson_v1_primitives.I_LSL);
    (I_LSR, Michelson_v1_primitives.I_LSR);
    (I_LT, Michelson_v1_primitives.I_LT);
    (I_MAP_MAP, Michelson_v1_primitives.I_MAP);
    (I_MAP_LIST, Michelson_v1_primitives.I_MAP);
    (I_MEM_SET, Michelson_v1_primitives.I_MEM);
    (I_MEM_MAP, Michelson_v1_primitives.I_MEM);
    (I_MUL, Michelson_v1_primitives.I_MUL);
    (I_NEG, Michelson_v1_primitives.I_NEG);
    (I_NEQ, Michelson_v1_primitives.I_NEQ);
    (I_NIL, Michelson_v1_primitives.I_NIL);
    (I_NONE, Michelson_v1_primitives.I_NONE);
    (I_NOT, Michelson_v1_primitives.I_NOT);
    (I_NOW, Michelson_v1_primitives.I_NOW);
    (I_OR, Michelson_v1_primitives.I_OR);
    (I_PAIR, Michelson_v1_primitives.I_PAIR);
    (I_UNPAIR, Michelson_v1_primitives.I_UNPAIR);
    (I_PUSH, Michelson_v1_primitives.I_PUSH);
    (I_RIGHT, Michelson_v1_primitives.I_RIGHT);
    (I_SIZE_SET, Michelson_v1_primitives.I_SIZE);
    (I_SIZE_MAP, Michelson_v1_primitives.I_SIZE);
    (I_SIZE_LIST, Michelson_v1_primitives.I_SIZE);
    (I_SIZE_STRING, Michelson_v1_primitives.I_SIZE);
    (I_SIZE_BYTES, Michelson_v1_primitives.I_SIZE);
    (I_SOME, Michelson_v1_primitives.I_SOME);
    (I_SOURCE, Michelson_v1_primitives.I_SOURCE);
    (I_SENDER, Michelson_v1_primitives.I_SENDER);
    (I_SELF, Michelson_v1_primitives.I_SELF);
    (I_SELF_ADDRESS, Michelson_v1_primitives.I_SELF_ADDRESS);
    (I_SLICE, Michelson_v1_primitives.I_SLICE);
    (I_STEPS_TO_QUOTA, Michelson_v1_primitives.I_STEPS_TO_QUOTA);
    (I_SUB, Michelson_v1_primitives.I_SUB);
    (I_SWAP, Michelson_v1_primitives.I_SWAP);
    (I_TRANSFER_TOKENS, Michelson_v1_primitives.I_TRANSFER_TOKENS);
    (I_SET_DELEGATE, Michelson_v1_primitives.I_SET_DELEGATE);
    (I_UNIT, Michelson_v1_primitives.I_UNIT);
    (I_UPDATE_SET, Michelson_v1_primitives.I_UPDATE);
    (I_UPDATE_MAP, Michelson_v1_primitives.I_UPDATE);
    (I_XOR, Michelson_v1_primitives.I_XOR);
    (I_ITER_MAP, Michelson_v1_primitives.I_ITER);
    (I_ITER_LIST, Michelson_v1_primitives.I_ITER);
    (I_ITER_SET, Michelson_v1_primitives.I_ITER);
    (I_LOOP_LEFT, Michelson_v1_primitives.I_LOOP_LEFT);
    (I_ADDRESS, Michelson_v1_primitives.I_ADDRESS);
    (I_CONTRACT, Michelson_v1_primitives.I_CONTRACT);
    (I_ISNAT, Michelson_v1_primitives.I_ISNAT);
    (I_CAST, Michelson_v1_primitives.I_CAST);
    (I_RENAME, Michelson_v1_primitives.I_RENAME);
    (I_SAPLING_EMPTY_STATE, Michelson_v1_primitives.I_SAPLING_EMPTY_STATE);
    (I_SAPLING_VERIFY_UPDATE, Michelson_v1_primitives.I_SAPLING_VERIFY_UPDATE);
    (I_DIG, Michelson_v1_primitives.I_DIG);
    (I_DUG, Michelson_v1_primitives.I_DUG);
    (I_NEVER, Michelson_v1_primitives.I_NEVER);
    (I_VOTING_POWER, Michelson_v1_primitives.I_VOTING_POWER);
    (I_TOTAL_VOTING_POWER, Michelson_v1_primitives.I_TOTAL_VOTING_POWER);
    (I_KECCAK, Michelson_v1_primitives.I_KECCAK);
    (I_SHA3, Michelson_v1_primitives.I_SHA3);
    (I_PAIRING_CHECK, Michelson_v1_primitives.I_PAIRING_CHECK);
    (I_TICKET, Michelson_v1_primitives.I_TICKET);
    (I_READ_TICKET, Michelson_v1_primitives.I_READ_TICKET);
    (I_SPLIT_TICKET, Michelson_v1_primitives.I_SPLIT_TICKET);
    (I_JOIN_TICKETS, Michelson_v1_primitives.I_JOIN_TICKETS);
    (T_bool, Michelson_v1_primitives.T_bool);
    (T_contract, Michelson_v1_primitives.T_contract);
    (T_int, Michelson_v1_primitives.T_int);
    (T_key, Michelson_v1_primitives.T_key);
    (T_key_hash, Michelson_v1_primitives.T_key_hash);
    (T_lambda, Michelson_v1_primitives.T_lambda);
    (T_list, Michelson_v1_primitives.T_list);
    (T_map, Michelson_v1_primitives.T_map);
    (T_big_map, Michelson_v1_primitives.T_big_map);
    (T_nat, Michelson_v1_primitives.T_nat);
    (T_option, Michelson_v1_primitives.T_option);
    (T_or, Michelson_v1_primitives.T_or);
    (T_pair, Michelson_v1_primitives.T_pair);
    (T_set, Michelson_v1_primitives.T_set);
    (T_signature, Michelson_v1_primitives.T_signature);
    (T_string, Michelson_v1_primitives.T_string);
    (T_bytes, Michelson_v1_primitives.T_bytes);
    (T_mutez, Michelson_v1_primitives.T_mutez);
    (T_timestamp, Michelson_v1_primitives.T_timestamp);
    (T_unit, Michelson_v1_primitives.T_unit);
    (T_operation, Michelson_v1_primitives.T_operation);
    (T_address, Michelson_v1_primitives.T_address);
    ( T_sapling_transaction_deprecated,
      Michelson_v1_primitives.T_sapling_transaction_deprecated );
    (T_sapling_state, Michelson_v1_primitives.T_sapling_state);
    (T_chain_id, Michelson_v1_primitives.T_chain_id);
    (T_never, Michelson_v1_primitives.T_never);
    (T_bls12_381_g1, Michelson_v1_primitives.T_bls12_381_g1);
    (T_bls12_381_g2, Michelson_v1_primitives.T_bls12_381_g2);
    (T_bls12_381_fr, Michelson_v1_primitives.T_bls12_381_fr);
    (T_ticket, Michelson_v1_primitives.T_ticket);
  ]

let relation_table =
  let table = Hashtbl.create 269 in
  List.iter
    (fun (mikhailsky, michelson) -> Hashtbl.add table mikhailsky michelson)
    relation ;
  table

exception Primitive_cannot_be_cast_back_to_Michelson of prim

let to_michelson prim =
  match Hashtbl.find relation_table prim with
  | exception Not_found ->
      raise (Primitive_cannot_be_cast_back_to_Michelson prim)
  | res -> res

let string_of_prim prim =
  match prim with
  | K_parameter -> "K_parameter"
  | K_storage -> "K_storage"
  | K_code -> "K_code"
  | D_False -> "D_False"
  | D_Elt -> "D_Elt"
  | D_Left -> "D_Left"
  | D_None -> "D_None"
  | D_Pair -> "D_Pair"
  | D_Right -> "D_Right"
  | D_Some -> "D_Some"
  | D_True -> "D_True"
  | D_Unit -> "D_Unit"
  | D_Ticket -> "D_Ticket"
  | I_PACK -> "I_PACK"
  | I_UNPACK -> "I_UNPACK"
  | I_BLAKE2B -> "I_BLAKE2B"
  | I_SHA256 -> "I_SHA256"
  | I_SHA512 -> "I_SHA512"
  | I_ABS -> "I_ABS"
  | I_ADD -> "I_ADD"
  | I_AMOUNT -> "I_AMOUNT"
  | I_AND -> "I_AND"
  | I_BALANCE -> "I_BALANCE"
  | I_CAR -> "I_CAR"
  | I_CDR -> "I_CDR"
  | I_CHAIN_ID -> "I_CHAIN_ID"
  | I_CHECK_SIGNATURE -> "I_CHECK_SIGNATURE"
  | I_COMPARE -> "I_COMPARE"
  | I_CONCAT -> "I_CONCAT"
  | I_CONS -> "I_CONS"
  | I_CREATE_ACCOUNT -> "I_CREATE_ACCOUNT"
  | I_CREATE_CONTRACT -> "I_CREATE_CONTRACT"
  | I_IMPLICIT_ACCOUNT -> "I_IMPLICIT_ACCOUNT"
  | I_DIP -> "I_DIP"
  | I_DROP -> "I_DROP"
  | I_DUP -> "I_DUP"
  | I_EDIV -> "I_EDIV"
  | I_EMPTY_BIG_MAP -> "I_EMPTY_BIG_MAP"
  | I_EMPTY_MAP -> "I_EMPTY_MAP"
  | I_EMPTY_SET -> "I_EMPTY_SET"
  | I_EQ -> "I_EQ"
  | I_EXEC -> "I_EXEC"
  | I_APPLY -> "I_APPLY"
  | I_FAILWITH -> "I_FAILWITH"
  | I_GE -> "I_GE"
  | I_GET_MAP -> "I_GET_MAP"
  | I_GET_AND_UPDATE_MAP -> "I_GET_AND_UPDATE_MAP"
  | I_GT -> "I_GT"
  | I_HASH_KEY -> "I_HASH_KEY"
  | I_IF -> "I_IF"
  | I_IF_CONS -> "I_IF_CONS"
  | I_IF_LEFT -> "I_IF_LEFT"
  | I_IF_NONE -> "I_IF_NONE"
  | I_INT -> "I_INT"
  | I_LAMBDA -> "I_LAMBDA"
  | I_LE -> "I_LE"
  | I_LEFT -> "I_LEFT"
  | I_LOOP -> "I_LOOP"
  | I_LSL -> "I_LSL"
  | I_LSR -> "I_LSR"
  | I_LT -> "I_LT"
  | I_MAP_MAP -> "I_MAP_MAP"
  | I_MAP_LIST -> "I_MAP_LIST"
  | I_MEM_SET -> "I_MEM_SET"
  | I_MEM_MAP -> "I_MEM_MAP"
  | I_MUL -> "I_MUL"
  | I_NEG -> "I_NEG"
  | I_NEQ -> "I_NEQ"
  | I_NIL -> "I_NIL"
  | I_NONE -> "I_NONE"
  | I_NOT -> "I_NOT"
  | I_NOW -> "I_NOW"
  | I_OR -> "I_OR"
  | I_PAIR -> "I_PAIR"
  | I_UNPAIR -> "I_UNPAIR"
  | I_PUSH -> "I_PUSH"
  | I_RIGHT -> "I_RIGHT"
  | I_SIZE_SET -> "I_SIZE_SET"
  | I_SIZE_MAP -> "I_SIZE_MAP"
  | I_SIZE_LIST -> "I_SIZE_LIST"
  | I_SIZE_STRING -> "I_SIZE_STRING"
  | I_SIZE_BYTES -> "I_SIZE_BYTES"
  | I_SOME -> "I_SOME"
  | I_SOURCE -> "I_SOURCE"
  | I_SENDER -> "I_SENDER"
  | I_SELF -> "I_SELF"
  | I_SLICE -> "I_SLICE"
  | I_STEPS_TO_QUOTA -> "I_STEPS_TO_QUOTA"
  | I_SUB -> "I_SUB"
  | I_SWAP -> "I_SWAP"
  | I_TRANSFER_TOKENS -> "I_TRANSFER_TOKENS"
  | I_SET_DELEGATE -> "I_SET_DELEGATE"
  | I_UNIT -> "I_UNIT"
  | I_UPDATE_SET -> "I_UPDATE_SET"
  | I_UPDATE_MAP -> "I_UPDATE_MAP"
  | I_XOR -> "I_XOR"
  | I_ITER_MAP -> "I_ITER_MAP"
  | I_ITER_LIST -> "I_ITER_LIST"
  | I_ITER_SET -> "I_ITER_SET"
  | I_LOOP_LEFT -> "I_LOOP_LEFT"
  | I_ADDRESS -> "I_ADDRESS"
  | I_CONTRACT -> "I_CONTRACT"
  | I_ISNAT -> "I_ISNAT"
  | I_CAST -> "I_CAST"
  | I_RENAME -> "I_RENAME"
  | I_DIG -> "I_DIG"
  | I_DUG -> "I_DUG"
  | I_LEVEL -> "I_LEVEL"
  | I_SELF_ADDRESS -> "I_SELF_ADDRESS"
  | I_NEVER -> "I_NEVER"
  | I_SAPLING_EMPTY_STATE -> "I_SAPLING_EMPTY_STATE"
  | I_SAPLING_VERIFY_UPDATE -> "I_SAPLING_VERIFY_UPDATE"
  | I_VOTING_POWER -> "I_VOTING_POWER"
  | I_TOTAL_VOTING_POWER -> "I_TOTAL_VOTING_POWER"
  | I_KECCAK -> "I_KECCAK"
  | I_SHA3 -> "I_SHA3"
  | I_PAIRING_CHECK -> "I_PAIRING_CHECK"
  | I_TICKET -> "I_TICKET"
  | I_READ_TICKET -> "I_READ_TICKET"
  | I_SPLIT_TICKET -> "I_SPLIT_TICKET"
  | I_JOIN_TICKETS -> "I_JOIN_TICKETS"
  | T_bool -> "T_bool"
  | T_contract -> "T_contract"
  | T_int -> "T_int"
  | T_key -> "T_key"
  | T_key_hash -> "T_key_hash"
  | T_lambda -> "T_lambda"
  | T_list -> "T_list"
  | T_map -> "T_map"
  | T_big_map -> "T_big_map"
  | T_nat -> "T_nat"
  | T_option -> "T_option"
  | T_or -> "T_or"
  | T_pair -> "T_pair"
  | T_set -> "T_set"
  | T_signature -> "T_signature"
  | T_string -> "T_string"
  | T_bytes -> "T_bytes"
  | T_mutez -> "T_mutez"
  | T_timestamp -> "T_timestamp"
  | T_unit -> "T_unit"
  | T_operation -> "T_operation"
  | T_address -> "T_address"
  | T_chain_id -> "T_chain_id"
  | T_never -> "T_never"
  | T_sapling_state -> "T_sapling_state"
  | T_sapling_transaction_deprecated -> "T_sapling_transaction_deprecated"
  | T_bls12_381_g1 -> "T_bls12_381_g1"
  | T_bls12_381_g2 -> "T_bls12_381_g2"
  | T_bls12_381_fr -> "T_bls12_381_fr"
  | T_ticket -> "T_ticket"
  | I_Hole -> "I_Hole"
  | D_Hole -> "D_Hole"
  | A_Int -> "A_Int"
  | A_Nat -> "A_Nat"
  | A_Timestamp -> "A_Timestamp"
  | A_Mutez -> "A_Mutez"
  | A_Key_hash -> "A_Key_hash"
  | A_Key -> "A_Key"
  | A_List -> "A_List"
  | A_Set -> "A_Set"
  | A_Map -> "A_Map"
  | A_Lambda -> "A_Lambda"

let pp fmtr prim = Format.fprintf fmtr "%s" (string_of_prim prim)

type kind = Data_kind | Instr_kind | Type_kind | Keyword_kind | Annot_kind

let kind (x : prim) =
  match x with
  | K_parameter | K_storage | K_code -> Keyword_kind
  | D_Hole | D_False | D_Elt | D_Left | D_None | D_Pair | D_Right | D_Some
  | D_True | D_Unit | D_Ticket ->
      Data_kind
  | I_PACK | I_UNPACK | I_BLAKE2B | I_SHA256 | I_SHA512 | I_ABS | I_ADD
  | I_AMOUNT | I_AND | I_BALANCE | I_CAR | I_CDR | I_CHAIN_ID
  | I_CHECK_SIGNATURE | I_COMPARE | I_CONCAT | I_CONS | I_CREATE_ACCOUNT
  | I_CREATE_CONTRACT | I_IMPLICIT_ACCOUNT | I_DIP | I_DROP | I_DUP | I_EDIV
  | I_EMPTY_BIG_MAP | I_EMPTY_MAP | I_EMPTY_SET | I_EQ | I_EXEC | I_APPLY
  | I_FAILWITH | I_GE | I_GET_MAP | I_GET_AND_UPDATE_MAP | I_GT | I_HASH_KEY
  | I_IF | I_IF_CONS | I_IF_LEFT | I_IF_NONE | I_INT | I_LAMBDA | I_LE | I_LEFT
  | I_LOOP | I_LSL | I_LSR | I_LT | I_MAP_MAP | I_MAP_LIST | I_MEM_SET
  | I_MEM_MAP | I_MUL | I_NEG | I_NEQ | I_NIL | I_NONE | I_NOT | I_NOW | I_OR
  | I_PAIR | I_UNPAIR | I_PUSH | I_RIGHT | I_SIZE_SET | I_SIZE_MAP | I_SIZE_LIST
  | I_SIZE_STRING | I_SIZE_BYTES | I_SOME | I_SOURCE | I_SENDER | I_SELF
  | I_SLICE | I_STEPS_TO_QUOTA | I_SUB | I_SWAP | I_TRANSFER_TOKENS
  | I_SET_DELEGATE | I_UNIT | I_UPDATE_SET | I_UPDATE_MAP | I_XOR | I_ITER_MAP
  | I_ITER_LIST | I_ITER_SET | I_LOOP_LEFT | I_ADDRESS | I_CONTRACT | I_ISNAT
  | I_CAST | I_RENAME | I_DIG | I_DUG | I_LEVEL | I_SELF_ADDRESS | I_NEVER
  | I_SAPLING_EMPTY_STATE | I_SAPLING_VERIFY_UPDATE | I_VOTING_POWER
  | I_TOTAL_VOTING_POWER | I_KECCAK | I_SHA3 | I_PAIRING_CHECK | I_TICKET
  | I_READ_TICKET | I_SPLIT_TICKET | I_JOIN_TICKETS | I_Hole ->
      Instr_kind
  | T_bool | T_contract | T_int | T_key | T_key_hash | T_lambda | T_list | T_map
  | T_big_map | T_nat | T_option | T_or | T_pair | T_set | T_signature
  | T_string | T_bytes | T_mutez | T_timestamp | T_unit | T_operation
  | T_address | T_chain_id | T_never | T_sapling_state
  | T_sapling_transaction_deprecated | T_bls12_381_g1 | T_bls12_381_g2
  | T_bls12_381_fr | T_ticket ->
      Type_kind
  (* Holes in programs and data. *)
  (* Annotations. *)
  | A_Int | A_Nat | A_Timestamp | A_Mutez | A_Key_hash | A_Key | A_List | A_Set
  | A_Map | A_Lambda ->
      Annot_kind
