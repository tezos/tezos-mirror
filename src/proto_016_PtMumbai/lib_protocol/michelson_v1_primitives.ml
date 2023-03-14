(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda,jp>                 *)
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

open Micheline

type error += Unknown_primitive_name of string

type error += Invalid_case of string

type error +=
  | Invalid_primitive_name of
      string Micheline.canonical * Micheline.canonical_location

type prim =
  | K_parameter
  | K_storage
  | K_code
  | K_view
  | D_False
  | D_Elt
  | D_Left
  | D_None
  | D_Pair
  | D_Right
  | D_Some
  | D_True
  | D_Unit
  | D_Lambda_rec
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
  | I_VIEW
  | I_EDIV
  | I_EMPTY_BIG_MAP
  | I_EMPTY_MAP
  | I_EMPTY_SET
  | I_EQ
  | I_EXEC
  | I_APPLY
  | I_FAILWITH
  | I_GE
  | I_GET
  | I_GET_AND_UPDATE
  | I_GT
  | I_HASH_KEY
  | I_IF
  | I_IF_CONS
  | I_IF_LEFT
  | I_IF_NONE
  | I_INT
  | I_LAMBDA
  | I_LAMBDA_REC
  | I_LE
  | I_LEFT
  | I_LEVEL
  | I_LOOP
  | I_LSL
  | I_LSR
  | I_LT
  | I_MAP
  | I_MEM
  | I_MUL
  | I_NEG
  | I_NEQ
  | I_NIL
  | I_NONE
  | I_NOT
  | I_NOW
  | I_MIN_BLOCK_TIME
  | I_OR
  | I_PAIR
  | I_UNPAIR
  | I_PUSH
  | I_RIGHT
  | I_SIZE
  | I_SOME
  | I_SOURCE
  | I_SENDER
  | I_SELF
  | I_SELF_ADDRESS
  | I_SLICE
  | I_STEPS_TO_QUOTA
  | I_SUB
  | I_SUB_MUTEZ
  | I_SWAP
  | I_TRANSFER_TOKENS
  | I_SET_DELEGATE
  | I_UNIT
  | I_UPDATE
  | I_XOR
  | I_ITER
  | I_LOOP_LEFT
  | I_ADDRESS
  | I_CONTRACT
  | I_ISNAT
  | I_CAST
  | I_RENAME
  | I_SAPLING_EMPTY_STATE
  | I_SAPLING_VERIFY_UPDATE
  | I_DIG
  | I_DUG
  | I_NEVER
  | I_VOTING_POWER
  | I_TOTAL_VOTING_POWER
  | I_KECCAK
  | I_SHA3
  | I_PAIRING_CHECK
  | I_TICKET
  | I_TICKET_DEPRECATED
  | I_READ_TICKET
  | I_SPLIT_TICKET
  | I_JOIN_TICKETS
  | I_OPEN_CHEST
  | I_EMIT
  | I_BYTES
  | I_NAT
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
  | T_tx_rollup_l2_address
  | T_sapling_transaction
  | T_sapling_transaction_deprecated
  | T_sapling_state
  | T_chain_id
  | T_never
  | T_bls12_381_g1
  | T_bls12_381_g2
  | T_bls12_381_fr
  | T_ticket
  | T_chest_key
  | T_chest
  | H_constant

(* Auxiliary types for error documentation.
   All the prim constructor prefixes must match their namespace. *)
type namespace =
  | (* prefix "T" *) Type_namespace
  | (* prefix "D" *) Constant_namespace
  | (* prefix "I" *) Instr_namespace
  | (* prefix "K" *) Keyword_namespace
  | (* prefix "H" *) Constant_hash_namespace

let namespace = function
  | K_code | K_view | K_parameter | K_storage -> Keyword_namespace
  | D_Elt | D_False | D_Left | D_None | D_Pair | D_Right | D_Some | D_True
  | D_Unit | D_Lambda_rec ->
      Constant_namespace
  | I_ABS | I_ADD | I_ADDRESS | I_AMOUNT | I_AND | I_APPLY | I_BALANCE
  | I_BLAKE2B | I_CAR | I_CAST | I_CDR | I_CHAIN_ID | I_CHECK_SIGNATURE
  | I_COMPARE | I_CONCAT | I_CONS | I_CONTRACT | I_CREATE_ACCOUNT
  | I_CREATE_CONTRACT | I_DIG | I_DIP | I_DROP | I_DUG | I_DUP | I_VIEW | I_EDIV
  | I_EMPTY_BIG_MAP | I_EMPTY_MAP | I_EMPTY_SET | I_EQ | I_EXEC | I_FAILWITH
  | I_GE | I_GET | I_GET_AND_UPDATE | I_GT | I_HASH_KEY | I_IF | I_IF_CONS
  | I_IF_LEFT | I_IF_NONE | I_IMPLICIT_ACCOUNT | I_INT | I_ISNAT | I_ITER
  | I_JOIN_TICKETS | I_KECCAK | I_LAMBDA | I_LAMBDA_REC | I_LE | I_LEFT
  | I_LEVEL | I_LOOP | I_LOOP_LEFT | I_LSL | I_LSR | I_LT | I_MAP | I_MEM
  | I_MUL | I_NEG | I_NEQ | I_NEVER | I_NIL | I_NONE | I_NOT | I_NOW
  | I_MIN_BLOCK_TIME | I_OR | I_PACK | I_PAIR | I_PAIRING_CHECK | I_PUSH
  | I_READ_TICKET | I_RENAME | I_RIGHT | I_SAPLING_EMPTY_STATE
  | I_SAPLING_VERIFY_UPDATE | I_SELF | I_SELF_ADDRESS | I_SENDER
  | I_SET_DELEGATE | I_SHA256 | I_SHA512 | I_SHA3 | I_SIZE | I_SLICE | I_SOME
  | I_SOURCE | I_SPLIT_TICKET | I_STEPS_TO_QUOTA | I_SUB | I_SUB_MUTEZ | I_SWAP
  | I_TICKET | I_TICKET_DEPRECATED | I_TOTAL_VOTING_POWER | I_TRANSFER_TOKENS
  | I_UNIT | I_UNPACK | I_UNPAIR | I_UPDATE | I_VOTING_POWER | I_XOR
  | I_OPEN_CHEST | I_EMIT | I_BYTES | I_NAT ->
      Instr_namespace
  | T_address | T_tx_rollup_l2_address | T_big_map | T_bool | T_bytes
  | T_chain_id | T_contract | T_int | T_key | T_key_hash | T_lambda | T_list
  | T_map | T_mutez | T_nat | T_never | T_operation | T_option | T_or | T_pair
  | T_sapling_state | T_sapling_transaction | T_sapling_transaction_deprecated
  | T_set | T_signature | T_string | T_timestamp | T_unit | T_bls12_381_fr
  | T_bls12_381_g1 | T_bls12_381_g2 | T_ticket | T_chest_key | T_chest ->
      Type_namespace
  | H_constant -> Constant_hash_namespace

let valid_case name =
  let is_lower = function '_' | 'a' .. 'z' -> true | _ -> false in
  let is_upper = function '_' | 'A' .. 'Z' -> true | _ -> false in
  let rec for_all a b f = Compare.Int.(a > b) || (f a && for_all (a + 1) b f) in
  let len = String.length name in
  Compare.Int.(len <> 0)
  && Compare.Char.(name.[0] <> '_')
  && ((is_upper name.[0] && for_all 1 (len - 1) (fun i -> is_upper name.[i]))
     || (is_upper name.[0] && for_all 1 (len - 1) (fun i -> is_lower name.[i]))
     || (is_lower name.[0] && for_all 1 (len - 1) (fun i -> is_lower name.[i]))
     )

let string_of_prim = function
  | K_parameter -> "parameter"
  | K_storage -> "storage"
  | K_code -> "code"
  | K_view -> "view"
  | D_False -> "False"
  | D_Elt -> "Elt"
  | D_Left -> "Left"
  | D_None -> "None"
  | D_Pair -> "Pair"
  | D_Right -> "Right"
  | D_Some -> "Some"
  | D_True -> "True"
  | D_Unit -> "Unit"
  | D_Lambda_rec -> "Lambda_rec"
  | I_PACK -> "PACK"
  | I_UNPACK -> "UNPACK"
  | I_BLAKE2B -> "BLAKE2B"
  | I_SHA256 -> "SHA256"
  | I_SHA512 -> "SHA512"
  | I_ABS -> "ABS"
  | I_ADD -> "ADD"
  | I_AMOUNT -> "AMOUNT"
  | I_AND -> "AND"
  | I_BALANCE -> "BALANCE"
  | I_CAR -> "CAR"
  | I_CDR -> "CDR"
  | I_CHAIN_ID -> "CHAIN_ID"
  | I_CHECK_SIGNATURE -> "CHECK_SIGNATURE"
  | I_COMPARE -> "COMPARE"
  | I_CONCAT -> "CONCAT"
  | I_CONS -> "CONS"
  | I_CREATE_ACCOUNT -> "CREATE_ACCOUNT"
  | I_CREATE_CONTRACT -> "CREATE_CONTRACT"
  | I_IMPLICIT_ACCOUNT -> "IMPLICIT_ACCOUNT"
  | I_DIP -> "DIP"
  | I_DROP -> "DROP"
  | I_DUP -> "DUP"
  | I_EDIV -> "EDIV"
  | I_EMPTY_BIG_MAP -> "EMPTY_BIG_MAP"
  | I_EMPTY_MAP -> "EMPTY_MAP"
  | I_EMPTY_SET -> "EMPTY_SET"
  | I_EQ -> "EQ"
  | I_EXEC -> "EXEC"
  | I_APPLY -> "APPLY"
  | I_FAILWITH -> "FAILWITH"
  | I_GE -> "GE"
  | I_GET -> "GET"
  | I_GET_AND_UPDATE -> "GET_AND_UPDATE"
  | I_GT -> "GT"
  | I_HASH_KEY -> "HASH_KEY"
  | I_IF -> "IF"
  | I_IF_CONS -> "IF_CONS"
  | I_IF_LEFT -> "IF_LEFT"
  | I_IF_NONE -> "IF_NONE"
  | I_INT -> "INT"
  | I_LAMBDA -> "LAMBDA"
  | I_LAMBDA_REC -> "LAMBDA_REC"
  | I_LE -> "LE"
  | I_LEFT -> "LEFT"
  | I_LEVEL -> "LEVEL"
  | I_LOOP -> "LOOP"
  | I_LSL -> "LSL"
  | I_LSR -> "LSR"
  | I_LT -> "LT"
  | I_MAP -> "MAP"
  | I_MEM -> "MEM"
  | I_MUL -> "MUL"
  | I_NEG -> "NEG"
  | I_NEQ -> "NEQ"
  | I_NIL -> "NIL"
  | I_NONE -> "NONE"
  | I_NOT -> "NOT"
  | I_NOW -> "NOW"
  | I_MIN_BLOCK_TIME -> "MIN_BLOCK_TIME"
  | I_OR -> "OR"
  | I_PAIR -> "PAIR"
  | I_PUSH -> "PUSH"
  | I_RIGHT -> "RIGHT"
  | I_SIZE -> "SIZE"
  | I_SOME -> "SOME"
  | I_SOURCE -> "SOURCE"
  | I_SENDER -> "SENDER"
  | I_SELF -> "SELF"
  | I_SELF_ADDRESS -> "SELF_ADDRESS"
  | I_SLICE -> "SLICE"
  | I_STEPS_TO_QUOTA -> "STEPS_TO_QUOTA"
  | I_SUB -> "SUB"
  | I_SUB_MUTEZ -> "SUB_MUTEZ"
  | I_SWAP -> "SWAP"
  | I_TRANSFER_TOKENS -> "TRANSFER_TOKENS"
  | I_SET_DELEGATE -> "SET_DELEGATE"
  | I_UNIT -> "UNIT"
  | I_UNPAIR -> "UNPAIR"
  | I_UPDATE -> "UPDATE"
  | I_XOR -> "XOR"
  | I_ITER -> "ITER"
  | I_LOOP_LEFT -> "LOOP_LEFT"
  | I_ADDRESS -> "ADDRESS"
  | I_CONTRACT -> "CONTRACT"
  | I_ISNAT -> "ISNAT"
  | I_CAST -> "CAST"
  | I_RENAME -> "RENAME"
  | I_SAPLING_EMPTY_STATE -> "SAPLING_EMPTY_STATE"
  | I_SAPLING_VERIFY_UPDATE -> "SAPLING_VERIFY_UPDATE"
  | I_DIG -> "DIG"
  | I_DUG -> "DUG"
  | I_NEVER -> "NEVER"
  | I_VOTING_POWER -> "VOTING_POWER"
  | I_TOTAL_VOTING_POWER -> "TOTAL_VOTING_POWER"
  | I_KECCAK -> "KECCAK"
  | I_SHA3 -> "SHA3"
  | I_PAIRING_CHECK -> "PAIRING_CHECK"
  | I_TICKET -> "TICKET"
  | I_TICKET_DEPRECATED -> "TICKET_DEPRECATED"
  | I_READ_TICKET -> "READ_TICKET"
  | I_SPLIT_TICKET -> "SPLIT_TICKET"
  | I_JOIN_TICKETS -> "JOIN_TICKETS"
  | I_OPEN_CHEST -> "OPEN_CHEST"
  | I_EMIT -> "EMIT"
  | I_VIEW -> "VIEW"
  | I_BYTES -> "BYTES"
  | I_NAT -> "NAT"
  | T_bool -> "bool"
  | T_contract -> "contract"
  | T_int -> "int"
  | T_key -> "key"
  | T_key_hash -> "key_hash"
  | T_lambda -> "lambda"
  | T_list -> "list"
  | T_map -> "map"
  | T_big_map -> "big_map"
  | T_nat -> "nat"
  | T_option -> "option"
  | T_or -> "or"
  | T_pair -> "pair"
  | T_set -> "set"
  | T_signature -> "signature"
  | T_string -> "string"
  | T_bytes -> "bytes"
  | T_mutez -> "mutez"
  | T_timestamp -> "timestamp"
  | T_unit -> "unit"
  | T_operation -> "operation"
  | T_address -> "address"
  | T_tx_rollup_l2_address -> "tx_rollup_l2_address"
  | T_sapling_state -> "sapling_state"
  | T_sapling_transaction -> "sapling_transaction"
  | T_sapling_transaction_deprecated -> "sapling_transaction_deprecated"
  | T_chain_id -> "chain_id"
  | T_never -> "never"
  | T_bls12_381_g1 -> "bls12_381_g1"
  | T_bls12_381_g2 -> "bls12_381_g2"
  | T_bls12_381_fr -> "bls12_381_fr"
  | T_ticket -> "ticket"
  | T_chest_key -> "chest_key"
  | T_chest -> "chest"
  | H_constant -> "constant"

let prim_of_string = function
  | "parameter" -> ok K_parameter
  | "storage" -> ok K_storage
  | "code" -> ok K_code
  | "view" -> ok K_view
  | "False" -> ok D_False
  | "Elt" -> ok D_Elt
  | "Left" -> ok D_Left
  | "None" -> ok D_None
  | "Pair" -> ok D_Pair
  | "Right" -> ok D_Right
  | "Some" -> ok D_Some
  | "True" -> ok D_True
  | "Unit" -> ok D_Unit
  | "Lambda_rec" -> ok D_Lambda_rec
  | "PACK" -> ok I_PACK
  | "UNPACK" -> ok I_UNPACK
  | "BLAKE2B" -> ok I_BLAKE2B
  | "SHA256" -> ok I_SHA256
  | "SHA512" -> ok I_SHA512
  | "ABS" -> ok I_ABS
  | "ADD" -> ok I_ADD
  | "AMOUNT" -> ok I_AMOUNT
  | "AND" -> ok I_AND
  | "BALANCE" -> ok I_BALANCE
  | "CAR" -> ok I_CAR
  | "CDR" -> ok I_CDR
  | "CHAIN_ID" -> ok I_CHAIN_ID
  | "CHECK_SIGNATURE" -> ok I_CHECK_SIGNATURE
  | "COMPARE" -> ok I_COMPARE
  | "CONCAT" -> ok I_CONCAT
  | "CONS" -> ok I_CONS
  | "CREATE_ACCOUNT" -> ok I_CREATE_ACCOUNT
  | "CREATE_CONTRACT" -> ok I_CREATE_CONTRACT
  | "IMPLICIT_ACCOUNT" -> ok I_IMPLICIT_ACCOUNT
  | "DIP" -> ok I_DIP
  | "DROP" -> ok I_DROP
  | "DUP" -> ok I_DUP
  | "VIEW" -> ok I_VIEW
  | "EDIV" -> ok I_EDIV
  | "EMPTY_BIG_MAP" -> ok I_EMPTY_BIG_MAP
  | "EMPTY_MAP" -> ok I_EMPTY_MAP
  | "EMPTY_SET" -> ok I_EMPTY_SET
  | "EQ" -> ok I_EQ
  | "EXEC" -> ok I_EXEC
  | "APPLY" -> ok I_APPLY
  | "FAILWITH" -> ok I_FAILWITH
  | "GE" -> ok I_GE
  | "GET" -> ok I_GET
  | "GET_AND_UPDATE" -> ok I_GET_AND_UPDATE
  | "GT" -> ok I_GT
  | "HASH_KEY" -> ok I_HASH_KEY
  | "IF" -> ok I_IF
  | "IF_CONS" -> ok I_IF_CONS
  | "IF_LEFT" -> ok I_IF_LEFT
  | "IF_NONE" -> ok I_IF_NONE
  | "INT" -> ok I_INT
  | "KECCAK" -> ok I_KECCAK
  | "LAMBDA" -> ok I_LAMBDA
  | "LAMBDA_REC" -> ok I_LAMBDA_REC
  | "LE" -> ok I_LE
  | "LEFT" -> ok I_LEFT
  | "LEVEL" -> ok I_LEVEL
  | "LOOP" -> ok I_LOOP
  | "LSL" -> ok I_LSL
  | "LSR" -> ok I_LSR
  | "LT" -> ok I_LT
  | "MAP" -> ok I_MAP
  | "MEM" -> ok I_MEM
  | "MUL" -> ok I_MUL
  | "NEG" -> ok I_NEG
  | "NEQ" -> ok I_NEQ
  | "NIL" -> ok I_NIL
  | "NONE" -> ok I_NONE
  | "NOT" -> ok I_NOT
  | "NOW" -> ok I_NOW
  | "MIN_BLOCK_TIME" -> ok I_MIN_BLOCK_TIME
  | "OR" -> ok I_OR
  | "PAIR" -> ok I_PAIR
  | "UNPAIR" -> ok I_UNPAIR
  | "PAIRING_CHECK" -> ok I_PAIRING_CHECK
  | "PUSH" -> ok I_PUSH
  | "RIGHT" -> ok I_RIGHT
  | "SHA3" -> ok I_SHA3
  | "SIZE" -> ok I_SIZE
  | "SOME" -> ok I_SOME
  | "SOURCE" -> ok I_SOURCE
  | "SENDER" -> ok I_SENDER
  | "SELF" -> ok I_SELF
  | "SELF_ADDRESS" -> ok I_SELF_ADDRESS
  | "SLICE" -> ok I_SLICE
  | "STEPS_TO_QUOTA" -> ok I_STEPS_TO_QUOTA
  | "SUB" -> ok I_SUB
  | "SUB_MUTEZ" -> ok I_SUB_MUTEZ
  | "SWAP" -> ok I_SWAP
  | "TRANSFER_TOKENS" -> ok I_TRANSFER_TOKENS
  | "SET_DELEGATE" -> ok I_SET_DELEGATE
  | "UNIT" -> ok I_UNIT
  | "UPDATE" -> ok I_UPDATE
  | "XOR" -> ok I_XOR
  | "ITER" -> ok I_ITER
  | "LOOP_LEFT" -> ok I_LOOP_LEFT
  | "ADDRESS" -> ok I_ADDRESS
  | "CONTRACT" -> ok I_CONTRACT
  | "ISNAT" -> ok I_ISNAT
  | "CAST" -> ok I_CAST
  | "RENAME" -> ok I_RENAME
  | "SAPLING_EMPTY_STATE" -> ok I_SAPLING_EMPTY_STATE
  | "SAPLING_VERIFY_UPDATE" -> ok I_SAPLING_VERIFY_UPDATE
  | "DIG" -> ok I_DIG
  | "DUG" -> ok I_DUG
  | "NEVER" -> ok I_NEVER
  | "VOTING_POWER" -> ok I_VOTING_POWER
  | "TOTAL_VOTING_POWER" -> ok I_TOTAL_VOTING_POWER
  | "TICKET" -> ok I_TICKET
  | "TICKET_DEPRECATED" -> ok I_TICKET_DEPRECATED
  | "READ_TICKET" -> ok I_READ_TICKET
  | "SPLIT_TICKET" -> ok I_SPLIT_TICKET
  | "JOIN_TICKETS" -> ok I_JOIN_TICKETS
  | "OPEN_CHEST" -> ok I_OPEN_CHEST
  | "EMIT" -> ok I_EMIT
  | "BYTES" -> ok I_BYTES
  | "NAT" -> ok I_NAT
  | "bool" -> ok T_bool
  | "contract" -> ok T_contract
  | "int" -> ok T_int
  | "key" -> ok T_key
  | "key_hash" -> ok T_key_hash
  | "lambda" -> ok T_lambda
  | "list" -> ok T_list
  | "map" -> ok T_map
  | "big_map" -> ok T_big_map
  | "nat" -> ok T_nat
  | "option" -> ok T_option
  | "or" -> ok T_or
  | "pair" -> ok T_pair
  | "set" -> ok T_set
  | "signature" -> ok T_signature
  | "string" -> ok T_string
  | "bytes" -> ok T_bytes
  | "mutez" -> ok T_mutez
  | "timestamp" -> ok T_timestamp
  | "unit" -> ok T_unit
  | "operation" -> ok T_operation
  | "address" -> ok T_address
  | "tx_rollup_l2_address" -> ok T_tx_rollup_l2_address
  | "sapling_state" -> ok T_sapling_state
  | "sapling_transaction" -> ok T_sapling_transaction
  | "sapling_transaction_deprecated" -> ok T_sapling_transaction_deprecated
  | "chain_id" -> ok T_chain_id
  | "never" -> ok T_never
  | "bls12_381_g1" -> ok T_bls12_381_g1
  | "bls12_381_g2" -> ok T_bls12_381_g2
  | "bls12_381_fr" -> ok T_bls12_381_fr
  | "ticket" -> ok T_ticket
  | "chest_key" -> ok T_chest_key
  | "chest" -> ok T_chest
  | "constant" -> ok H_constant
  | n ->
      if valid_case n then error (Unknown_primitive_name n)
      else error (Invalid_case n)

let prims_of_strings expr =
  let rec convert = function
    | (Int _ | String _ | Bytes _) as expr -> ok expr
    | Prim (loc, prim, args, annot) ->
        Error_monad.record_trace
          (Invalid_primitive_name (expr, loc))
          (prim_of_string prim)
        >>? fun prim ->
        List.map_e convert args >|? fun args -> Prim (loc, prim, args, annot)
    | Seq (loc, args) -> List.map_e convert args >|? fun args -> Seq (loc, args)
  in
  convert (root expr) >|? fun expr -> strip_locations expr

let strings_of_prims expr =
  let rec convert = function
    | (Int _ | String _ | Bytes _) as expr -> expr
    | Prim (loc, prim, args, annot) ->
        let prim = string_of_prim prim in
        let args = List.map convert args in
        Prim (loc, prim, args, annot)
    | Seq (loc, args) ->
        let args = List.map convert args in
        Seq (loc, args)
  in
  strip_locations (convert (root expr))

let prim_encoding =
  let open Data_encoding in
  def "michelson.v1.primitives"
  @@ string_enum
       (* Add the comment below every 10 lines *)
       [
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("parameter", K_parameter);
         ("storage", K_storage);
         ("code", K_code);
         ("False", D_False);
         ("Elt", D_Elt);
         ("Left", D_Left);
         ("None", D_None);
         ("Pair", D_Pair);
         ("Right", D_Right);
         ("Some", D_Some);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("True", D_True);
         ("Unit", D_Unit);
         ("PACK", I_PACK);
         ("UNPACK", I_UNPACK);
         ("BLAKE2B", I_BLAKE2B);
         ("SHA256", I_SHA256);
         ("SHA512", I_SHA512);
         ("ABS", I_ABS);
         ("ADD", I_ADD);
         ("AMOUNT", I_AMOUNT);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("AND", I_AND);
         ("BALANCE", I_BALANCE);
         ("CAR", I_CAR);
         ("CDR", I_CDR);
         ("CHECK_SIGNATURE", I_CHECK_SIGNATURE);
         ("COMPARE", I_COMPARE);
         ("CONCAT", I_CONCAT);
         ("CONS", I_CONS);
         ("CREATE_ACCOUNT", I_CREATE_ACCOUNT);
         ("CREATE_CONTRACT", I_CREATE_CONTRACT);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("IMPLICIT_ACCOUNT", I_IMPLICIT_ACCOUNT);
         ("DIP", I_DIP);
         ("DROP", I_DROP);
         ("DUP", I_DUP);
         ("EDIV", I_EDIV);
         ("EMPTY_MAP", I_EMPTY_MAP);
         ("EMPTY_SET", I_EMPTY_SET);
         ("EQ", I_EQ);
         ("EXEC", I_EXEC);
         ("FAILWITH", I_FAILWITH);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("GE", I_GE);
         ("GET", I_GET);
         ("GT", I_GT);
         ("HASH_KEY", I_HASH_KEY);
         ("IF", I_IF);
         ("IF_CONS", I_IF_CONS);
         ("IF_LEFT", I_IF_LEFT);
         ("IF_NONE", I_IF_NONE);
         ("INT", I_INT);
         ("LAMBDA", I_LAMBDA);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("LE", I_LE);
         ("LEFT", I_LEFT);
         ("LOOP", I_LOOP);
         ("LSL", I_LSL);
         ("LSR", I_LSR);
         ("LT", I_LT);
         ("MAP", I_MAP);
         ("MEM", I_MEM);
         ("MUL", I_MUL);
         ("NEG", I_NEG);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("NEQ", I_NEQ);
         ("NIL", I_NIL);
         ("NONE", I_NONE);
         ("NOT", I_NOT);
         ("NOW", I_NOW);
         ("OR", I_OR);
         ("PAIR", I_PAIR);
         ("PUSH", I_PUSH);
         ("RIGHT", I_RIGHT);
         ("SIZE", I_SIZE);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("SOME", I_SOME);
         ("SOURCE", I_SOURCE);
         ("SENDER", I_SENDER);
         ("SELF", I_SELF);
         ("STEPS_TO_QUOTA", I_STEPS_TO_QUOTA);
         ("SUB", I_SUB);
         ("SWAP", I_SWAP);
         ("TRANSFER_TOKENS", I_TRANSFER_TOKENS);
         ("SET_DELEGATE", I_SET_DELEGATE);
         ("UNIT", I_UNIT);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("UPDATE", I_UPDATE);
         ("XOR", I_XOR);
         ("ITER", I_ITER);
         ("LOOP_LEFT", I_LOOP_LEFT);
         ("ADDRESS", I_ADDRESS);
         ("CONTRACT", I_CONTRACT);
         ("ISNAT", I_ISNAT);
         ("CAST", I_CAST);
         ("RENAME", I_RENAME);
         ("bool", T_bool);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("contract", T_contract);
         ("int", T_int);
         ("key", T_key);
         ("key_hash", T_key_hash);
         ("lambda", T_lambda);
         ("list", T_list);
         ("map", T_map);
         ("big_map", T_big_map);
         ("nat", T_nat);
         ("option", T_option);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("or", T_or);
         ("pair", T_pair);
         ("set", T_set);
         ("signature", T_signature);
         ("string", T_string);
         ("bytes", T_bytes);
         ("mutez", T_mutez);
         ("timestamp", T_timestamp);
         ("unit", T_unit);
         ("operation", T_operation);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("address", T_address);
         (* Alpha_002 addition *)
         ("SLICE", I_SLICE);
         (* Alpha_005 addition *)
         ("DIG", I_DIG);
         ("DUG", I_DUG);
         ("EMPTY_BIG_MAP", I_EMPTY_BIG_MAP);
         ("APPLY", I_APPLY);
         ("chain_id", T_chain_id);
         ("CHAIN_ID", I_CHAIN_ID);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         (* Alpha_008 addition *)
         ("LEVEL", I_LEVEL);
         ("SELF_ADDRESS", I_SELF_ADDRESS);
         ("never", T_never);
         ("NEVER", I_NEVER);
         ("UNPAIR", I_UNPAIR);
         ("VOTING_POWER", I_VOTING_POWER);
         ("TOTAL_VOTING_POWER", I_TOTAL_VOTING_POWER);
         ("KECCAK", I_KECCAK);
         ("SHA3", I_SHA3);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         (* Alpha_008 addition *)
         ("PAIRING_CHECK", I_PAIRING_CHECK);
         ("bls12_381_g1", T_bls12_381_g1);
         ("bls12_381_g2", T_bls12_381_g2);
         ("bls12_381_fr", T_bls12_381_fr);
         ("sapling_state", T_sapling_state);
         ("sapling_transaction_deprecated", T_sapling_transaction_deprecated);
         ("SAPLING_EMPTY_STATE", I_SAPLING_EMPTY_STATE);
         ("SAPLING_VERIFY_UPDATE", I_SAPLING_VERIFY_UPDATE);
         ("ticket", T_ticket);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         (* Alpha_008 addition *)
         ("TICKET_DEPRECATED", I_TICKET_DEPRECATED);
         ("READ_TICKET", I_READ_TICKET);
         ("SPLIT_TICKET", I_SPLIT_TICKET);
         ("JOIN_TICKETS", I_JOIN_TICKETS);
         ("GET_AND_UPDATE", I_GET_AND_UPDATE);
         (* Alpha_011 addition *)
         ("chest", T_chest);
         ("chest_key", T_chest_key);
         ("OPEN_CHEST", I_OPEN_CHEST);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         ("VIEW", I_VIEW);
         ("view", K_view);
         ("constant", H_constant);
         (* Alpha_012 addition *)
         ("SUB_MUTEZ", I_SUB_MUTEZ);
         (* Alpha_013 addition *)
         ("tx_rollup_l2_address", T_tx_rollup_l2_address);
         ("MIN_BLOCK_TIME", I_MIN_BLOCK_TIME);
         ("sapling_transaction", T_sapling_transaction);
         (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, FOR BACKWARD COMPATIBILITY OF THE ENCODING. *)
         (* Alpha_014 addition *)
         ("EMIT", I_EMIT);
         (* Alpha_015 addition *)
         ("Lambda_rec", D_Lambda_rec);
         ("LAMBDA_REC", I_LAMBDA_REC);
         ("TICKET", I_TICKET);
         ("BYTES", I_BYTES);
         ("NAT", I_NAT)
         (* New instructions must be added here, for backward compatibility of the encoding. *)
         (* Keep the comment above at the end of the list *);
       ]

let () =
  register_error_kind
    `Permanent
    ~id:"michelson_v1.unknown_primitive_name"
    ~title:"Unknown primitive name"
    ~description:"In a script or data expression, a primitive was unknown."
    ~pp:(fun ppf n -> Format.fprintf ppf "Unknown primitive %s." n)
    Data_encoding.(obj1 (req "wrong_primitive_name" @@ string Plain))
    (function Unknown_primitive_name got -> Some got | _ -> None)
    (fun got -> Unknown_primitive_name got) ;
  register_error_kind
    `Permanent
    ~id:"michelson_v1.invalid_primitive_name_case"
    ~title:"Invalid primitive name case"
    ~description:
      "In a script or data expression, a primitive name is neither uppercase, \
       lowercase or capitalized."
    ~pp:(fun ppf n -> Format.fprintf ppf "Primitive %s has invalid case." n)
    Data_encoding.(obj1 (req "wrong_primitive_name" @@ string Plain))
    (function Invalid_case name -> Some name | _ -> None)
    (fun name -> Invalid_case name) ;
  register_error_kind
    `Permanent
    ~id:"michelson_v1.invalid_primitive_name"
    ~title:"Invalid primitive name"
    ~description:
      "In a script or data expression, a primitive name is unknown or has a \
       wrong case."
    ~pp:(fun ppf _ -> Format.fprintf ppf "Invalid primitive.")
    Data_encoding.(
      obj2
        (req
           "expression"
           (Micheline.canonical_encoding ~variant:"generic" @@ string Plain))
        (req "location" Micheline.canonical_location_encoding))
    (function
      | Invalid_primitive_name (expr, loc) -> Some (expr, loc) | _ -> None)
    (fun (expr, loc) -> Invalid_primitive_name (expr, loc))

let string_of_namespace = function
  | Type_namespace -> "T"
  | Constant_namespace -> "D"
  | Instr_namespace -> "I"
  | Keyword_namespace -> "K"
  | Constant_hash_namespace -> "H"
