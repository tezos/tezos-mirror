(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

type error += (* `Permanent *) Unknown_primitive_name of string

type error += (* `Permanent *) Invalid_case of string

type error +=
  | (* `Permanent *)
      Invalid_primitive_name of
      string Micheline.canonical * Micheline.canonical_location

(** Types of nodes in Michelson's AST. They fall into 4 categories:
    - types (prefixed with [T_]);
    - constants (prefixed with [D_]);
    - instructions (prefixed with [I_]);
    - keywords (prefixed with [K_]).

    Recall that Micheline is essentially just S-expressions with
    a few extra atom types for strings and numbers. This variant
    represents the values the [Prim] atoms in the Michelson subset
    of Micheline. Other types (such as ['a Micheline.canonical]) are
    frequently parameterized by this type. This gives us a strongly-typed
    subset of Micheline while keeping the set of primitives independent
    from the definition of Micheline for easier changes.
*)
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
  | D_Ticket
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
  | I_IS_IMPLICIT_ACCOUNT
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
  | I_INDEX_ADDRESS
  | I_GET_ADDRESS_INDEX
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
  (* See the interface of [Global_constants_storage]. *)
  | H_constant

(** Auxiliary types for error documentation.
    All the prim constructor prefixes must match their namespace. *)

type namespace =
  | (* prefix "T" *) Type_namespace
  | (* prefix "D" *) Constant_namespace
  | (* prefix "I" *) Instr_namespace
  | (* prefix "K" *) Keyword_namespace
  (* The Constant Hash namespace is a singleton reserved
     for the constant keyword. Unlike other primitives,
     constants have no representation in the typed IR,
     being fully expanded away before typechecking. *)
  | (* prefix "H" *) Constant_hash_namespace

val namespace : prim -> namespace

val prim_encoding : prim Data_encoding.encoding

val string_of_prim : prim -> string

val prim_of_string : string -> prim tzresult

val prims_of_strings :
  string Micheline.canonical -> prim Micheline.canonical tzresult

val strings_of_prims : prim Micheline.canonical -> string Micheline.canonical

(** The string corresponds to the constructor prefix from the given namespace
    (i.e. "T", "D", "I" or "K") *)
val string_of_namespace : namespace -> string
