(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** A Tezos X meta-block groups together the per-runtime block hashes that
    share the same L2 level. *)

(** Per-runtime block hashes.

    [michelson] is [None] when the Michelson runtime has not been activated
    yet. Michelson block hashes use the regular Tezos b58check encoding, while
    EVM block hashes use the Ethereum hexadecimal encoding. *)
type hashes = {evm : Ethereum_types.block_hash; michelson : Block_hash.t option}

type t = {level : Ethereum_types.quantity; hashes : hashes}

val hashes_encoding : hashes Data_encoding.t

val encoding : t Data_encoding.t

(** A block hash identifying a meta-block: either an EVM hash (hex) or a
    Michelson hash (b58check). The JSON encoding accepts a string and
    dispatches based on its format. *)
type block_hash_identifier =
  | Evm of Ethereum_types.block_hash
  | Michelson of Block_hash.t

val block_hash_identifier_encoding : block_hash_identifier Data_encoding.t
