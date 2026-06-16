(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type hashes = {evm : Ethereum_types.block_hash; michelson : Block_hash.t option}

type t = {level : Ethereum_types.quantity; hashes : hashes}

let hashes_encoding =
  let open Data_encoding in
  conv
    (fun {evm; michelson} -> (evm, michelson))
    (fun (evm, michelson) -> {evm; michelson})
  @@ obj2
       (req "evm_block_hash" Ethereum_types.block_hash_encoding)
       (opt "michelson_block_hash" Block_hash.encoding)

let encoding =
  let open Data_encoding in
  conv
    (fun {level; hashes} -> (level, hashes))
    (fun (level, hashes) -> {level; hashes})
  @@ obj2
       (req "level" Ethereum_types.quantity_encoding)
       (req "block_hashes" hashes_encoding)

type block_hash_identifier =
  | Evm of Ethereum_types.block_hash
  | Michelson of Block_hash.t

let block_hash_identifier_encoding =
  let open Data_encoding in
  conv
    (function
      | Evm (Ethereum_types.Block_hash h) -> Ethereum_types.hex_to_string h
      | Michelson hash -> Block_hash.to_b58check hash)
    (fun s ->
      if String.starts_with ~prefix:"0x" s then
        Evm (Ethereum_types.Block_hash (Ethereum_types.hex_of_string s))
      else Michelson (Block_hash.of_b58check_exn s))
    string
