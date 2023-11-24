(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

let keccak256 (Hex s) =
  let bytes = Hex.to_bytes_exn (`Hex s) in
  Tezos_crypto.Hacl.Hash.Keccak_256.digest bytes

module Bits = struct
  (* Sets to 1 the [position] in [bytes] *)
  let set (bytes : bytes) position =
    let byte_index = Int.div position 8 in
    let bit_value = 1 lsl (7 - Int.rem position 8) in
    let original_byte = Bytes.get_uint8 bytes byte_index in
    let modified_byte = original_byte lor bit_value in
    Bytes.set_uint8 bytes byte_index modified_byte

  let get (bytes : bytes) position =
    let byte_index = Int.div position 8 in
    let original_byte = Bytes.get_uint8 bytes byte_index in
    original_byte land (1 lsl (7 - Int.rem position 8)) != 0
end

let positions input =
  let hash = keccak256 input in
  let pos_for_idx idx =
    let hash_bytes = Bytes.sub hash idx 2 in
    let bit_to_set = Int.logand (Bytes.get_uint16_be hash_bytes 0) 0x07FF in
    0x07FF - bit_to_set
  in
  List.map pos_for_idx [0; 2; 4]

type t = bytes

let make () = Bytes.make 256 '\000'

let is_empty = Bytes.equal (make ())

let contains_input ~input filter =
  List.for_all (Bits.get filter) (positions input)

let contains_bloom f1 f2 = Bytes.(equal (logor f1 f2) f1)

let accrue ~input filter = List.iter (Bits.set filter) (positions input)

let accrue_bloom f1 f2 =
  let union = Bytes.logor f1 f2 in
  Bytes.blit union 0 f1 0 (Bytes.length union)
