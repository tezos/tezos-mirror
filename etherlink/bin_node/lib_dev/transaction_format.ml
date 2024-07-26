(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* The hard limit is 4096 but it needs to add the external message tag. *)
let max_input_size = 4095

let smart_rollup_address_size = 20

let transaction_tag_size = 1

let framing_protocol_tag_size = 1

type transaction =
  | Simple of string
  | NewChunked of (string * int * string)
  | Chunk of string

let encode_transaction ~smart_rollup_address kind =
  let data =
    match kind with
    | Simple data -> "\000" ^ data
    | NewChunked (tx_hash, len, all_chunk_hashes) ->
        let number_of_chunks_bytes = Ethereum_types.u16_to_bytes len in
        "\001" ^ tx_hash ^ number_of_chunks_bytes ^ all_chunk_hashes
    | Chunk data -> "\002" ^ data
  in
  "\000" ^ smart_rollup_address ^ data

let chunk_transaction ~tx_hash_raw ~tx_raw =
  let open Result_syntax in
  let size_per_chunk =
    max_input_size - framing_protocol_tag_size - smart_rollup_address_size
    - transaction_tag_size - 2 (* Index as u16 *)
    - (Ethereum_types.transaction_hash_size * 2)
  in
  let* chunks = String.chunk_bytes size_per_chunk (Bytes.of_string tx_raw) in
  let all_chunk_hashes, chunks =
    List.fold_left_i
      (fun i (all_chunk_hashes, chunks) chunk ->
        let chunk_hash =
          Tezos_crypto.Hacl.Hash.Keccak_256.digest (String.to_bytes chunk)
          |> Bytes.to_string
        in
        let all_chunk_hashes = all_chunk_hashes ^ chunk_hash in
        let chunk =
          Chunk
            (tx_hash_raw ^ Ethereum_types.u16_to_bytes i ^ chunk_hash ^ chunk)
        in
        (all_chunk_hashes, chunk :: chunks))
      ("", [])
      chunks
  in
  let new_chunk_transaction =
    NewChunked (tx_hash_raw, List.length chunks, all_chunk_hashes)
  in
  return (new_chunk_transaction :: chunks)

let make_evm_inbox_transactions tx_raw =
  let open Result_syntax in
  (* Maximum size describes the maximum size of [tx_raw] to fit
     in a simple transaction. *)
  let maximum_size =
    max_input_size - framing_protocol_tag_size - smart_rollup_address_size
    - transaction_tag_size - Ethereum_types.transaction_hash_size
  in
  let tx_hash = Ethereum_types.hash_raw_tx tx_raw in
  let tx_hash_raw = Ethereum_types.hash_to_bytes tx_hash in
  if String.length tx_raw <= maximum_size then
    (* Simple transaction, fits in a single input. *)
    let tx = Simple (tx_hash_raw ^ tx_raw) in
    return (tx_hash, [tx])
  else
    let* chunked = chunk_transaction ~tx_hash_raw ~tx_raw in
    return (tx_hash, chunked)

(** [make_encoded_messages ~smart_rollup_address raw_tx] returns the
    hash of the transaction, and a list of transactions to include in the inbox.
    - [smart_rollup_address] is encoded on 20 bytes
    - [raw_tx] is an ethereum transaction in hex format (without the 0x prefix).

    All messages go through the same encoding, but will only be chunked if
    necessary. *)
let make_encoded_messages ~smart_rollup_address tx_raw =
  let open Result_syntax in
  let* tx_hash, messages = make_evm_inbox_transactions tx_raw in
  let messages = List.map (encode_transaction ~smart_rollup_address) messages in
  return (tx_hash, messages)
