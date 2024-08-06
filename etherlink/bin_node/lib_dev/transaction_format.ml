(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** max size of a chunk *)
let size_per_chunk =
  Message_format.usable_size_in_message - 2 (* Index as u16 *)
  - (Ethereum_types.transaction_hash_size * 2)

(** Maximum size describes the maximum size of [tx_raw] to fit
   in a simple transaction. *)
let maximum_size =
  Message_format.usable_size_in_message - Ethereum_types.transaction_hash_size

let encode_transaction ~smart_rollup_address (kind, data) =
  Message_format.frame_message smart_rollup_address kind data

let chunk_transaction ~tx_hash_raw ~tx_raw =
  let open Result_syntax in
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
          ( Message_format.Transaction_chunk,
            tx_hash_raw ^ Ethereum_types.u16_to_bytes i ^ chunk_hash ^ chunk )
        in
        (all_chunk_hashes, chunk :: chunks))
      ("", [])
      chunks
  in
  let new_chunk_transaction =
    let number_of_chunks_bytes =
      Ethereum_types.u16_to_bytes @@ List.length chunks
    in
    ( Message_format.New_chunked_transaction,
      tx_hash_raw ^ number_of_chunks_bytes ^ all_chunk_hashes )
  in
  return (new_chunk_transaction :: chunks)

let make_evm_inbox_transactions ~tx_hash ~tx_raw =
  let open Result_syntax in
  let tx_hash_raw = Ethereum_types.hash_to_bytes tx_hash in
  if String.length tx_raw <= maximum_size then
    (* Simple transaction, fits in a single input. *)
    return [(Message_format.Simple_transaction, tx_hash_raw ^ tx_raw)]
  else chunk_transaction ~tx_hash_raw ~tx_raw

let make_encoded_messages ~smart_rollup_address tx_raw =
  let open Result_syntax in
  let tx_hash = Ethereum_types.hash_raw_tx tx_raw in
  let* messages = make_evm_inbox_transactions ~tx_hash ~tx_raw in
  let messages = List.map (encode_transaction ~smart_rollup_address) messages in
  return (tx_hash, messages)
