(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

(* U256 *)
let blueprint_number_size = 32

(* U16 *)
let nb_chunks_size = 2

(* U16 *)
let chunk_index_size = 2

let blueprint_tag_size = 1

(* ED25519  *)
let signature_size = 64

(* Tags added by RLP encoding for the sequencer blueprint.
    The sequencer blueprint follows the format:
     [ chunk, <- max size around 4kb, requires tag of 3 bytes
       number, <- 32 bytes, requires a tag of 1 byte
       nb_chunks, <- 2 bytes, requires a tag of 1 byte
       chunk_index <- 2 bytes, requires a tag of 1 byte
     ] <- outer list requires tag of 3 bytes.

   In total, the tags take 9 bytes. We use 16 to be safe.
*)
let rlp_tags_size = 16

let max_chunk_size =
  let open Transaction_format in
  (* max_input_size already considers the external tag *)
  max_input_size - framing_protocol_tag_size - smart_rollup_address_size
  - blueprint_tag_size - blueprint_number_size - nb_chunks_size
  - chunk_index_size - rlp_tags_size - signature_size

let encode_transaction hash raw =
  let open Rlp in
  List
    [
      Value (Bytes.of_string hash);
      List [Value (Bytes.of_string "\001"); Value (Bytes.of_string raw)];
    ]

let encode_delayed_transaction Delayed_transaction.{kind; hash; raw} =
  match kind with
  | Transaction -> encode_transaction (hash_to_bytes hash) raw
  | Deposit ->
      let open Rlp in
      let hash = hash_to_bytes hash in
      let rlp = decode_exn (Bytes.of_string raw) in
      List
        [
          Value (hash |> Bytes.of_string);
          List [Value (Bytes.of_string "\002"); rlp];
        ]

let make_blueprint_chunks ~timestamp ~transactions ~delayed_transactions
    ~parent_hash =
  let open Rlp in
  let delayed_hashes =
    List
      (List.map
         (fun tx ->
           Value
             (Delayed_transaction.hash tx |> hash_to_bytes |> Bytes.of_string))
         delayed_transactions)
  in
  let delayed_transactions =
    List.map encode_delayed_transaction delayed_transactions
  in
  let messages, full_messages =
    let m =
      List.map
        (fun transaction ->
          let tx_hash_str = Ethereum_types.hash_raw_tx transaction in
          encode_transaction tx_hash_str transaction)
        transactions
    in
    (List m, List (delayed_transactions @ m))
  in
  let timestamp = Value (Ethereum_types.timestamp_to_bytes timestamp) in
  let parent_hash =
    Value (block_hash_to_bytes parent_hash |> Bytes.of_string)
  in
  let to_publish =
    List [parent_hash; delayed_hashes; messages; timestamp] |> encode
  in
  let to_execute =
    List [parent_hash; List []; full_messages; timestamp] |> encode
  in
  match
    ( String.chunk_bytes max_chunk_size to_publish,
      String.chunk_bytes max_chunk_size to_execute )
  with
  | Ok to_publish, Ok to_execute -> (to_publish, to_execute)
  | _ ->
      (* [chunk_bytes] can only return an [Error] if the optional
         argument [error_on_partial_chunk] is passed. As this is not
         the case in this call, this branch is impossible. *)
      assert false

let encode_u16_le i =
  let bytes = Bytes.make 2 '\000' in
  Bytes.set_uint16_le bytes 0 i ;
  bytes

type t = {
  to_publish : Blueprint_types.payload;
  to_execute : Blueprint_types.payload;
}

let create ~cctxt ~sequencer_key ~timestamp ~smart_rollup_address ~number
    ~parent_hash ~delayed_transactions ~transactions =
  let open Lwt_result_syntax in
  let open Rlp in
  let number = Value (encode_u256_le number) in
  let to_publish_chunks, to_execute_chunks =
    make_blueprint_chunks
      ~timestamp
      ~transactions
      ~delayed_transactions
      ~parent_hash
  in
  let nb_chunks_publish =
    Rlp.Value (encode_u16_le @@ List.length to_publish_chunks)
  in
  let nb_chunks_execute =
    Rlp.Value (encode_u16_le @@ List.length to_execute_chunks)
  in
  let message_from_chunk nb_chunks chunk_index chunk =
    let chunk_index = Rlp.Value (encode_u16_le chunk_index) in
    let value = Value (Bytes.of_string chunk) in
    (* Takes the blueprints fields and sign them. *)
    let rlp_unsigned_blueprint =
      List [value; number; nb_chunks; chunk_index] |> encode
    in
    let* signature =
      Client_keys.sign cctxt sequencer_key rlp_unsigned_blueprint
    in
    let signature_bytes = Signature.to_bytes signature in
    (* Encode the blueprints fields and its signature. *)
    let rlp_sequencer_blueprint =
      List [value; number; nb_chunks; chunk_index; Value signature_bytes]
      |> encode |> Bytes.to_string
    in
    `External
      ("\000" (* Framed protocol *) ^ smart_rollup_address
      ^ "\003"
      ^ (* Sequencer blueprint *)
      rlp_sequencer_blueprint)
    |> return
  in
  let* to_publish =
    List.mapi_ep (message_from_chunk nb_chunks_publish) to_publish_chunks
  in
  let* to_execute =
    List.mapi_ep (message_from_chunk nb_chunks_execute) to_execute_chunks
  in
  return {to_publish; to_execute}
