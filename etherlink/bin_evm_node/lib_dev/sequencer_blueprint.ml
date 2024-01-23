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

let make_blueprint_chunks ~timestamp ~transactions =
  let open Rlp in
  let messages =
    List
      (List.map
         (fun transaction ->
           let tx_hash_str = Ethereum_types.hash_raw_tx transaction in
           List
             [
               Value (Bytes.of_string tx_hash_str);
               List
                 [
                   Value (Bytes.of_string "\001");
                   Value (Bytes.of_string transaction);
                 ];
             ])
         transactions)
  in
  let timestamp = Value (Helpers.timestamp_to_bytes timestamp) in
  (* To be added in a future MR *)
  let delayed_transaction_hashes = List [] in
  let blob = List [delayed_transaction_hashes; messages; timestamp] |> encode in
  match String.chunk_bytes max_chunk_size blob with
  | Ok chunks -> chunks
  | Error _ ->
      (* [chunk_bytes] can only return an [Error] if the optional
         argument [error_on_partial_chunk] is passed. As this is not
         the case in this call, this branch is impossible. *)
      assert false

let encode_u16_le i =
  let bytes = Bytes.make 2 '\000' in
  Bytes.set_uint16_le bytes 0 i ;
  bytes

let create ~secret_key ~timestamp ~smart_rollup_address ~number ~transactions =
  let open Rlp in
  let number = Value (encode_u256_le number) in
  let chunks = make_blueprint_chunks ~timestamp ~transactions in
  let nb_chunks = Value (encode_u16_le @@ List.length chunks) in
  let message_from_chunk chunk_index chunk =
    let chunk_index = Value (encode_u16_le chunk_index) in
    let value = Value (Bytes.of_string chunk) in

    (* Takes the blueprints fields and sign them. *)
    let rlp_unsigned_blueprint =
      List [value; number; nb_chunks; chunk_index] |> encode
    in
    let signature =
      Signature.(sign secret_key rlp_unsigned_blueprint |> to_bytes)
    in

    (* Encode the blueprints fields and its signature. *)
    let rlp_sequencer_blueprint =
      List [value; number; nb_chunks; chunk_index; Value signature]
      |> encode |> Bytes.to_string
    in

    `External
      ("\000" (* Framed protocol *) ^ smart_rollup_address
      ^ "\003"
      ^ (* Sequencer blueprint *)
      rlp_sequencer_blueprint)
  in
  List.mapi message_from_chunk chunks
