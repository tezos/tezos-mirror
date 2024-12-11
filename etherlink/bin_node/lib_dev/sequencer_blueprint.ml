(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

(* U256 *)
let blueprint_number_size = 32

(* U16 *)
let nb_chunks_size = 2

(* U16 *)
let chunk_index_size = 2

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
  Message_format.usable_size_in_message - blueprint_number_size - nb_chunks_size
  - chunk_index_size - rlp_tags_size - signature_size

let maximum_usable_space_in_blueprint chunks_count =
  chunks_count * max_chunk_size

let maximum_chunks_per_l1_level = 512 * 1024 / 4096

let encode_transaction raw =
  let open Rlp in
  Value (Bytes.of_string raw)

type kernel_blueprint = {
  parent_hash : block_hash;
  delayed_transactions : hash list;
  transactions : string list;
  timestamp : Time.Protocol.t;
}

let kernel_blueprint_to_rlp
    {parent_hash; delayed_transactions; transactions; timestamp} =
  let open Rlp in
  let delayed_transactions =
    List
      (List.map
         (fun hash -> Value (hash_to_bytes hash |> Bytes.of_string))
         delayed_transactions)
  in
  let messages =
    let m = List.map encode_transaction transactions in
    List m
  in
  let timestamp = Value (Ethereum_types.timestamp_to_bytes timestamp) in
  let parent_hash =
    Value (block_hash_to_bytes parent_hash |> Bytes.of_string)
  in
  List [parent_hash; delayed_transactions; messages; timestamp]

let kernel_blueprint_parent_hash_of_rlp s =
  match Rlp.decode s with
  | Ok
      Rlp.(
        List [Value parent_hash; _delayed_transactions; _messages; _timestamp])
    ->
      Some (decode_block_hash parent_hash)
  | _ -> None

let make_blueprint_chunks kernel_blueprint =
  let blueprint = Rlp.encode @@ kernel_blueprint_to_rlp kernel_blueprint in
  match String.chunk_bytes max_chunk_size blueprint with
  | Ok chunks -> chunks
  | _ ->
      (* [chunk_bytes] can only return an [Error] if the optional
         argument [error_on_partial_chunk] is passed. As this is not
         the case in this call, this branch is impossible. *)
      assert false

let encode_u16_le i =
  let bytes = Bytes.make 2 '\000' in
  Bytes.set_uint16_le bytes 0 i ;
  bytes

let decode_u16_le bytes = Bytes.get_uint16_le bytes 0

type unsigned_chunk =
  | Unsigned_chunk of {
      value : bytes;
      number : quantity;
      nb_chunks : int;
      chunk_index : int;
    }

type chunk = {unsigned_chunk : unsigned_chunk; signature : Signature.t}

type t = chunk

let chunk_encoding =
  Data_encoding.(
    let bytes_hex = bytes' Hex in
    conv
      (fun {
             unsigned_chunk =
               Unsigned_chunk {value; number; nb_chunks; chunk_index};
             signature;
           } -> (value, number, nb_chunks, chunk_index, signature))
      (fun (value, number, nb_chunks, chunk_index, signature) ->
        {
          unsigned_chunk = Unsigned_chunk {value; number; nb_chunks; chunk_index};
          signature;
        })
      (obj5
         (req "value" bytes_hex)
         (req "number" quantity_encoding)
         (req "nb_chunks" int31)
         (req "chunk_index" int31)
         (req "signature" Signature.encoding)))

let unsigned_chunk_to_rlp
    (Unsigned_chunk {value; number; nb_chunks; chunk_index}) =
  Rlp.(
    List
      [
        Value value;
        Value (encode_u256_le number);
        Value (encode_u16_le nb_chunks);
        Value (encode_u16_le chunk_index);
      ])

let chunk_to_rlp
    {
      unsigned_chunk = Unsigned_chunk {value; number; nb_chunks; chunk_index};
      signature;
    } =
  Rlp.(
    List
      [
        Value value;
        Value (encode_u256_le number);
        Value (encode_u16_le nb_chunks);
        Value (encode_u16_le chunk_index);
        Value (Signature.to_bytes signature);
      ])

let chunk_of_rlp s =
  match Rlp.decode s with
  | Ok
      Rlp.(
        List
          [
            Value value;
            Value number;
            Value nb_chunks;
            Value chunk_index;
            Value signature;
          ]) ->
      let number = decode_number_le number in
      let nb_chunks = decode_u16_le nb_chunks in
      let chunk_index = decode_u16_le chunk_index in
      Option.map
        (fun signature ->
          {
            unsigned_chunk =
              Unsigned_chunk {value; number; nb_chunks; chunk_index};
            signature;
          })
        (Signature.of_bytes_opt signature)
  | _ -> None

let prepare ~cctxt ~sequencer_key ~timestamp ~number ~parent_hash
    ~(delayed_transactions : Ethereum_types.hash list) ~transactions =
  let open Lwt_result_syntax in
  let open Rlp in
  let chunks =
    make_blueprint_chunks
      {parent_hash; delayed_transactions; transactions; timestamp}
  in
  let nb_chunks = List.length chunks in
  let message_from_chunk nb_chunks chunk_index chunk =
    let value = Bytes.of_string chunk in
    let unsigned_chunk =
      Unsigned_chunk {value; number; nb_chunks; chunk_index}
    in
    (* Takes the blueprints fields and sign them. *)
    let rlp_unsigned_blueprint =
      unsigned_chunk_to_rlp unsigned_chunk |> encode
    in
    let+ signature =
      Client_keys.sign cctxt sequencer_key rlp_unsigned_blueprint
    in
    {unsigned_chunk; signature}
  in
  List.mapi_ep (message_from_chunk nb_chunks) chunks

let prepare_message smart_rollup_address kind rlp =
  let rlp_sequencer_blueprint = rlp |> Rlp.encode |> Bytes.to_string in
  `External
    Message_format.(
      frame_message smart_rollup_address kind rlp_sequencer_blueprint)

let create_inbox_payload ~smart_rollup_address ~chunks : Blueprint_types.payload
    =
  List.map
    (fun chunk ->
      chunk_to_rlp chunk
      |> prepare_message smart_rollup_address Message_format.Blueprint_chunk)
    chunks

let decode_inbox_payload sequencer (payload : Blueprint_types.payload) =
  List.filter_map
    (fun (`External chunk) ->
      let open Option_syntax in
      let len = String.length chunk in
      if len <= Message_format.header_size then fail
      else
        let chunk_bytes =
          String.(
            sub
              chunk
              Message_format.header_size
              (length chunk - Message_format.header_size))
        in
        let* chunk = chunk_of_rlp (Bytes.unsafe_of_string chunk_bytes) in
        let unsigned_chunk_bytes =
          Rlp.encode (unsigned_chunk_to_rlp chunk.unsigned_chunk)
        in
        let correctly_signed =
          Signature.check sequencer chunk.signature unsigned_chunk_bytes
        in
        if correctly_signed then return chunk else fail)
    payload
  |> List.sort
       (fun
         {unsigned_chunk = Unsigned_chunk {chunk_index = x; _}; _}
         {unsigned_chunk = Unsigned_chunk {chunk_index = y; _}; _}
       -> compare x y)

let create_dal_payloads chunks =
  List.map
    (fun {unsigned_chunk; signature = _} ->
      unsigned_chunk_to_rlp unsigned_chunk
      |> Rlp.encode |> Bytes.to_string
      |> Message_format.frame_dal_message Blueprint_chunk)
    chunks

let kernel_blueprint_parent_hash_of_payload sequencer payload =
  let chunks = decode_inbox_payload sequencer payload in
  let bytes =
    List.fold_left
      (fun buffer {unsigned_chunk = Unsigned_chunk {value; _}; _} ->
        Bytes.cat buffer value)
      Bytes.empty
      chunks
  in
  kernel_blueprint_parent_hash_of_rlp bytes
