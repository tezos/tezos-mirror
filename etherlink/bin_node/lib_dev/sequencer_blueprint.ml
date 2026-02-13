(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type error +=
  | Not_a_blueprint
  | Bad_chunk_index of {expected : int; actual : int}
  | Bad_nb_chunks of {expected : int; actual : int; chunk_index : int}

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_not_a_blueprint"
    ~title:"Not a blueprint"
    ~description:"Tried to decode a payload that is not a valid blueprint"
    Data_encoding.empty
    (function Not_a_blueprint -> Some () | _ -> None)
    (fun () -> Not_a_blueprint) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_bad_chunk_index"
    ~title:"Bad chunk index"
    ~description:
      "When decoding a blueprint chunk, got an unexpected value for the \
       chunk_index field"
    ~pp:(fun ppf (expected, actual) ->
      Format.fprintf
        ppf
        "When decoding a blueprint chunk, got an unexpected value for the \
         chunk_index field, expected %d, got %d"
        expected
        actual)
    Data_encoding.(obj2 (req "expected" int31) (req "actual" int31))
    (function
      | Bad_chunk_index {expected; actual} -> Some (expected, actual)
      | _ -> None)
    (fun (expected, actual) -> Bad_chunk_index {expected; actual}) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_bad_nb_chunks"
    ~title:"Bad nb_chunks field"
    ~description:
      "When decoding a blueprint chunk, got an unexpected value for the \
       nb_chunks field"
    ~pp:(fun ppf (expected, actual, chunk_index) ->
      Format.fprintf
        ppf
        "When decoding a blueprint chunk, got an unexpected value for the \
         nb_chunks field for the chunk at index %d, expected %d, got %d"
        chunk_index
        expected
        actual)
    Data_encoding.(
      obj3 (req "expected" int31) (req "actual" int31) (req "chunk_index" int31))
    (function
      | Bad_nb_chunks {expected; actual; chunk_index} ->
          Some (expected, actual, chunk_index)
      | _ -> None)
    (fun (expected, actual, chunk_index) ->
      Bad_nb_chunks {expected; actual; chunk_index})

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

type blueprint_version = Legacy | V1

let evm_runtime_id = "\001"

let encode_transaction ~version raw =
  let open Rlp in
  match version with
  | Legacy -> Value (Bytes.of_string raw)
  | V1 -> Value (Bytes.of_string (evm_runtime_id ^ raw))

type kernel_blueprint = {
  version : blueprint_version;
  parent_hash : block_hash;
  delayed_transactions : hash list;
  transactions : string list;
  timestamp : Time.Protocol.t;
}

let kernel_blueprint_to_rlp
    {version; parent_hash; delayed_transactions; transactions; timestamp} =
  let open Rlp in
  let delayed_transactions =
    List
      (List.map
         (fun hash -> Value (hash_to_bytes hash |> Bytes.of_string))
         delayed_transactions)
  in
  let messages =
    let m = List.map (encode_transaction ~version) transactions in
    List m
  in
  let timestamp = Value (Ethereum_types.timestamp_to_bytes timestamp) in
  let parent_hash =
    Value (block_hash_to_bytes parent_hash |> Bytes.of_string)
  in
  let unversioned_blueprint =
    [parent_hash; delayed_transactions; messages; timestamp]
  in
  match version with
  | Legacy -> List unversioned_blueprint
  | V1 ->
      let version_char = Char.chr 1 in
      List (Value (Bytes.make 1 version_char) :: unversioned_blueprint)

let kernel_blueprint_parent_hash_of_rlp s =
  match Rlp.decode s with
  | Ok
      Rlp.(
        List [Value parent_hash; _delayed_transactions; _messages; _timestamp])
    ->
      Some (decode_block_hash parent_hash)
  | Ok
      Rlp.(
        List
          [
            _version;
            Value parent_hash;
            _delayed_transactions;
            _messages;
            _timestamp;
          ]) ->
      Some (decode_block_hash parent_hash)
  | _ -> None

let encode_u16_le i =
  let bytes = Bytes.make 2 '\000' in
  Bytes.set_uint16_le bytes 0 i ;
  bytes

let decode_u16_le bytes = Bytes.get_uint16_le bytes 0

type unsigned_chunk = {
  value : bytes;
  number : quantity;
  nb_chunks : int;
  chunk_index : int;
}

(* Invariants:
   - the nb_chunks field of each unsigned chunk is the length of the list,
   - the chunk_index field of the ith chunk of the list is i.
*)
type unsigned_chunked_blueprint = unsigned_chunk list

type t = {unsigned_chunk : unsigned_chunk; signature : Signature.t}

type chunked_blueprint = t list

(* We have the invariant that the nb_chunks field is the same for all
   chunks and is the length of the list. This lets us compute the
   length of the list in constant time. *)
let nb_chunks = function
  | [] -> 0
  | {unsigned_chunk = {nb_chunks; _}; _} :: _ -> nb_chunks

let unsigned_chunk_encoding =
  Data_encoding.(
    let bytes_hex = bytes' Hex in
    conv
      (fun {value; number; nb_chunks; chunk_index} ->
        (value, number, nb_chunks, chunk_index))
      (fun (value, number, nb_chunks, chunk_index) ->
        {value; number; nb_chunks; chunk_index})
      (obj4
         (req "value" bytes_hex)
         (req "number" quantity_encoding)
         (req "nb_chunks" int31)
         (req "chunk_index" int31)))

let unsigned_chunked_blueprint_encoding =
  Data_encoding.list unsigned_chunk_encoding

let chunk_encoding =
  Data_encoding.(
    conv
      (fun {unsigned_chunk; signature} -> (unsigned_chunk, signature))
      (fun (unsigned_chunk, signature) -> {unsigned_chunk; signature})
      (merge_objs
         unsigned_chunk_encoding
         (obj1 (req "signature" Signature.encoding))))

let chunked_blueprint_encoding =
  Data_encoding.(list (dynamic_size chunk_encoding))

let unsigned_chunk_to_rlp {value; number; nb_chunks; chunk_index} =
  Rlp.(
    List
      [
        Value value;
        Value (encode_u256_le number);
        Value (encode_u16_le nb_chunks);
        Value (encode_u16_le chunk_index);
      ])

let chunk_to_rlp
    {unsigned_chunk = {value; number; nb_chunks; chunk_index}; signature} =
  Rlp.(
    List
      [
        Value value;
        Value (encode_u256_le number);
        Value (encode_u16_le nb_chunks);
        Value (encode_u16_le chunk_index);
        Value (Signature.to_bytes signature);
      ])

let chunk_of_rlp ~expected_chunk_index ~expected_nb_chunks s =
  let open Result_syntax in
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
      let chunk_index = decode_u16_le chunk_index in
      let* () =
        if chunk_index <> expected_chunk_index then
          tzfail
          @@ Bad_chunk_index
               {expected = expected_chunk_index; actual = chunk_index}
        else return_unit
      in
      let nb_chunks = decode_u16_le nb_chunks in
      let* () =
        if nb_chunks <> expected_nb_chunks then
          tzfail
          @@ Bad_nb_chunks
               {expected = expected_nb_chunks; actual = nb_chunks; chunk_index}
        else return_unit
      in
      let* signature =
        match Signature.of_bytes_opt signature with
        | Some signature -> return signature
        | None -> tzfail Not_a_blueprint
      in
      return
        {unsigned_chunk = {value; number; nb_chunks; chunk_index}; signature}
  | _ -> tzfail Not_a_blueprint

let chunk_of_external_message ~expected_chunk_index ~expected_nb_chunks
    (`External chunk) =
  let open Result_syntax in
  let len = String.length chunk in
  if len <= Message_format.header_size then tzfail Not_a_blueprint
  else
    let chunk_bytes =
      String.(
        sub
          chunk
          Message_format.header_size
          (length chunk - Message_format.header_size))
    in
    chunk_of_rlp
      ~expected_chunk_index
      ~expected_nb_chunks
      (Bytes.unsafe_of_string chunk_bytes)

let chunks_of_external_messages payload =
  let expected_nb_chunks = List.length payload in
  List.mapi_e
    (fun expected_chunk_index chunk ->
      chunk_of_external_message ~expected_nb_chunks ~expected_chunk_index chunk)
    payload

let to_rlp payload =
  let open Result_syntax in
  let expected_nb_chunks = List.length payload in
  let* bytes =
    List.mapi_e
      (fun expected_chunk_index chunk ->
        let+ chunk =
          chunk_of_external_message
            ~expected_nb_chunks
            ~expected_chunk_index
            chunk
        in
        chunk.unsigned_chunk.value)
      payload
  in
  Rlp.decode Bytes.(concat empty bytes)

let make_blueprint_chunks ~number kernel_blueprint =
  let blueprint = Rlp.encode @@ kernel_blueprint_to_rlp kernel_blueprint in
  match String.chunk_bytes max_chunk_size blueprint with
  | Ok chunks ->
      let nb_chunks = List.length chunks in
      List.mapi
        (fun chunk_index chunk ->
          let value = Bytes.of_string chunk in
          {value; number; nb_chunks; chunk_index})
        chunks
  | _ ->
      (* [chunk_bytes] can only return an [Error] if the optional
         argument [error_on_partial_chunk] is passed. As this is not
         the case in this call, this branch is impossible. *)
      assert false

let sign ~signer ~chunks =
  let open Lwt_result_syntax in
  let open Rlp in
  let message_from_chunk unsigned_chunk =
    (* Takes the blueprints fields and sign them. *)
    let rlp_unsigned_blueprint =
      unsigned_chunk_to_rlp unsigned_chunk |> encode
    in
    let+ signature = Signer.sign signer rlp_unsigned_blueprint in
    {unsigned_chunk; signature}
  in
  List.map_ep message_from_chunk chunks

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

let unsafe_drop_signature chunk = chunk.unsigned_chunk

let unsafe_drop_signatures l = List.map unsafe_drop_signature l

let check_signature_opt sequencer chunk =
  let unsigned_chunk_bytes =
    Rlp.encode (unsigned_chunk_to_rlp chunk.unsigned_chunk)
  in
  let correctly_signed =
    Signature.check sequencer chunk.signature unsigned_chunk_bytes
  in
  if correctly_signed then Some chunk.unsigned_chunk else None

let check_signature sequencer chunk =
  let open Result_syntax in
  match check_signature_opt sequencer chunk with
  | Some chunk -> return chunk
  | None ->
      error_with
        "Signature check failed for the provided blueprint with public key %a"
        Signature.Public_key.pp
        sequencer

let check_signatures signer l = List.map_e (check_signature signer) l

let decode_inbox_payload sequencer (payload : Blueprint_types.payload) =
  let open Result_syntax in
  let expected_nb_chunks = List.length payload in
  List.mapi_e
    (fun expected_chunk_index chunk ->
      let* chunk =
        chunk_of_external_message
          ~expected_nb_chunks
          ~expected_chunk_index
          chunk
      in
      check_signature sequencer chunk)
    payload

let create_dal_payloads chunks =
  List.map
    (fun {unsigned_chunk; signature = _} ->
      unsigned_chunk_to_rlp unsigned_chunk
      |> Rlp.encode |> Bytes.to_string
      |> Message_format.frame_dal_message Blueprint_chunk)
    chunks

let kernel_blueprint_parent_hash_of_payload sequencer payload =
  let open Result_syntax in
  let* chunks = decode_inbox_payload sequencer payload in
  let bytes =
    List.fold_left
      (fun buffer {value; _} -> Bytes.cat buffer value)
      Bytes.empty
      chunks
  in
  return (kernel_blueprint_parent_hash_of_rlp bytes)
