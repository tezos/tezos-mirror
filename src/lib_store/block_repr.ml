(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Store_errors

type contents = {
  header : Block_header.t;
  operations : Operation.t list list;
  block_metadata_hash : Block_metadata_hash.t option;
  operations_metadata_hashes : Operation_metadata_hash.t list list option;
}

type metadata = {
  message : string option;
  max_operations_ttl : int;
  last_allowed_fork_level : Int32.t;
  block_metadata : Bytes.t;
  operations_metadata : Block_validation.operation_metadata list list;
}

type legacy_metadata = {
  legacy_message : string option;
  legacy_max_operations_ttl : int;
  legacy_last_allowed_fork_level : Int32.t;
  legacy_block_metadata : Bytes.t;
  legacy_operations_metadata : Bytes.t list list;
}

type block = {
  hash : Block_hash.t;
  contents : contents;
  mutable metadata : metadata option;
      (* allows updating metadata field when loading cemented metadata *)
}

type legacy_block = {
  legacy_hash : Block_hash.t;
  legacy_contents : contents;
  mutable legacy_metadata : legacy_metadata option;
      (* allows updating metadata field when loading cemented metadata *)
}

type t = block

let create_genesis_block ~genesis context =
  let shell : Block_header.shell_header =
    {
      level = 0l;
      proto_level = 0;
      predecessor = genesis.Genesis.block;
      (* genesis' predecessor is genesis *)
      timestamp = genesis.Genesis.time;
      fitness = [];
      validation_passes = 0;
      operations_hash = Operation_list_list_hash.empty;
      context;
    }
  in
  let header : Block_header.t = {shell; protocol_data = Bytes.create 0} in
  let contents =
    {
      header;
      operations = [];
      block_metadata_hash = None;
      operations_metadata_hashes = None;
    }
  in
  let metadata =
    Some
      {
        message = Some "Genesis";
        max_operations_ttl = 0;
        last_allowed_fork_level = 0l;
        block_metadata = Bytes.create 0;
        operations_metadata = [];
      }
  in
  {hash = genesis.block; contents; metadata}

let contents_encoding =
  let open Data_encoding in
  conv
    (fun {header; operations; block_metadata_hash; operations_metadata_hashes} ->
      (header, operations, block_metadata_hash, operations_metadata_hashes))
    (fun (header, operations, block_metadata_hash, operations_metadata_hashes) ->
      {header; operations; block_metadata_hash; operations_metadata_hashes})
    (obj4
       (req "header" (dynamic_size Block_header.encoding))
       (req "operations" (list (list (dynamic_size Operation.encoding))))
       (opt "block_metadata_hash" Block_metadata_hash.encoding)
       (opt
          "operations_metadata_hashes"
          (list (list Operation_metadata_hash.encoding))))

let metadata_encoding : metadata Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           message;
           max_operations_ttl;
           last_allowed_fork_level;
           block_metadata;
           operations_metadata;
         } ->
      ( message,
        max_operations_ttl,
        last_allowed_fork_level,
        block_metadata,
        operations_metadata ))
    (fun ( message,
           max_operations_ttl,
           last_allowed_fork_level,
           block_metadata,
           operations_metadata ) ->
      {
        message;
        max_operations_ttl;
        last_allowed_fork_level;
        block_metadata;
        operations_metadata;
      })
    (obj5
       (opt "message" string)
       (req "max_operations_ttl" uint16)
       (req "last_allowed_fork_level" int32)
       (req "block_metadata" bytes)
       (req
          "operations_metadata"
          (list (list Block_validation.operation_metadata_encoding))))

let legacy_metadata_encoding : legacy_metadata Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           legacy_message;
           legacy_max_operations_ttl;
           legacy_last_allowed_fork_level;
           legacy_block_metadata;
           legacy_operations_metadata;
         } ->
      ( legacy_message,
        legacy_max_operations_ttl,
        legacy_last_allowed_fork_level,
        legacy_block_metadata,
        legacy_operations_metadata ))
    (fun ( legacy_message,
           legacy_max_operations_ttl,
           legacy_last_allowed_fork_level,
           legacy_block_metadata,
           legacy_operations_metadata ) ->
      {
        legacy_message;
        legacy_max_operations_ttl;
        legacy_last_allowed_fork_level;
        legacy_block_metadata;
        legacy_operations_metadata;
      })
    (obj5
       (opt "legacy_message" string)
       (req "legacy_max_operations_ttl" uint16)
       (req "legacy_last_allowed_fork_level" int32)
       (req "legacy_block_metadata" bytes)
       (req "legacy_operations_metadata" (list (list bytes))))

let encoding =
  let open Data_encoding in
  conv
    (fun {hash; contents; metadata} -> (hash, contents, metadata))
    (fun (hash, contents, metadata) -> {hash; contents; metadata})
    (dynamic_size
       ~kind:`Uint30
       (obj3
          (req "hash" Block_hash.encoding)
          (req "contents" contents_encoding)
          (varopt "metadata" metadata_encoding)))

let legacy_encoding =
  let open Data_encoding in
  conv
    (fun {legacy_hash; legacy_contents; legacy_metadata} ->
      (legacy_hash, legacy_contents, legacy_metadata))
    (fun (legacy_hash, legacy_contents, legacy_metadata) ->
      {legacy_hash; legacy_contents; legacy_metadata})
    (dynamic_size
       ~kind:`Uint30
       (obj3
          (req "legacy_hash" Block_hash.encoding)
          (req "legacy_contents" contents_encoding)
          (varopt "legacy_metadata" legacy_metadata_encoding)))

let pp_json fmt b =
  let json = Data_encoding.Json.construct encoding b in
  Data_encoding.Json.pp fmt json

(* Contents accessors *)

let descriptor blk = (blk.hash, blk.contents.header.Block_header.shell.level)

let hash blk = blk.hash

let header blk = blk.contents.header

let operations blk = blk.contents.operations

let block_metadata_hash blk = blk.contents.block_metadata_hash

let operations_metadata_hashes blk = blk.contents.operations_metadata_hashes

let shell_header blk = blk.contents.header.Block_header.shell

let level blk = blk.contents.header.Block_header.shell.level

let proto_level blk = blk.contents.header.Block_header.shell.proto_level

let predecessor blk = blk.contents.header.Block_header.shell.predecessor

let timestamp blk = blk.contents.header.Block_header.shell.timestamp

let validation_passes blk =
  blk.contents.header.Block_header.shell.validation_passes

let operations_hash blk = blk.contents.header.Block_header.shell.operations_hash

let fitness blk = blk.contents.header.Block_header.shell.fitness

let context blk = blk.contents.header.Block_header.shell.context

let protocol_data blk = blk.contents.header.Block_header.protocol_data

(* Metadata accessors *)

let metadata blk = blk.metadata

let message metadata = metadata.message

let max_operations_ttl metadata = metadata.max_operations_ttl

let last_allowed_fork_level metadata = metadata.last_allowed_fork_level

let block_metadata metadata = metadata.block_metadata

let operations_metadata metadata = metadata.operations_metadata

let check_block_consistency ?genesis_hash ?pred_block block =
  let open Lwt_result_syntax in
  let block_header = header block in
  let block_hash = hash block in
  let result_hash = Block_header.hash block_header in
  let* () =
    fail_unless
      (Block_hash.equal block_hash result_hash
      ||
      match genesis_hash with
      | Some genesis_hash -> Block_hash.equal block_hash genesis_hash
      | None -> false)
      (Inconsistent_block_hash
         {
           level = level block;
           expected_hash = block_hash;
           computed_hash = result_hash;
         })
  in
  let* () =
    match pred_block with
    | None -> return_unit
    | Some pred_block ->
        fail_unless
          (Block_hash.equal (hash pred_block) (predecessor block)
          && Compare.Int32.(level block = Int32.succ (level pred_block)))
          (Inconsistent_block_predecessor
             {
               block_hash;
               level = level block;
               expected_hash = hash pred_block;
               computed_hash = predecessor block;
             })
  in
  let computed_operations_hash =
    Operation_list_list_hash.compute
      (List.map
         Operation_list_hash.compute
         (List.map (List.map Operation.hash) (operations block)))
  in
  let* () =
    fail_unless
      (Operation_list_list_hash.equal
         computed_operations_hash
         (operations_hash block))
      (Store_errors.Inconsistent_operations_hash
         {expected = operations_hash block; got = computed_operations_hash})
  in
  return_unit

let convert_legacy_metadata (legacy_metadata : legacy_metadata) : metadata =
  let {
    legacy_message;
    legacy_max_operations_ttl;
    legacy_last_allowed_fork_level;
    legacy_block_metadata;
    legacy_operations_metadata;
  } =
    legacy_metadata
  in
  {
    message = legacy_message;
    max_operations_ttl = legacy_max_operations_ttl;
    last_allowed_fork_level = legacy_last_allowed_fork_level;
    block_metadata = legacy_block_metadata;
    operations_metadata =
      List.map
        (List.map (fun b -> Block_validation.Metadata b))
        legacy_operations_metadata;
  }

let decode_block_repr encoding block_bytes =
  try Data_encoding.Binary.of_bytes_exn encoding block_bytes
  with _ ->
    (* If the decoding fails, try with the legacy block_repr encoding
       *)
    let legacy_block =
      Data_encoding.Binary.of_bytes_exn legacy_encoding block_bytes
    in
    let legacy_metadata = legacy_block.legacy_metadata in
    let metadata =
      match legacy_metadata with
      | Some metadata ->
          let {
            legacy_message;
            legacy_max_operations_ttl;
            legacy_last_allowed_fork_level;
            legacy_block_metadata;
            legacy_operations_metadata;
          } =
            metadata
          in
          let operations_metadata =
            (List.map (List.map (fun x -> Block_validation.Metadata x)))
              legacy_operations_metadata
          in
          Some
            ({
               message = legacy_message;
               max_operations_ttl = legacy_max_operations_ttl;
               last_allowed_fork_level = legacy_last_allowed_fork_level;
               block_metadata = legacy_block_metadata;
               operations_metadata;
             }
              : metadata)
      | None -> None
    in
    {
      hash = legacy_block.legacy_hash;
      contents = legacy_block.legacy_contents;
      metadata;
    }

(* FIXME handle I/O errors *)
let read_next_block_exn fd =
  let open Lwt_syntax in
  (* Read length *)
  let length_bytes = Bytes.create 4 in
  let* () = Lwt_utils_unix.read_bytes ~pos:0 ~len:4 fd length_bytes in
  let block_length_int32 = Bytes.get_int32_be length_bytes 0 in
  let block_length = Int32.to_int block_length_int32 in
  let block_bytes = Bytes.extend length_bytes 0 block_length in
  let* () = Lwt_utils_unix.read_bytes ~pos:4 ~len:block_length fd block_bytes in
  Lwt.return (decode_block_repr encoding block_bytes, 4 + block_length)

let read_next_block fd = Option.catch_s (fun () -> read_next_block_exn fd)

let pread_block_exn fd ~file_offset =
  let open Lwt_syntax in
  (* Read length *)
  let length_bytes = Bytes.create 4 in
  let* () =
    Lwt_utils_unix.read_bytes ~file_offset ~pos:0 ~len:4 fd length_bytes
  in
  let block_length_int32 = Bytes.get_int32_be length_bytes 0 in
  let block_length = Int32.to_int block_length_int32 in
  let block_bytes = Bytes.extend length_bytes 0 block_length in
  let* () =
    Lwt_utils_unix.read_bytes
      ~file_offset:(file_offset + 4)
      ~pos:4
      ~len:block_length
      fd
      block_bytes
  in
  Lwt.return (decode_block_repr encoding block_bytes, 4 + block_length)

let pread_block fd ~file_offset =
  Option.catch_s (fun () -> pread_block_exn fd ~file_offset)

let decode_metadata b =
  Data_encoding.Binary.of_string_opt metadata_encoding b |> function
  | Some metadata -> Some metadata
  | None ->
      Option.map
        convert_legacy_metadata
        (Data_encoding.Binary.of_string_opt legacy_metadata_encoding b)
