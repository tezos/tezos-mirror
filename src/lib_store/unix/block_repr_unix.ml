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

open Block_repr

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
               last_preserved_block_level = legacy_last_allowed_fork_level;
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

let raw_pruned_block_length bytes =
  (* Hypothesis: (Int32.to_int total_len + 4 <= Bytes.length bytes) *)
  let offset = 4 in
  let offset = offset + Block_hash.size (* hash *) in
  let header_length = Bytes.get_int32_be bytes offset in
  let offset = offset + 4 + Int32.to_int header_length in
  let operations_length = Bytes.get_int32_be bytes offset in
  let offset = offset + 4 + Int32.to_int operations_length in
  let offset =
    (* block metadata hash *)
    if Bytes.get_uint8 bytes offset = 0xff then
      offset + 1 + Block_metadata_hash.size
    else offset + 1
  in
  let offset =
    (* operation metadata hashes *)
    if Bytes.get_uint8 bytes offset = 0xff then
      let offset = offset + 1 in
      let operation_metadata_hashes_length = Bytes.get_int32_be bytes offset in
      offset + 4 + Int32.to_int operation_metadata_hashes_length
    else offset + 1
  in
  (* metadata are 'varopt' which means there is no option tag *)
  offset

let prune_raw_block_bytes bytes =
  let pruned_block_length = raw_pruned_block_length bytes in
  (* rewrite total block length *)
  Bytes.set_int32_be bytes 0 (Int32.of_int (pruned_block_length - 4)) ;
  pruned_block_length

let hash_offset = 4

let header_length_offset = hash_offset + Block_hash.size

let level_offset = header_length_offset + 4

let predecessor_offset = level_offset + 4 (* level *) + 1 (* proto_level *)

let raw_get_block_hash block_bytes =
  Block_hash.of_bytes_exn (Bytes.sub block_bytes hash_offset Block_hash.size)

let raw_get_block_level block_bytes =
  Bytes.get_int32_be block_bytes level_offset

let raw_get_block_predecessor block_bytes =
  Block_hash.of_bytes_exn
    (Bytes.sub block_bytes predecessor_offset Block_hash.size)

let raw_get_last_preserved_block_level block_bytes total_block_length =
  let header_length = Bytes.get_int32_be block_bytes header_length_offset in
  let operations_length_offset =
    header_length_offset + 4 + Int32.to_int header_length
  in
  let operations_length =
    Bytes.get_int32_be block_bytes operations_length_offset
  in
  let block_metadata_hash_offset =
    operations_length_offset + 4 + Int32.to_int operations_length
  in
  let operation_metadata_hashes_offset =
    (* block metadata hash *)
    if Bytes.get_uint8 block_bytes block_metadata_hash_offset = 0xff then
      block_metadata_hash_offset + 1 + Block_metadata_hash.size
    else block_metadata_hash_offset + 1
  in
  let metadata_offset =
    (* operation metadata hashes *)
    if Bytes.get_uint8 block_bytes operation_metadata_hashes_offset = 0xff then
      let operation_metadata_hashes_length =
        Bytes.get_int32_be block_bytes (operation_metadata_hashes_offset + 1)
      in
      operation_metadata_hashes_offset + 1 + 4
      + Int32.to_int operation_metadata_hashes_length
    else operation_metadata_hashes_offset + 1
  in
  if metadata_offset = total_block_length then (* Pruned *) None
  else
    let lpbl_offset =
      (* max op ttl *)
      2
      +
      (* message *)
      if Bytes.get_uint8 block_bytes metadata_offset = 0xff then
        let message_length =
          Bytes.get_int32_be block_bytes (metadata_offset + 1)
        in
        metadata_offset + 1 + 4 + Int32.to_int message_length
      else metadata_offset + 1
    in
    Some (Bytes.get_int32_be block_bytes lpbl_offset)

let fitness_length_offset =
  predecessor_offset + Block_hash.size + 8 (* timestamp *) + 1
  (* validation pass *) + 32 (* operation list list hash *)

let raw_get_context block_bytes =
  let fitness_length = Bytes.get_int32_be block_bytes fitness_length_offset in
  let context_offset =
    fitness_length_offset + 4 + Int32.to_int fitness_length
  in
  Context_hash.of_bytes_exn
    (Bytes.sub block_bytes context_offset Context_hash.size)
