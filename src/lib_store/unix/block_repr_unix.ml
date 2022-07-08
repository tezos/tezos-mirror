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
