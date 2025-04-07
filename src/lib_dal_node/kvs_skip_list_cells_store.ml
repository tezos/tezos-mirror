(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Functori,     <contact@functori.com>                   *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module KVS = Key_value_store

let ( // ) = Filename.concat

module Cells = struct
  type file = Dal_proto_types.Skip_list_hash.t

  type key = unit

  type value = Dal_proto_types.Skip_list_cell.t

  type t = {
    file_layout : (file, key, value) KVS.file_layout;
    (* The Key-Value store. We store one cell per file, hence the unit as a
       second type argument. *)
    store : (file, key, value) KVS.t;
  }

  let fixed_size_cells_encoding_with_padding ~padded_encoded_cell_size =
    let open Data_encoding in
    (* We remember the size of original bytes as a Uint16. *)
    let length_prefix_encoding = uint16 in
    (* Uint16 requires 2 bytes in binary. *)
    let length_prefix_size = 2 in
    (* We reserve 2 bytes for the real data length encoding in the original
       [padded_encoded_cell_size], and compute the real padded cell length. *)
    let padded_encoded_cell_size =
      padded_encoded_cell_size - length_prefix_size
    in
    assert (padded_encoded_cell_size > 0) ;
    (* We define the cells encoding as a fixed encoding to be able to use it in
       the KVS. *)
    let value_encoding = Fixed.bytes padded_encoded_cell_size in
    conv_with_guard
      (fun v ->
        let original_bytes =
          Binary.to_bytes_exn Dal_proto_types.Skip_list_cell.encoding v
        in
        let len = Bytes.length original_bytes in
        if len > padded_encoded_cell_size then
          Stdlib.failwith "Max encoded value size exceeded" ;
        let bytes =
          if len = padded_encoded_cell_size then original_bytes
          else Bytes.extend original_bytes 0 @@ (padded_encoded_cell_size - len)
        in
        (len, bytes))
      (fun (len, bytes_with_padding) ->
        let open Result_syntax in
        let* cell_bytes =
          try Ok (Bytes.sub bytes_with_padding 0 len)
          with Invalid_argument s ->
            Error (Format.sprintf "Decoding a skip list cell failed: %s" s)
        in
        Binary.of_bytes_opt Dal_proto_types.Skip_list_cell.encoding cell_bytes
        |> Result.of_option ~error:"Decoding a skip list cell failed.")
      (obj2
         (req "size" length_prefix_encoding)
         (req "value_with_padding" value_encoding))

  (* Since the size of Skip list cells is increasing over time, we define
     here a max size we support. Any cell that fits in less than this value
     when encoded in bytes is padded, because the Key-Value store doesn't
     handle data with a non-fixed size. *)
  let file_layout padded_encoded_cell_size =
    let encoding =
      fixed_size_cells_encoding_with_padding ~padded_encoded_cell_size
    in
    fun ~root_dir hash ->
      let hash_string =
        let bytes =
          Data_encoding.Binary.to_bytes_exn
            Dal_proto_types.Skip_list_hash.encoding
            hash
        in
        (* We use base64 encoding for the names of files, since we don't have
           access to function [to_b58check] here. *)
        Bytes.to_string bytes |> Base64.(encode_exn ~alphabet:uri_safe_alphabet)
      in
      let filepath = root_dir // hash_string in
      Key_value_store.layout
        ~encoding (* encoding has a fixed size here *)
        ~filepath
        ~eq:Dal_proto_types.Skip_list_cell.equal
        ~index_of:(fun () -> 0)
        ~number_of_keys_per_file:1
        ()

  let init ~node_store_dir ~skip_list_store_dir ~padded_encoded_cell_size =
    let open Lwt_result_syntax in
    (* We store the cells under /cells sub-directory. *)
    let root_dir = node_store_dir // skip_list_store_dir // "cells" in
    (* The size of the LRU is fixed to 2, as we don't need to frequently access
       files: it's "write once & forget", and possibility read the value in case
       of refutation. *)
    let+ store = KVS.init ~lru_size:2 ~root_dir in
    {store; file_layout = file_layout padded_encoded_cell_size}

  let write_values {store; file_layout} values =
    KVS.write_values ~override:true store file_layout values

  let read_value {store; file_layout} hash =
    KVS.read_value store file_layout hash ()

  let remove_file {store; file_layout} hash =
    KVS.remove_file store file_layout hash
end

module Hashes = struct
  type file = int32 (* a level *)

  type key = int (* a slot index *)

  type value = Dal_proto_types.Skip_list_hash.t

  type t = {
    store : (file, key, value) KVS.t;
    file_layout : (file, key, value) KVS.file_layout;
  }

  let fixed_size_hash_encoding ~encoded_hash_size =
    let open Data_encoding in
    conv
      (Data_encoding.Binary.to_bytes_exn
         Dal_proto_types.Skip_list_hash.encoding)
      (Data_encoding.Binary.of_bytes_exn
         Dal_proto_types.Skip_list_hash.encoding)
      (obj1 (req "hash" (Fixed.bytes encoded_hash_size)))

  (* For the Key-Value store of hashes, we store [number_of_slots] hashes per
     file, and a file is identified by the corresponding attested level. *)
  let file_layout encoded_hash_size =
    let encoding = fixed_size_hash_encoding ~encoded_hash_size in
    fun ~root_dir attested_level ->
      let attested_level_string = Int32.to_string attested_level in
      let filepath = root_dir // attested_level_string in
      Key_value_store.layout
        ~encoding
        ~filepath
        ~eq:Dal_proto_types.Skip_list_hash.equal
        ~index_of:Fun.id
        ~number_of_keys_per_file:(Value_size_hooks.number_of_slots ())
        ()

  let init ~node_store_dir ~skip_list_store_dir ~encoded_hash_size =
    let open Lwt_result_syntax in
    let root_dir = node_store_dir // skip_list_store_dir // "hashes" in
    let+ store = KVS.init ~lru_size:2 ~root_dir in
    let file_layout = file_layout encoded_hash_size in
    {store; file_layout}

  let write_values {store; file_layout; _} values =
    KVS.write_values ~override:true store file_layout values

  let read_value {store; file_layout; _} attested_level key =
    KVS.read_value store file_layout attested_level key

  let remove_file {store; file_layout; _} attested_level =
    KVS.remove_file store file_layout attested_level
end

(* The main (exposed) content of the module. *)

type t = {cells_store : Cells.t; hashes_store : Hashes.t}

let init ~node_store_dir ~skip_list_store_dir ~padded_encoded_cell_size
    ~encoded_hash_size =
  let open Lwt_result_syntax in
  let* cells_store =
    Cells.init ~node_store_dir ~skip_list_store_dir ~padded_encoded_cell_size
  in
  let* hashes_store =
    Hashes.init ~node_store_dir ~skip_list_store_dir ~encoded_hash_size
  in
  return {cells_store; hashes_store}

let insert {cells_store; hashes_store} ~attested_level items =
  let open Lwt_result_syntax in
  let cells, hashes =
    List.to_seq items
    |> Seq.zip (Seq.ints 0)
    |> Seq.map (fun (index, (hash, cell)) ->
           ((hash, (), cell), (attested_level, index, hash)))
    |> Seq.split
  in
  let* () = Cells.(write_values cells_store cells) in
  let* () = Hashes.write_values hashes_store hashes in
  return_unit

let find {cells_store; hashes_store = _} hash =
  Cells.read_value cells_store hash

let remove {cells_store; hashes_store} ~attested_level =
  let open Lwt_result_syntax in
  let number_of_slots = Value_size_hooks.number_of_slots () in
  let* () =
    List.iter_es
      (fun idx ->
        let*! hash = Hashes.read_value hashes_store attested_level idx in
        match hash with
        | Error _ -> return_unit
        | Ok hash -> Cells.remove_file cells_store hash)
      (0 -- (number_of_slots - 1))
  in
  Hashes.remove_file hashes_store attested_level

let close {cells_store; hashes_store} =
  let open Lwt_result_syntax in
  let* () = KVS.close cells_store.store in
  let* () = KVS.close hashes_store.store in
  return_unit

module Internal_for_tests = struct
  let skip_list_hash_exists {cells_store; hashes_store = _} hash =
    let {Cells.store; file_layout; _} = cells_store in
    KVS.value_exists store file_layout hash ()
end

module Internal_for_migrations = struct
  let get_attested_levels t =
    let store = KVS.root_dir t.hashes_store.store in
    let files = Lwt_unix.files_of_directory store in
    Lwt_stream.filter_map Int32.of_string_opt files

  let find_hash {cells_store = _; hashes_store} ~attested_level ~slot_index =
    Hashes.read_value hashes_store attested_level slot_index
end
