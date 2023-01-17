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

open Filename.Infix
open Store_types

type 'dirname directory = {dir_path : string}

type 'kind file = {file_path : string}

type ('kind, 'data) encoded_file = 'data Stored_data.file

let dir_path {dir_path} = dir_path

let file_path {file_path} = file_path

let is_json_file file = file.Stored_data.json

let make_encoded_file ?(json = false) dir ~filename encoding eq =
  let filepath = dir_path dir // filename in
  Stored_data.make_file ~json ~filepath encoding eq

let encoded_file_path {Stored_data.path; _} = path

let file_encoding {Stored_data.encoding; _} = encoding

(* Utility functions *)

let mk_file dir name = {file_path = dir.dir_path // name}

let mk_dir dir dir_name = {dir_path = dir.dir_path // dir_name}

let store_dir ~dir_path = {dir_path}

let protocol_store_dir base_dir = mk_dir base_dir "protocols"

let protocol_file dir proto_hash =
  mk_file dir (Protocol_hash.to_b58check proto_hash)

let chain_dir dir chain_id =
  mk_dir dir (Format.asprintf "chain_%a" Chain_id.pp_short chain_id)

let chain_config_file dir =
  make_encoded_file
    ~json:true
    dir
    ~filename:"config.json"
    chain_config_encoding
    chain_config_equal

let lock_file dir = mk_file dir "lock"

let gc_lockfile dir = mk_file dir "gc_lock"

let reconstruction_lock_file dir = mk_file dir "reconstruction_lock"

let testchains_dir dir = mk_dir dir "testchains"

let protocol_levels_file dir =
  make_encoded_file
    dir
    ~filename:"protocol_levels"
    Protocol_levels.encoding
    Store_types.Protocol_levels.equal

let legacy_protocol_levels_file dir =
  make_encoded_file
    dir
    ~filename:"protocol_levels"
    Protocol_levels.Legacy.encoding
    Protocol_levels.Legacy.equal

let genesis_block_file dir =
  make_encoded_file dir ~filename:"genesis" Block_repr.encoding Block_repr.equal

let current_head_file dir =
  make_encoded_file
    dir
    ~filename:"current_head"
    block_descriptor_encoding
    block_descriptor_equal

let cementing_highwatermark_file dir =
  make_encoded_file
    dir
    ~filename:"cementing_highwatermark"
    Data_encoding.(option int32)
    (Option.equal Int32.equal)

let checkpoint_file dir =
  make_encoded_file
    dir
    ~filename:"checkpoint"
    block_descriptor_encoding
    block_descriptor_equal

let target_file dir =
  make_encoded_file
    dir
    ~filename:"target"
    (Data_encoding.option block_descriptor_encoding)
    (Option.equal block_descriptor_equal)

let invalid_blocks_file dir =
  make_encoded_file
    dir
    ~filename:"invalid_blocks"
    (Block_hash.Map.encoding invalid_block_encoding)
    (Block_hash.Map.equal invalid_block_equal)

let forked_chains_file dir =
  make_encoded_file
    dir
    ~filename:"forked_chains"
    (Chain_id.Map.encoding Block_hash.encoding)
    (Chain_id.Map.equal Block_hash.equal)

let savepoint_file dir =
  make_encoded_file
    dir
    ~filename:"savepoint"
    block_descriptor_encoding
    Store_types.block_descriptor_equal

let caboose_file dir =
  make_encoded_file
    dir
    ~filename:"caboose"
    block_descriptor_encoding
    Store_types.block_descriptor_equal

type block_store_status = Idle | Merging

let block_store_status_encoding =
  let open Data_encoding in
  conv
    (function Idle -> false | Merging -> true)
    (function false -> Idle | true -> Merging)
    bool

let status_equal s1 s2 =
  match (s1, s2) with
  | Idle, Idle -> true
  | Merging, Merging -> true
  | Idle, Merging | Merging, Idle -> false

let block_store_status_file dir =
  make_encoded_file
    dir
    ~filename:"status"
    block_store_status_encoding
    status_equal

let cemented_blocks_dir dir = mk_dir dir "cemented"

let cemented_blocks_level_index_dir dir = mk_dir dir "level_index"

let cemented_blocks_level_lock_file dir =
  mk_file dir (Filename.concat "index" "lock")

let cemented_blocks_hash_index_dir dir = mk_dir dir "hash_index"

let cemented_blocks_hash_lock_file dir =
  mk_file dir (Filename.concat "index" "lock")

let cemented_blocks_file dir ~start_level ~end_level =
  mk_file dir (Printf.sprintf "%ld_%ld" start_level end_level)

let cemented_blocks_metadata_dir dir = mk_dir dir "metadata"

let cemented_blocks_metadata_file dir {file_path} =
  mk_file dir (Filename.basename file_path ^ ".zip")

let cemented_blocks_tmp_metadata_file dir {file_path} =
  mk_file dir (Filename.basename file_path ^ ".zip.part")

type floating_kind = RO | RW | RW_TMP | RO_TMP | Restore of floating_kind

let floating_blocks_dir dir kind =
  let rec loop = function
    | RO -> "ro_floating"
    | RW -> "rw_floating"
    | RO_TMP -> "ro_tmp_floating"
    | RW_TMP -> "rw_tmp_floating"
    | Restore kind -> "restore_" ^ loop kind
  in
  mk_dir dir (loop kind)

let floating_blocks_index_dir dir = mk_dir dir "index"

let floating_blocks_file dir = mk_file dir "blocks"

let snapshot_dir ?snapshot_path () =
  let snapshot_path =
    match snapshot_path with Some path -> path | None -> "tezos-snapshot"
  in
  {dir_path = snapshot_path}

let snapshot_file ~snapshot_filename dir = mk_file dir snapshot_filename

let snapshot_tmp_dir snapshot_dir = {dir_path = snapshot_dir.dir_path ^ "_tmp"}

let snapshot_tmp_tar_file dir = mk_file dir "tmp_snapshot.tar"

let snapshot_block_data_file dir = mk_file dir "block_data"

let snapshot_context_file dir = mk_file dir "context"

let snapshot_floating_blocks_file dir = mk_file dir "floating_blocks"

let snapshot_metadata_file dir = mk_file dir "metadata.json"

let snapshot_version_file dir = mk_file dir "snapshot_version.json"

let snapshot_protocol_levels_file dir =
  make_encoded_file
    dir
    ~filename:"protocol_levels"
    Protocol_levels.encoding
    Protocol_levels.equal

let snapshot_tar_root = {dir_path = ""}
