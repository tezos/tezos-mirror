(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* This module provides a subset of the rollup node storage. It could
   be factorised with existing rollup node storage module if
   needed/more used. *)

module Last_finalized_level = Indexed_store.Make_singleton (struct
  type t = int32

  let name = "finalized_level"

  let encoding = Data_encoding.int32
end)

module Block_key = struct
  include Block_hash

  let hash_size = 31

  let t =
    let open Repr in
    map
      (bytes_of (`Fixed hash_size))
      (fun b -> Block_hash.of_bytes_exn b)
      (fun bh -> Block_hash.to_bytes bh)

  let encode bh = Block_hash.to_string bh

  let encoded_size = Block_hash.size

  let decode str off =
    let str = String.sub str off encoded_size in
    Block_hash.of_string_exn str

  let pp = Block_hash.pp
end

module L2_blocks =
  Indexed_store.Make_indexed_file
    (struct
      let name = "l2_blocks"
    end)
    (Block_key)
    (struct
      type t = (unit, unit) Sc_rollup_block.block

      let name = "sc_rollup_block_info"

      let encoding =
        Sc_rollup_block.block_encoding Data_encoding.unit Data_encoding.unit

      module Header = struct
        type t = Sc_rollup_block.header

        let name = "sc_rollup_block_header"

        let encoding = Sc_rollup_block.header_encoding

        let fixed_size = Sc_rollup_block.header_size
      end
    end)

module Levels_to_hashes =
  Indexed_store.Make_indexable
    (struct
      let name = "tezos_levels"
    end)
    (Indexed_store.Make_index_key (struct
      type t = int32

      let encoding = Data_encoding.int32

      let name = "level"

      let fixed_size = 4

      let equal = Int32.equal
    end))
    (Block_key)

(** [load ~rollup_node_data_dir] load the needed storage from the
    rollup node: last_finalized_level, levels_to_hashes, and
    l2_blocks.  default [index_buffer_size] is [10_000] an
    [cache_size] is [300_000]. They are the same value as for the
    rollup node. *)
let load ?(index_buffer_size = 10_000) ?(cache_size = 300_000)
    ~rollup_node_data_dir () =
  let open Lwt_result_syntax in
  let store = Filename.Infix.(rollup_node_data_dir // "storage") in
  let* last_finalized_level =
    Last_finalized_level.load
      Read_only
      ~path:(Filename.concat store "last_finalized_level")
  in
  let* levels_to_hashes =
    Levels_to_hashes.load
      Read_only
      ~index_buffer_size
      ~path:(Filename.concat store "levels_to_hashes")
  in
  let* l2_blocks =
    L2_blocks.load
      Read_only
      ~index_buffer_size
      ~cache_size
      ~path:(Filename.concat store "l2_blocks")
  in
  return (last_finalized_level, levels_to_hashes, l2_blocks)
