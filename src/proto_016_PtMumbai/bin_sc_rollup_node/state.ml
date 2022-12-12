(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Protocol
open Alpha_context
module Raw_store = Store

module Store = struct
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4392
     Use file. *)
  module L2_head =
    Store.Make_mutable_value
      (struct
        let path = ["l2_head"]
      end)
      (struct
        type value = Sc_rollup_block.t

        let name = "l2_block"

        let encoding = Sc_rollup_block.encoding
      end)

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4392
     Use file. *)
  module Last_finalized_head =
    Store.Make_mutable_value
      (struct
        let path = ["tezos"; "finalized_head"]
      end)
      (struct
        type value = Layer1.head

        let name = "head"

        let encoding = Layer1.head_encoding
      end)

  (** Table from L1 levels to blocks hashes. *)
  module Levels_to_hashes =
    Store.Make_updatable_map
      (struct
        let path = ["tezos"; "levels"]
      end)
      (struct
        type key = int32

        let to_path_representation = Int32.to_string
      end)
      (struct
        type value = Tezos_crypto.Block_hash.t

        let name = "block_hash"

        let encoding = Tezos_crypto.Block_hash.encoding
      end)

  (** Table from L1 blocks hashes to levels. *)
  module Hashes_to_levels =
    Store.Make_append_only_map
      (struct
        let path = ["tezos"; "blocks_hashes"]
      end)
      (struct
        type key = Tezos_crypto.Block_hash.t

        let to_path_representation = Tezos_crypto.Block_hash.to_b58check
      end)
      (struct
        type value = int32

        let name = "level"

        let encoding = Data_encoding.int32
      end)
end

let hash_of_level store level = Store.Levels_to_hashes.get store level

let level_of_hash store hash =
  let open Lwt_result_syntax in
  let*! level = Store.Hashes_to_levels.find store hash in
  match level with
  | None ->
      failwith "No level known for block %a" Tezos_crypto.Block_hash.pp hash
  | Some l -> return l

let save_l2_block store (head : Sc_rollup_block.t) =
  let open Lwt_syntax in
  let* () = Raw_store.L2_blocks.add store head.header.block_hash head in
  Store.L2_head.set store head

let is_processed store head = Raw_store.L2_blocks.mem store head

let last_processed_head_opt store = Store.L2_head.find store

let mark_finalized_head store head = Store.Last_finalized_head.set store head

let get_finalized_head_opt store = Store.Last_finalized_head.find store

let set_block_level_and_hash store Layer1.{hash; level} =
  let open Lwt_syntax in
  let* () = Store.Hashes_to_levels.add store hash level in
  Store.Levels_to_hashes.add store level hash

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4532
   Make this logarithmic, by storing pointers to muliple predecessor and
   by dichotomy. *)
let block_before store tick =
  let open Lwt_result_syntax in
  let*! head = Store.L2_head.find store in
  match head with
  | None -> return_none
  | Some head ->
      let rec search block_hash =
        let*! block = Raw_store.L2_blocks.find store block_hash in
        match block with
        | None ->
            failwith "Missing block %a" Tezos_crypto.Block_hash.pp block_hash
        | Some block ->
            if Sc_rollup.Tick.(block.initial_tick <= tick) then
              return_some block
            else search block.header.predecessor
      in
      search head.header.block_hash
