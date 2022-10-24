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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3433
   Check what the actual value of `reorganization_window_length`
   should be, and if we want to make it configurable.
*)
let reorganization_window_length = 10

module Store = struct
  module ProcessedHashes =
    Store.Make_append_only_map
      (struct
        let path = ["tezos"; "processed_blocks"]

        let keep_last_n_entries_in_memory = Some reorganization_window_length
      end)
      (struct
        type key = Block_hash.t

        let to_path_representation = Block_hash.to_b58check
      end)
      (struct
        type value = int32

        let name = "level"

        let encoding = Data_encoding.int32
      end)

  module LastProcessedHead =
    Store.Make_mutable_value
      (struct
        let path = ["tezos"; "processed_head"]

        let keep_last_n_entries_in_memory = None
      end)
      (struct
        type value = Layer1.head

        let name = "head"

        let encoding = Layer1.head_encoding
      end)

  module LastFinalizedHead =
    Store.Make_mutable_value
      (struct
        let path = ["tezos"; "finalized_head"]

        let keep_last_n_entries_in_memory = None
      end)
      (struct
        type value = Layer1.head

        let name = "head"

        let encoding = Layer1.head_encoding
      end)

  module Levels =
    Store.Make_updatable_map
      (struct
        let path = ["tezos"; "levels"]

        let keep_last_n_entries_in_memory = Some reorganization_window_length
      end)
      (struct
        type key = int32

        let to_path_representation = Int32.to_string
      end)
      (struct
        type value = Block_hash.t

        let name = "block_hash"

        let encoding = Block_hash.encoding
      end)
end

let hash_of_level store level = Store.Levels.get store level

let level_of_hash store hash =
  let open Lwt_result_syntax in
  let*! level = Store.ProcessedHashes.find store hash in
  match level with
  | None -> failwith "No level known for block %a" Block_hash.pp hash
  | Some l -> return l

let mark_processed_head store Layer1.({hash; level} as head) =
  let open Lwt_syntax in
  let* () = Store.ProcessedHashes.add store hash level in
  let* () = Store.Levels.add store level hash in
  Store.LastProcessedHead.set store head

let is_processed store head = Store.ProcessedHashes.mem store head

let last_processed_head_opt store = Store.LastProcessedHead.find store

let mark_finalized_head store head = Store.LastFinalizedHead.set store head

let get_finalized_head_opt store = Store.LastFinalizedHead.find store
