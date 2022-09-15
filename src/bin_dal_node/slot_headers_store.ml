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

include Store_utils
module Maker = Irmin_pack_unix.Maker (Tezos_context_encoding.Context.Conf)

module IStore = Irmin_store.Make (struct
  let name = "Tezos Data Availability Layer node"
end)

module Store = Store_utils.Make (IStore)

type t = Store.t

let load = IStore.load

(* Published slot headers per block hash,
   stored as a list of bindings from `Dal_slot_index.t`
   to `Dal.Slot.t`. The encoding function converts this
   list into a `Dal.Slot_index.t`-indexed map. *)
include
  Store.Make_nested_map
    (struct
      let path = ["dal"; "slot_headers"]

      (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3527
         This value is currently not used, but required by the module type. *)
      let keep_last_n_entries_in_memory = Some 10
    end)
    (struct
      type key = Block_hash.t

      let to_path_representation = Block_hash.to_b58check
    end)
    (struct
      type key = int

      let compare = Int.compare

      let encoding = Data_encoding.uint8

      let name = "slot index"
    end)
    (struct
      type value = Cryptobox.commitment

      let name = "slots_metadata"

      let encoding = Cryptobox.Commitment.encoding
    end)
