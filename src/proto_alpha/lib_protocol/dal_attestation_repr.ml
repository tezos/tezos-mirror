(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3103

   This may be a bit heavy in practice. We could also assume that in
   practice, many bits in this bitfield will be set to one. Hence, we
   could consider a better encoding which is smaller in the optimistic
   case. For example:

   1. When all the slots are attested, the encoding can be represented
   in one bit.

   2. Otherwise, we can pack slots by [8]. Have a header of [slots/8]
   which is [1] if all the slots in this set are [1], [0]
   otherwise. For all pack with a bit set to [0], we give the explicit
   representation. Hence, if there are [256] slots, and [2] are not
   attested, this representation will be of size [32] bits + [16] bits
   = [48] bits which is better than [256] bits. *)

(* A set of (attested) slot indexes. *)
type t = Bitset.t

let encoding = Bitset.encoding

let empty = Bitset.empty

let is_attested t index =
  let open Dal_slot_index_repr in
  match Bitset.mem t (to_int index) with
  | Ok b -> b
  | Error _ ->
      (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3104

         Should we do something here? *)
      false

let commit t index =
  let open Dal_slot_index_repr in
  match Bitset.add t (to_int index) with
  | Ok t -> t
  | Error _ ->
      (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3104

         Should we do something here? *)
      t

let occupied_size_in_bits = Bitset.occupied_size_in_bits

let expected_size_in_bits ~max_index =
  (* We compute an encoding of the data-availability attestations
     which is a (tight) upper bound of what we expect. *)
  let open Bitset in
  let open Dal_slot_index_repr in
  match add empty @@ to_int max_index with
  | Error _ -> (* Happens if max_index < 1 *) 0
  | Ok t -> occupied_size_in_bits t

let number_of_attested_slots = Bitset.hamming_weight

type shard_index = int

module Shard_map = Map.Make (struct
  type t = shard_index

  let compare = Compare.Int.compare
end)

module Accountability = struct
  type attested_slots = t

  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3109

     Think hard about this data structure and whether it needs to be
     optimized.
  *)

  module SlotMap = Map.Make (Compare.Int)

  type t = {number_of_attested_shards : int SlotMap.t; number_of_slots : int}

  let init ~number_of_slots =
    {number_of_attested_shards = SlotMap.empty; number_of_slots}

  (* This function must be called at most once for a given attester; otherwise
     the count will be flawed. *)
  let record_number_of_attested_shards t baker_attested_slots
      number_of_baker_shards =
    let rec iter slot_index map =
      if Compare.Int.(slot_index >= t.number_of_slots) then map
      else
        let map =
          match Bitset.mem baker_attested_slots slot_index with
          | Error _ ->
              (* impossible, as [slot_index] is non-negative *)
              map
          | Ok true ->
              (* slot is attested by baker *)
              SlotMap.update
                slot_index
                (function
                  | None -> Some number_of_baker_shards
                  | Some old_number_of_attested_shards ->
                      Some
                        (old_number_of_attested_shards + number_of_baker_shards))
                map
          | Ok false ->
              (* slot is not attested by baker, nothing to update *)
              map
        in
        iter (slot_index + 1) map
    in
    let number_of_attested_shards = iter 0 t.number_of_attested_shards in
    {t with number_of_attested_shards}

  let is_slot_attested t ~threshold ~number_of_shards slot_index =
    let index = Dal_slot_index_repr.to_int slot_index in
    let number_of_attested_shards =
      match SlotMap.find index t.number_of_attested_shards with
      | None -> 0
      | Some v -> v
    in
    Compare.Int.(
      number_of_attested_shards >= threshold * number_of_shards / 100)
end
