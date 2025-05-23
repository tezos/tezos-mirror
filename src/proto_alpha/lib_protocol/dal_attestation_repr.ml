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

(* A set of (attested) slot indexes. *)
type t = Bitset.t

let encoding = Bitset.encoding

let empty = Bitset.empty

let to_z = Bitset.to_z

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

let number_of_attested_slots = Bitset.cardinal

let intersection = Bitset.inter

type shard_index = int

module Shard_map = Map.Make (struct
  type t = shard_index

  let compare = Compare.Int.compare
end)

module Accountability = struct
  type attested_slots = t

  module SlotMap = Map.Make (Compare.Int)

  type t = {number_of_attested_shards : int SlotMap.t; number_of_slots : int}

  type attestation_status = {
    total_shards : int;
    attested_shards : int;
    is_proto_attested : bool;
  }

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

  (* Given a slot encoded as [number_of_shards] shards and for which
     [number_of_attested_shards] are attested by the bakers. The slot is
     declated as attested_slots IFF at least [threshold] % of the total shards
     are attested by bakers.

     On rationals, the condition above means:

     number_of_attested_shards / number_of_shards >= threshold / 100,

     which is equivalent, on rationals, to:

     number_of_attested_shards >= (threshold * number_of_shards) / 100

     Note that the last reformulation translates to integers. *)
  let compute_proto_attestation_status ~number_of_attested_shards ~threshold
      ~number_of_shards =
    Compare.Int.(
      number_of_attested_shards >= threshold * number_of_shards / 100)

  let is_slot_attested t ~threshold ~number_of_shards slot_index =
    let index = Dal_slot_index_repr.to_int slot_index in
    let number_of_attested_shards =
      match SlotMap.find index t.number_of_attested_shards with
      | None -> 0
      | Some v -> v
    in
    let is_proto_attested =
      compute_proto_attestation_status
        ~number_of_attested_shards
        ~threshold
        ~number_of_shards
    in
    {
      is_proto_attested;
      attested_shards = number_of_attested_shards;
      total_shards = number_of_shards;
    }
end

module Dal_dependent_signing = struct
  module HashModule =
    Blake2B.Make
      (Base58)
      (struct
        let name = "Dal attestation hash"

        let title = "Dal attestation hash"

        let b58check_prefix = "\056\012\165" (* dba(53) *)

        let size = Some 32
      end)

  (* Computes the hash of the public keys, the message [op] and the DAL attestation slot.
     This hash is used as a weight for the companion key and companion signature
     in the aggregated linear combinations, in [aggregate_pk] and [aggregate_sig]. *)
  let weight ~consensus_pk ~companion_pk ~op t =
    let consensus_bytes =
      Bls.Public_key.hash consensus_pk |> Bls.Public_key_hash.to_bytes
    in
    let companion_bytes =
      Bls.Public_key.hash companion_pk |> Bls.Public_key_hash.to_bytes
    in
    let bitset_bytes = Z.to_bits (to_z t) |> Bytes.of_string in
    HashModule.(
      hash_bytes [consensus_bytes; companion_bytes; op; bitset_bytes]
      |> to_bytes)
    |> Bytes.to_string |> Z.of_bits

  (* Computes [(weight t) * companion_agg + consensus_agg].

     Produces the same result but is faster than calling
     [aggregate_weighted_opt
       [(weight t, companion_agg); (Z.one, consensus_agg)]]. *)
  let aggregate ~subgroup_check ~aggregate_opt ~aggregate_weighted_opt
      ~consensus_pk ~companion_pk ~consensus_agg ~companion_agg ~op t =
    let subgroup_check = Some subgroup_check in
    let z = weight ~consensus_pk ~companion_pk ~op t in
    let weighted_companion_opt =
      aggregate_weighted_opt ?subgroup_check [(z, companion_agg)]
    in
    Option.bind weighted_companion_opt (fun weighted_companion ->
        aggregate_opt ?subgroup_check [weighted_companion; consensus_agg])

  let aggregate_pk ~subgroup_check ~consensus_pk ~companion_pk ~op t =
    aggregate
      ~subgroup_check
      ~aggregate_opt:Bls.aggregate_public_key_opt
      ~aggregate_weighted_opt:Bls.aggregate_public_key_weighted_opt
      ~consensus_pk
      ~companion_pk
      ~consensus_agg:consensus_pk
      ~companion_agg:companion_pk
      ~op
      t

  let aggregate_sig ~subgroup_check ~consensus_pk ~companion_pk ~consensus_sig
      ~companion_sig ~op t =
    aggregate
      ~subgroup_check
      ~aggregate_opt:Bls.aggregate_signature_opt
      ~aggregate_weighted_opt:Bls.aggregate_signature_weighted_opt
      ~consensus_pk
      ~companion_pk
      ~consensus_agg:consensus_sig
      ~companion_agg:companion_sig
      ~op
      t
end

module Internal_for_tests = struct
  let of_z = Bitset.from_z
end
