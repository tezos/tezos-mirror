(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* A compact representation of multiple DAL attestations at different lags. *)
type t = Bitset.t

let encoding = Bitset.encoding

let empty = Bitset.empty

let is_empty = Bitset.is_empty

let to_z = Bitset.to_z

(* Helper to safely check membership in a bitset, returning false on error. *)
let bitset_mem bitset idx =
  match Bitset.mem bitset idx with Ok b -> b | Error _ -> false

(* Helper to safely add to a bitset, returning the original bitset on error. *)
let bitset_add bitset idx =
  match Bitset.add bitset idx with Ok b -> b | Error _ -> bitset

(* Helper: compute the data bit position for a slot at a given lag_index.
   Returns None if the attestation at lag_index is empty. *)
let compute_data_bit_position t ~number_of_slots ~number_of_lags ~lag_index
    slot_index =
  (* Check if the attestation at [lag_index] is non-empty *)
  let is_non_empty = bitset_mem t lag_index in
  if not is_non_empty then None
  else
    (* Count non-empty attestations before [lag_index] *)
    let rec count_preceding acc idx =
      if Compare.Int.(idx >= lag_index) then acc
      else
        let is_set = bitset_mem t idx in
        count_preceding (if is_set then acc + 1 else acc) (idx + 1)
    in
    let num_preceding_non_empty = count_preceding 0 0 in
    let slot_bit_idx = Dal_slot_index_repr.to_int slot_index in
    let data_bit_pos =
      number_of_lags
      + (num_preceding_non_empty * number_of_slots)
      + slot_bit_idx
    in
    Some data_bit_pos

let is_attested t ~number_of_slots ~number_of_lags ~lag_index slot_index =
  assert (Compare.Int.(lag_index >= 0 && lag_index < number_of_lags)) ;
  assert (Compare.Int.(Dal_slot_index_repr.to_int slot_index < number_of_slots)) ;
  match
    compute_data_bit_position
      t
      ~number_of_slots
      ~number_of_lags
      ~lag_index
      slot_index
  with
  | None -> false
  | Some data_bit_pos -> bitset_mem t data_bit_pos

(* Set a slot bit at a given lag in the encoded bitset.
   [bitset] is the full attestations bitset.
   [number_of_slots] is the number of slots per attestation.
   [lag_index] is the index of the attestation lag (0-based).
   [number_of_lags] is the total number of lags.
   [slot_index] is the slot to set.
   Returns the updated attestations bitset.

   This only sets bits (never clears them), so transitions are:
   - Empty -> Non-empty (slow path: inserts attestation data)
   - Non-empty -> Non-empty (fast path: sets bit in place) *)
let commit bitset ~number_of_slots ~number_of_lags ~lag_index slot_index =
  assert (Compare.Int.(lag_index >= 0 && lag_index < number_of_lags)) ;
  assert (Compare.Int.(Dal_slot_index_repr.to_int slot_index < number_of_slots)) ;

  let is_non_empty_at idx = bitset_mem bitset idx in

  (* Count non-empty attestations before [lag_index] *)
  let rec count_before acc idx =
    if Compare.Int.(idx >= lag_index) then acc
    else count_before (if is_non_empty_at idx then acc + 1 else acc) (idx + 1)
  in
  let num_before = count_before 0 0 in

  if is_non_empty_at lag_index then
    (* Fast path: attestation already exists, just set the bit *)
    let data_start = number_of_lags + (num_before * number_of_slots) in
    let slot_idx = Dal_slot_index_repr.to_int slot_index in
    bitset_add bitset (data_start + slot_idx)
  else
    (* Slow path: attestation is empty. Build new bitset by copying old bits,
       shifting those at or after [insertion_point] by [number_of_slots] to make
       room for the new attestation data. *)
    let insertion_point = number_of_lags + (num_before * number_of_slots) in
    (* Copy all existing bits, shifting data bits at or after [insertion_point] *)
    let result =
      List.fold_left
        (fun res index ->
          let new_index =
            if Compare.Int.(index >= insertion_point) then
              index + number_of_slots
            else index
          in
          bitset_add res new_index)
        Bitset.empty
        (Bitset.to_list bitset)
    in
    (* Set the new lag index bit in the prefix *)
    let result = bitset_add result lag_index in
    (* Set the new slot bit at the insertion point *)
    let slot_idx = Dal_slot_index_repr.to_int slot_index in
    bitset_add result (insertion_point + slot_idx)

let occupied_size_in_bits = Bitset.occupied_size_in_bits

let expected_max_size_in_bits ~number_of_slots ~number_of_lags =
  (* [number_of_lags] for the prefix and [number_of_lags * number_of_slots] for
     the data section. *)
  number_of_lags * (1 + number_of_slots)

let weight t = Bitset.cardinal t

let number_of_attested_slots t ~number_of_lags =
  (* Count all 1 bits and subtract the prefix bits (which are 1 for non-empty lags) *)
  let total_bits = Bitset.cardinal t in
  (* Count prefix 1s: how many of the first number_of_lags bits are set *)
  let prefix_ones =
    Misc.(0 --> (number_of_lags - 1))
    |> List.filter (fun i ->
           match Bitset.mem t i with Ok b -> b | Error _ -> assert false)
    |> List.length
  in
  total_bits - prefix_ones

(** Type alias for use in submodules. *)
type attestation = t

(** Slot availability represents the protocol's attestation result for a block.
    It is an alias to the main attestation type. *)
module Slot_availability = struct
  type nonrec t = t

  let empty = empty

  let encoding = encoding

  let is_attested = is_attested

  let commit = commit

  let number_of_attested_slots = number_of_attested_slots

  let intersection sa attestations ~number_of_slots ~attestation_lags =
    (* Compute intersection of two attestation bitsets.

       Strategy: AND the prefix bits to find common lags, then for each
       common lag, AND the data sections. Only include lags with non-empty
       intersection in the result.

       We maintain running counts of non-empty lags for both inputs to compute
       data offsets incrementally, achieving O(number_of_lags) complexity. *)
    let number_of_lags = List.length attestation_lags in
    let prefix_mask =
      match Bitset.fill ~length:number_of_lags with
      | Ok mask -> mask
      | Error _ -> assert false
    in
    let common_prefix =
      Bitset.inter (Bitset.inter sa attestations) prefix_mask
    in

    (* Extract [number_of_slots] bits at [offset], normalized to position 0 *)
    let extract_data_section bitset ~offset =
      let z = Bitset.to_z bitset in
      let mask =
        Z.(shift_left (pred (shift_left one number_of_slots)) offset)
      in
      let normalized_z = Z.shift_right (Z.logand z mask) offset in
      match Bitset.from_z normalized_z with
      | Ok bs -> bs
      | Error _ ->
          (* unreachable; the previous operations don't make the parameter negative *)
          assert false
    in

    (* Single pass through lags, tracking running counts for offset computation *)
    let rec build_result lag_index ~sa_count ~att_count result result_offset =
      if Compare.Int.(lag_index >= number_of_lags) then result
      else
        let sa_is_set = bitset_mem sa lag_index in
        let att_is_set = bitset_mem attestations lag_index in

        if not (bitset_mem common_prefix lag_index) then
          (* Lag not in both - update counts for whichever is set *)
          build_result
            (lag_index + 1)
            ~sa_count:(if sa_is_set then sa_count + 1 else sa_count)
            ~att_count:(if att_is_set then att_count + 1 else att_count)
            result
            result_offset
        else
          (* Lag is in both - compute offsets from running counts *)
          let sa_offset = number_of_lags + (sa_count * number_of_slots) in
          let att_offset = number_of_lags + (att_count * number_of_slots) in
          let sa_data = extract_data_section sa ~offset:sa_offset in
          let att_data = extract_data_section attestations ~offset:att_offset in
          let common_data = Bitset.inter sa_data att_data in

          if Bitset.is_empty common_data then
            build_result
              (lag_index + 1)
              ~sa_count:(sa_count + 1)
              ~att_count:(att_count + 1)
              result
              result_offset
          else
            let result = bitset_add result lag_index in
            let result_z =
              Z.logor
                (Bitset.to_z result)
                (Z.shift_left (Bitset.to_z common_data) result_offset)
            in
            let result =
              match Bitset.from_z result_z with
              | Ok r -> r
              | Error _ ->
                  (* unreachable; the previous operations don't make the parameter negative *)
                  assert false
            in
            build_result
              (lag_index + 1)
              ~sa_count:(sa_count + 1)
              ~att_count:(att_count + 1)
              result
              (result_offset + number_of_slots)
    in
    build_result 0 ~sa_count:0 ~att_count:0 empty number_of_lags
end

type shard_index = int

module Shard_map = Map.Make (struct
  type t = shard_index

  let compare = Compare.Int.compare
end)

module Accountability = struct
  type attested_slots = t

  module SlotMap = Map.Make (Compare.Int)

  type t = {
    shard_attestations : (Signature.Public_key_hash.Set.t * int) SlotMap.t;
    number_of_slots : int;
  }

  type attestation_status = {
    total_shards : int;
    attested_shards : int;
    attesters : Signature.Public_key_hash.Set.t;
    is_proto_attested : bool;
  }

  let init ~number_of_slots =
    {shard_attestations = SlotMap.empty; number_of_slots}

  (* This function must be called at most once for a given attester; otherwise
     the count will be flawed. *)
  let record_number_of_attested_shards t baker_attested_slots ~delegate
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
                  | None ->
                      Some
                        ( Signature.Public_key_hash.Set.singleton delegate,
                          number_of_baker_shards )
                  | Some (delegate_set, old_number_of_attested_shards) ->
                      Some
                        ( Signature.Public_key_hash.Set.add delegate delegate_set,
                          old_number_of_attested_shards + number_of_baker_shards
                        ))
                map
          | Ok false ->
              (* slot is not attested by baker, nothing to update *)
              map
        in
        iter (slot_index + 1) map
    in
    let shard_attestations = iter 0 t.shard_attestations in
    {t with shard_attestations}

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
    let attesters, number_of_attested_shards =
      match SlotMap.find index t.shard_attestations with
      | None -> (Signature.Public_key_hash.Set.empty, 0)
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
      attesters;
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
