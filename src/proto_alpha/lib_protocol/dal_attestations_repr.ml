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
           match Bitset.mem t i with
           | Ok b -> b
           | Error _ -> (* unreachable: [i] is non-negative *) assert false)
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
      | Error _ ->
          (* unreachable: [number_of_lags] is non-negative *) assert false
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

module Accountability = struct
  type attested_slots = t

  module SlotMap = Dal_slot_index_repr.Map
  module PublishedLevelMap = Raw_level_repr.Map
  (* the keys represent published levels *)

  type slot_attestation_info = {
    attesters : Signature.Public_key_hash.Set.t;
    attested_shards_count : int;
  }

  type t = {
    shard_attestations : slot_attestation_info SlotMap.t PublishedLevelMap.t;
    number_of_slots : int;
    number_of_lags : int;
  }

  let init ~number_of_slots ~number_of_lags =
    {
      shard_attestations = PublishedLevelMap.empty;
      number_of_slots;
      number_of_lags;
    }

  let get_shard_attestations t = t.shard_attestations

  let record_number_of_attested_shards t ~number_of_slots ~attestation_lag ~lags
      ~delegate ~attested_level delegate_attested_slots
      committee_level_to_shard_count =
    let update published_level_map ~published_level ~slot_index
        number_of_delegate_shards =
      PublishedLevelMap.update
        published_level
        (function
          | None ->
              Some
                (SlotMap.singleton
                   slot_index
                   {
                     attesters = Signature.Public_key_hash.Set.singleton delegate;
                     attested_shards_count = number_of_delegate_shards;
                   })
          | Some slot_map ->
              Some
                (SlotMap.update
                   slot_index
                   (function
                     | None ->
                         Some
                           {
                             attesters =
                               Signature.Public_key_hash.Set.singleton delegate;
                             attested_shards_count = number_of_delegate_shards;
                           }
                     | Some
                         ({
                            attesters;
                            attested_shards_count =
                              old_number_of_attested_shards;
                          } as v) ->
                         if Signature.Public_key_hash.Set.mem delegate attesters
                         then Some v
                         else
                           Some
                             {
                               attesters =
                                 Signature.Public_key_hash.Set.add
                                   delegate
                                   attesters;
                               attested_shards_count =
                                 old_number_of_attested_shards
                                 + number_of_delegate_shards;
                             })
                   slot_map))
        published_level_map
    in
    let number_of_lags = List.length lags in
    let _number_of_lags, published_level_map =
      List.fold_left
        (fun (lag_index, published_level_map) lag ->
          let published_level_opt = Raw_level_repr.sub attested_level lag in
          match published_level_opt with
          | None -> (lag_index + 1, published_level_map)
          | Some published_level ->
              let committee_level =
                Raw_level_repr.add published_level (attestation_lag - 1)
              in
              let published_level_map =
                List.fold_left
                  (fun published_level_map slot_index ->
                    let attested =
                      is_attested
                        delegate_attested_slots
                        ~number_of_slots
                        ~number_of_lags
                        ~lag_index
                        slot_index
                    in
                    let number_of_delegate_shards =
                      match
                        Raw_level_repr.Map.find
                          committee_level
                          committee_level_to_shard_count
                      with
                      | None -> 0
                      | Some v -> v
                    in
                    if attested && Compare.Int.(number_of_delegate_shards > 0)
                    then
                      update
                        published_level_map
                        ~published_level
                        ~slot_index
                        number_of_delegate_shards
                    else published_level_map)
                  published_level_map
                  (Dal_slot_index_repr.all_slots ~number_of_slots)
              in
              (lag_index + 1, published_level_map))
        (0, t.shard_attestations)
        lags
    in
    {t with shard_attestations = published_level_map}

  (* Given a slot encoded as [number_of_shards] shards and for which
     [number_of_attested_shards] are attested by the bakers. The slot is
     declared as attested_slots IFF at least [threshold] % of the total shards
     are attested by bakers.

     On rationals, the condition above means:

       number_of_attested_shards / number_of_shards >= threshold / 100,

     which is equivalent, on rationals, to:

       number_of_attested_shards >= (threshold * number_of_shards) / 100

     Below we do the computation of the right-hand side,
     [threshold_number_of_shards], on integers.  Therefore, the real threshold
     is actually smaller, namely [100 * threshold_number_of_shards /
     number_of_shards]. For instance, for protocol T parameters, it is 63.87%
     instead of 64%. *)
  let is_threshold_reached ~threshold ~number_of_shards ~attested_shards =
    Compare.Int.(attested_shards >= threshold * number_of_shards / 100)

  type attestation_status = {
    total_shards : int;
    attested_shards : int;
    attesters : Signature.Public_key_hash.Set.t;
    is_proto_attested : bool;
  }

  type history =
    attestation_status Dal_slot_index_repr.Map.t Raw_level_repr.Map.t

  let attestation_status_encoding =
    let open Data_encoding in
    conv
      (fun {total_shards; attested_shards; attesters; is_proto_attested} ->
        ( total_shards,
          attested_shards,
          Signature.Public_key_hash.Set.elements attesters,
          is_proto_attested ))
      (fun (total_shards, attested_shards, attesters_list, is_proto_attested) ->
        {
          total_shards;
          attested_shards;
          attesters = Signature.Public_key_hash.Set.of_list attesters_list;
          is_proto_attested;
        })
      (obj4
         (req "total_shards" int31)
         (req "attested_shards" int31)
         (req "attesters" (list Signature.Public_key_hash.encoding))
         (req "is_proto_attested" bool))

  let level_info_encoding =
    let open Data_encoding in
    conv
      (fun m -> Dal_slot_index_repr.Map.bindings m)
      (fun bindings ->
        List.fold_left
          (fun m (k, v) -> Dal_slot_index_repr.Map.add k v m)
          Dal_slot_index_repr.Map.empty
          bindings)
      (list (tup2 Dal_slot_index_repr.encoding attestation_status_encoding))

  let history_encoding =
    let open Data_encoding in
    conv
      (fun m -> Raw_level_repr.Map.bindings m)
      (fun bindings ->
        List.fold_left
          (fun m (k, v) -> Raw_level_repr.Map.add k v m)
          Raw_level_repr.Map.empty
          bindings)
      (list (tup2 Raw_level_repr.encoding level_info_encoding))

  let empty_history = Raw_level_repr.Map.empty
end

module Dal_dependent_signing = struct
  module HashModule =
    Blake2B.Make
      (Base58)
      (struct
        let name = "DAL attestations hash"

        let title = "DAL attestations hash"

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
