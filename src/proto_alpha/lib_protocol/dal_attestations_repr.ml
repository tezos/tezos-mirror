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
