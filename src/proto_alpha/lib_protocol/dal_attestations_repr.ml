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

(* Erros when reading/writing the [dal_content] of an attestation. *)
type dal_indices = Lag_index of int | Slot_index of int

let dal_indices_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Lag_index"
        int31
        (function Lag_index i -> Some i | _ -> None)
        (fun i -> Lag_index i);
      case
        (Tag 1)
        ~title:"Slot_index"
        int31
        (function Slot_index i -> Some i | _ -> None)
        (fun i -> Slot_index i);
    ]

type error +=
  | Dal_invalid_attestation_bitset of Bitset.t
  | Dal_invalid_read_of_attestaion_bitset of dal_indices

let () =
  register_error_kind
    `Permanent
    ~id:"dal_invalid_attestation_bitset"
    ~title:"Dal invalid attestation bitset"
    ~description:"The attestation features an invalid DAL content"
    ~pp:(fun ppf bitset ->
      Format.fprintf
        ppf
        "DAL content of the attestation is %a which is not a valid \
         representation of a DAL attestation."
        Z.pp_print
        (Bitset.to_z bitset))
    Data_encoding.(obj1 (req "bitset" Bitset.encoding))
    (function
      | Dal_invalid_attestation_bitset bitset -> Some bitset | _ -> None)
    (fun bitset -> Dal_invalid_attestation_bitset bitset) ;
  register_error_kind
    `Permanent
    ~id:"dal_invalid_read_of_attestaion_bitset"
    ~title:"Dal invalid read of attestaion bitset"
    ~description:"The DAL attestation bitset was read with invalid indices"
    ~pp:(fun ppf dal_index ->
      match dal_index with
      | Lag_index i ->
          Format.fprintf
            ppf
            "The DAL attestation bitset cannot be read at lag index %d."
            i
      | Slot_index i ->
          Format.fprintf
            ppf
            "The DAL attestation bitset cannot be read at slot index %d."
            i)
    Data_encoding.(obj1 (req "index" dal_indices_encoding))
    (function
      | Dal_invalid_read_of_attestaion_bitset dal_index -> Some dal_index
      | _ -> None)
    (fun dal_index -> Dal_invalid_read_of_attestaion_bitset dal_index)

(* A DAL attestation is made of chunks with 1 bit [is_last] followed by
   [slots_per_chunk] bits to state which slots are attested. *)
let slots_per_chunk = 7

let bits_per_chunk = slots_per_chunk + 1

let max_chunks ~number_of_slots =
  let ceil_div a b = (a + b - 1) / b in
  ceil_div number_of_slots slots_per_chunk

(* [lag_data] contains the information about the representation of a specific
   lag in the attestation.
   We assume that the represented lag is non empty, so [nb_chunks > 0] and
   [offset_start < offset_after] *)
type lag_data = {offset_start : int; offset_after : int; nb_chunks : int}

(* Helper to safely check membership in a bitset, returning false on error. *)
let bitset_mem bitset idx =
  match Bitset.mem bitset idx with Ok b -> b | Error _ -> false

(* Helper to safely add to a bitset, returning the original bitset on error. *)
let bitset_add bitset idx =
  match Bitset.add bitset idx with Ok b -> b | Error _ -> bitset

(* Helper to safely remove from a bitset, returning the original bitset on error. *)
let bitset_remove bitset idx =
  match Bitset.remove bitset idx with Ok b -> b | Error _ -> bitset

(* Check if the attestation at [lag_index] is empty *)
let is_empty_at_lag_index t ~lag_index = not @@ bitset_mem t lag_index

(* Shift all bits at or after [from] by [shift] positions. *)
let shift_bits_after bitset ~from ~shift =
  if Compare.Int.(shift = 0) then bitset
  else
    List.fold_left
      (fun res index ->
        let new_index =
          if Compare.Int.(index >= from) then index + shift else index
        in
        bitset_add res new_index)
      Bitset.empty
      (Bitset.to_list bitset)

(* Scan chunks starting at [start] and return (chunks_count, end_offset). *)
let scan_chunks t ~start ~max_chunks =
  let rec loop nb_chunks offset =
    let open Result_syntax in
    (* The last chunk has to have the [is_last] bit to true. *)
    let* () =
      error_unless
        Compare.Int.(nb_chunks < max_chunks)
        (Dal_invalid_attestation_bitset t)
    in
    let next_offset = offset + bits_per_chunk in
    let is_last = bitset_mem t offset in
    if is_last then return (nb_chunks + 1, next_offset)
    else loop (nb_chunks + 1) next_offset
  in
  loop 0 start

(* Scan the data section from lag 0 up to [lag_index], returning the lag data
   for [lag_index] (if any) and its offset.  *)
let scan_to_lag t ~number_of_slots ~number_of_lags ~lag_index =
  let max_chunks = max_chunks ~number_of_slots in
  let rec scan current_lag_index offset =
    let open Result_syntax in
    let* () =
      error_unless
        Compare.Int.(current_lag_index < number_of_lags)
        (Dal_invalid_attestation_bitset t)
    in
    if bitset_mem t current_lag_index then
      let* nb_chunks, next_offset = scan_chunks t ~start:offset ~max_chunks in
      if Compare.Int.(current_lag_index = lag_index) then
        return
          ( Some {offset_start = offset; offset_after = next_offset; nb_chunks},
            offset )
      else scan (current_lag_index + 1) next_offset
    else if Compare.Int.(current_lag_index = lag_index) then
      return (None, offset)
    else scan (current_lag_index + 1) offset
  in
  scan 0 number_of_lags

let find_lag_data t ~number_of_slots ~number_of_lags ~lag_index =
  Result.map fst (scan_to_lag t ~number_of_slots ~number_of_lags ~lag_index)

(* Return the data offset before [lag_index]. *)
let data_offset_before_lag t ~number_of_slots ~number_of_lags ~lag_index =
  Result.map snd (scan_to_lag t ~number_of_slots ~number_of_lags ~lag_index)

let slot_bit_position ~offset_start ~slot_index =
  let chunk_index = slot_index / slots_per_chunk in
  (* The first bit in each chunk is [is_last] which tells if the chunk is the
     last associated to current lag. *)
  let bit_in_chunk = 1 + (slot_index mod slots_per_chunk) in
  offset_start + (chunk_index * bits_per_chunk) + bit_in_chunk

(* Helper: compute the data bit position for a slot at a given lag_index.
   Returns None if the attestation at lag_index is empty. *)
let compute_data_bit_position t ~number_of_slots ~number_of_lags ~lag_index
    slot_index =
  let open Result_syntax in
  (* Check if the attestation at [lag_index] is non-empty *)
  let is_non_empty = bitset_mem t lag_index in
  if not is_non_empty then return None
  else
    let* lag_data =
      find_lag_data t ~number_of_slots ~number_of_lags ~lag_index
    in
    match lag_data with
    | None -> return_none
    | Some {offset_start; nb_chunks = chunks; _} ->
        let slot_bit_idx = Dal_slot_index_repr.to_int slot_index in
        let chunk_index = slot_bit_idx / slots_per_chunk in
        if Compare.Int.(chunk_index >= chunks) then return_none
        else
          return_some (slot_bit_position ~offset_start ~slot_index:slot_bit_idx)

let is_attested t ~number_of_slots ~number_of_lags ~lag_index slot_index =
  let open Result_syntax in
  let read_attested =
    let* () =
      error_unless
        Compare.Int.(lag_index >= 0 && lag_index < number_of_lags)
        (Dal_invalid_read_of_attestaion_bitset (Lag_index lag_index))
    in
    let slot = Dal_slot_index_repr.to_int slot_index in
    let* () =
      error_unless
        Compare.Int.(slot >= 0 && slot < number_of_slots)
        (Dal_invalid_read_of_attestaion_bitset (Slot_index slot))
    in
    let* data_bit_pos =
      compute_data_bit_position
        t
        ~number_of_slots
        ~number_of_lags
        ~lag_index
        slot_index
    in
    match data_bit_pos with
    | None -> return false
    | Some data_bit_pos -> return (bitset_mem t data_bit_pos)
  in
  match read_attested with Ok is_attested -> is_attested | Error _ -> false

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
  let open Result_syntax in
  (* Write chunk data for a lag at [offset_start] into [bitset].
   [slots] is a list of attested slot indices.
   [chunks_needed] is the number of chunks to allocate.
   Sets the [is_last] bit on the last chunk and each slot's bit.
   Does NOT clear the potentially already existing [is_last] bits.
   Does NOT set the prefix bit — the caller must do that. *)
  let write_lag_data bitset ~offset_start ~chunks_needed slots =
    let result =
      bitset_add bitset (offset_start + ((chunks_needed - 1) * bits_per_chunk))
    in
    List.fold_left
      (fun acc slot_index ->
        bitset_add acc (slot_bit_position ~offset_start ~slot_index))
      result
      slots
  in
  let is_non_empty_at idx = bitset_mem bitset idx in
  let slot_idx = Dal_slot_index_repr.to_int slot_index in
  let chunk_index = slot_idx / slots_per_chunk in
  let chunks_needed = chunk_index + 1 in
  let commit_result =
    let* () =
      error_unless
        Compare.Int.(lag_index >= 0 && lag_index < number_of_lags)
        (Dal_invalid_read_of_attestaion_bitset (Lag_index lag_index))
    in
    let* () =
      error_unless
        Compare.Int.(slot_idx >= 0 && slot_idx < number_of_slots)
        (Dal_invalid_read_of_attestaion_bitset (Slot_index slot_idx))
    in
    if is_non_empty_at lag_index then
      let* lag_data =
        find_lag_data bitset ~number_of_slots ~number_of_lags ~lag_index
      in
      match lag_data with
      | None ->
          tzfail (Dal_invalid_read_of_attestaion_bitset (Lag_index lag_index))
      | Some {offset_start; nb_chunks; offset_after} ->
          if Compare.Int.(chunk_index < nb_chunks) then
            (* Fast path: slot is within existing chunks. *)
            return
              (bitset_add
                 bitset
                 (slot_bit_position ~offset_start ~slot_index:slot_idx))
          else
            (* Extend the lag with extra chunks. *)
            let extra_chunks = chunks_needed - nb_chunks in
            let shift = extra_chunks * bits_per_chunk in
            let result = shift_bits_after bitset ~from:offset_after ~shift in
            let result =
              (* The previously last chunk is not the last anymore. *)
              bitset_remove
                result
                (offset_start + ((nb_chunks - 1) * bits_per_chunk))
            in
            return
              (write_lag_data result ~offset_start ~chunks_needed [slot_idx])
    else
      (* Lag is empty: insert its chunks in order. *)
      let* insertion_point =
        data_offset_before_lag
          bitset
          ~number_of_slots
          ~number_of_lags
          ~lag_index
      in
      let shift = chunks_needed * bits_per_chunk in
      let result = shift_bits_after bitset ~from:insertion_point ~shift in
      (* The lag index is not empty anymore, so one has to update the prefix. *)
      let result = bitset_add result lag_index in
      return
        (write_lag_data
           result
           ~offset_start:insertion_point
           ~chunks_needed
           [slot_idx])
  in
  match commit_result with Ok bitset -> bitset | Error _ -> bitset

let occupied_size_in_bits = Bitset.occupied_size_in_bits

let expected_max_size_in_bits ~number_of_slots ~number_of_lags =
  (* [number_of_lags] for the prefix and [number_of_lags] chunks of 8 bits. *)
  let max_chunks = max_chunks ~number_of_slots in
  number_of_lags + (number_of_lags * max_chunks * bits_per_chunk)

let weight t = Bitset.cardinal t

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
    let published_level_map =
      List.fold_left_i
        (fun lag_index published_level_map lag ->
          let published_level_opt = Raw_level_repr.sub attested_level lag in
          match published_level_opt with
          | None -> published_level_map
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
              published_level_map)
        t.shard_attestations
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

  let empty_history = Raw_level_repr.Map.empty

  (** {2 Bitset-based storage types}

      These types support a more compact storage format where attestations
      are represented as bitsets indexed by delegate position in the cycle's
      stake distribution. *)

  (** A bitset representing attestations for a single slot.
      Bit i is set if the delegate at position i (in the cycle's stake
      distribution) attested this slot. *)
  type slot_attestation_bitset = Bitset.t

  (** Compressed representation of the attestation history, using bitsets
      instead of explicit attester sets. Maps published levels to slot-indexed
      bitsets, where each bitset encodes which delegates attested a given slot
      based on their position in the committee ordering. *)
  type packed_history = slot_attestation_bitset SlotMap.t PublishedLevelMap.t

  (** [attesters_to_bitset ~ordered_delegates ~attesters] converts a set
    of attester public key hashes to a bitset representation.

    Each bit in the result corresponds to a delegate's position in the cycle's
    stake distribution. Bit [i] is set if the delegate at position [i] is in the
    [attesters] set.

    @param ~ordered_delegates List of delegates in the committee
    @param ~attesters The set of delegates who attested
    @return A bitset where bit [i] is set if the [i]th delegate attested *)
  let attesters_to_bitset ~ordered_delegates ~attesters =
    (* Build index map: delegate -> index *)
    let delegate_to_index =
      List.fold_left_i
        (fun i acc delegate -> Signature.Public_key_hash.Map.add delegate i acc)
        Signature.Public_key_hash.Map.empty
        ordered_delegates
    in
    (* Set bits for attesters *)
    Signature.Public_key_hash.Set.fold
      (fun attester acc ->
        match Signature.Public_key_hash.Map.find attester delegate_to_index with
        | Some i -> (
            (* Set bit at index i *)
            match Bitset.add acc i with
            | Ok bs -> bs
            | Error _ ->
                (* index cannot be negative *)
                assert false)
        | None ->
            (* by construction only bakers in the distribution can attest *)
            assert false)
      attesters
      Bitset.empty

  (** [bitset_to_attesters ~ordered_delegates ~bitset] converts a bitset
    representation back to a set of attester public key hashes.

    Each set bit in the bitset corresponds to a delegate at that position in the
    cycle's stake distribution.

    @param ~ordered_delegates list of delegates in the committee
    @param bitset The bitset where bit [i] indicates if the [i]th delegate attested
    @return A set of public key hashes of attesters *)
  let bitset_to_attesters ~ordered_delegates ~bitset =
    (* Build set from bitset indices *)
    List.fold_left_i
      (fun i acc delegate ->
        let is_set =
          match Bitset.mem bitset i with
          | Ok b -> b
          | Error _ -> (* [i] cannot be negative *) assert false
        in
        if is_set then Signature.Public_key_hash.Set.add delegate acc else acc)
      Signature.Public_key_hash.Set.empty
      ordered_delegates

  (** [attested_shards ~delegate_to_shard_count ~attesters] calculates the
      total number of attested shards for a given set of attesters.

      For each attester in [attesters], looks up its shard count in
      [delegate_to_shard_count] and sums them.

      @param delegate_to_shard_count map from delegate public key hash to the
        number of shards assigned to that delegate
      @param attesters the set of delegates who attested
      @return the total number of attested shards *)
  let attested_shards ~delegate_to_shard_count ~attesters =
    (* Sum shards for delegates with bit set *)
    Signature.Public_key_hash.Set.fold
      (fun delegate acc ->
        let shard_count =
          match
            Signature.Public_key_hash.Map.find delegate delegate_to_shard_count
          with
          | None -> 0
          | Some n -> n
        in
        acc + shard_count)
      attesters
      0

  (** [bitset_to_attestation_status ~threshold ~number_of_shards ~bitset
      ~ordered_delegates ~delegate_to_shard_count] reconstructs an
      {!attestation_status} from a bitset representation and other necessary
      information. *)
  let bitset_to_attestation_status ~threshold ~number_of_shards ~bitset
      ~ordered_delegates ~delegate_to_shard_count =
    let attesters = bitset_to_attesters ~ordered_delegates ~bitset in
    let attested_shards = attested_shards ~delegate_to_shard_count ~attesters in
    let is_proto_attested =
      is_threshold_reached ~threshold ~number_of_shards ~attested_shards
    in
    {
      total_shards = number_of_shards;
      attested_shards;
      attesters;
      is_proto_attested;
    }

  (** [attestation_status_to_bitset ~ordered_delegates attestation_status]
      converts an {!attestation_status} to its bitset representation by
      extracting the attester set and encoding it as a bitset according to the
      delegate ordering in [ordered_delegates]. *)
  let attestation_status_to_bitset ~ordered_delegates attestation_status =
    let {attesters; _} = attestation_status in
    attesters_to_bitset ~ordered_delegates ~attesters

  (** [levels_of_packed_history h] returns the list of published levels present
      in the packed history [h]. *)
  let levels_of_packed_history h =
    Raw_level_repr.Map.fold (fun level _ acc -> level :: acc) h []

  (** Precondition: all published levels in [history] must have an entry in
      [committee_level_map] and the corresponding committee level must have an
      entry in [ordered_delegates_for_level].  *)
  let unpack_history ~delegate_to_shard_count ~ordered_delegates_for_level
      ~threshold ~number_of_shards ~committee_level_map stored_history =
    PublishedLevelMap.(
      fold
        (fun published_level history_of_level decoded_history ->
          let shard_assignment_level =
            match
              Raw_level_repr.Map.find published_level committee_level_map
            with
            | None ->
                (* The precondition ensures that we always have an entry for
                   levels of [committee_level_map] *)
                assert false
            | Some level -> level
          in
          let ordered_delegates =
            match ordered_delegates_for_level ~shard_assignment_level with
            | None ->
                (* The precondition ensures that we always have an entry for
                   levels of the [stored_history] *)
                assert false
            | Some ordered_delegates -> ordered_delegates
          in
          let delegate_to_shard_count =
            match
              Raw_level_repr.Map.find
                shard_assignment_level
                delegate_to_shard_count
            with
            | None -> Signature.Public_key_hash.Map.empty
            | Some map -> map
          in
          let decoded_history_of_level =
            Dal_slot_index_repr.Map.(
              fold
                (fun slot_index bitset acc ->
                  let attestation_status =
                    bitset_to_attestation_status
                      ~threshold
                      ~number_of_shards
                      ~bitset
                      ~ordered_delegates
                      ~delegate_to_shard_count
                  in
                  add slot_index attestation_status acc)
                history_of_level
                empty)
          in
          add published_level decoded_history_of_level decoded_history)
        stored_history
        empty)

  (** [pack_history ~ordered_delegates_for_level
      ~committee_level_of_published_level history] converts an unpacked
      {!history} into a {!packed_history} by replacing each slot's attester set
      with its bitset representation.

      Precondition: all published levels in [history] must have an entry in
      [committee_level_map] and the corresponding committee level must have an
      entry in [ordered_delegates_for_level].  *)
  let pack_history ~ordered_delegates_for_level ~committee_level_map history =
    PublishedLevelMap.(
      fold
        (fun published_level history_of_level history ->
          let shard_assignment_level =
            match
              Raw_level_repr.Map.find published_level committee_level_map
            with
            | None ->
                (* The precondition ensures that we always have an entry for
                 levels of [committee_level_map] *)
                assert false
            | Some level -> level
          in
          let ordered_delegates =
            match ordered_delegates_for_level ~shard_assignment_level with
            | None ->
                (* The precondition ensures that we always have an entry for
                   levels of [history] *)
                assert false
            | Some ordered_delegates -> ordered_delegates
          in
          let encoded_history_of_level =
            Dal_slot_index_repr.Map.(
              fold
                (fun slot_index status acc ->
                  let attestation_status =
                    attestation_status_to_bitset ~ordered_delegates status
                  in
                  add slot_index attestation_status acc)
                history_of_level
                empty)
          in
          add published_level encoded_history_of_level history)
        history
        empty)

  (** Encoding for a single slot's attestation bitset. *)
  let slot_attestation_encoding = Bitset.encoding

  (** Encoding for a single level's attestation data: a list of
      (slot_index, bitset) pairs. *)
  let level_attestations_encoding =
    let open Data_encoding in
    conv
      (fun m -> Dal_slot_index_repr.Map.bindings m)
      (fun bindings ->
        List.fold_left
          (fun m (k, v) -> Dal_slot_index_repr.Map.add k v m)
          Dal_slot_index_repr.Map.empty
          bindings)
      (list (tup2 Dal_slot_index_repr.encoding slot_attestation_encoding))

  (** Encoding for {!packed_history}: a list of
      (published_level, level_attestations) pairs. *)
  let packed_history_encoding =
    let open Data_encoding in
    conv
      (fun m -> Raw_level_repr.Map.bindings m)
      (fun bindings ->
        List.fold_left
          (fun m (k, v) -> Raw_level_repr.Map.add k v m)
          Raw_level_repr.Map.empty
          bindings)
      (list (tup2 Raw_level_repr.encoding level_attestations_encoding))
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

  let attesters_to_bitset = Accountability.attesters_to_bitset

  let bitset_to_attesters = Accountability.bitset_to_attesters

  let attested_shards = Accountability.attested_shards
end
