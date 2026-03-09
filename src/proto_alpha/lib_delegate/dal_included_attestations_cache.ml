(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Protocol.Alpha_context
open Baking_state_types

type error +=
  | Attestation_deduplication_cache_internal_error of {
      message : string;
      attested_level : int32;
      head_level : int32;
    }
  | Committee_not_found of {committee_level : int32}
  | Slot_to_delegate_lookup_failed of {slot : Slot.t; committee_level : int32}

let () =
  register_error_kind
    `Permanent
    ~id:"dal_included_attestations_cache.cache_internal_error"
    ~title:"DAL attestation deduplication cache internal error"
    ~description:
      "An internal inconsistency was detected in the DAL attestation \
       deduplication cache"
    ~pp:(fun ppf (message, attested_level, head_level) ->
      Format.fprintf
        ppf
        "DAL attestation deduplication cache internal error: %s \
         (attested_level=%ld, head_level=%ld)"
        message
        attested_level
        head_level)
    Data_encoding.(
      obj3
        (req "message" string)
        (req "attested_level" int32)
        (req "head_level" int32))
    (function
      | Attestation_deduplication_cache_internal_error
          {message; attested_level; head_level} ->
          Some (message, attested_level, head_level)
      | _ -> None)
    (fun (message, attested_level, head_level) ->
      Attestation_deduplication_cache_internal_error
        {message; attested_level; head_level}) ;
  register_error_kind
    `Permanent
    ~id:"dal_included_attestations_cache.committee_not_found"
    ~title:"Committee not found for level"
    ~description:"Committee information not available for the requested level"
    ~pp:(fun ppf committee_level ->
      Format.fprintf ppf "Committee not found for level %ld" committee_level)
    Data_encoding.(obj1 (req "committee_level" int32))
    (function
      | Committee_not_found {committee_level} -> Some committee_level
      | _ -> None)
    (fun committee_level -> Committee_not_found {committee_level}) ;
  register_error_kind
    `Permanent
    ~id:"dal_included_attestations_cache.slot_to_delegate_lookup_failed"
    ~title:"Failed to resolve slot to delegate"
    ~description:"Could not map consensus slot to delegate public key hash"
    ~pp:(fun ppf (slot, committee_level) ->
      Format.fprintf
        ppf
        "Failed to resolve slot %a to delegate at level %ld"
        Slot.pp
        slot
        committee_level)
    Data_encoding.(
      obj2 (req "slot" Slot.encoding) (req "committee_level" int32))
    (function
      | Slot_to_delegate_lookup_failed {slot; committee_level} ->
          Some (slot, committee_level)
      | _ -> None)
    (fun (slot, committee_level) ->
      Slot_to_delegate_lookup_failed {slot; committee_level})

module Level_map =
  Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
    (struct
      include Int32

      let hash = Hashtbl.hash
    end)

module SlotSet = Set.Make (Int)
module SlotMap = Map.Make (Int)

type slot_attestation = {
  attested_level : int32;
      (** The consensus level at which this slot was first seen attested
          on-chain. *)
  block_hashes : Block_hash.t list;
      (** Hash(es) of the block(s) at [attested_level] that included the
          attestation. *)
}

(** Published level indexed map of per-slot attestation info. *)
type published_level_cache = slot_attestation SlotMap.t Level_map.t

(** Committee lookup function: given a slot, returns the delegate pkh. *)
type committee_lookup = Slot.t -> Signature.Public_key_hash.t option

type t = {
  cache : published_level_cache Delegate_id.Table.t;
  committees : committee_lookup Level_map.t;
  attestation_lags : int list;
  number_of_slots : int;
}

let create_delegate_table () = Delegate_id.Table.create 10

(** [get_or_create_published_level_cache t ~delegate_id] returns the published
    levels map for the given delegate, creating an empty one if needed. *)
let get_or_create_published_level_cache t ~delegate_id =
  match Delegate_id.Table.find_opt t.cache delegate_id with
  | Some published_level_cache -> published_level_cache
  | None ->
      let max_lag = List.fold_left max 0 t.attestation_lags in
      let published_level_cache = Level_map.create (3 * max_lag) in
      Delegate_id.Table.replace t.cache delegate_id published_level_cache ;
      published_level_cache

let set_committee t ~level lookup_fn =
  Level_map.replace t.committees level lookup_fn

(** [get_committee t ~level] retrieves the committee lookup function for the
    given level from the cache. *)
let get_committee t ~level = Level_map.find_opt t.committees level

(** For each slot, the decision
    depends on how "deep" the attesting block is relative to [~head_level]:
    - depth >= 2: the attesting block is final by Tenderbake - filter the
      slot out unconditionally;
    - depth 0 or 1: the attesting block is not yet final — filter the slot
      out only if one of the recorded block hashes sits on the current chain
      (matched against [~head_hash] or [~predecessor_hash]), respectively;
    - depth < 0 (attested_level > head_level): returns an error. *)
let filter_attestable_slots t ~delegate_id ~published_level ~attestable_slots
    ~head_level ~head_hash ~predecessor_hash =
  let open Result_syntax in
  match Delegate_id.Table.find_opt t.cache delegate_id with
  | None -> return attestable_slots
  | Some published_level_cache -> (
      match Level_map.find_opt published_level_cache published_level with
      | None -> return attestable_slots
      | Some slot_map ->
          SlotSet.fold_e
            (fun slot_index acc ->
              match SlotMap.find_opt slot_index slot_map with
              | None -> return @@ SlotSet.add slot_index acc
              | Some {attested_level; block_hashes} ->
                  let level_diff = Int32.sub head_level attested_level in
                  if level_diff >= 2l then return acc
                  else if level_diff < 0l then
                    tzfail
                      (Attestation_deduplication_cache_internal_error
                         {
                           message = "attested_level > head_level";
                           attested_level;
                           head_level;
                         })
                  else
                    let target_block_hash =
                      if level_diff = 0l then head_hash else predecessor_hash
                    in
                    return
                    @@
                    if
                      List.exists
                        (Block_hash.equal target_block_hash)
                        block_hashes
                    then acc
                    else SlotSet.add slot_index acc)
            attestable_slots
            SlotSet.empty)

(** [extract_attested_pairs] decodes the attestation bitset and returns one
    entry per [published_level] with the set of all slot indices attested at
    that level. Entries for lag positions where no slots are attested are
    omitted. *)
let extract_attested_pairs ~attestation_lags ~number_of_slots ~number_of_lags
    ~attested_level dal_content =
  let open Result_syntax in
  let+ decoded =
    Environment.wrap_tzresult
    @@ Dal.Attestations.decode dal_content ~number_of_slots ~number_of_lags
  in
  let attestation_lags_array = Array.of_list attestation_lags in
  List.filter_map
    (fun Dal.Attestations.{lag_index; slot_indices} ->
      match attestation_lags_array.(lag_index) with
      | attestation_lag ->
          let published_level =
            Int32.(sub attested_level (of_int attestation_lag))
          in
          let slots = SlotSet.of_list slot_indices in
          if SlotSet.is_empty slots then None else Some (published_level, slots)
      | exception Invalid_argument _ -> None)
    decoded

(** Extract DAL attestations from consensus operations using protocol data.
    Uses the committee lookup to resolve attestation slots to delegate PKHs
    without requiring operation receipts. *)
let extract_dal_attestations_from_operations t ~attested_level operations =
  let open Result_syntax in
  let number_of_lags = List.length t.attestation_lags in
  let number_of_slots = t.number_of_slots in
  let acc = create_delegate_table () in
  (* Attestations at level L attest the predecessor at level L-1 *)
  let committee_level = Int32.pred attested_level in
  (* Get the committee lookup function for this level *)
  match get_committee t ~level:committee_level with
  | None -> tzfail (Committee_not_found {committee_level})
  | Some slot_to_pkh ->
      let* () =
        List.iter_e
          (fun op ->
            match op.protocol_data with
            | Operation_data
                {
                  contents =
                    Single
                      (Attestation
                         {
                           consensus_content;
                           dal_content = Some {attestations};
                           _;
                         });
                  _;
                } -> (
                match slot_to_pkh consensus_content.slot with
                | None ->
                    tzfail
                      (Slot_to_delegate_lookup_failed
                         {slot = consensus_content.slot; committee_level})
                | Some delegate_pkh ->
                    let delegate_id = Delegate_id.of_pkh delegate_pkh in
                    let* attested_pairs =
                      extract_attested_pairs
                        ~attestation_lags:t.attestation_lags
                        ~number_of_slots
                        ~number_of_lags
                        ~attested_level
                        attestations
                    in
                    Delegate_id.Table.replace acc delegate_id attested_pairs ;
                    return_unit)
            | Operation_data
                {contents = Single (Attestations_aggregate {committee; _}); _}
              ->
                List.iter_e
                  (fun (slot, dal_content_opt) ->
                    match dal_content_opt with
                    | None -> return_unit
                    | Some {attestations} -> (
                        match slot_to_pkh slot with
                        | None ->
                            tzfail
                              (Slot_to_delegate_lookup_failed
                                 {slot; committee_level})
                        | Some delegate_pkh ->
                            let delegate_id = Delegate_id.of_pkh delegate_pkh in
                            let* attested_pairs =
                              extract_attested_pairs
                                ~attestation_lags:t.attestation_lags
                                ~number_of_slots
                                ~number_of_lags
                                ~attested_level
                                attestations
                            in
                            Delegate_id.Table.replace
                              acc
                              delegate_id
                              attested_pairs ;
                            return_unit))
                  committee
            | _ -> return_unit)
          operations
      in
      return acc

(** [update_slot_attestation ~attested_level ~block_hash ~grandparent
    existing_slot_attestation_opt] returns the updated [slot_attestation] for
    a single slot, preserving the invariant that the cache keeps the earliest
    on-chain attestation seen. *)
let update_slot_attestation ~attested_level ~block_hash ~predecessor_hash
    ~grandparent = function
  | None -> {attested_level; block_hashes = [block_hash]}
  | Some existing_slot_attestation ->
      let level_diff =
        Int32.sub attested_level existing_slot_attestation.attested_level
      in
      if level_diff < 0l then
        (* Out-of-order block during a reorg - replace. *)
        {attested_level; block_hashes = [block_hash]}
      else if level_diff = 0l then
        (* Competing proposals at the same level - keep both hashes. *)
        {
          attested_level;
          block_hashes = block_hash :: existing_slot_attestation.block_hashes;
        }
      else if level_diff = 1l then
        (* Depth 1, not yet final - check via [predecessor_hash] whether
           the existing entry is on the current chain. *)
        if
          List.exists
            (Block_hash.equal predecessor_hash)
            existing_slot_attestation.block_hashes
        then
          (* Same chain — keep the earlier entry. *)
          existing_slot_attestation
        else
          (* Different branch — replace:
             A - B  (attests P)
               \ B' (predecessor) - C' (new block, attests P). *)
          {attested_level; block_hashes = [block_hash]}
      else if level_diff = 2l then
        (* Depth 2, Tenderbake finality boundary - check via [grandparent]
           whether the existing entry is on the canonical chain. *)
        if
          List.exists
            (Block_hash.equal grandparent)
            existing_slot_attestation.block_hashes
        then
          (* On the canonical chain — keep:
             A - B (grandparent, attests P) - C - D (new block, attests P). *)
          {
            attested_level = existing_slot_attestation.attested_level;
            block_hashes = [grandparent];
          }
        else
          (* On an abandoned fork — replace:
             A - B  (attests P)
               \ B' (grandparent) - C' - D' (new block, attests P). *)
          {attested_level; block_hashes = [block_hash]}
      else
        (* Depth > 2: only [predecessor_hash] (depth 1) and [grandparent]
           (depth 2) are available, so chain membership at this depth
           cannot be verified - replace with the fresh attestation. *)
        {attested_level; block_hashes = [block_hash]}

let update_from_proposal t ~attested_level ~block_hash ~predecessor_hash
    ~grandparent ~operations =
  let open Result_syntax in
  let+ attestations =
    extract_dal_attestations_from_operations t ~attested_level operations
  in
  Delegate_id.Table.iter
    (fun delegate_id attested_pairs ->
      let published_level_cache =
        get_or_create_published_level_cache t ~delegate_id
      in
      List.iter
        (fun (published_level, slots) ->
          let existing_slot_map =
            match Level_map.find_opt published_level_cache published_level with
            | None -> SlotMap.empty
            | Some slot_map -> slot_map
          in
          let updated_slot_map =
            SlotSet.fold
              (fun slot_index slot_map ->
                let existing_slot_attestation_opt =
                  SlotMap.find_opt slot_index slot_map
                in
                let updated_slot_attestation =
                  update_slot_attestation
                    ~attested_level
                    ~block_hash
                    ~predecessor_hash
                    ~grandparent
                    existing_slot_attestation_opt
                in
                SlotMap.add slot_index updated_slot_attestation slot_map)
              slots
              existing_slot_map
          in
          Level_map.replace
            published_level_cache
            published_level
            updated_slot_map)
        attested_pairs)
    attestations

let create ~attestation_lags ~number_of_slots =
  let max_lag = List.fold_left max 0 attestation_lags in
  {
    cache = create_delegate_table ();
    committees = Level_map.create (3 * max_lag);
    attestation_lags;
    number_of_slots;
  }
