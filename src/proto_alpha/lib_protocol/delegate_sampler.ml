(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
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

type error += Invalid_slot of {level : Level_repr.t; slot : Slot_repr.t}

let () =
  register_error_kind
    `Permanent
    ~id:"validate.invalid_slot"
    ~title:"Invalid slot"
    ~description:"The provided slot is not valid."
    ~pp:(fun ppf (level, slot) ->
      Format.fprintf
        ppf
        "Cannot provide the delegate for slot %a at level %a."
        Slot_repr.pp
        slot
        Level_repr.pp
        level)
    Data_encoding.(
      obj2 (req "level" Level_repr.encoding) (req "slot" Slot_repr.encoding))
    (function Invalid_slot {level; slot} -> Some (level, slot) | _ -> None)
    (fun (level, slot) -> Invalid_slot {level; slot})

module Delegate_sampler_state = struct
  module Cache_client = struct
    type cached_value = Delegate_consensus_key.pk Sampler.t

    let namespace = Cache_repr.create_namespace "sampler_state"

    let cache_index = 2

    let value_of_identifier ctxt identifier =
      let cycle = Cycle_repr.of_string_exn identifier in
      Storage.Delegate_sampler_state.get ctxt cycle
  end

  module Cache = (val Cache_repr.register_exn (module Cache_client))

  let identifier_of_cycle cycle = Format.asprintf "%a" Cycle_repr.pp cycle

  (* that's symbolic: 1 cycle = 1 entry *)
  let size = 1

  let init ctxt cycle sampler_state =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let* ctxt = Storage.Delegate_sampler_state.init ctxt cycle sampler_state in
    let*? ctxt = Cache.update ctxt id (Some (sampler_state, size)) in
    return ctxt

  let get ctxt cycle =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let* ctxt, v_opt = Cache.find ctxt id in
    match v_opt with
    | None ->
        let* v = Storage.Delegate_sampler_state.get ctxt cycle in
        let*? ctxt = Cache.update ctxt id (Some (v, size)) in
        return (ctxt, v)
    | Some v -> return (ctxt, v)

  let remove ctxt cycle =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let*? ctxt = Cache.update ctxt id None in
    let*! ctxt = Storage.Delegate_sampler_state.remove ctxt cycle in
    return ctxt
end

module Delegate_stake_info = struct
  module Cache_client = struct
    type cached_value = Raw_context.stake_info

    let namespace = Cache_repr.create_namespace "stake_info"

    let cache_index = 3

    let value_of_identifier ctxt identifier =
      let cycle = Cycle_repr.of_string_exn identifier in
      Storage.Delegate_stake_info.get ctxt cycle
  end

  module Cache = (val Cache_repr.register_exn (module Cache_client))

  let identifier_of_cycle cycle = Format.asprintf "%a" Cycle_repr.pp cycle

  (* that's symbolic: 1 cycle = 1 entry *)
  let size = 1

  let init ctxt cycle stake_info =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let* ctxt = Storage.Delegate_stake_info.init ctxt cycle stake_info in
    let*? ctxt = Cache.update ctxt id (Some (stake_info, size)) in
    return ctxt

  let add ctxt cycle stake_info =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let*! ctxt = Storage.Delegate_stake_info.add ctxt cycle stake_info in
    let*? ctxt = Cache.update ctxt id (Some (stake_info, size)) in
    return ctxt

  let find ctxt cycle =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let* ctxt, v_opt = Cache.find ctxt id in
    match v_opt with
    | Some v -> return (ctxt, Some v)
    | None -> (
        let* v = Storage.Delegate_stake_info.find ctxt cycle in
        match v with
        | None -> return (ctxt, None)
        | Some v ->
            let*? ctxt = Cache.update ctxt id (Some (v, size)) in
            return (ctxt, Some v))

  let remove ctxt cycle =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let*? ctxt = Cache.update ctxt id None in
    let*! ctxt = Storage.Delegate_stake_info.remove ctxt cycle in
    return ctxt
end

module Random = struct
  (* [init_random_state] initialize a random sequence drawing state
     that's unique for a given (seed, level, index) triple. Elements
     from this sequence are drawn using [take_int64], updating the
     state for the next draw. The initial state is the Blake2b hash of
     the three randomness sources, and an offset set to zero
     (indicating that zero bits of randomness have been
     consumed). When drawing random elements, bits are extracted from
     the state until exhaustion (256 bits), at which point the state
     is rehashed and the offset reset to 0. *)

  let init_random_state seed level index =
    ( Raw_hashes.blake2b
        (Data_encoding.Binary.to_bytes_exn
           Data_encoding.(tup3 Seed_repr.seed_encoding int32 int32)
           (seed, level.Level_repr.cycle_position, Int32.of_int index)),
      0 )

  let take_int64 bound state =
    let drop_if_over =
      (* This function draws random values in [0-(bound-1)] by drawing
         in [0-(2^63-1)] (64-bit) and computing the value modulo
         [bound]. For the application of [mod bound] to preserve
         uniformity, the input space must be of the form
         [0-(n*bound-1)]. We enforce this by rejecting 64-bit samples
         above this limit (in which case, we draw a new 64-sample from
         the sequence and try again). *)
      Int64.sub Int64.max_int (Int64.rem Int64.max_int bound)
    in
    let rec loop (bytes, n) =
      let consumed_bytes = 8 in
      let state_size = Bytes.length bytes in
      if Compare.Int.(n > state_size - consumed_bytes) then
        loop (Raw_hashes.blake2b bytes, 0)
      else
        let r = TzEndian.get_int64 bytes n in
        (* The absolute value of min_int is min_int.  Also, every
           positive integer is represented twice (positive and negative),
           but zero is only represented once.  We fix both problems at
           once. *)
        let r = if Compare.Int64.(r = Int64.min_int) then 0L else Int64.abs r in
        if Compare.Int64.(r >= drop_if_over) then
          loop (bytes, n + consumed_bytes)
        else
          let v = Int64.rem r bound in
          (v, (bytes, n + consumed_bytes))
    in
    loop state

  (** [sampler_for_cycle ctxt cycle] reads the sampler for [cycle] from
      [ctxt] if it has been previously inited. Otherwise it initializes
      the sampler and caches it in [ctxt] with
      [Raw_context.set_sampler_for_cycle]. *)
  let sampler_for_cycle ctxt cycle =
    let open Lwt_result_syntax in
    let read ctxt =
      let* seed = Seed_storage.for_cycle ctxt cycle in
      let+ ctxt, state = Delegate_sampler_state.get ctxt cycle in
      (ctxt, seed, state)
    in
    Raw_context.sampler_for_cycle ~read ctxt cycle

  let owner c (level : Level_repr.t) offset =
    let open Lwt_result_syntax in
    let cycle = level.Level_repr.cycle in
    let* c, seed, state = sampler_for_cycle c cycle in
    let sample ~int_bound ~mass_bound =
      let state = init_random_state seed level offset in
      let i, state = take_int64 (Int64.of_int int_bound) state in
      let elt, _ = take_int64 mass_bound state in
      (Int64.to_int i, elt)
    in
    let pk = Sampler.sample state sample in
    return (c, pk)
end

(** [baking_rights_owner ctxt level round] returns the delegate with baking
    rights for [level] at [round]. This function should be constant time.

    Fallback chain: SWRR → Alias sampling 
    1. Try SWRR first: deterministic precomputed selection 
    2. If SWRR returns None: fall back to Alias sampling (stake-weighted lottery)

    SWRR returns None when:
    - Feature flag [swrr_new_baker_lottery_enable] is false
    - Precomputed data not found (protocol migration, old cycle)
    - Cache miss and storage lookup fails

    This precedence ensures smooth migration: SWRR can be toggled on/off
    via protocol constant without breaking consensus. *)
let baking_rights_owner ctxt (level : Level_repr.t) ~round =
  let open Lwt_result_syntax in
  (* This committee is used for rounds *)
  let committee_size = Constants_storage.consensus_committee_size ctxt in
  (* We use [Round.to_slot] to have a limited number of unique rounds
     (it should loop after some time) *)
  let*? slot = Round_repr.to_slot ~committee_size round in
  let* ctxt, pk =
    let* ctxt, baker_opt = Swrr_sampler.get_baker ctxt level round in
    match baker_opt with
    | Some pk -> return (ctxt, pk)
    | None -> Random.owner ctxt level (Slot_repr.to_int slot)
  in
  return (ctxt, slot, pk)

let load_sampler_for_cycle ctxt cycle =
  let open Lwt_result_syntax in
  let* ctxt, (_ : Seed_repr.seed), (_ : Raw_context.consensus_pk Sampler.t) =
    Random.sampler_for_cycle ctxt cycle
  in
  return ctxt

let sort_stakes_pk_for_stake_info stakes_pk =
  (* The stakes_pk is supposedly already sorted by decreasing stake, from
     the call to get_selected_distribution when it was initialized.
     We sort them here by lexicographical order on the pkh of the delegate instead.
  *)
  List.sort
    (fun (del1 : Raw_context.delegate_stake_info)
         (del2 : Raw_context.delegate_stake_info)
       ->
      Signature.Public_key_hash.compare
        del1.consensus_pk.delegate
        del2.consensus_pk.delegate)
    (stakes_pk : Raw_context.delegate_stake_info list)

(** [stake_info_for_cycle ctxt cycle] reads the stake info for [cycle] from
    [ctxt] if it has been previously initialized. Otherwise it initializes
    the sampler and caches it in [ctxt] with
    [Raw_context.set_stake_info_for_cycle]. *)
let stake_info_for_cycle ctxt cycle =
  let open Lwt_result_syntax in
  let* ctxt, r_opt = Delegate_stake_info.find ctxt cycle in
  match r_opt with
  | Some s -> return (ctxt, s)
  (* The none case happens if the table hasn't been computed in advance.
     This happens for a couple of cycles after the activation that includes
     the new storage [Delegate_stake_info]. *)
  | None ->
      let* total_stake = Stake_storage.get_total_active_stake ctxt cycle in
      let total_stake_weight = Stake_repr.staking_weight total_stake in
      let* ctxt, stakes_pkh =
        Stake_storage.get_selected_distribution ctxt cycle
      in
      let* stakes_pk =
        List.rev_map_es
          (fun (pkh, stake) ->
            let+ pk =
              Delegate_consensus_key.active_pubkey_for_cycle ctxt pkh cycle
            in
            Raw_context.
              {
                consensus_pk = pk;
                stake_weight = Stake_repr.staking_weight stake;
              })
          stakes_pkh
      in
      let stakes_pk = sort_stakes_pk_for_stake_info stakes_pk in
      let r = Raw_context.{total_stake_weight; delegates = stakes_pk} in
      let* ctxt = Delegate_stake_info.add ctxt cycle r in
      return (ctxt, r)

let stake_info ctxt level =
  let cycle = level.Level_repr.cycle in
  stake_info_for_cycle ctxt cycle

let attestation_slot_owner ~all_bakers_attest_enabled ctxt level slot =
  let open Lwt_result_syntax in
  if all_bakers_attest_enabled then
    let* ctxt, {delegates = info; _} = stake_info ctxt level in
    let i = Slot_repr.to_int slot in
    match List.nth info i with
    | None -> tzfail (Invalid_slot {level; slot})
    | Some {consensus_pk = owner; _} -> return (ctxt, owner)
  else Random.owner ctxt level (Slot_repr.to_int slot)

let get_delegate_stake_from_staking_balance ctxt delegate staking_balance =
  let open Lwt_result_syntax in
  let* staking_parameters =
    Delegate_staking_parameters.of_delegate ctxt delegate
  in
  Lwt.return
    (Stake_context.apply_limits ctxt staking_parameters staking_balance)

let get_stakes ctxt =
  let open Lwt_result_syntax in
  let minimal_frozen_stake = Constants_storage.minimal_frozen_stake ctxt in
  let minimal_stake = Constants_storage.minimal_stake ctxt in
  Stake_storage.fold_on_active_delegates_with_minimal_stake_es
    ctxt
    ~order:`Sorted
    ~f:(fun delegate acc ->
      let* staking_balance =
        Stake_storage.get_full_staking_balance ctxt delegate
      in
      (* This function is called after slashing has been applied at cycle end,
         hence there is no need to apply slashing on [staking_balance] as it
         used to be when the value was taken from a snapshot. *)
      if
        Full_staking_balance_repr.has_minimal_frozen_stake
          ~minimal_frozen_stake
          staking_balance
      then
        let* stake_for_cycle =
          get_delegate_stake_from_staking_balance ctxt delegate staking_balance
        in
        if
          Stake_repr.has_minimal_stake_to_participate
            ~minimal_stake
            stake_for_cycle
        then
          let stakes, total_stake = acc in
          let*? total_stake = Stake_repr.(total_stake +? stake_for_cycle) in
          return ((delegate, stake_for_cycle) :: stakes, total_stake)
        else return acc
      else return acc)
    ~init:([], Stake_repr.zero)

let select_distribution_for_cycle ctxt cycle =
  let open Lwt_result_syntax in
  let* seed = Seed_storage.raw_for_cycle ctxt cycle in
  let* stakes, total_stake = get_stakes ctxt in
  let* ctxt =
    Stake_storage.set_selected_distribution_for_cycle
      ctxt
      cycle
      stakes
      total_stake
  in
  let* stakes_pk, tz4_number_bakers, total_stake_weight =
    List.fold_left_es
      (fun (list_acc, tz4_count_acc, total_stake_weight) (pkh, stake) ->
        let* pk =
          Delegate_consensus_key.active_pubkey_for_cycle ctxt pkh cycle
        in
        let stake_weight = Stake_repr.staking_weight stake in
        let tz4_count =
          match pk.consensus_pk with
          | Bls _ -> tz4_count_acc + 1
          | _ -> tz4_count_acc
        in
        return
          ( (pk, stake_weight) :: list_acc,
            tz4_count,
            Int64.add total_stake_weight stake_weight ))
      ([], 0, 0L)
      stakes
  in
  (* Cycle-end baker selection dispatch

     If SWRR enabled: precompute deterministic baker list via weighted round-robin
     If SWRR disabled: use Alias sampling with Sampler state
   *)
  let* ctxt =
    if Constants_storage.swrr_new_baker_lottery_enable ctxt then
      Swrr_sampler.select_bakers_at_cycle_end ctxt ~target_cycle:cycle
    else
      let state = Sampler.create stakes_pk in
      let* ctxt = Delegate_sampler_state.init ctxt cycle state in
      (* pre-allocate the sampler *)
      let*? ctxt = Raw_context.init_sampler_for_cycle ctxt cycle seed state in
      return ctxt
  in
  (* Update stake info *)
  (* At the end of the cycle, we store the data used to
     compute the [sampler_state] as [stake_info]. This include the
     [total_staking_power] for the cycle, as well as the list
     of active stakes and consensus keys for each active delegate.
     They are also sorted with [sort_stakes_pk_for_stake_info], as
     it is important for ABAAB that they are sorted in a
     deterministic way. *)
  let delegates =
    List.map
      (fun (consensus_pk, stake_weight) ->
        Raw_context.{consensus_pk; stake_weight})
      stakes_pk
  in
  let delegates = sort_stakes_pk_for_stake_info delegates in
  (* Note: it is impossible that this entry for this cycle would have been
     initialised before, because it needs the sampler info, which is being done
     is this very function *)
  let* ctxt =
    Delegate_stake_info.init ctxt cycle {total_stake_weight; delegates}
  in
  (* Update all bakers attest activation level if tz4 stake
     is above activation threshold *)
  let*! ctxt =
    All_bakers_attest_activation_storage
    .may_update_all_bakers_attest_first_level
      ctxt
      cycle
      ~total_number_bakers:(List.length stakes)
      ~tz4_number_bakers
  in
  return ctxt

let select_new_distribution_at_cycle_end ctxt ~new_cycle =
  let consensus_rights_delay = Constants_storage.consensus_rights_delay ctxt in
  let for_cycle = Cycle_repr.add new_cycle consensus_rights_delay in
  select_distribution_for_cycle ctxt for_cycle

(* Clean up old cycle data for both SWRR and alias sampler.

   Alias sampler cleanup: [Delegate_sampler_state.remove] 
   is used because when SWRR is enabled, new cycles don't create alias sampler state.
   However, old cycles (before SWRR activation) still have alias sampler state.
   Using [remove] allows safe cleanup of both:
   - Old cycles with alias sampler state (created before SWRR)
   - New cycles without alias sampler state (created after SWRR activation)
   If [remove_existing] were used, cleanup would fail for post-SWRR cycles.

   SWRR cleanup: [Swrr_sampler.remove_outdated_cycle] uses [remove] not
   [remove_existing] because SWRR data may not exist if the feature flag
   [swrr_new_baker_lottery_enable] was disabled during that cycle.
   This allows safe cleanup even when toggling between SWRR/alias modes.

   Timing: called after [preserved_cycles] window to bound storage growth.
   Safe to remove: consensus only needs recent cycles for validation. *)
let clear_outdated_sampling_data ctxt ~new_cycle =
  let open Lwt_result_syntax in
  match Cycle_storage.cycle_to_clear_of_sampling_data ~new_cycle with
  | None -> return ctxt
  | Some outdated_cycle ->
      let* ctxt = Delegate_sampler_state.remove ctxt outdated_cycle in
      (* We cannot use [remove_existing] for [Delegate_stake_info] because
         there exists some cycles for which the storage does not exist.
         This happens because this data doesn't exist for cycles before
         the storage has been introduced. *)
      let* ctxt = Delegate_stake_info.remove ctxt outdated_cycle in
      let* ctxt = Seed_storage.remove_for_cycle ctxt outdated_cycle in
      let*! ctxt = Swrr_sampler.remove_outdated_cycle ctxt outdated_cycle in
      return ctxt

let attesting_power ~all_bakers_attest_enabled ctxt level =
  let open Lwt_result_syntax in
  if all_bakers_attest_enabled then
    let* ctxt, {delegates; _} = stake_info ctxt level in
    let map =
      List.fold_left
        (fun map_acc Raw_context.{consensus_pk : consensus_pk; stake_weight} ->
          Signature.Public_key_hash.Map.add
            consensus_pk.delegate
            stake_weight
            map_acc)
        Signature.Public_key_hash.Map.empty
        delegates
    in
    return (ctxt, map)
  else
    let consensus_committee_size =
      Constants_storage.consensus_committee_size ctxt
    in
    let*? slots =
      Slot_repr.Range.create ~min:0 ~count:consensus_committee_size
    in
    Slot_repr.Range.fold_es
      (fun (ctxt, map) slot ->
        let* ctxt, consensus_pk =
          (* all_bakers_attest_enabled = false *)
          attestation_slot_owner ~all_bakers_attest_enabled ctxt level slot
        in
        let map =
          Signature.Public_key_hash.Map.update
            consensus_pk.delegate
            (function
              | None -> Some 1L | Some slots_n -> Some (Int64.succ slots_n))
            map
        in
        return (ctxt, map))
      (ctxt, Signature.Public_key_hash.Map.empty)
      slots

module For_RPC = struct
  let delegate_current_baking_power ctxt delegate =
    let open Lwt_result_syntax in
    let* stake = Storage.Stake.Staking_balance.get ctxt delegate in
    let* staking_parameters =
      Delegate_staking_parameters.of_delegate ctxt delegate
    in
    Lwt.return @@ Stake_context.baking_weight ctxt staking_parameters stake

  let total_baking_power ctxt cycle =
    let open Lwt_result_syntax in
    let+ total_active_stake = Stake_storage.get_total_active_stake ctxt cycle in
    Stake_repr.staking_weight total_active_stake
end
