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

  let init ctxt cycle sampler_state =
    let id = identifier_of_cycle cycle in
    Storage.Delegate_sampler_state.init ctxt cycle sampler_state
    >>=? fun ctxt ->
    let size = 1 (* that's symbolic: 1 cycle = 1 entry *) in
    Cache.update ctxt id (Some (sampler_state, size)) >>?= fun ctxt ->
    return ctxt

  let get ctxt cycle =
    let id = identifier_of_cycle cycle in
    Cache.find ctxt id >>=? function
    | None -> Storage.Delegate_sampler_state.get ctxt cycle
    | Some v -> return v

  let remove_existing ctxt cycle =
    let id = identifier_of_cycle cycle in
    Cache.update ctxt id None >>?= fun ctxt ->
    Storage.Delegate_sampler_state.remove_existing ctxt cycle
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
    let read ctxt =
      Seed_storage.for_cycle ctxt cycle >>=? fun seed ->
      Delegate_sampler_state.get ctxt cycle >>=? fun state ->
      return (seed, state)
    in
    Raw_context.sampler_for_cycle ~read ctxt cycle

  let owner c (level : Level_repr.t) offset =
    let cycle = level.Level_repr.cycle in
    sampler_for_cycle c cycle >>=? fun (c, seed, state) ->
    let sample ~int_bound ~mass_bound =
      let state = init_random_state seed level offset in
      let i, state = take_int64 (Int64.of_int int_bound) state in
      let elt, _ = take_int64 mass_bound state in
      (Int64.to_int i, elt)
    in
    let pk = Sampler.sample state sample in
    return (c, pk)
end

let slot_owner c level slot = Random.owner c level (Slot_repr.to_int slot)

let baking_rights_owner c (level : Level_repr.t) ~round =
  Round_repr.to_int round >>?= fun round ->
  let consensus_committee_size = Constants_storage.consensus_committee_size c in
  Slot_repr.of_int (round mod consensus_committee_size) >>?= fun slot ->
  slot_owner c level slot >>=? fun (ctxt, pk) -> return (ctxt, slot, pk)

let load_sampler_for_cycle ctxt cycle =
  let open Lwt_result_syntax in
  let* ctxt, (_ : Seed_repr.seed), (_ : Raw_context.consensus_pk Sampler.t) =
    Random.sampler_for_cycle ctxt cycle
  in
  return ctxt

let get_delegate_stake_from_staking_balance ctxt
    ~staking_over_baking_global_limit_millionth ~delegation_over_baking_limit
    delegate staking_balance =
  let open Lwt_result_syntax in
  let delegate_contract = Contract_repr.Implicit delegate in
  let* delegate_own_pseudotokens =
    Staking_pseudotokens_storage.costaking_pseudotokens_balance
      ctxt
      delegate_contract
  in
  let* delegate_own_frozen_deposits =
    Staking_pseudotokens_storage.tez_of_frozen_deposits_pseudotokens
      ctxt
      delegate
      delegate_own_pseudotokens
  in
  let* {staking_over_baking_limit_millionth; _} =
    Delegate_staking_parameters.of_delegate ctxt delegate
  in
  let staking_over_baking_limit_millionth =
    let delegate_staking_over_baking_limit_millionth =
      Int64.of_int32 staking_over_baking_limit_millionth
    in
    Compare.Int64.min
      staking_over_baking_global_limit_millionth
      delegate_staking_over_baking_limit_millionth
  in
  let staking_over_baking_limit_plus_1_millionth =
    Int64.add 1_000_000L staking_over_baking_limit_millionth
  in
  let open Tez_repr in
  let* {current_amount = all_frozen_deposits; initial_amount = _} =
    Frozen_deposits_storage.get ctxt delegate_contract
  in
  let frozen =
    match
      mul_ratio
        delegate_own_frozen_deposits
        ~num:staking_over_baking_limit_plus_1_millionth
        ~den:1_000_000L
    with
    | Ok max_allowed_frozen_deposits ->
        min all_frozen_deposits max_allowed_frozen_deposits
        (* Over-co-staked frozen deposits counts towards delegated stake. *)
    | Error _max_allowed_frozen_deposits_overflows -> all_frozen_deposits
  in
  (* This subtraction may result in a negative value if tez were frozen
     after the snapshot. This is fine, they are then taken into account as
     frozen stake rather than delegated. *)
  let available_delegated =
    sub_opt staking_balance frozen |> Option.value ~default:zero
  in
  let delegated =
    match delegate_own_frozen_deposits *? delegation_over_baking_limit with
    | Ok max_allowed_delegated -> min max_allowed_delegated available_delegated
    | Error _max_allowed_delegated_overflows -> available_delegated
  in
  return (Stake_repr.make ~frozen ~delegated)

let get_stakes_for_selected_index ctxt index =
  let open Lwt_result_syntax in
  let delegation_over_baking_limit =
    Int64.of_int (Constants_storage.delegation_over_baking_limit ctxt)
  in
  let staking_over_baking_global_limit_millionth =
    Int64.(
      mul
        1_000_000L
        (of_int
           (Constants_storage
            .adaptive_inflation_staking_over_baking_global_limit
              ctxt)))
  in
  Stake_storage.fold_snapshot
    ctxt
    ~index
    ~f:(fun (delegate, staking_balance) (acc, total_stake) ->
      let* stake_for_cycle =
        get_delegate_stake_from_staking_balance
          ~staking_over_baking_global_limit_millionth
          ~delegation_over_baking_limit
          ctxt
          delegate
          staking_balance
      in
      let*? total_stake = Stake_repr.(total_stake +? stake_for_cycle) in
      return ((delegate, stake_for_cycle) :: acc, total_stake))
    ~init:([], Stake_repr.zero)

let compute_snapshot_index_for_seed ~max_snapshot_index seed =
  let rd = Seed_repr.initialize_new seed [Bytes.of_string "stake_snapshot"] in
  let seq = Seed_repr.sequence rd 0l in
  Seed_repr.take_int32 seq (Int32.of_int max_snapshot_index)
  |> fst |> Int32.to_int |> return

let compute_snapshot_index ctxt cycle ~max_snapshot_index =
  Seed_storage.for_cycle ctxt cycle >>=? fun seed ->
  compute_snapshot_index_for_seed ~max_snapshot_index seed

let select_distribution_for_cycle ctxt cycle =
  Stake_storage.max_snapshot_index ctxt >>=? fun max_snapshot_index ->
  Seed_storage.raw_for_cycle ctxt cycle >>=? fun seed ->
  compute_snapshot_index_for_seed ~max_snapshot_index seed
  >>=? fun selected_index ->
  get_stakes_for_selected_index ctxt selected_index
  >>=? fun (stakes, total_stake) ->
  Stake_storage.set_selected_distribution_for_cycle
    ctxt
    cycle
    stakes
    total_stake
  >>=? fun ctxt ->
  List.fold_left_es
    (fun acc (pkh, stake) ->
      Delegate_consensus_key.active_pubkey_for_cycle ctxt pkh cycle
      >|=? fun pk -> (pk, Stake_context.staking_weight ctxt stake) :: acc)
    []
    stakes
  >>=? fun stakes_pk ->
  let state = Sampler.create stakes_pk in
  Delegate_sampler_state.init ctxt cycle state >>=? fun ctxt ->
  (* pre-allocate the sampler *)
  Lwt.return (Raw_context.init_sampler_for_cycle ctxt cycle seed state)

let delegate_baking_power_from_staking_balance ctxt delegate staking_balance =
  let open Lwt_result_syntax in
  let delegation_over_baking_limit =
    Int64.of_int (Constants_storage.delegation_over_baking_limit ctxt)
  in
  let staking_over_baking_global_limit_millionth =
    Int64.(
      mul
        1_000_000L
        (of_int
           (Constants_storage
            .adaptive_inflation_staking_over_baking_global_limit
              ctxt)))
  in
  let+ stake =
    get_delegate_stake_from_staking_balance
      ctxt
      ~staking_over_baking_global_limit_millionth
      ~delegation_over_baking_limit
      delegate
      staking_balance
  in
  Stake_context.staking_weight ctxt stake

let select_new_distribution_at_cycle_end ctxt ~new_cycle =
  let preserved = Constants_storage.preserved_cycles ctxt in
  let for_cycle = Cycle_repr.add new_cycle preserved in
  select_distribution_for_cycle ctxt for_cycle

let clear_outdated_sampling_data ctxt ~new_cycle =
  let max_slashing_period = Constants_storage.max_slashing_period ctxt in
  match Cycle_repr.sub new_cycle max_slashing_period with
  | None -> return ctxt
  | Some outdated_cycle ->
      Delegate_sampler_state.remove_existing ctxt outdated_cycle
      >>=? fun ctxt -> Seed_storage.remove_for_cycle ctxt outdated_cycle

module For_RPC = struct
  let delegate_baking_power_for_cycle ctxt cycle delegate =
    let open Lwt_result_syntax in
    let* max_snapshot_index = Stake_storage.max_snapshot_index ctxt in
    let* seed = Seed_storage.raw_for_cycle ctxt cycle in
    let* selected_index =
      compute_snapshot_index_for_seed ~max_snapshot_index seed
    in
    let* staking_balance =
      Storage.Stake.Staking_balance.Snapshot.get ctxt (selected_index, delegate)
    in
    delegate_baking_power_from_staking_balance ctxt delegate staking_balance

  let delegate_current_baking_power ctxt delegate =
    let open Lwt_result_syntax in
    let* staking_balance = Stake_storage.get_staking_balance ctxt delegate in
    delegate_baking_power_from_staking_balance ctxt delegate staking_balance
end
