(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Misc

module Selected_distribution_for_cycle = struct
  module Cache_client = struct
    type cached_value = (Signature.Public_key_hash.t * Tez_repr.t) list

    let namespace = Cache_repr.create_namespace "stake_distribution"

    let cache_index = 1

    let value_of_identifier ctxt identifier =
      let cycle = Cycle_repr.of_string_exn identifier in
      Storage.Stake.Selected_distribution_for_cycle.get ctxt cycle
  end

  module Cache = (val Cache_repr.register_exn (module Cache_client))

  let identifier_of_cycle cycle = Format.asprintf "%a" Cycle_repr.pp cycle

  let init ctxt cycle stakes =
    let id = identifier_of_cycle cycle in
    Storage.Stake.Selected_distribution_for_cycle.init ctxt cycle stakes
    >>=? fun ctxt ->
    let size = 1 (* that's symbolic: 1 cycle = 1 entry *) in
    Cache.update ctxt id (Some (stakes, size)) >>?= fun ctxt -> return ctxt

  let get ctxt cycle =
    let id = identifier_of_cycle cycle in
    Cache.find ctxt id >>=? function
    | None -> Storage.Stake.Selected_distribution_for_cycle.get ctxt cycle
    | Some v -> return v

  let remove_existing ctxt cycle =
    let id = identifier_of_cycle cycle in
    Cache.update ctxt id None >>?= fun ctxt ->
    Storage.Stake.Selected_distribution_for_cycle.remove_existing ctxt cycle
end

module Delegate_sampler_state = struct
  module Cache_client = struct
    type cached_value =
      (Signature.Public_key.t * Signature.Public_key_hash.t) Sampler.t

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

let get_staking_balance = Storage.Stake.Staking_balance.get

let get_initialized_stake ctxt delegate =
  Storage.Stake.Staking_balance.find ctxt delegate >>=? function
  | Some staking_balance -> return (staking_balance, ctxt)
  | None ->
      Frozen_deposits_storage.init ctxt delegate >>=? fun ctxt ->
      let balance = Tez_repr.zero in
      Storage.Stake.Staking_balance.init ctxt delegate balance >>=? fun ctxt ->
      return (balance, ctxt)

let remove_stake ctxt delegate amount =
  get_initialized_stake ctxt delegate >>=? fun (staking_balance_before, ctxt) ->
  Tez_repr.(staking_balance_before -? amount) >>?= fun staking_balance ->
  Storage.Stake.Staking_balance.update ctxt delegate staking_balance
  >>=? fun ctxt ->
  let tokens_per_roll = Constants_storage.tokens_per_roll ctxt in
  if Tez_repr.(staking_balance_before >= tokens_per_roll) then
    Delegate_activation_storage.is_inactive ctxt delegate >>=? fun inactive ->
    if (not inactive) && Tez_repr.(staking_balance < tokens_per_roll) then
      Storage.Stake.Active_delegate_with_one_roll.remove ctxt delegate
      >>= fun ctxt -> return ctxt
    else return ctxt
  else
    (* The delegate was not in Stake.Active_delegate_with_one_roll,
       either because it was inactive, or because it did not have a
       roll, in which case it still does not have a roll. *)
    return ctxt

let add_stake ctxt delegate amount =
  get_initialized_stake ctxt delegate >>=? fun (staking_balance_before, ctxt) ->
  Tez_repr.(amount +? staking_balance_before) >>?= fun staking_balance ->
  Storage.Stake.Staking_balance.update ctxt delegate staking_balance
  >>=? fun ctxt ->
  let tokens_per_roll = Constants_storage.tokens_per_roll ctxt in
  if Tez_repr.(staking_balance >= tokens_per_roll) then
    Delegate_activation_storage.is_inactive ctxt delegate >>=? fun inactive ->
    if inactive || Tez_repr.(staking_balance_before >= tokens_per_roll) then
      return ctxt
    else
      Storage.Stake.Active_delegate_with_one_roll.add ctxt delegate ()
      >>= fun ctxt -> return ctxt
  else
    (* The delegate was not in Stake.Active_delegate_with_one_roll,
       because it did not have a roll (as otherwise it would have a
       roll now). *)
    return ctxt

let deactivate_only_call_from_delegate_storage ctxt delegate =
  Storage.Stake.Active_delegate_with_one_roll.remove ctxt delegate

let activate_only_call_from_delegate_storage ctxt delegate =
  get_initialized_stake ctxt delegate >>=? fun (staking_balance, ctxt) ->
  let tokens_per_roll = Constants_storage.tokens_per_roll ctxt in
  if Tez_repr.(staking_balance >= tokens_per_roll) then
    Storage.Stake.Active_delegate_with_one_roll.add ctxt delegate ()
    >>= fun ctxt -> return ctxt
  else return ctxt

let snapshot ctxt =
  Storage.Stake.Last_snapshot.get ctxt >>=? fun index ->
  Storage.Stake.Last_snapshot.update ctxt (index + 1) >>=? fun ctxt ->
  Storage.Stake.Staking_balance.snapshot ctxt index >>=? fun ctxt ->
  Storage.Stake.Active_delegate_with_one_roll.snapshot ctxt index

let get_stakes_for_selected_index ctxt index =
  Storage.Stake.Active_delegate_with_one_roll.fold_snapshot
    ctxt
    index
    ~order:`Sorted
    ~init:([], Tez_repr.zero)
    ~f:(fun delegate () (acc, total_stake) ->
      Storage.Stake.Staking_balance.Snapshot.get ctxt (index, delegate)
      >>=? fun staking_balance ->
      let delegate_contract = Contract_repr.implicit_contract delegate in
      Storage.Contract.Frozen_deposits_limit.find ctxt delegate_contract
      >>=? fun frozen_deposits_limit ->
      Storage.Contract.Spendable_balance.get ctxt delegate_contract
      >>=? fun balance ->
      Frozen_deposits_storage.get ctxt delegate_contract
      >>=? fun frozen_deposits ->
      Tez_repr.(balance +? frozen_deposits.current_amount)
      >>?= fun total_balance ->
      let frozen_deposits_percentage =
        Constants_storage.frozen_deposits_percentage ctxt
      in
      let stake_to_consider =
        match frozen_deposits_limit with
        | Some frozen_deposits_limit -> (
            try
              let open Tez_repr in
              let max_mutez = of_mutez_exn Int64.max_int in
              if frozen_deposits_limit > div_exn max_mutez 100 then
                let frozen_deposits_limit_by_10 =
                  mul_exn frozen_deposits_limit 10
                in
                if frozen_deposits_limit_by_10 < staking_balance then
                  frozen_deposits_limit_by_10
                else staking_balance
              else
                min
                  staking_balance
                  (div_exn
                     (mul_exn frozen_deposits_limit 100)
                     frozen_deposits_percentage)
            with _ -> staking_balance)
        | None -> staking_balance
      in
      Tez_repr.(total_balance *? 100L) >>?= fun expanded_balance ->
      Tez_repr.(expanded_balance /? Int64.of_int frozen_deposits_percentage)
      >>?= fun max_staking_capacity ->
      let stake_for_cycle =
        Tez_repr.min stake_to_consider max_staking_capacity
      in
      Tez_repr.(total_stake +? stake_for_cycle) >>?= fun total_stake ->
      return ((delegate, stake_for_cycle) :: acc, total_stake))

let select_distribution_for_cycle ctxt cycle pubkey =
  Storage.Stake.Last_snapshot.get ctxt >>=? fun max_index ->
  Storage.Seed.For_cycle.get ctxt cycle >>=? fun seed ->
  let rd = Seed_repr.initialize_new seed [Bytes.of_string "stake_snapshot"] in
  let seq = Seed_repr.sequence rd 0l in
  let selected_index =
    Seed_repr.take_int32 seq (Int32.of_int max_index) |> fst |> Int32.to_int
  in
  List.fold_left_es
    (fun ctxt index ->
      (if Compare.Int.(index = selected_index) then
       get_stakes_for_selected_index ctxt index
       >>=? fun (stakes, total_stake) ->
       let stakes =
         List.sort (fun (_, x) (_, y) -> Tez_repr.compare y x) stakes
       in
       Selected_distribution_for_cycle.init ctxt cycle stakes >>=? fun ctxt ->
       Storage.Total_active_stake.add ctxt cycle total_stake >>= fun ctxt ->
       List.fold_left_es
         (fun acc (pkh, stake) ->
           pubkey ctxt pkh >>=? fun pk ->
           return (((pk, pkh), Tez_repr.to_mutez stake) :: acc))
         []
         stakes
       >>=? fun stakes_pk ->
       let state = Sampler.create stakes_pk in
       Delegate_sampler_state.init ctxt cycle state
      else return ctxt)
      >>=? fun ctxt ->
      Storage.Stake.Staking_balance.delete_snapshot ctxt index >>= fun ctxt ->
      Storage.Stake.Active_delegate_with_one_roll.delete_snapshot ctxt index
      >>= fun ctxt -> return ctxt)
    ctxt
    Misc.(0 --> (max_index - 1))
  >>=? fun ctxt -> Storage.Stake.Last_snapshot.update ctxt 0

let select_distribution_for_cycle_do_not_call_except_for_migration =
  select_distribution_for_cycle

let clear_cycle ctxt cycle =
  Storage.Total_active_stake.remove_existing ctxt cycle >>=? fun ctxt ->
  Selected_distribution_for_cycle.remove_existing ctxt cycle >>=? fun ctxt ->
  Delegate_sampler_state.remove_existing ctxt cycle >>=? fun ctxt ->
  Storage.Seed.For_cycle.remove_existing ctxt cycle

let init_first_cycles ctxt pubkey =
  let preserved = Constants_storage.preserved_cycles ctxt in
  List.fold_left_es
    (fun ctxt c ->
      let cycle = Cycle_repr.of_int32_exn (Int32.of_int c) in
      snapshot ctxt >>=? fun ctxt ->
      (* NB: we need to take several snapshots because
         select_distribution_for_cycle deletes the snapshots *)
      select_distribution_for_cycle ctxt cycle pubkey)
    ctxt
    (0 --> preserved)

let fold ctxt ~f ~order init =
  Storage.Stake.Active_delegate_with_one_roll.fold
    ctxt
    ~order
    ~init:(Ok init)
    ~f:(fun delegate () acc ->
      acc >>?= fun acc ->
      get_staking_balance ctxt delegate >>=? fun stake ->
      f (delegate, stake) acc)

let select_new_distribution_at_cycle_end ctxt ~new_cycle =
  let preserved = Constants_storage.preserved_cycles ctxt in
  let for_cycle = Cycle_repr.add new_cycle preserved in
  select_distribution_for_cycle ctxt for_cycle

let clear_at_cycle_end ctxt ~new_cycle =
  let max_slashing_period = Constants_storage.max_slashing_period ctxt in
  match Cycle_repr.sub new_cycle max_slashing_period with
  | None -> return ctxt
  | Some cycle_to_clear -> clear_cycle ctxt cycle_to_clear

let get ctxt delegate =
  Storage.Stake.Active_delegate_with_one_roll.mem ctxt delegate >>= function
  | true -> get_staking_balance ctxt delegate
  | false -> return Tez_repr.zero

let fold_on_active_delegates_with_rolls =
  Storage.Stake.Active_delegate_with_one_roll.fold

let get_selected_distribution = Selected_distribution_for_cycle.get

let find_selected_distribution =
  Storage.Stake.Selected_distribution_for_cycle.find

let prepare_stake_distribution ctxt =
  let level = Level_storage.current ctxt in
  Selected_distribution_for_cycle.get ctxt level.cycle >>=? fun stakes ->
  let stake_distribution =
    List.fold_left
      (fun map (pkh, stake) -> Signature.Public_key_hash.Map.add pkh stake map)
      Signature.Public_key_hash.Map.empty
      stakes
  in
  return
    (Raw_context.init_stake_distribution_for_current_cycle
       ctxt
       stake_distribution)

let get_total_active_stake = Storage.Total_active_stake.get
