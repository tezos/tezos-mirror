(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module Selected_distribution_for_cycle = struct
  module Cache_client = struct
    type cached_value = (Signature.Public_key_hash.t * Stake_repr.t) list

    let namespace = Cache_repr.create_namespace "stake_distribution"

    let cache_index = 1

    let value_of_identifier ctxt identifier =
      let cycle = Cycle_repr.of_string_exn identifier in
      Storage.Stake.Selected_distribution_for_cycle.get ctxt cycle
  end

  module Cache = (val Cache_repr.register_exn (module Cache_client))

  let identifier_of_cycle cycle = Format.asprintf "%a" Cycle_repr.pp cycle

  let init ctxt cycle stakes =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let* ctxt =
      Storage.Stake.Selected_distribution_for_cycle.init ctxt cycle stakes
    in
    let size =
      1
      (* that's symbolic: 1 cycle = 1 entry *)
    in
    let*? ctxt = Cache.update ctxt id (Some (stakes, size)) in
    return ctxt

  let get ctxt cycle =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let* value_opt = Cache.find ctxt id in
    match value_opt with
    | None -> Storage.Stake.Selected_distribution_for_cycle.get ctxt cycle
    | Some v -> return v

  let find ctxt cycle =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let* value_opt = Cache.find ctxt id in
    match value_opt with
    | None -> Storage.Stake.Selected_distribution_for_cycle.find ctxt cycle
    | Some _ as some_v -> return some_v

  let remove_existing ctxt cycle =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let*? ctxt = Cache.update ctxt id None in
    Storage.Stake.Selected_distribution_for_cycle.remove_existing ctxt cycle

  let remove ctxt cycle =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let*? ctxt = Cache.update ctxt id None in
    let*! ctxt =
      Storage.Stake.Selected_distribution_for_cycle.remove ctxt cycle
    in
    return ctxt
end

let set_selected_distribution_for_cycle ctxt cycle stakes total_stake =
  let open Lwt_result_syntax in
  let stakes = List.sort (fun (_, x) (_, y) -> Stake_repr.compare y x) stakes in
  let* ctxt = Selected_distribution_for_cycle.init ctxt cycle stakes in
  let*! ctxt = Storage.Stake.Total_active_stake.add ctxt cycle total_stake in
  return ctxt

let get_selected_distribution = Selected_distribution_for_cycle.get

let find_selected_distribution = Selected_distribution_for_cycle.find

let get_selected_distribution_as_map ctxt cycle =
  let open Lwt_result_syntax in
  let+ stakes = Selected_distribution_for_cycle.get ctxt cycle in
  List.fold_left
    (fun map (pkh, stake) -> Signature.Public_key_hash.Map.add pkh stake map)
    Signature.Public_key_hash.Map.empty
    stakes

let prepare_stake_distribution ctxt =
  let open Lwt_result_syntax in
  let level = Level_storage.current ctxt in
  let+ stake_distribution = get_selected_distribution_as_map ctxt level.cycle in
  Raw_context.init_stake_distribution_for_current_cycle ctxt stake_distribution

let get_total_active_stake = Storage.Stake.Total_active_stake.get
