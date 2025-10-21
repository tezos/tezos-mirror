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
    let* ctxt, value_opt = Cache.find ctxt id in
    match value_opt with
    | None ->
        let* v = Storage.Stake.Selected_distribution_for_cycle.get ctxt cycle in
        return (ctxt, v)
    | Some v -> return (ctxt, v)

  let find ctxt cycle =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let* ctxt, value_opt = Cache.find ctxt id in
    match value_opt with
    | None ->
        let* v =
          Storage.Stake.Selected_distribution_for_cycle.find ctxt cycle
        in
        return (ctxt, v)
    | Some _ as some_v -> return (ctxt, some_v)

  let remove_existing ctxt cycle =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let*? ctxt = Cache.update ctxt id None in
    Storage.Stake.Selected_distribution_for_cycle.remove_existing ctxt cycle
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
  let* ctxt, stakes = Selected_distribution_for_cycle.get ctxt cycle in
  let stakes_map =
    List.fold_left
      (fun map (pkh, stake) -> Signature.Public_key_hash.Map.add pkh stake map)
      Signature.Public_key_hash.Map.empty
      stakes
  in
  return (ctxt, stakes_map)

let prepare_stake_distribution ctxt =
  let open Lwt_result_syntax in
  let level = Level_storage.current ctxt in
  let+ ctxt, stake_distribution =
    get_selected_distribution_as_map ctxt level.cycle
  in
  Raw_context.init_stake_distribution_for_current_cycle ctxt stake_distribution

let get_total_active_stake = Storage.Stake.Total_active_stake.get
