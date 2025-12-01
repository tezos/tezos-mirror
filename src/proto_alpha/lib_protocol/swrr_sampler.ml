(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type credit_info = {pkh : Signature.public_key_hash; credit : Z.t; stake : Z.t}

let select_bakers_at_cycle_end ctxt ~target_cycle =
  let open Lwt_result_syntax in
  let* total_stake = Stake_storage.get_total_active_stake ctxt target_cycle in
  let total_stake = Z.of_int64 (Stake_repr.staking_weight total_stake) in

  let* ctxt, stakes_pkh =
    Stake_storage.get_selected_distribution ctxt target_cycle
  in
  let blocks_per_cycle = Constants_storage.blocks_per_cycle ctxt in
  let nb_slots = Int32.to_int blocks_per_cycle in

  let* init_credit_list =
    List.fold_left_es
      (fun acc (pkh, stake) ->
        let* credit_opt =
          Storage.Contract.SWRR_credit.find ctxt (Contract_repr.Implicit pkh)
        in
        let old_credit = Option.value ~default:Z.zero credit_opt in
        let weight = Stake_repr.staking_weight stake in
        let acc =
          {pkh; credit = old_credit; stake = Z.of_int64 weight} :: acc
        in
        return acc)
      []
      stakes_pkh
  in

  let rec loop i (credit_list : credit_info list)
      (acc : Signature.public_key_hash list) :
      credit_info list * Signature.public_key_hash list =
    if Compare.Int.(i >= nb_slots) then
      let bakers = List.rev acc in
      (credit_list, bakers)
    else
      (* Increase everyone's credit by their stake, and find the max *)
      let best_credit_info_opt, updated_credit_list =
        List.fold_left
          (fun (current_best_credit_opt, updated_credit_list)
               {pkh; credit; stake}
             ->
            let new_credit = Z.add credit stake in
            let new_credit_info = {pkh; credit = new_credit; stake} in
            match current_best_credit_opt with
            (* First iteration, so the first credit is the best *)
            | None -> (Some new_credit_info, updated_credit_list)
            (* The new staker has better credit: we add the previous one to the list, and keep going. *)
            | Some previous_best_credit
              when Compare.Z.(new_credit > previous_best_credit.credit) ->
                ( Some new_credit_info,
                  previous_best_credit :: updated_credit_list )
            (* The new credit is still less than the best. *)
            | Some _ as x -> (x, new_credit_info :: updated_credit_list))
          (None, [])
          credit_list
      in
      (* At this point `updated_credit_list` doesn't contain `best_credit_info_opt`, so we can simply update it and add it to the list. *)
      let updated_credit_list, acc =
        match best_credit_info_opt with
        | None -> (updated_credit_list, acc) (* Should not happen *)
        | Some ({credit; pkh; _} as best_credit_info) ->
            ( {best_credit_info with credit = Z.sub credit total_stake}
              :: updated_credit_list,
              pkh :: acc )
      in
      loop (i + 1) updated_credit_list acc
  in
  let new_credits, selected_bakers = loop 0 init_credit_list [] in
  (* update context *)
  let*! ctxt =
    Storage.Stake.Selected_bakers.add ctxt target_cycle selected_bakers
  in
  let* ctxt =
    List.fold_left_es
      (fun ctxt {pkh; credit; _} ->
        Storage.Contract.SWRR_credit.update
          ctxt
          (Contract_repr.Implicit pkh)
          credit)
      ctxt
      new_credits
  in
  return ctxt

let get_baker ctxt level round =
  let open Lwt_result_syntax in
  let cycle = level.Level_repr.cycle in
  let pos = level.Level_repr.cycle_position in
  let*? round_int = Round_repr.to_int round in

  let* selected_bakers = Storage.Stake.Selected_bakers.get ctxt cycle in
  let len = List.length selected_bakers in
  let idx = (Int32.to_int pos + (3 * round_int)) mod len in

  match List.nth_opt selected_bakers idx with
  | None ->
      assert
        false (* should not happen if select_bakers_at_cycle_end is correct *)
  | Some pkh ->
      let* pk = Delegate_consensus_key.active_pubkey ctxt pkh in
      return (ctxt, pk)

let reset_credit_for_deactivated_delegates ctxt deactivated_delegates =
  let open Lwt_result_syntax in
  let*! ctxt =
    List.fold_left_s
      (fun ctxt pkh ->
        Storage.Contract.SWRR_credit.add
          ctxt
          (Contract_repr.Implicit pkh)
          Z.zero)
      ctxt
      deactivated_delegates
  in
  return ctxt
