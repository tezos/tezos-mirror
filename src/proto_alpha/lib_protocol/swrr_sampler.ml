(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let select_bakers_at_cycle_end ctxt ~target_cycle =
  let open Lwt_result_syntax in
  let* total_stake = Stake_storage.get_total_active_stake ctxt target_cycle in
  let total_stake = Z.of_int64 (Stake_repr.staking_weight total_stake) in

  let* ctxt, stakes_pkh =
    Stake_storage.get_selected_distribution ctxt target_cycle
  in
  let blocks_per_cycle = Constants_storage.blocks_per_cycle ctxt in
  let nb_slots = Int32.to_int blocks_per_cycle in

  let rec loop i ctxt acc =
    if Compare.Int.(i >= nb_slots) then
      let bakers = List.rev acc in
      let*! raw_ctxt =
        Storage.Stake.Selected_bakers.add ctxt target_cycle bakers
      in
      return raw_ctxt
    else
      let* ctxt, best_pkh_opt, updated_rev =
        List.fold_left_es
          (fun (ctxt, best_opt, acc) (pkh, stake) ->
            let* credit_opt =
              Storage.Contract.SWRR_credit.find
                ctxt
                (Contract_repr.Implicit pkh)
            in
            let old_credit = Option.value ~default:Z.zero credit_opt in
            let weight = Stake_repr.staking_weight stake in
            let new_credit = Z.add old_credit (Z.of_int64 weight) in
            (* TO DO change from Int64 to Z *)

            let best_opt =
              match best_opt with
              | None -> Some (pkh, new_credit)
              | Some (_best_pkh, best_credit)
                when Compare.Z.(new_credit > best_credit) ->
                  Some (pkh, new_credit)
              | Some _ as x -> x
            in
            let acc = (pkh, new_credit) :: acc in
            return (ctxt, best_opt, acc))
          (ctxt, None, [])
          stakes_pkh
      in
      let best_pkh, best_credit, updated =
        match best_pkh_opt with
        | Some (pkh, credit) ->
            let updated = List.rev updated_rev in
            (pkh, credit, updated)
        | None -> assert false
        (* only happens if stakes_pkh is empty, which shouldn't happen *)
      in
      let winner_credit = Z.sub best_credit total_stake in

      let* ctxt =
        List.fold_left_es
          (fun ctxt (pkh, credit) ->
            let credit =
              if Signature.Public_key_hash.equal pkh best_pkh then winner_credit
              else credit
            in
            Storage.Contract.SWRR_credit.update
              ctxt
              (Contract_repr.Implicit pkh)
              credit)
          ctxt
          updated
      in
      loop (i + 1) ctxt (best_pkh :: acc)
  in
  loop 0 ctxt []

let get_baker _ _ _ = assert false (* TO DO *)
