(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** CLST finalization logic

    Note that most of the code is adapted from `Unstake_request_storage`.

    This module is responsible for handling finalization requests, and
    maintaining {!Storage.Clst.Redemption_requests} through {!Clst_storage}. Due to
    the fact the code itself needs to call {!Token} to handle moving tez from
    the clst redeemed frozen deposits, this code cannot be moved directly to
    {!Clst_storage} as is.
*)

type requests = Storage.Unstake_request.requests

type prepared_finalize_redemption = {
  finalizable : requests;
  unfinalizable : requests;
}

let split_redemption_requests_uncarbonated ctxt redemption_requests =
  let open Lwt_result_syntax in
  match redemption_requests with
  | [] -> return_none
  | requests -> (
      match Cycle_storage.greatest_unstake_finalizable_cycle ctxt with
      | None (* no finalizable cycle *) ->
          return_some {finalizable = []; unfinalizable = requests}
      | Some greatest_finalizable_cycle ->
          (* For now, slashing is not implemented/ *)
          let finalizable, unfinalizable =
            List.partition_map
              (fun request ->
                let redemption_request_cycle, _request_amount = request in
                if
                  Cycle_repr.(
                    redemption_request_cycle <= greatest_finalizable_cycle)
                then Left request
                else Right request)
              requests
          in
          return_some {finalizable; unfinalizable})

let split_redemption_requests ctxt redemption_requests =
  let open Lwt_result_syntax in
  let*? ctxt =
    Raw_context.consume_gas
      ctxt
      Adaptive_issuance_costs.prepare_finalize_unstake_cost
  in
  let* prepared =
    split_redemption_requests_uncarbonated ctxt redemption_requests
  in
  return (ctxt, prepared)

let perform_finalizable_redemption_transfers ctxt clst_contract staker
    finalizable =
  let open Lwt_result_syntax in
  List.fold_left_es
    (fun (ctxt, balance_updates, cumulated_amount) (cycle, amount) ->
      let*? cumulated_amount = Tez_repr.(cumulated_amount +? amount) in
      let+ ctxt, new_balance_updates =
        Token.transfer
          ctxt
          (`CLST_redeemed_frozen_deposits (staker, cycle))
          (`Contract clst_contract)
          amount
      in
      (ctxt, new_balance_updates @ balance_updates, cumulated_amount))
    (ctxt, [], Tez_repr.zero)
    finalizable

let finalize ctxt ~clst_contract ~staker redemption_requests =
  let open Lwt_result_syntax in
  let* ctxt, prepared_opt =
    split_redemption_requests ctxt redemption_requests
  in
  match prepared_opt with
  | None -> return (ctxt, [], Tez_repr.zero, [])
  | Some {finalizable; unfinalizable} ->
      let*? ctxt =
        Raw_context.consume_gas
          ctxt
          Adaptive_issuance_costs.finalize_unstake_and_check_cost
      in
      let* ctxt, balance_updates_finalized, total_finalized_amount =
        perform_finalizable_redemption_transfers
          ctxt
          clst_contract
          staker
          finalizable
      in
      return
        (ctxt, balance_updates_finalized, total_finalized_amount, unfinalizable)
