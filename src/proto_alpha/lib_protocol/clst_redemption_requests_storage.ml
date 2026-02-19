(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Abstraction for low level storage to handle CLST redemption requests *)

(* Adapted from {Unstake_requests_storage.set_stored_requests}.

   Update the storage with the given requests.

   If the given structure contains an empty list of requests, it means that
   there are no more funds to unstake, and thus there is no need to keep an
   entry for the contract.
*)
let set_stored_requests ctxt contract updated_requests =
  match updated_requests with
  | [] -> Storage.Clst.Redemption_requests.remove ctxt contract
  | _ :: _ ->
      Storage.Clst.Redemption_requests.add ctxt contract updated_requests

(* At some point, this function will also include implicit finalization of
   finalizable redemption requests. *)
let add_redemption_request ctxt contract cycle amount =
  let open Lwt_result_syntax in
  let* requests_opt = Storage.Clst.Redemption_requests.find ctxt contract in
  let requests = Option.value ~default:[] requests_opt in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/8228

     Storage.Unstake_request implements the logic to merge requests, but it DOES
     NOT update anything in the storage.  *)
  let*? requests = Storage.Unstake_request.add cycle amount requests in
  let*! ctxt = set_stored_requests ctxt contract requests in
  return ctxt

let finalize ctxt ~clst_contract ~staker =
  let open Lwt_result_syntax in
  let* redemption_requests =
    Storage.Clst.Redemption_requests.find ctxt staker
  in
  let redemption_requests = Option.value ~default:[] redemption_requests in
  let* ctxt, balance_updates, total_finalized_amount, unfinalizable_requests =
    Clst_finalization.finalize ctxt ~clst_contract ~staker redemption_requests
  in

  let*! ctxt = set_stored_requests ctxt staker unfinalizable_requests in
  return (ctxt, balance_updates, total_finalized_amount)

module For_RPC = struct
  let get_redeemed_balance ctxt =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/8227
       Handle slashing in the RPC. *)
    let open Lwt_result_syntax in
    function
    | Contract_repr.Originated _ -> return_none
    | Implicit _ as contract -> (
        let* redemption_requests =
          Storage.Clst.Redemption_requests.find ctxt contract
        in
        let redemption_requests =
          Option.value ~default:[] redemption_requests
        in
        let* redeemed_frozen_requests =
          Clst_finalization.For_RPC.split_redemption_requests
            ctxt
            redemption_requests
        in
        match redeemed_frozen_requests with
        | None -> return_some (Tez_repr.zero, Tez_repr.zero)
        | Some {finalizable; unfinalizable} ->
            let*? sum_unfinalizable =
              List.fold_left_e
                (fun acc (_cycle, tz) -> Tez_repr.(acc +? tz))
                Tez_repr.zero
                unfinalizable
            in
            let*? sum_finalizable =
              List.fold_left_e
                (fun acc (_cycle, tz) -> Tez_repr.(acc +? tz))
                Tez_repr.zero
                finalizable
            in
            return_some (sum_unfinalizable, sum_finalizable))

  let get_finalizable_redeemed_balance ctxt contract =
    let open Lwt_result_syntax in
    let* balances = get_redeemed_balance ctxt contract in
    return (Option.map snd balances)

  let get_unfinalizable_redeemed_balance ctxt contract =
    let open Lwt_result_syntax in
    let* balances = get_redeemed_balance ctxt contract in
    return (Option.map fst balances)
end
