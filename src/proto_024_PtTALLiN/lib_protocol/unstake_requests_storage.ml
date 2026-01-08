(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type error +=
  | Cannot_unstake_with_unfinalizable_unstake_requests_to_another_delegate

let () =
  register_error_kind
    `Permanent
    ~id:
      "operation.cannot_unstake_with_unfinalizable_unstake_requests_to_another_delegate"
    ~title:
      "Cannot unstake with unfinalizable unstake requests to another delegate"
    ~description:
      "Cannot unstake with unfinalizable unstake requests to another delegate"
    Data_encoding.unit
    (function
      | Cannot_unstake_with_unfinalizable_unstake_requests_to_another_delegate
        ->
          Some ()
      | _ -> None)
    (fun () ->
      Cannot_unstake_with_unfinalizable_unstake_requests_to_another_delegate)

type finalizable =
  (Signature.Public_key_hash.t * Cycle_repr.t * Tez_repr.t) list

type transfer_result = Raw_context.t * Receipt_repr.balance_update_item list

let finalizable_encoding =
  let open Data_encoding in
  let elt_encoding =
    obj3
      (req "delegate" Signature.Public_key_hash.encoding)
      (req "cycle" Cycle_repr.encoding)
      (req "amount" Tez_repr.encoding)
  in
  list elt_encoding

type stored_requests = Storage.Unstake_request.t = {
  delegate : Signature.Public_key_hash.t;
  requests : (Cycle_repr.t * Tez_repr.t) list;
}

let stored_requests_encoding =
  let open Data_encoding in
  let request_encoding =
    obj2 (req "cycle" Cycle_repr.encoding) (req "amount" Tez_repr.encoding)
  in
  conv
    (fun {delegate; requests} -> (delegate, requests))
    (fun (delegate, requests) -> {delegate; requests})
    (obj2
       (req "delegate" Signature.Public_key_hash.encoding)
       (req "requests" (list request_encoding)))

type prepared_finalize_unstake = {
  finalizable : finalizable;
  unfinalizable : stored_requests;
}

let prepared_finalize_unstake_encoding :
    prepared_finalize_unstake Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {finalizable; unfinalizable} -> (finalizable, unfinalizable))
    (fun (finalizable, unfinalizable) -> {finalizable; unfinalizable})
    (obj2
       (req "finalizable" finalizable_encoding)
       (req "unfinalizable" stored_requests_encoding))

let apply_slashes ctxt slashing_history ~unstake_request_cycle amount =
  let slashable_deposits_period =
    Constants_storage.slashable_deposits_period ctxt
  in
  (* [slashing_history] is sorted so slashings always happen in the same order. *)
  List.fold_left
    (fun remain (misbehaviour_cycle, slashing_percentage) ->
      if
        Cycle_repr.(
          (* The unstake request is recent enough to be slashed for
             the misbehaviour, because its funds were still staked at
             the time the baking rights for the misbehaviour level
             were computed. *)
          misbehaviour_cycle
          <= add unstake_request_cycle slashable_deposits_period
          &&
          (* The unstake request is old enough that it already
                existed when the misbehaviour was slashed (or will be
                slashed if it hasn't happened yet). Indeed, unstake
                requests created after the slashing contain funds that
                have already been slashed while they were still staked
                so they should not be slashed again. *)
          unstake_request_cycle
          <= add misbehaviour_cycle Constants_repr.slashing_delay)
      then
        Tez_repr.(
          sub_opt
            remain
            (mul_percentage ~rounding:`Up amount slashing_percentage))
        |> Option.value ~default:Tez_repr.zero
      else remain)
    amount
    slashing_history

let prepare_finalize_unstake_uncarbonated ctxt
    ~check_delegate_of_unfinalizable_requests contract =
  let open Lwt_result_syntax in
  let* requests_opt = Storage.Contract.Unstake_requests.find ctxt contract in
  match requests_opt with
  | None | Some {delegate = _; requests = []} -> return_none
  | Some {delegate; requests} -> (
      match Cycle_storage.greatest_unstake_finalizable_cycle ctxt with
      | None (* no finalizable cycle *) ->
          return_some {finalizable = []; unfinalizable = {delegate; requests}}
      | Some greatest_finalizable_cycle ->
          let* slashing_history_opt =
            Storage.Slashed_deposits.find ctxt delegate
          in
          let slashing_history =
            Option.value slashing_history_opt ~default:[]
          in
          (* Oxford values *)
          let* slashing_history_opt_o =
            Storage.Contract.Slashed_deposits__Oxford.find
              ctxt
              (Contract_repr.Implicit delegate)
          in
          let slashing_history_o =
            Option.value slashing_history_opt_o ~default:[]
            |> List.map (fun (a, b) -> (a, Percentage.convert_from_o_to_p b))
          in

          let slashing_history =
            List.fold_left
              (fun acc (cycle, percentage) ->
                Storage.Slashed_deposits_history.add cycle percentage acc)
              slashing_history_o
              slashing_history
          in
          let finalizable, unfinalizable_requests =
            List.partition_map
              (fun request ->
                let unstake_request_cycle, request_amount = request in
                if
                  Cycle_repr.(
                    unstake_request_cycle <= greatest_finalizable_cycle)
                then
                  let new_amount =
                    apply_slashes
                      ctxt
                      slashing_history
                      ~unstake_request_cycle
                      request_amount
                  in
                  Left (delegate, unstake_request_cycle, new_amount)
                else Right request)
              requests
          in
          let unfinalizable =
            Storage.Unstake_request.
              {delegate; requests = unfinalizable_requests}
          in
          let* () =
            if not (List.is_empty unfinalizable_requests) then
              check_delegate_of_unfinalizable_requests delegate
            else return_unit
          in
          return_some {finalizable; unfinalizable})

let prepare_finalize_unstake ctxt ~check_delegate_of_unfinalizable_requests
    contract =
  let open Lwt_result_syntax in
  let*? ctxt =
    Raw_context.consume_gas
      ctxt
      Adaptive_issuance_costs.prepare_finalize_unstake_cost
  in
  let* prepared =
    prepare_finalize_unstake_uncarbonated
      ctxt
      ~check_delegate_of_unfinalizable_requests
      contract
  in
  return (ctxt, prepared)

(* Update the storage with the given requests.

   If the given structure contains an empty list of requests, it means that
   there are no more funds to unstake, and thus there is no need to keep an
   entry for the contract.
*)
let set_stored_requests ctxt contract updated_requests =
  match updated_requests.requests with
  | [] -> Storage.Contract.Unstake_requests.remove ctxt contract
  | _ :: _ ->
      Storage.Contract.Unstake_requests.add ctxt contract updated_requests

let handle_finalizable_and_clear ctxt contract
    ~check_delegate_of_unfinalizable_requests ~handle_finalizable =
  let open Lwt_result_syntax in
  let* ctxt, prepared_opt =
    prepare_finalize_unstake
      ~check_delegate_of_unfinalizable_requests
      ctxt
      contract
  in
  match prepared_opt with
  | None -> return (ctxt, [])
  | Some {finalizable; unfinalizable} ->
      let*? ctxt =
        Raw_context.consume_gas
          ctxt
          Adaptive_issuance_costs.finalize_unstake_and_check_cost
      in
      let* ctxt, balance_updates_finalized =
        handle_finalizable ctxt finalizable
      in
      let*! ctxt = set_stored_requests ctxt contract unfinalizable in
      return (ctxt, balance_updates_finalized)

let finalize_and_add ctxt ~contract ~delegate ~handle_finalizable cycle amount =
  let open Lwt_result_syntax in
  let* ctxt, prepared_opt =
    prepare_finalize_unstake
      ~check_delegate_of_unfinalizable_requests:(fun request_delegate ->
        if Signature.Public_key_hash.(delegate <> request_delegate) then
          (* This would happen if the staker was allowed to stake towards
             a new delegate while having unfinalizable unstake requests,
             which is not allowed: it will fail earlier. Also, unstaking
             for 0 tez is a noop and does not change the state of the storage,
             so it does not allow to reach this error either. *)
          tzfail
            Cannot_unstake_with_unfinalizable_unstake_requests_to_another_delegate
        else return_unit)
      ctxt
      contract
  in
  let finalizable, requests =
    match prepared_opt with
    | None -> ([], [])
    | Some {finalizable; unfinalizable = {delegate = _; requests}} ->
        (finalizable, requests)
  in
  let*? requests = Storage.Unstake_request.add cycle amount requests in
  let unstake_request = Storage.Unstake_request.{delegate; requests} in
  let*! ctxt = set_stored_requests ctxt contract unstake_request in
  handle_finalizable ctxt finalizable

let can_stake_from_unstake ctxt ~delegate =
  let open Lwt_result_syntax in
  let* slashing_history_opt = Storage.Slashed_deposits.find ctxt delegate in
  let slashing_history = Option.value slashing_history_opt ~default:[] in

  let* slashing_history_opt_o =
    Storage.Contract.Slashed_deposits__Oxford.find
      ctxt
      (Contract_repr.Implicit delegate)
  in
  let slashing_history_o =
    Option.value slashing_history_opt_o ~default:[]
    |> List.map (fun (a, b) -> (a, Percentage.convert_from_o_to_p b))
  in

  let slashing_history = slashing_history @ slashing_history_o in

  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let slashable_deposits_period =
    Constants_storage.slashable_deposits_period ctxt
  in
  let oldest_slashable_cycle =
    Cycle_repr.sub current_cycle (slashable_deposits_period + 1)
    |> Option.value ~default:Cycle_repr.root
  in
  let*! is_denounced =
    Pending_denunciations_storage.has_pending_denunciations ctxt delegate
  in
  let is_slashed =
    List.exists
      (fun (x, _) -> Cycle_repr.(x >= oldest_slashable_cycle))
      slashing_history
  in
  return @@ not (is_denounced || is_slashed)

let remove_from_unfinalizable_requests_and_finalize ctxt ~contract ~delegate
    ~check_delegate_of_unfinalizable_requests
    ~(transfer_from_unstake :
       Raw_context.t ->
       Cycle_repr.t ->
       Signature.public_key_hash ->
       Contract_repr.t ->
       Tez_repr.t ->
       (Raw_context.t * 'a list) tzresult Lwt.t)
    ~(handle_finalizable :
       Raw_context.t -> finalizable -> transfer_result tzresult Lwt.t) amount :
    (transfer_result * Tez_repr.t) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* allowed =
    match contract with
    | Contract_repr.Implicit contract
      when Signature.Public_key_hash.(contract = delegate) ->
        can_stake_from_unstake ctxt ~delegate
    | Contract_repr.Originated _ | Contract_repr.Implicit _ -> return false
  in
  if not allowed then
    (* a slash could have modified the unstaked frozen deposits:
       unfinalizable stake requests cannot be spent *)
    let* ctxt, balance_updates =
      handle_finalizable_and_clear
        ~check_delegate_of_unfinalizable_requests
        ~handle_finalizable
        ctxt
        contract
    in
    return ((ctxt, balance_updates), amount)
  else
    let* ctxt, prepared_opt =
      prepare_finalize_unstake
        ~check_delegate_of_unfinalizable_requests
        ctxt
        contract
    in
    match prepared_opt with
    | None -> return ((ctxt, []), amount)
    | Some {finalizable; unfinalizable = {delegate; requests}} ->
        let requests_sorted =
          List.sort
            (fun (cycle1, _) (cycle2, _) ->
              Cycle_repr.compare cycle2 cycle1
              (* decreasing cycle order, to release first the tokens
                 that would be frozen for the longest time *))
            requests
        in
        let rec transfer_from_all_unstake ctxt balance_updates
            remaining_amount_to_transfer updated_requests_rev requests =
          if Tez_repr.(remaining_amount_to_transfer = zero) then
            return
              ( ctxt,
                balance_updates,
                Tez_repr.zero,
                List.rev_append requests updated_requests_rev )
          else
            match requests with
            | [] ->
                return
                  ( ctxt,
                    balance_updates,
                    remaining_amount_to_transfer,
                    updated_requests_rev )
            | (cycle, requested_amount) :: t ->
                if Tez_repr.(remaining_amount_to_transfer >= requested_amount)
                then
                  let* ctxt, cycle_balance_updates =
                    transfer_from_unstake
                      ctxt
                      cycle
                      delegate
                      contract
                      requested_amount
                  in
                  let*? remaining_amount =
                    Tez_repr.(remaining_amount_to_transfer -? requested_amount)
                  in
                  transfer_from_all_unstake
                    ctxt
                    (balance_updates @ cycle_balance_updates)
                    remaining_amount
                    updated_requests_rev
                    t
                else
                  let* ctxt, cycle_balance_updates =
                    transfer_from_unstake
                      ctxt
                      cycle
                      delegate
                      contract
                      remaining_amount_to_transfer
                  in
                  let*? new_requested_amount =
                    Tez_repr.(requested_amount -? remaining_amount_to_transfer)
                  in
                  return
                    ( ctxt,
                      balance_updates @ cycle_balance_updates,
                      Tez_repr.zero,
                      List.rev_append
                        t
                        ((cycle, new_requested_amount) :: updated_requests_rev)
                    )
        in
        let* ( ctxt,
               balance_updates,
               remaining_amount_to_transfer,
               updated_requests_rev ) =
          transfer_from_all_unstake ctxt [] amount [] requests_sorted
        in
        let updated_requests = List.rev updated_requests_rev in
        let*! ctxt =
          set_stored_requests
            ctxt
            contract
            {delegate; requests = updated_requests}
        in
        let* ctxt, balance_updates_finalisation =
          handle_finalizable ctxt finalizable
        in
        return
          ( (ctxt, balance_updates @ balance_updates_finalisation),
            remaining_amount_to_transfer )

module For_RPC = struct
  type nonrec prepared_finalize_unstake = prepared_finalize_unstake = {
    finalizable : finalizable;
    unfinalizable : stored_requests;
  }

  type nonrec stored_requests = stored_requests = {
    delegate : Signature.Public_key_hash.t;
    requests : (Cycle_repr.t * Tez_repr.t) list;
  }

  let prepared_finalize_unstake_encoding :
      prepared_finalize_unstake Data_encoding.t =
    prepared_finalize_unstake_encoding

  let prepare_finalize_unstake =
    prepare_finalize_unstake_uncarbonated
      ~check_delegate_of_unfinalizable_requests:(fun _ -> return_unit)

  let apply_slash_to_unstaked_unfinalizable ctxt {requests; delegate} =
    let open Lwt_result_syntax in
    let* slashing_history_opt = Storage.Slashed_deposits.find ctxt delegate in
    let slashing_history = Option.value slashing_history_opt ~default:[] in

    (* Oxford values *)
    let* slashing_history_opt =
      Storage.Contract.Slashed_deposits__Oxford.find
        ctxt
        (Contract_repr.Implicit delegate)
    in
    let slashing_history_o =
      Option.value slashing_history_opt ~default:[]
      |> List.map (fun (a, b) -> (a, Percentage.convert_from_o_to_p b))
    in

    let slashing_history =
      List.fold_left
        (fun acc (cycle, percentage) ->
          Storage.Slashed_deposits_history.add cycle percentage acc)
        slashing_history_o
        slashing_history
    in

    List.map_es
      (fun (unstake_request_cycle, request_amount) ->
        let new_amount =
          apply_slashes
            ctxt
            slashing_history
            ~unstake_request_cycle
            request_amount
        in
        return (unstake_request_cycle, new_amount))
      requests
end
