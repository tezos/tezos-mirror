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

type finalizable =
  (Signature.Public_key_hash.t * Cycle_repr.t * Tez_repr.t) list

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

let apply_slashes ~preserved_cycles slashing_history ~from_cycle amount =
  let first_cycle_to_apply_slash = from_cycle in
  let last_cycle_to_apply_slash = Cycle_repr.add from_cycle preserved_cycles in
  (* [slashing_history] is sorted so slashings always happen in the same order. *)
  List.fold_left
    (fun remain (slashing_cycle, slashing_percentage) ->
      if
        Cycle_repr.(
          slashing_cycle >= first_cycle_to_apply_slash
          && slashing_cycle <= last_cycle_to_apply_slash)
      then
        Tez_repr.(
          sub_opt
            remain
            (mul_percentage ~rounding:`Up amount slashing_percentage))
        |> Option.value ~default:Tez_repr.zero
      else remain)
    amount
    slashing_history

let prepare_finalize_unstake ctxt ~for_next_cycle_use_only_after_slashing
    contract =
  let open Lwt_result_syntax in
  let preserved_cycles = Constants_storage.preserved_cycles ctxt in
  let max_slashing_period = Constants_repr.max_slashing_period in
  let preserved_plus_slashing = preserved_cycles + max_slashing_period in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let current_cycle =
    if for_next_cycle_use_only_after_slashing then Cycle_repr.succ current_cycle
    else current_cycle
  in
  let* requests_opt = Storage.Contract.Unstake_requests.find ctxt contract in
  match requests_opt with
  | None | Some {delegate = _; requests = []} -> return_none
  | Some {delegate; requests} -> (
      match Cycle_repr.sub current_cycle preserved_plus_slashing with
      | None (* no finalizable cycle *) ->
          return_some {finalizable = []; unfinalizable = {delegate; requests}}
      | Some greatest_finalizable_cycle ->
          let* slashing_history_opt =
            Storage.Contract.Slashed_deposits.find
              ctxt
              (Contract_repr.Implicit delegate)
          in
          let slashing_history =
            Option.value slashing_history_opt ~default:[]
          in
          let finalizable, unfinalizable_requests =
            List.partition_map
              (fun request ->
                let request_cycle, request_amount = request in
                if Cycle_repr.(request_cycle <= greatest_finalizable_cycle) then
                  let new_amount =
                    apply_slashes
                      ~preserved_cycles
                      slashing_history
                      ~from_cycle:request_cycle
                      request_amount
                  in
                  Left (delegate, request_cycle, new_amount)
                else Right request)
              requests
          in
          let unfinalizable =
            Storage.Unstake_request.
              {delegate; requests = unfinalizable_requests}
          in
          return_some {finalizable; unfinalizable})

let update = Storage.Contract.Unstake_requests.update

let add ctxt ~contract ~delegate cycle amount =
  let open Lwt_result_syntax in
  let* requests_opt = Storage.Contract.Unstake_requests.find ctxt contract in
  let requests =
    match requests_opt with
    | None -> []
    | Some {delegate = request_delegate; requests} ->
        assert (Signature.Public_key_hash.(delegate = request_delegate)) ;
        requests
  in
  let*? requests = Storage.Unstake_request.add cycle amount requests in
  let unstake_request = Storage.Unstake_request.{delegate; requests} in
  let*! ctxt =
    Storage.Contract.Unstake_requests.add ctxt contract unstake_request
  in
  return ctxt

module For_RPC = struct
  let apply_slash_to_unstaked_unfinalizable ctxt {requests; delegate} =
    let open Lwt_result_syntax in
    let current_level = Raw_context.current_level ctxt in
    let cycle_eras = Raw_context.cycle_eras ctxt in
    let is_last_of_cycle = Level_repr.last_of_cycle ~cycle_eras current_level in
    let preserved_cycles = Constants_storage.preserved_cycles ctxt in
    let* slashing_history_opt =
      Storage.Contract.Slashed_deposits.find
        ctxt
        (Contract_repr.Implicit delegate)
    in
    let slashing_history = Option.value slashing_history_opt ~default:[] in
    let* slashing_history =
      if not (Constants_storage.adaptive_issuance_ns_enable ctxt) then
        (* Remove slashes that haven't been applied yet *)
        let slashing_history =
          if not is_last_of_cycle then
            List.filter
              (fun (cycle, _) -> Cycle_repr.(cycle < current_level.cycle))
              slashing_history
          else slashing_history
        in
        (* We get the current cycle's denunciations for events in the previous cycle,
           and remove them from the slashing events (since they haven't been applied yet).
           Another solution would be to add the slashing cycle in Storage.Contract.Slashed_deposits,
           but since it's only used for this specific RPC, let's not. *)
        let* denunciations_opt =
          Storage.Pending_denunciations.find ctxt delegate
        in
        let denunciations = Option.value denunciations_opt ~default:[] in
        let not_yet_slashed_pct =
          if is_last_of_cycle then Int_percentage.p0
          else
            List.fold_left
              (fun acc Denunciations_repr.{misbehaviour; _} ->
                let misbehaviour_cycle =
                  (Level_repr.level_from_raw
                     ~cycle_eras:(Raw_context.cycle_eras ctxt)
                     misbehaviour.level)
                    .cycle
                in
                if Cycle_repr.(misbehaviour_cycle = current_level.cycle) then
                  acc
                else
                  match misbehaviour.kind with
                  | Double_baking ->
                      Int_percentage.add_bounded
                        acc
                        (Constants_storage
                         .percentage_of_frozen_deposits_slashed_per_double_baking
                           ctxt)
                  | Double_attesting ->
                      Int_percentage.add_bounded
                        acc
                        (Constants_storage
                         .percentage_of_frozen_deposits_slashed_per_double_attestation
                           ctxt))
              Int_percentage.p0
              denunciations
        in
        return
        @@ List.map
             (fun (cycle, pct) ->
               if Cycle_repr.(succ cycle = current_level.cycle) then
                 (cycle, Int_percentage.(sub_bounded pct not_yet_slashed_pct))
               else (cycle, pct))
             slashing_history
      else if not is_last_of_cycle then
        return
        @@ List.filter
             (fun (cycle, _) -> Cycle_repr.(succ cycle < current_level.cycle))
             slashing_history
      else
        return
        @@ List.filter
             (fun (cycle, _) -> Cycle_repr.(cycle < current_level.cycle))
             slashing_history
    in
    List.map_es
      (fun (request_cycle, request_amount) ->
        let new_amount =
          apply_slashes
            ~preserved_cycles
            slashing_history
            ~from_cycle:request_cycle
            request_amount
        in
        return (request_cycle, new_amount))
      requests
end
