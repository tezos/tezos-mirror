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

let z100 = Z.of_int 100

let apply_slashes ~preserved_plus_slashing slashing_history ~from_cycle amount =
  let first_cycle_to_apply_slash = from_cycle in
  let last_cycle_to_apply_slash =
    Cycle_repr.add from_cycle (preserved_plus_slashing - 1)
  in
  (* [slashing_history] is sorted so slashings always happen in the same order. *)
  let amount = Z.of_int64 (Tez_repr.to_mutez amount) in
  let amount =
    List.fold_left
      (fun amount (slashing_cycle, slashing_percentage) ->
        if
          Cycle_repr.(
            slashing_cycle >= first_cycle_to_apply_slash
            && slashing_cycle <= last_cycle_to_apply_slash)
        then Z.div (Z.mul amount (Z.of_int (100 - slashing_percentage))) z100
        else amount)
      amount
      slashing_history
  in
  Tez_repr.of_mutez_exn (Z.to_int64 amount)

let prepare_finalize_unstake ctxt contract =
  let open Lwt_result_syntax in
  let preserved_cycles = Constants_storage.preserved_cycles ctxt in
  let max_slashing_period = Constants_storage.max_slashing_period ctxt in
  let preserved_plus_slashing = preserved_cycles + max_slashing_period in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  match Cycle_repr.sub current_cycle preserved_plus_slashing with
  | None (* no finalizable cycle *) -> return None
  | Some greatest_finalizable_cycle -> (
      let* requests_opt =
        Storage.Contract.Unstake_requests.find ctxt contract
      in
      match requests_opt with
      | None | Some {delegate = _; requests = []} -> return None
      | Some {delegate; requests} ->
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
                      ~preserved_plus_slashing
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
    let preserved_cycles = Constants_storage.preserved_cycles ctxt in
    let max_slashing_period = Constants_storage.max_slashing_period ctxt in
    let preserved_plus_slashing = preserved_cycles + max_slashing_period in
    let* slashing_history_opt =
      Storage.Contract.Slashed_deposits.find
        ctxt
        (Contract_repr.Implicit delegate)
    in
    let slashing_history = Option.value slashing_history_opt ~default:[] in
    List.map_es
      (fun (request_cycle, request_amount) ->
        let new_amount =
          apply_slashes
            ~preserved_plus_slashing
            slashing_history
            ~from_cycle:request_cycle
            request_amount
        in
        return (request_cycle, new_amount))
      requests
end
