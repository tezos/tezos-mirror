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

let current_unslashable_cycle ctxt =
  let cycle = (Raw_context.current_level ctxt).cycle in
  let preserved_cycles = Constants_storage.preserved_cycles ctxt in
  let max_slashing_period = Constants_storage.max_slashing_period ctxt in
  Cycle_repr.sub cycle (preserved_cycles + max_slashing_period - 1)

module Internal = struct
  let get_all ctxt contract =
    let open Lwt_result_syntax in
    let* unstaked_frozen_deposits_opt =
      Storage.Contract.Unstaked_frozen_deposits.find ctxt contract
    in
    let unslashable_cycle = current_unslashable_cycle ctxt in
    match unstaked_frozen_deposits_opt with
    | None -> return (Unstaked_frozen_deposits_repr.empty ~unslashable_cycle)
    | Some unstaked_frozen_deposits ->
        Lwt.return
        @@ Unstaked_frozen_deposits_repr.squash_unslashable
             ~unslashable_cycle
             unstaked_frozen_deposits

  let get ctxt contract ~normalized_cycle =
    let open Lwt_result_syntax in
    let+ unstaked_frozen_deposits = get_all ctxt contract in
    Unstaked_frozen_deposits_repr.get ~normalized_cycle unstaked_frozen_deposits

  let update_balance ~f ctxt contract ~normalized_cycle =
    let open Lwt_result_syntax in
    let* unstaked_frozen_deposits = get_all ctxt contract in
    let*? unstaked_frozen_deposits =
      Unstaked_frozen_deposits_repr.update
        ~f
        ~normalized_cycle
        unstaked_frozen_deposits
    in
    let*! ctxt =
      Storage.Contract.Unstaked_frozen_deposits.add
        ctxt
        contract
        unstaked_frozen_deposits.t
    in
    return ctxt
end

let normalized_cycle ctxt ~cycle =
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  assert (Cycle_repr.(cycle <= current_cycle)) ;
  match current_unslashable_cycle ctxt with
  | None -> cycle
  | Some unslashable_cycle -> Cycle_repr.max cycle unslashable_cycle

let get ctxt delegate cycle =
  let contract = Contract_repr.Implicit delegate in
  let normalized_cycle = normalized_cycle ctxt ~cycle in
  Internal.get ctxt contract ~normalized_cycle

let balance ctxt delegate cycle =
  let open Lwt_result_syntax in
  let+ frozen_deposits = get ctxt delegate cycle in
  frozen_deposits.current_amount

let credit_only_call_from_token ctxt delegate cycle amount =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit delegate in
  let normalized_cycle = normalized_cycle ctxt ~cycle in
  let f deposits = Deposits_repr.(deposits +? amount) in
  let* ctxt = Stake_storage.add_stake ctxt delegate amount in
  Internal.update_balance ~f ctxt contract ~normalized_cycle

let spend_only_call_from_token ctxt delegate cycle amount =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit delegate in
  let normalized_cycle = normalized_cycle ctxt ~cycle in
  let f Deposits_repr.{initial_amount; current_amount} =
    let open Result_syntax in
    let+ current_amount = Tez_repr.(current_amount -? amount) in
    Deposits_repr.{initial_amount; current_amount}
  in
  let* ctxt = Stake_storage.remove_stake ctxt delegate amount in
  Internal.update_balance ~f ctxt contract ~normalized_cycle
