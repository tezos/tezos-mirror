(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let is_inactive ctxt delegate =
  let open Lwt_result_syntax in
  let*! inactive =
    Storage.Contract.Inactive_delegate.mem
      ctxt
      (Contract_repr.Implicit delegate)
  in
  if inactive then Lwt.return_ok inactive
  else
    let+ cycle_opt =
      Storage.Contract.Delegate_last_cycle_before_deactivation.find
        ctxt
        (Contract_repr.Implicit delegate)
    in
    match cycle_opt with
    | Some last_active_cycle ->
        let ({Level_repr.cycle = current_cycle; _} : Level_repr.t) =
          Raw_context.current_level ctxt
        in
        Cycle_repr.(last_active_cycle < current_cycle)
    | None ->
        (* This case is only when called from `set_active`, when creating
             a contract. *)
        false

let last_cycle_before_deactivation ctxt delegate =
  let contract = Contract_repr.Implicit delegate in
  Storage.Contract.Delegate_last_cycle_before_deactivation.get ctxt contract

let set_inactive ctxt delegate =
  Storage.Contract.Inactive_delegate.add ctxt (Contract_repr.Implicit delegate)

let set_active ctxt delegate =
  let open Lwt_result_syntax in
  let* inactive = is_inactive ctxt delegate in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let preserved_cycles = Constants_storage.preserved_cycles ctxt in
  (* We allow a number of cycles before a delegate is deactivated as follows:
     - if the delegate is active, we give it at least `1 + preserved_cycles`
     after the current cycle before to be deactivated.
     - if the delegate is new or inactive, we give it additionally
     `preserved_cycles` because the delegate needs this number of cycles to
     receive rights, so `1 + 2 * preserved_cycles` in total. *)
  let delegate_contract = Contract_repr.Implicit delegate in
  let* current_last_active_cycle =
    Storage.Contract.Delegate_last_cycle_before_deactivation.find
      ctxt
      delegate_contract
  in
  let last_active_cycle =
    match current_last_active_cycle with
    | None -> Cycle_repr.add current_cycle (1 + (2 * preserved_cycles))
    | Some current_last_active_cycle ->
        let delay =
          if inactive then 1 + (2 * preserved_cycles) else 1 + preserved_cycles
        in
        let updated = Cycle_repr.add current_cycle delay in
        Cycle_repr.max current_last_active_cycle updated
  in
  let*! ctxt =
    Storage.Contract.Delegate_last_cycle_before_deactivation.add
      ctxt
      delegate_contract
      last_active_cycle
  in
  if not inactive then return (ctxt, inactive)
  else
    let*! ctxt =
      Storage.Contract.Inactive_delegate.remove ctxt delegate_contract
    in
    return (ctxt, inactive)
