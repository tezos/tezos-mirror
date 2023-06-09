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

let of_delegate ctxt delegate =
  let open Lwt_result_syntax in
  let* t =
    Storage.Contract.Staking_parameters.find
      ctxt
      (Contract_repr.Implicit delegate)
  in
  match t with
  | None -> return Staking_parameters_repr.default
  | Some t -> return t

let find ctxt delegate =
  Storage.Contract.Staking_parameters.find
    ctxt
    (Contract_repr.Implicit delegate)

let raw_pending_updates ctxt delegate =
  Storage.Contract.Pending_staking_parameters.bindings
    (ctxt, Contract_repr.Implicit delegate)

let pending_updates ctxt delegate =
  let open Lwt_syntax in
  let* updates = raw_pending_updates ctxt delegate in
  let updates =
    List.sort (fun (c1, _) (c2, _) -> Cycle_repr.compare c1 c2) updates
  in
  return updates

let raw_of_delegate_for_cycle ctxt delegate cycle =
  let open Lwt_result_syntax in
  let*! pendings = raw_pending_updates ctxt delegate in
  let* active = of_delegate ctxt delegate in
  let current_level = Raw_context.current_level ctxt in
  let active_for_cycle =
    List.fold_left
      (fun (c1, active) (c2, update) ->
        if Cycle_repr.(c1 < c2 && c2 <= cycle) then (c2, update)
        else (c1, active))
      (current_level.cycle, active)
      pendings
  in
  return active_for_cycle

let of_delegate_for_cycle ctxt delegate cycle =
  let open Lwt_result_syntax in
  let* _, t = raw_of_delegate_for_cycle ctxt delegate cycle in
  return t

let register_update ctxt delegate t =
  let open Lwt_result_syntax in
  let update_cycle =
    let current_level = Raw_context.current_level ctxt in
    let preserved_cycles = Constants_storage.preserved_cycles ctxt in
    Cycle_repr.add current_level.cycle (preserved_cycles + 1)
  in
  let*! ctxt =
    Storage.Contract.Pending_staking_parameters.add
      (ctxt, Contract_repr.Implicit delegate)
      update_cycle
      t
  in
  return ctxt

let activate ctxt ~new_cycle =
  let open Lwt_result_syntax in
  Storage.Delegates.fold
    ctxt
    ~order:`Undefined
    ~init:(ok ctxt)
    ~f:(fun delegate ctxt ->
      let*? ctxt in
      let delegate = Contract_repr.Implicit delegate in
      let* update =
        Storage.Contract.Pending_staking_parameters.find
          (ctxt, delegate)
          new_cycle
      in
      match update with
      | None -> return ctxt
      | Some t ->
          let*! ctxt =
            Storage.Contract.Staking_parameters.add ctxt delegate t
          in
          let*! ctxt =
            Storage.Contract.Pending_staking_parameters.remove
              (ctxt, delegate)
              new_cycle
          in
          return ctxt)
