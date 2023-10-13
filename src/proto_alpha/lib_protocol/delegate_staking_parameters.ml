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

let pending_updates ctxt delegate =
  let contract = Contract_repr.Implicit delegate in
  let preserved_cycles = Constants_storage.preserved_cycles ctxt in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let to_cycle = Cycle_repr.add current_cycle (preserved_cycles + 1) in
  List.filter_map_es
    (fun cycle ->
      let open Lwt_result_syntax in
      let+ param_opt =
        Storage.Pending_staking_parameters.find (ctxt, cycle) contract
      in
      Option.map (fun param -> (cycle, param)) param_opt)
    Cycle_repr.(current_cycle ---> to_cycle)

let register_update ctxt delegate t =
  let open Lwt_result_syntax in
  let update_cycle =
    let current_level = Raw_context.current_level ctxt in
    let preserved_cycles = Constants_storage.preserved_cycles ctxt in
    Cycle_repr.add current_level.cycle (preserved_cycles + 1)
  in
  let*! ctxt =
    Storage.Pending_staking_parameters.add
      (ctxt, update_cycle)
      (Contract_repr.Implicit delegate)
      t
  in
  return ctxt

let activate ctxt ~new_cycle =
  let open Lwt_syntax in
  let* ctxt =
    Storage.Pending_staking_parameters.fold
      (ctxt, new_cycle)
      ~order:`Undefined
      ~init:ctxt
      ~f:(fun delegate t ctxt ->
        Storage.Contract.Staking_parameters.add ctxt delegate t)
  in
  Storage.Pending_staking_parameters.clear (ctxt, new_cycle)
