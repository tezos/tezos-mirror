(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Note that this implementation of is mostly copied from
   {!Delegate_staking_parameters}. *)

let get_delegate_parameters ctxt delegate =
  Storage.Clst.Registered_delegates.find ctxt (Contract_repr.Implicit delegate)

let get_pending_parameters ctxt delegate =
  let contract = Contract_repr.Implicit delegate in
  let activation_delay = Constants_storage.consensus_rights_delay ctxt in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let to_cycle = Cycle_repr.add current_cycle (activation_delay + 1) in
  List.filter_map_es
    (fun cycle ->
      let open Lwt_result_syntax in
      let+ param_opt =
        Storage.Clst.Pending_delegate_parameters.find (ctxt, cycle) contract
      in
      Option.map (fun param -> (cycle, param)) param_opt)
    Cycle_repr.(current_cycle ---> to_cycle)

let add_update ctxt delegate t =
  let open Lwt_result_syntax in
  let update_cycle =
    let current_level = Raw_context.current_level ctxt in
    let activation_delay = Constants_storage.consensus_rights_delay ctxt in
    Cycle_repr.add current_level.cycle (activation_delay + 1)
  in
  let*! ctxt =
    Storage.Clst.Pending_delegate_parameters.add
      (ctxt, update_cycle)
      (Contract_repr.Implicit delegate)
      t
  in
  return ctxt

let register_pending_parameters ctxt delegate t =
  add_update ctxt delegate (Update t)

let unregister ctxt delegate = add_update ctxt delegate Unregister

let activate_parameters ctxt ~new_cycle =
  let open Lwt_syntax in
  let* ctxt =
    Storage.Clst.Pending_delegate_parameters.fold
      (ctxt, new_cycle)
      ~order:`Undefined
      ~init:ctxt
      ~f:(fun contract t ctxt ->
        match t with
        | Update t -> Storage.Clst.Registered_delegates.add ctxt contract t
        | Unregister -> Storage.Clst.Registered_delegates.remove ctxt contract)
  in
  Storage.Clst.Pending_delegate_parameters.clear (ctxt, new_cycle)
