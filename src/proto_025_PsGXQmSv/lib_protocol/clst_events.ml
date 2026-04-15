(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_typed_ir
open Local_gas_counter
open Script_native_types

let transfer_event (ctxt, sc) ~(entrypoint : Entrypoint.t) ~(sender : address)
    ~(receiver : address) ~(token_id : CLST_types.nat)
    ~(amount : CLST_types.nat) :
    (Script_typed_ir.operation * context) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let lgs, ctxt = local_gas_counter_and_outdated_context ctxt in
  let*? {untyped; typed} = CLST_types.transfer_event_type in
  let unparsed_ty = Micheline.strip_locations untyped in
  let (Ty_ex_c event_type) = typed in
  let event_data = (sender, (receiver, (token_id, amount))) in
  let* op, outdated_ctxt, lgs =
    Script_interpreter_defs.emit_event
      (ctxt, sc)
      lgs
      ~event_type
      ~unparsed_ty
      ~tag:entrypoint
      ~event_data
  in
  let ctxt = update_context lgs outdated_ctxt in
  return (op, ctxt)

let balance_update_event (ctxt, sc) ~(entrypoint : Entrypoint.t)
    ~(owner : address) ~(token_id : CLST_types.nat)
    ~(new_balance : CLST_types.nat) ~(diff : CLST_types.int) :
    (Script_typed_ir.operation * context) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let lgs, ctxt = local_gas_counter_and_outdated_context ctxt in
  let*? {untyped; typed} = CLST_types.balance_update_event_type in
  let unparsed_ty = Micheline.strip_locations untyped in
  let (Ty_ex_c event_type) = typed in
  let event_data = (owner, (token_id, (new_balance, diff))) in
  let* op, outdated_ctxt, lgs =
    Script_interpreter_defs.emit_event
      (ctxt, sc)
      lgs
      ~event_type
      ~unparsed_ty
      ~tag:entrypoint
      ~event_data
  in
  let ctxt = update_context lgs outdated_ctxt in
  return (op, ctxt)

let total_supply_update_event (ctxt, sc) ~(entrypoint : Entrypoint.t)
    ~(token_id : CLST_types.nat) ~(new_total_supply : CLST_types.nat)
    ~(diff : CLST_types.int) :
    (Script_typed_ir.operation * context) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let lgs, ctxt = local_gas_counter_and_outdated_context ctxt in
  let*? {untyped; typed} = CLST_types.total_supply_update_event_type in
  let unparsed_ty = Micheline.strip_locations untyped in
  let (Ty_ex_c event_type) = typed in
  let event_data = (token_id, (new_total_supply, diff)) in
  let* op, outdated_ctxt, lgs =
    Script_interpreter_defs.emit_event
      (ctxt, sc)
      lgs
      ~event_type
      ~unparsed_ty
      ~tag:entrypoint
      ~event_data
  in
  let ctxt = update_context lgs outdated_ctxt in
  return (op, ctxt)

let allowance_update_event (ctxt, sc) ~(entrypoint : Entrypoint.t)
    ~(owner : address) ~(spender : address) ~(token_id : CLST_types.nat)
    ~(new_allowance : CLST_types.nat) ~(diff : CLST_types.int) :
    (Script_typed_ir.operation * context) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let lgs, ctxt = local_gas_counter_and_outdated_context ctxt in
  let*? {untyped; typed} = CLST_types.allowance_update_event_type in
  let unparsed_ty = Micheline.strip_locations untyped in
  let (Ty_ex_c event_type) = typed in
  let event_data = (owner, (spender, (token_id, (new_allowance, diff)))) in
  let* op, outdated_ctxt, lgs =
    Script_interpreter_defs.emit_event
      (ctxt, sc)
      lgs
      ~event_type
      ~unparsed_ty
      ~tag:entrypoint
      ~event_data
  in
  let ctxt = update_context lgs outdated_ctxt in
  return (op, ctxt)

let operator_update_event (ctxt, sc) ~(entrypoint : Entrypoint.t)
    ~(owner : address) ~(operator : address) ~(token_id : CLST_types.nat)
    ~(is_operator : bool) : (Script_typed_ir.operation * context) tzresult Lwt.t
    =
  let open Lwt_result_syntax in
  let lgs, ctxt = local_gas_counter_and_outdated_context ctxt in
  let*? {untyped; typed} = CLST_types.operator_update_event_type in
  let unparsed_ty = Micheline.strip_locations untyped in
  let (Ty_ex_c event_type) = typed in
  let event_data = (owner, (operator, (token_id, is_operator))) in
  let* op, outdated_ctxt, lgs =
    Script_interpreter_defs.emit_event
      (ctxt, sc)
      lgs
      ~event_type
      ~unparsed_ty
      ~tag:entrypoint
      ~event_data
  in
  let ctxt = update_context lgs outdated_ctxt in
  return (op, ctxt)

let token_metadata_update_event (ctxt, sc) ~(entrypoint : Entrypoint.t)
    ~(token_id : CLST_types.nat) ~(new_metadata : CLST_types.token_info option)
    : (Script_typed_ir.operation * context) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let lgs, ctxt = local_gas_counter_and_outdated_context ctxt in
  let*? {untyped; typed} = CLST_types.token_metadata_update_event_type in
  let unparsed_ty = Micheline.strip_locations untyped in
  let (Ty_ex_c event_type) = typed in
  let event_data = (token_id, new_metadata) in
  let* op, outdated_ctxt, lgs =
    Script_interpreter_defs.emit_event
      (ctxt, sc)
      lgs
      ~event_type
      ~unparsed_ty
      ~tag:entrypoint
      ~event_data
  in
  let ctxt = update_context lgs outdated_ctxt in
  return (op, ctxt)

let compare_address (a : address) (b : address) =
  Destination.compare a.destination b.destination

let compare_two_addresses (a1, a2) (b1, b2) =
  let c1 = compare_address a1 b1 in
  if Compare.Int.equal c1 0 then compare_address a2 b2 else c1

(* This map is used for avoiding [balance_update_event] duplication:
   [owner] -> [(new_balance, diff)] *)
module Address_map = Map.Make (struct
  type t = address

  let compare = compare_address
end)

(* This map is used for avoiding [allowance_update_event] and
   [operator_update_event] duplication:
   
   - [(owner, operator)] -> [is_operator]
   - [(owner, spender)] -> [(new_allowance, diff)] *)
module Pair_address_map = Map.Make (struct
  type t = address * address

  let compare = compare_two_addresses
end)

type new_value_diff = {
  new_value : CLST_types.nat;
  diff : Script_int.z Script_int.num;
}

type events = {
  balance_events : new_value_diff Address_map.t;
  allowance_events : new_value_diff Pair_address_map.t;
  operator_events : bool Pair_address_map.t;
  transfer_events : Script_typed_ir.operation list;
}

let init_events =
  {
    balance_events = Address_map.empty;
    allowance_events = Pair_address_map.empty;
    operator_events = Pair_address_map.empty;
    transfer_events = [];
  }

let update_balance_events (events : events) ~(owner : address)
    ~(new_balance : CLST_types.nat) ~(diff : Script_int.z Script_int.num) =
  let balance_events =
    Address_map.update
      owner
      (function
        | None -> Some {new_value = new_balance; diff}
        | Some acc ->
            Some {new_value = new_balance; diff = Script_int.add acc.diff diff})
      events.balance_events
  in
  {events with balance_events}

let update_allowance_events (events : events) ~(owner : address)
    ~(spender : address) ~(new_allowance : CLST_types.nat)
    ~(diff : Script_int.z Script_int.num) =
  let allowance_events =
    Pair_address_map.update
      (owner, spender)
      (function
        | None -> Some {new_value = new_allowance; diff}
        | Some acc ->
            Some
              {new_value = new_allowance; diff = Script_int.add acc.diff diff})
      events.allowance_events
  in
  {events with allowance_events}

let update_operator_events events ~(owner : address) ~(operator : address)
    ~(is_operator : bool) =
  let operator_events =
    Pair_address_map.add (owner, operator) is_operator events.operator_events
  in
  {events with operator_events}

let add_transfer_event events op =
  {events with transfer_events = op :: events.transfer_events}

let emit_balance_events (ctxt, sc) ~(entrypoint : Entrypoint.t)
    (balance_events : new_value_diff Address_map.t) ops =
  let open Lwt_result_syntax in
  Address_map.fold_es
    (fun owner {new_value; diff} (ops, ctxt) ->
      let* op, ctxt =
        balance_update_event
          (ctxt, sc)
          ~entrypoint
          ~owner
          ~token_id:Clst_contract_storage.token_id
          ~new_balance:new_value
          ~diff
      in
      return (op :: ops, ctxt))
    balance_events
    (ops, ctxt)

let emit_allowance_events (ctxt, sc) ~(entrypoint : Entrypoint.t)
    (allowance_events : new_value_diff Pair_address_map.t) ops =
  let open Lwt_result_syntax in
  Pair_address_map.fold_es
    (fun (owner, spender) {new_value; diff} (ops, ctxt) ->
      let* op, ctxt =
        allowance_update_event
          (ctxt, sc)
          ~entrypoint
          ~owner
          ~spender
          ~token_id:Clst_contract_storage.token_id
          ~new_allowance:new_value
          ~diff
      in
      return (op :: ops, ctxt))
    allowance_events
    (ops, ctxt)

let emit_operator_events (ctxt, sc) ~(entrypoint : Entrypoint.t)
    (operator_events : bool Pair_address_map.t) ops =
  let open Lwt_result_syntax in
  Pair_address_map.fold_es
    (fun (owner, operator) is_operator (ops, ctxt) ->
      let* op, ctxt =
        operator_update_event
          (ctxt, sc)
          ~entrypoint
          ~owner
          ~operator
          ~token_id:Clst_contract_storage.token_id
          ~is_operator
      in
      return (op :: ops, ctxt))
    operator_events
    (ops, ctxt)

let emit_all_events (ctxt, sc) ~(entrypoint : Entrypoint.t) (events : events) =
  let open Lwt_result_syntax in
  let* ops, ctxt =
    emit_balance_events
      (ctxt, sc)
      ~entrypoint
      events.balance_events
      events.transfer_events
  in
  let* ops, ctxt =
    emit_allowance_events (ctxt, sc) ~entrypoint events.allowance_events ops
  in
  emit_operator_events (ctxt, sc) ~entrypoint events.operator_events ops
