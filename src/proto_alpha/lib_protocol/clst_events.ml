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
