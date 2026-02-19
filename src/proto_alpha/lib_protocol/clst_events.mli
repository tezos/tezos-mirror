(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_typed_ir
open Script_native_types

(** [transfer_event] returns an internal event operation of type
   (pair %transfer_event
     (address %from_)
     (pair (address %to_) (pair (nat %token_id) (nat %amount))))
    with tag [entrypoint]. *)
val transfer_event :
  context * step_constants ->
  entrypoint:Entrypoint.t ->
  sender:address ->
  receiver:address ->
  token_id:CLST_types.nat ->
  amount:CLST_types.nat ->
  (Script_typed_ir.operation * context) tzresult Lwt.t

(** [balance_update_event] returns an internal event operation of type
    (pair %balance_update
      (address %owner)
      (pair (nat %token_id) (pair (nat %new_balance) (int %diff))))
    with tag [entrypoint]. *)
val balance_update_event :
  context * step_constants ->
  entrypoint:Entrypoint.t ->
  owner:address ->
  token_id:CLST_types.nat ->
  new_balance:CLST_types.nat ->
  diff:CLST_types.int ->
  (Script_typed_ir.operation * context) tzresult Lwt.t

(** [total_supply_update_event] returns an internal event operation of type
    (pair %total_supply_update
      (nat %token_id) (pair (nat %new_total_supply) (int %diff)))
    with tag [entrypoint]. *)
val total_supply_update_event :
  context * step_constants ->
  entrypoint:Entrypoint.t ->
  token_id:CLST_types.nat ->
  new_total_supply:CLST_types.nat ->
  diff:CLST_types.int ->
  (Script_typed_ir.operation * context) tzresult Lwt.t

(** [allowance_update_event] returns an internal event operation of type
    (pair %allowance_update
      (address %owner)
      (pair (address %spender)
      (pair (nat %token_id) (pair (nat %new_allowance) (int %diff)))))
    with tag [entrypoint]. *)
val allowance_update_event :
  context * step_constants ->
  entrypoint:Entrypoint.t ->
  owner:address ->
  spender:address ->
  token_id:CLST_types.nat ->
  new_allowance:CLST_types.nat ->
  diff:CLST_types.int ->
  (Script_typed_ir.operation * context) tzresult Lwt.t
