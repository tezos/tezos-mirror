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
