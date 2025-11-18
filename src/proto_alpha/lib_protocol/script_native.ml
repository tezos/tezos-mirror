(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_native_types
open Script_typed_ir

module CLST_contract = struct
  open Script_native_types.CLST_types

  let execute_deposit (ctxt, (step_constants : Script_typed_ir.step_constants))
      (() : deposit) (ledger : storage) :
      ((operation Script_list.t, storage) pair * context) tzresult Lwt.t =
    let open Lwt_result_syntax in
    let address =
      {destination = step_constants.sender; entrypoint = Entrypoint.default}
    in
    let* amount_opt, ctxt = Script_big_map.get ctxt address ledger in
    let added_amount =
      Tez.to_mutez step_constants.amount
      |> Script_int.of_int64 |> Script_int.abs
    in
    let new_amount =
      Script_int.(add_n added_amount (Option.value ~default:zero_n amount_opt))
    in
    let* new_ledger, ctxt =
      Script_big_map.update ctxt address (Some new_amount) ledger
    in
    return ((Script_list.empty, new_ledger), ctxt)

  let execute (ctxt, (step_constants : step_constants)) (value : arg)
      (storage : storage) =
    execute_deposit (ctxt, step_constants) value storage
end

let execute (type arg storage) (ctxt, step_constants)
    (kind : (arg, storage) kind) (arg : arg) (storage : storage) :
    ((operation Script_list.t, storage) pair * context, error trace) result
    Lwt.t =
  match kind with
  | CLST_kind -> CLST_contract.execute (ctxt, step_constants) arg storage
