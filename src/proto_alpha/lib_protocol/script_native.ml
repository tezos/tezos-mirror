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

  type error += Empty_deposit

  let execute_deposit (ctxt, (step_constants : Script_typed_ir.step_constants))
      (() : deposit) (storage : storage) :
      ((operation Script_list.t, storage) pair * context) tzresult Lwt.t =
    let open Lwt_result_syntax in
    let*? () = error_when Tez.(step_constants.amount = zero) Empty_deposit in
    let address =
      {destination = step_constants.sender; entrypoint = Entrypoint.default}
    in
    let* amount, ctxt =
      Clst_storage.get_balance_from_storage ctxt storage address
    in
    let added_amount =
      Tez.to_mutez step_constants.amount
      |> Script_int.of_int64 |> Script_int.abs
    in
    let new_amount = Script_int.(add_n added_amount amount) in
    let* new_ledger, ctxt =
      Script_big_map.update ctxt address (Some new_amount) storage
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

let () =
  register_error_kind
    `Branch
    ~id:"clst.empty_deposit"
    ~title:"Empty deposit"
    ~description:"Forbidden to deposit 0ꜩ to CLST contract."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Deposit of 0ꜩ on CLST are forbidden.")
    Data_encoding.unit
    (function CLST_contract.Empty_deposit -> Some () | _ -> None)
    (fun () -> CLST_contract.Empty_deposit)
