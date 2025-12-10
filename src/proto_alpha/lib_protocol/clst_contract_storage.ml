(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_native_types
open Script_typed_ir

(* In case of single assets contracts, the token id of the asset is always 0. *)
let token_id = Script_int.zero_n

let get_storage ctxt =
  let open Lwt_result_syntax in
  let* clst_contract_hash = Contract.get_clst_contract_hash ctxt in
  let* ctxt, clst_storage = Contract.get_storage ctxt clst_contract_hash in
  let identity : Alpha_context.t -> CLST_types.storage -> 'a =
   fun ctxt storage -> return (Some storage, ctxt)
  in
  match clst_storage with
  | Some clst_storage ->
      let*? (Ex_kind_and_types (CLST_kind, {storage_type; _})) =
        Script_native_types.get_typed_kind_and_types Script_native_repr.CLST
      in
      let elab_conf = Script_ir_translator_config.make ~legacy:true () in
      let* storage, ctxt =
        Script_ir_translator.parse_storage
          ~elab_conf
          ctxt
          ~allow_forged_tickets:true
          ~allow_forged_lazy_storage_id:true
          storage_type
          ~storage:(Script.lazy_expr clst_storage)
      in
      (* `parse_storage`'s result cannot be retrieved as is, as it will yield a
         GADT error (basically the storage escaping its scope, due to
         `Ex_kind_and_types`). As such we return the storage through an identity
         function. Thanks @lrand for the trick. *)
      identity ctxt storage
  | None -> return (None, ctxt)

let get_balance_from_storage ctxt (ledger, _total_supply) contract =
  let open Lwt_result_syntax in
  let* balance, ctxt = Script_big_map.get ctxt contract ledger in
  return (Option.value balance ~default:Script_int.zero_n, ctxt)

let get_balance ctxt contract =
  let open Lwt_result_syntax in
  let contract =
    Script_typed_ir.
      {
        destination = Destination.Contract contract;
        entrypoint = Entrypoint.default;
      }
  in
  let* storage_opt, ctxt = get_storage ctxt in
  match storage_opt with
  | Some storage -> get_balance_from_storage ctxt storage contract
  | None -> return (Script_int.zero_n, ctxt)

let set_balance_from_storage ctxt (ledger, total_supply) contract amount =
  let open Lwt_result_syntax in
  let* ledger, ctxt =
    Script_big_map.update ctxt contract (Some amount) ledger
  in
  return ((ledger, total_supply), ctxt)

let get_total_supply ctxt =
  let open Lwt_result_syntax in
  let* storage_opt, ctxt = get_storage ctxt in
  let total_supply =
    match storage_opt with
    | Some (_ledger, total_supply) -> total_supply
    | None -> Script_int.zero_n
  in
  return (total_supply, ctxt)

let deposit_to_clst_deposits ctxt ~clst_contract_hash amount =
  let clst_contract = Contract.Originated clst_contract_hash in
  Token.transfer ctxt (`Contract clst_contract) `CLST_deposits amount

let withdraw_from_clst_deposits ctxt ~clst_contract_hash amount =
  let clst_contract = Contract.Originated clst_contract_hash in
  Token.transfer ctxt `CLST_deposits (`Contract clst_contract) amount
