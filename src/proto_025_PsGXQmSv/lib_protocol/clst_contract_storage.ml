(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_native_types
open Script_typed_ir

type error += Total_supply_underflow

type t = {
  ledger : CLST_types.ledger;
  total_supply : CLST_types.total_supply;
  operators_table : CLST_types.operators_table;
  token_metadata : CLST_types.token_metadata;
}

let from_clst_storage (ledger, (total_supply, (operators_table, token_metadata)))
    =
  {ledger; total_supply; operators_table; token_metadata}

let to_clst_storage {ledger; total_supply; operators_table; token_metadata} =
  (ledger, (total_supply, (operators_table, token_metadata)))

(* In case of single assets contracts, the token id of the asset is always 0. *)
let token_id = Script_int.zero_n

let ticket_token ~clst_hash =
  let open Lwt_result_syntax in
  let*? cty = CLST_types.clst_contents_ticket_ty in
  return
  @@ Ticket_token.Ex_token
       {
         ticketer = Contract.Originated clst_hash;
         contents_type = cty;
         contents = (token_id, (None : bytes option));
       }

let get_storage ctxt : (t option * context) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* clst_contract_hash = Contract.get_clst_contract_hash ctxt in
  let* ctxt, clst_storage = Contract.get_storage ctxt clst_contract_hash in
  let identity : Alpha_context.t -> CLST_types.storage -> 'a =
   fun ctxt storage -> return (Some (from_clst_storage storage), ctxt)
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

let get_balance_from_storage ctxt {ledger; _} contract =
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

let set_balance_from_storage ctxt storage contract amount =
  let open Lwt_result_syntax in
  let* ledger, ctxt =
    Script_big_map.update ctxt contract (Some amount) storage.ledger
  in
  return ({storage with ledger}, ctxt)

let get_total_supply ctxt =
  let open Lwt_result_syntax in
  let* storage_opt, ctxt = get_storage ctxt in
  let total_supply =
    match storage_opt with
    | Some {total_supply; _} -> total_supply
    | None -> Script_int.zero_n
  in
  return (total_supply, ctxt)

let get_total_supply_from_storage {total_supply; _} = total_supply

let increment_total_supply storage added_amount =
  {
    storage with
    total_supply = Script_int.add_n storage.total_supply added_amount;
  }

let decrement_total_supply storage removed_amount =
  match
    Script_int.sub storage.total_supply removed_amount |> Script_int.is_nat
  with
  | None -> error Total_supply_underflow
  | Some total_supply -> ok {storage with total_supply}

let exchange_rate_from_storage ctxt ~total_supply =
  let open Lwt_result_syntax in
  let* total_amount_of_tez = Clst.total_amount_of_tez ctxt in
  let total_amount_of_tez = Tez.to_mutez total_amount_of_tez in
  let total_supply = Script_int.to_zint total_supply in
  let rate =
    if Compare.Int64.equal total_amount_of_tez 0L || Z.equal total_supply Z.zero
    then
      (* This follows Invariant 1 from Staking_pseudotokens_storage: when there
         are no tokens, the rate is 1. *)
      Q.one
    else Q.div (Q.of_int64 total_amount_of_tez) (Q.of_bigint total_supply)
  in
  return rate

let exchange_rate ctxt =
  let open Lwt_result_syntax in
  let* total_supply, _ = get_total_supply ctxt in
  exchange_rate_from_storage ctxt ~total_supply

let tez_to_clst_tokens ctxt ~total_supply tez_amount =
  let open Lwt_result_syntax in
  let* {num = total_amount_of_tez; den = total_supply} =
    exchange_rate_from_storage ctxt ~total_supply
  in
  let total_supply_int64 = Z.to_int64 total_supply in
  let total_amount_of_mutez = Z.to_int64 total_amount_of_tez in
  let*? result =
    Tez.mul_ratio
      ~rounding:`Down
      tez_amount
      ~num:total_supply_int64
      ~den:total_amount_of_mutez
  in
  return (Script_int.(abs (of_int64 (Tez.to_mutez result))), ctxt)

let clst_tokens_to_tez ctxt ~total_supply token_amount =
  let open Lwt_result_syntax in
  let* {num = total_amount_of_tez; den = total_supply} =
    exchange_rate_from_storage ctxt ~total_supply
  in
  let total_supply_int64 = Z.to_int64 total_supply in
  let total_amount_of_mutez =
    (* We know that the total supply of tez cannot overflow, otherwise the total
       supply of tez of the whole protocol would have overflown before the
       contract. And it cannot be negative. *)
    Z.to_int64 total_amount_of_tez
    |> Tez.of_mutez
    |> Option.value ~default:Tez.zero
  in
  let token_amount_int64 =
    Option.value ~default:0L (Script_int.to_int64 token_amount)
  in
  let*? result =
    Tez.mul_ratio
      ~rounding:`Down
      total_amount_of_mutez
      ~num:token_amount_int64
      ~den:total_supply_int64
  in
  return (result, ctxt)

let deposit_to_clst_deposits ctxt ~clst_contract_hash amount =
  let clst_contract = Contract.Originated clst_contract_hash in
  Token.transfer ctxt (`Contract clst_contract) `CLST_deposits amount

let redeem_from_clst_deposits ctxt ~staker amount =
  let open Lwt_result_syntax in
  let current_cycle = (Level.current ctxt).cycle in
  let* ctxt, redeem_balance_update =
    Token.transfer
      ctxt
      `CLST_deposits
      (`CLST_redeemed_frozen_deposits (staker, current_cycle))
      amount
  in
  let* ctxt = Clst.add_redemption_request ctxt staker current_cycle amount in
  return (ctxt, redeem_balance_update)

let finalize ctxt ~clst_contract_hash ~staker =
  let open Lwt_result_syntax in
  let clst_contract = Contract.Originated clst_contract_hash in
  let* ctxt, unstake_balance_update, finalized_amount =
    Clst.finalize ctxt ~clst_contract ~staker
  in
  return (ctxt, unstake_balance_update, finalized_amount)

type allowance = Infinite | Finite of CLST_types.nat

let get_account_operator_allowance context storage ~owner ~spender =
  let open Lwt_result_syntax in
  let* operators, context =
    Script_big_map.get context owner storage.operators_table
  in
  let account_operators =
    Option.value ~default:(Script_map.empty address_t) operators
  in
  let allowance = Script_map.get spender account_operators in
  let allowance =
    match allowance with
    | Some None -> Some Infinite
    | Some (Some n) -> Some (Finite n)
    | None -> None
  in
  return (allowance, context)

let set_account_operator_allowance context storage ~owner ~spender new_allowance
    =
  let open Lwt_result_syntax in
  let* operators, context =
    Script_big_map.get context owner storage.operators_table
  in
  let account_operators =
    Option.value ~default:(Script_map.empty address_t) operators
  in
  let new_allowance =
    match new_allowance with
    | None -> None
    | Some Infinite -> Some None
    | Some (Finite n) -> Some (Some n)
  in
  let updated_operators =
    Script_map.update spender new_allowance account_operators
  in
  let* operators_table, context =
    Script_big_map.update
      context
      owner
      (Some updated_operators)
      storage.operators_table
  in
  return ({storage with operators_table}, context)

let get_token_info context storage ~token_id =
  Script_big_map.get context token_id storage.token_metadata

let is_delegate_registered ctxt delegate =
  let open Lwt_result_syntax in
  let* registered_parameters =
    Clst.Delegates.get_delegate_parameters ctxt delegate
  in
  return (Option.is_some registered_parameters)

let delegate_last_pending_update ctxt delegate =
  let open Lwt_result_syntax in
  let* pending_parameters =
    Clst.Delegates.get_pending_parameters ctxt delegate
  in
  let rec check = function
    | [] -> None
    | [(_, update)] -> Some update
    | _ :: rem -> check rem
  in
  return (check pending_parameters)

let is_delegate_eventually_registered ctxt delegate =
  let open Lwt_result_syntax in
  let* last_pending_update = delegate_last_pending_update ctxt delegate in
  match last_pending_update with
  (* If it has no pending updates, we simply look at the active parameters *)
  | None -> is_delegate_registered ctxt delegate
  | Some (Clst_delegates_parameters_repr.Update _) -> return_true
  | Some Unregister -> return_false

let register_delegate ctxt ~delegate ~edge_of_clst_staking_over_baking_millionth
    ~ratio_of_clst_staking_over_direct_staking_billionth =
  let open Lwt_result_syntax in
  let edge_of_clst_staking_over_baking_millionth =
    Script_int.to_zint edge_of_clst_staking_over_baking_millionth
  in
  let ratio_of_clst_staking_over_direct_staking_billionth =
    Script_int.to_zint ratio_of_clst_staking_over_direct_staking_billionth
  in
  let*? parameters =
    if
      not
        (Z.fits_int32 edge_of_clst_staking_over_baking_millionth
        || Z.fits_int32 ratio_of_clst_staking_over_direct_staking_billionth)
    then
      Result_syntax.tzfail
        Clst_delegates_parameters_repr.Invalid_clst_delegates_parameters
    else
      Clst_delegates_parameters_repr.make
        ~edge_of_clst_staking_over_baking_millionth:
          (Z.to_int32 edge_of_clst_staking_over_baking_millionth)
        ~ratio_of_clst_staking_over_direct_staking_billionth:
          (Z.to_int32 ratio_of_clst_staking_over_direct_staking_billionth)
  in
  Alpha_context.Clst.Delegates.register_pending_parameters
    ctxt
    delegate
    parameters

let unregister_delegate ctxt ~delegate = Clst.Delegates.unregister ctxt delegate
