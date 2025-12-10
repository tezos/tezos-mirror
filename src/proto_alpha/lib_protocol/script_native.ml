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

  type error +=
    | Empty_transfer
    | Non_empty_transfer of Destination.t * Tez.t
    | Non_implicit_contract of Destination.t
    | Balance_too_low of Destination.t * nat * nat
    | Amount_too_large of Destination.t * nat

  let is_implicit : Destination.t -> bool = function
    | Destination.Contract (Contract.Implicit _) -> true
    | _ -> false

  let execute_deposit (ctxt, (step_constants : Script_typed_ir.step_constants))
      (() : deposit) (storage : storage) :
      ((operation Script_list.t, storage) pair * context) tzresult Lwt.t =
    let open Lwt_result_syntax in
    let*? () = error_when Tez.(step_constants.amount = zero) Empty_transfer in
    let*? () =
      error_when
        (not (is_implicit step_constants.sender))
        (Non_implicit_contract step_constants.sender)
    in
    let address =
      {destination = step_constants.sender; entrypoint = Entrypoint.default}
    in
    let* amount, ctxt =
      Clst_contract_storage.get_balance_from_storage ctxt storage address
    in
    let added_amount =
      Tez.to_mutez step_constants.amount
      |> Script_int.of_int64 |> Script_int.abs
    in
    let new_amount = Script_int.(add_n added_amount amount) in
    let* new_storage, ctxt =
      Clst_contract_storage.set_balance_from_storage
        ctxt
        storage
        address
        new_amount
    in
    let new_ledger, total_supply = new_storage in
    let total_supply = Script_int.add_n total_supply added_amount in
    let* ctxt, _balance_updates =
      Clst_contract_storage.deposit_to_clst_deposits
        ctxt
        ~clst_contract_hash:step_constants.self
        step_constants.amount
    in
    return ((Script_list.empty, (new_ledger, total_supply)), ctxt)

  let execute_withdraw (ctxt, (step_constants : Script_typed_ir.step_constants))
      (amount : withdraw) (storage : storage) :
      ((operation Script_list.t, storage) pair * context) tzresult Lwt.t =
    let open Lwt_result_syntax in
    let*? () =
      error_when
        Tez.(step_constants.amount <> zero)
        (Non_empty_transfer (step_constants.sender, step_constants.amount))
    in
    let*? () =
      error_when
        Compare.Int.(Script_int.(compare zero_n amount) = 0)
        Empty_transfer
    in
    let*? () =
      error_when
        Compare.Int.(
          Script_int.(compare (of_int64 Int64.max_int) (int amount)) < 0)
        (Amount_too_large (step_constants.sender, amount))
    in
    let* typed_account =
      match step_constants.sender with
      | Contract (Implicit pkh) -> return (Typed_implicit pkh)
      | sender -> tzfail (Non_implicit_contract sender)
    in
    let address =
      {destination = step_constants.sender; entrypoint = Entrypoint.default}
    in
    let* current_amount, ctxt =
      Clst_contract_storage.get_balance_from_storage ctxt storage address
    in
    let* removed_amount =
      if Compare.Int.(Script_int.compare current_amount amount < 0) then
        tzfail (Balance_too_low (step_constants.sender, current_amount, amount))
      else return amount
    in
    let new_amount = Script_int.(abs (sub current_amount removed_amount)) in
    let* new_storage, ctxt =
      Clst_contract_storage.set_balance_from_storage
        ctxt
        storage
        address
        new_amount
    in
    let amount_tez =
      Tez.of_mutez_exn
        (Option.value ~default:0L (Script_int.to_int64 removed_amount))
    in
    let* ctxt, _balance_updates =
      Clst_contract_storage.withdraw_from_clst_deposits
        ctxt
        ~clst_contract_hash:step_constants.self
        amount_tez
    in
    let gas_counter, outdated_ctxt =
      Local_gas_counter.local_gas_counter_and_outdated_context ctxt
    in
    let* op, outdated_ctxt, gas_counter =
      Script_interpreter_defs.transfer
        (outdated_ctxt, step_constants)
        gas_counter
        amount_tez
        Micheline.dummy_location
        typed_account
        ()
    in
    let ctxt = Local_gas_counter.update_context gas_counter outdated_ctxt in
    let new_ledger, total_supply = new_storage in
    let total_supply = Script_int.(abs (sub total_supply removed_amount)) in
    return ((Script_list.of_list [op], (new_ledger, total_supply)), ctxt)

  let execute (ctxt, (step_constants : step_constants)) (value : arg)
      (storage : storage) =
    match entrypoint_from_arg value with
    | Deposit () -> execute_deposit (ctxt, step_constants) () storage
    | Withdraw amount -> execute_withdraw (ctxt, step_constants) amount storage

  module Views = struct
    let balance : storage ex_view tzresult =
      let open Result_syntax in
      let* name = Script_string.of_string "get_balance" in
      let* ty = CLST_types.balance_view_ty in
      let implementation (ctxt, _step_constants) ((address : address), token_id)
          (storage : storage) =
        let open Lwt_result_syntax in
        if
          Compare.Int.(
            Script_int.compare token_id Clst_contract_storage.token_id = 0)
        then Clst_contract_storage.get_balance_from_storage ctxt storage address
        else return (Script_int.zero_n, ctxt)
      in
      return (Ex_view {name; ty; implementation})

    let total_supply : storage ex_view tzresult =
      let open Result_syntax in
      let* name = Script_string.of_string "get_total_supply" in
      let implementation (ctxt, _step_constants) (() : unit)
          ((_ledger, total_supply) : storage) =
        let open Lwt_result_syntax in
        return (total_supply, ctxt)
      in
      return
        (Ex_view {name; ty = CLST_types.total_supply_view_ty; implementation})

    let is_token : storage ex_view tzresult =
      let open Result_syntax in
      let* name = Script_string.of_string "is_token" in
      let implementation (ctxt, _step_constants) (token_id : nat)
          (_storage : storage) =
        let open Lwt_result_syntax in
        let is_token =
          Compare.Int.(
            Script_int.compare token_id Clst_contract_storage.token_id = 0)
        in
        return (is_token, ctxt)
      in
      return (Ex_view {name; ty = CLST_types.is_token_view_ty; implementation})

    let view_map : storage Script_native_types.view_map tzresult =
      let open Result_syntax in
      let* (Ex_view {name = get_balance_name; _} as get_balance) = balance in
      let* (Ex_view {name = get_total_supply_name; _} as get_total_supply) =
        total_supply
      in
      let* (Ex_view {name = is_token_name; _} as is_token) = is_token in
      let view_map =
        Script_map.update
          get_balance_name
          (Some get_balance)
          (Script_map.empty string_t)
      in
      let view_map =
        Script_map.update get_total_supply_name (Some get_total_supply) view_map
      in
      let view_map = Script_map.update is_token_name (Some is_token) view_map in
      return view_map
  end
end

let get_views : type arg storage.
    (arg, storage) kind -> storage Script_native_types.view_map tzresult =
  function
  | CLST_kind -> CLST_contract.Views.view_map

let execute (type arg storage) (ctxt, step_constants)
    (kind : (arg, storage) kind) (arg : arg) (storage : storage) :
    ((operation Script_list.t, storage) pair * context, error trace) result
    Lwt.t =
  match kind with
  | CLST_kind -> CLST_contract.execute (ctxt, step_constants) arg storage

let () =
  register_error_kind
    `Branch
    ~id:"clst.empty_transfer"
    ~title:"Empty transfer"
    ~description:"Forbidden to deposit or withdraw 0ꜩ on CLST contract."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Deposit or withdraw 0ꜩ on CLST are forbidden.")
    Data_encoding.unit
    (function CLST_contract.Empty_transfer -> Some () | _ -> None)
    (fun () -> CLST_contract.Empty_transfer) ;
  register_error_kind
    `Branch
    ~id:"clst.non_empty_transfer"
    ~title:"Non empty transfer"
    ~description:"Transferred amount is not used"
    ~pp:(fun ppf (address, amount) ->
      Format.fprintf
        ppf
        "Transferred amount %a from contract %a is not used"
        Tez.pp
        amount
        Destination.pp
        address)
    Data_encoding.(
      obj2 (req "address" Destination.encoding) (req "amount" Tez.encoding))
    (function
      | CLST_contract.Non_empty_transfer (address, amount) ->
          Some (address, amount)
      | _ -> None)
    (fun (address, amount) ->
      CLST_contract.Non_empty_transfer (address, amount)) ;
  register_error_kind
    `Branch
    ~id:"clst.non_implicit_contract"
    ~title:"Non implicit contract"
    ~description:"Only implicit contracts can deposit on CLST."
    ~pp:(fun ppf address ->
      Format.fprintf
        ppf
        "Only implicit contracts can deposit, %a is not implicit."
        Destination.pp
        address)
    Data_encoding.(obj1 (req "address" Destination.encoding))
    (function
      | CLST_contract.Non_implicit_contract address -> Some address | _ -> None)
    (fun address -> CLST_contract.Non_implicit_contract address) ;
  register_error_kind
    `Branch
    ~id:"clst.balance_too_low"
    ~title:"Balance is too low"
    ~description:"Spending more clst tokens than the contract has"
    ~pp:(fun ppf (address, balance, amount) ->
      Format.fprintf
        ppf
        "Balance of contract %a too low (%s) to spend %s"
        Destination.pp
        address
        (Script_int.to_string balance)
        (Script_int.to_string amount))
    Data_encoding.(
      obj3
        (req "address" Destination.encoding)
        (req "balance" Script_int.n_encoding)
        (req "amount" Script_int.n_encoding))
    (function
      | CLST_contract.Balance_too_low (address, balance, amount) ->
          Some (address, balance, amount)
      | _ -> None)
    (fun (address, balance, amount) ->
      CLST_contract.Balance_too_low (address, balance, amount)) ;
  register_error_kind
    `Branch
    ~id:"clst.amount_too_large"
    ~title:"Amount is too large"
    ~description:"Amount is too large for transfer"
    ~pp:(fun ppf (address, amount) ->
      Format.fprintf
        ppf
        "Amount %s is too large to transfer to contract %a"
        (Script_int.to_string amount)
        Destination.pp
        address)
    Data_encoding.(
      obj2
        (req "address" Destination.encoding)
        (req "amount" Script_int.n_encoding))
    (function
      | CLST_contract.Amount_too_large (address, amount) ->
          Some (address, amount)
      | _ -> None)
    (fun (address, amount) -> CLST_contract.Amount_too_large (address, amount))
