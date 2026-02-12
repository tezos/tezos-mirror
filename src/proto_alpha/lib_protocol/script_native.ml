(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_native_types
open Script_typed_ir

(** Returns the same error as when executing [PUSH string
    <error_mnemonic> ; FAILWITH] in a script.

    Used in native contracts that must be FA2.1-compliant, so that
    errors that are specified in the standard will behave identically
    to scripted FA2.1 contracts.  *)
let standard_error ~mnemonic =
  let open Micheline in
  Script_interpreter_errors.Reject
    (dummy_location, strip_locations (String (dummy_location, mnemonic)), None)

module CLST_contract = struct
  open Script_native_types.CLST_types

  type error +=
    | Empty_transfer
    | Non_empty_transfer of Destination.t * Tez.t
    | Non_implicit_contract of Destination.t
    | Balance_too_low of Destination.t * nat * nat
    | Amount_too_large of Destination.t * nat
    | Only_owner_can_change_operator of Destination.t * Destination.t

  let is_implicit : Destination.t -> bool = function
    | Destination.Contract (Contract.Implicit _) -> true
    | _ -> false

  type entrypoint_execution_result =
    operation Script_list.t
    * Clst_contract_storage.t
    * Receipt.balance_updates
    * context

  let execute_deposit (ctxt, (step_constants : Script_typed_ir.step_constants))
      (() : deposit) (storage : Clst_contract_storage.t) :
      entrypoint_execution_result tzresult Lwt.t =
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
    let new_storage =
      Clst_contract_storage.increment_total_supply new_storage added_amount
    in
    let* ctxt, balance_updates =
      Clst_contract_storage.deposit_to_clst_deposits
        ctxt
        ~clst_contract_hash:step_constants.self
        step_constants.amount
    in
    let*? entrypoint_str = Entrypoint.of_string_lax "deposit" in
    let* op_balance_event, ctxt =
      Clst_events.balance_update_event
        (ctxt, step_constants)
        ~entrypoint:entrypoint_str
        ~owner:address
        ~token_id:Clst_contract_storage.token_id
        ~new_balance:new_amount
        ~diff:(Script_int.int added_amount)
    in
    let* op_total_supply_event, ctxt =
      Clst_events.total_supply_update_event
        (ctxt, step_constants)
        ~entrypoint:entrypoint_str
        ~token_id:Clst_contract_storage.token_id
        ~new_total_supply:
          (Clst_contract_storage.get_total_supply_from_storage new_storage)
        ~diff:(Script_int.int added_amount)
    in
    return
      ( Script_list.of_list [op_balance_event; op_total_supply_event],
        new_storage,
        balance_updates,
        ctxt )

  let execute_redeem (ctxt, (step_constants : Script_typed_ir.step_constants))
      (amount : redeem) (storage : Clst_contract_storage.t) :
      entrypoint_execution_result tzresult Lwt.t =
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
    let* account =
      match step_constants.sender with
      | Contract (Implicit _ as implicit) -> return implicit
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
    let*? new_storage =
      Clst_contract_storage.decrement_total_supply new_storage removed_amount
    in
    let* ctxt, balance_updates =
      Clst_contract_storage.redeem_from_clst_deposits
        ctxt
        ~staker:account
        amount_tez
    in
    let*? entrypoint_str = Entrypoint.of_string_lax "redeem" in
    let* op_balance_event, ctxt =
      Clst_events.balance_update_event
        (ctxt, step_constants)
        ~entrypoint:entrypoint_str
        ~owner:address
        ~token_id:Clst_contract_storage.token_id
        ~new_balance:new_amount
        ~diff:(Script_int.neg removed_amount)
    in
    let* op_total_supply_event, ctxt =
      Clst_events.total_supply_update_event
        (ctxt, step_constants)
        ~entrypoint:entrypoint_str
        ~token_id:Clst_contract_storage.token_id
        ~new_total_supply:
          (Clst_contract_storage.get_total_supply_from_storage new_storage)
        ~diff:(Script_int.neg removed_amount)
    in
    return
      ( Script_list.of_list [op_balance_event; op_total_supply_event],
        new_storage,
        balance_updates,
        ctxt )

  let check_token_id token_id =
    error_unless
      Compare.Int.(
        Script_int.(compare token_id Clst_contract_storage.token_id) = 0)
      (standard_error ~mnemonic:"FA2_TOKEN_UNDEFINED")

  let execute_finalize (ctxt, (step_constants : step_constants)) ()
      (storage : Clst_contract_storage.t) :
      entrypoint_execution_result tzresult Lwt.t =
    let open Lwt_result_syntax in
    let* account, typed_account =
      match step_constants.sender with
      | Contract (Implicit pkh as implicit) ->
          return (implicit, Typed_implicit pkh)
      | sender -> tzfail (Non_implicit_contract sender)
    in
    let* ctxt, balance_updates, finalized_amount =
      Clst_contract_storage.finalize
        ctxt
        ~clst_contract_hash:step_constants.self
        ~staker:account
    in
    let gas_counter, outdated_ctxt =
      Local_gas_counter.local_gas_counter_and_outdated_context ctxt
    in
    let* op, outdated_ctxt, gas_counter =
      Script_interpreter_defs.transfer
        (outdated_ctxt, step_constants)
        gas_counter
        finalized_amount
        Micheline.dummy_location
        typed_account
        ()
    in
    let ctxt = Local_gas_counter.update_context gas_counter outdated_ctxt in
    return (Script_list.of_list [op], storage, balance_updates, ctxt)

  (** Implementation of the [transfer] entrypoint, compliant with the
      FA2.1 standard:
      https://tzip.tezosagora.org/proposal/tzip-26/#entrypoint-semantics

      This implementation is not optimized: the storage is updated
      with each individual transfer. It would be more efficient to
      keep the balances of affected accounts in memory and update the
      storage only at the end, but this is good enough and less
      error-prone. *)
  let execute_transfer (ctxt, (step_constants : Script_typed_ir.step_constants))
      (transfer : transfer) (storage : Clst_contract_storage.t) :
      entrypoint_execution_result tzresult Lwt.t =
    let open Lwt_result_syntax in
    let*? entrypoint_str = Entrypoint.of_string_lax "transfer" in
    let*? () =
      error_unless
        Tez.(step_constants.amount = zero)
        (Non_empty_transfer (step_constants.sender, step_constants.amount))
    in
    let execute_one from_ (ctxt, storage, ops) (to_, (token_id, amount)) =
      let*? () = check_token_id token_id in
      (* Checking that [from_] is the sender here instead of in
         [execute_from] means that we repeat the check for each new
         transfer from the same origin. On the other hand, it lets the
         whole execution succeed if [from_] is not the sender but its
         associated list of transfers is empty, which satisfies the
         atomical behavior prescribed by the standard. Moreover, once
         operators are implemented, the transferred [amount] will be
         relevant to determining permission. *)
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/8214
         Update permission policy once operators are implemented *)
      let from_is_sender =
        let {destination; entrypoint} = from_ in
        Destination.equal destination step_constants.sender
        && Entrypoint.is_default entrypoint
      in
      let*? () =
        error_unless
          from_is_sender
          (standard_error ~mnemonic:"FA2_NOT_OPERATOR")
      in
      (* Smart contracts cannot hold tokens at all.

         We do check that [from_] is implicit too, because otherwise a
         transfer of zero would succeed and make it appear in the
         storage, which might break invariants in some tools. *)
      let*? () =
        error_unless
          (is_implicit from_.destination)
          (Non_implicit_contract from_.destination)
      in
      let*? () =
        error_unless
          (is_implicit to_.destination)
          (Non_implicit_contract to_.destination)
      in
      let* balance_from, ctxt =
        Clst_contract_storage.get_balance_from_storage ctxt storage from_
      in
      let*? () =
        error_when
          Compare.Int.(Script_int.compare balance_from amount < 0)
          (standard_error ~mnemonic:"FA2_INSUFFICIENT_BALANCE")
      in
      (* Note: Transferring 0 from or to an account that didn't have a
         balance, will set its balance to 0. This is in accordance
         with the FA2 standard: transfers of zero must be treated as
         normal transfers.

         And if we ever want to add an invariant that accounts with no
         tokens are not present at all in the ledger, then it's
         probably {!Clst_storage.set_balance_from_storage}'s job to
         enforce it anyway. *)
      let* storage, ctxt =
        Clst_contract_storage.set_balance_from_storage
          ctxt
          storage
          from_
          Script_int.(abs (sub balance_from amount))
      in
      let* balance_to, ctxt =
        Clst_contract_storage.get_balance_from_storage ctxt storage to_
      in
      let* storage, ctxt =
        Clst_contract_storage.set_balance_from_storage
          ctxt
          storage
          to_
          Script_int.(add_n balance_to amount)
      in
      let* op_balance_event_source, ctxt =
        Clst_events.balance_update_event
          (ctxt, step_constants)
          ~entrypoint:entrypoint_str
          ~owner:from_
          ~token_id:Clst_contract_storage.token_id
          ~new_balance:balance_from
          ~diff:(Script_int.neg amount)
      in
      let* op_balance_event_dest, ctxt =
        Clst_events.balance_update_event
          (ctxt, step_constants)
          ~entrypoint:entrypoint_str
          ~owner:to_
          ~token_id:Clst_contract_storage.token_id
          ~new_balance:balance_to
          ~diff:(Script_int.int amount)
      in
      let* op_transfer_event, ctxt =
        Clst_events.transfer_event
          (ctxt, step_constants)
          ~entrypoint:entrypoint_str
          ~sender:from_
          ~receiver:to_
          ~token_id:Clst_contract_storage.token_id
          ~amount
      in
      return
        ( ctxt,
          storage,
          op_balance_event_source :: op_balance_event_dest :: op_transfer_event
          :: ops )
    in
    let execute_from (ctxt, storage, ops) (from_, txs) =
      List.fold_left_es
        (execute_one from_)
        (ctxt, storage, ops)
        (Script_list.to_list txs)
    in
    let* ctxt, storage, rev_ops =
      List.fold_left_es
        execute_from
        (ctxt, storage, [])
        (Script_list.to_list transfer)
    in
    return (Script_list.of_list (List.rev rev_ops), storage, [], ctxt)

  let execute_approval (step_constants : step_constants)
      (operations, storage, ctxt)
      ((owner, (spender, (token_id, delta))) : approval) =
    let open Lwt_result_syntax in
    let*? entrypoint_str = Entrypoint.of_string_lax "approve" in
    (* Restrict operator updates only to the owner of the token. *)
    let*? () =
      error_when
        (not (Destination.equal step_constants.sender owner.destination))
        (Only_owner_can_change_operator
           (step_constants.sender, owner.destination))
    in
    (* Check token_id is the one defined by the contract. *)
    let*? () = check_token_id token_id in
    let* allowance, ctxt =
      Clst_contract_storage.get_account_operator_allowance
        ctxt
        storage
        ~owner
        ~spender
    in
    let new_allowance, diff =
      match (allowance, delta) with
      (* "Operator has precedence over spender" (cf TZIP26), i.e. if an
       infinite allowance is set, then only the [update_operators]
       entrypoint will have an effect on it. *)
      | Some None, _ -> (None, Script_int.zero)
      (* Operator already has an allowance *)
      | Some (Some allowance), L increase ->
          (Some (Script_int.add_n allowance increase), Script_int.int increase)
      (* If the allowance would underflow, TZIP26 considers it as zero,
       and not an error. *)
      | Some (Some allowance), R decrease ->
          let new_allowance =
            Script_int.sub allowance decrease
            |> Script_int.is_nat
            |> Option.value ~default:Script_int.zero_n
          in
          (Some new_allowance, Script_int.sub new_allowance allowance)
      (* The operator has no allowance defined already. *)
      | None, L increase -> (Some increase, Script_int.int increase)
      | None, R _decrease -> (Some Script_int.zero_n, Script_int.zero)
    in
    let* storage, ctxt =
      Clst_contract_storage.set_account_operator_allowance
        ctxt
        storage
        ~owner
        ~spender
        (Some new_allowance)
    in
    match new_allowance with
    | Some new_allowance ->
        let* op_allowance_update_event, ctxt =
          Clst_events.allowance_update_event
            (ctxt, step_constants)
            ~entrypoint:entrypoint_str
            ~owner
            ~spender
            ~token_id:Clst_contract_storage.token_id
            ~new_allowance
            ~diff
        in
        return (op_allowance_update_event :: operations, storage, ctxt)
    | None ->
        (* the allowance_update event is not issued iff the allowance
           is infinite *)
        return (operations, storage, ctxt)

  let execute_approve (ctxt, (step_constants : step_constants))
      (value : approve) (storage : Clst_contract_storage.t) =
    let open Lwt_result_syntax in
    let* rev_ops, storage, ctxt =
      List.fold_left_es
        (execute_approval step_constants)
        ([], storage, ctxt)
        (Script_list.to_list value)
    in
    return (Script_list.of_list (List.rev rev_ops), storage, [], ctxt)

  let execute_update_operator (step_constants : step_constants)
      (operations, storage, ctxt) ((owner, (operator, token_id)) : operator)
      action =
    let open Lwt_result_syntax in
    let*? entrypoint_str = Entrypoint.of_string_lax "update_operators" in
    (* Restrict operator updates only to the owner of the token. *)
    let*? () =
      error_when
        (not (Destination.equal step_constants.sender owner.destination))
        (Only_owner_can_change_operator
           (step_constants.sender, owner.destination))
    in
    (* Check token_id is the one defined by the contract. *)
    let*? () = check_token_id token_id in
    let new_allowance, is_operator =
      match action with `Add -> (Some None, true) | `Remove -> (None, false)
    in
    let* storage, ctxt =
      Clst_contract_storage.set_account_operator_allowance
        ctxt
        storage
        ~owner
        ~spender:operator
        new_allowance
    in
    let* op_operator_update_event, ctxt =
      Clst_events.operator_update_event
        (ctxt, step_constants)
        ~entrypoint:entrypoint_str
        ~owner
        ~operator
        ~token_id:Clst_contract_storage.token_id
        ~is_operator
    in
    return (op_operator_update_event :: operations, storage, ctxt)

  let execute_update_operators (ctxt, (step_constants : step_constants))
      (value : update_operators) (storage : Clst_contract_storage.t) =
    let open Lwt_result_syntax in
    let* rev_ops, storage, ctxt =
      List.fold_left_es
        (fun acc add_or_remove ->
          let action, op =
            match add_or_remove with L op -> (`Add, op) | R op -> (`Remove, op)
          in
          execute_update_operator step_constants acc op action)
        ([], storage, ctxt)
        (Script_list.to_list value)
    in
    return (Script_list.of_list (List.rev rev_ops), storage, [], ctxt)

  let execute_with_wrapped_storage (ctxt, (step_constants : step_constants))
      (value : arg) (storage : Clst_contract_storage.t) =
    match entrypoint_from_arg value with
    | Deposit () -> execute_deposit (ctxt, step_constants) () storage
    | Redeem amount -> execute_redeem (ctxt, step_constants) amount storage
    | Finalize () -> execute_finalize (ctxt, step_constants) () storage
    | Transfer transfer ->
        execute_transfer (ctxt, step_constants) transfer storage
    | Approve approvals ->
        execute_approve (ctxt, step_constants) approvals storage
    | Update_operators operators ->
        execute_update_operators (ctxt, step_constants) operators storage
    | Balance_of _ -> assert false

  let execute (ctxt, step_constants) value storage =
    let open Lwt_result_syntax in
    let storage = Clst_contract_storage.from_clst_storage storage in
    let* operations, storage, balance_updates, context =
      execute_with_wrapped_storage (ctxt, step_constants) value storage
    in
    return
      ( ( operations,
          Clst_contract_storage.to_clst_storage storage,
          balance_updates ),
        context )

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
        then
          Clst_contract_storage.get_balance_from_storage
            ctxt
            (Clst_contract_storage.from_clst_storage storage)
            address
        else return (Script_int.zero_n, ctxt)
      in
      return (Ex_view {name; ty; implementation})

    let total_supply : storage ex_view tzresult =
      let open Result_syntax in
      let* name = Script_string.of_string "get_total_supply" in
      let implementation (ctxt, _step_constants) (() : unit)
          ((_ledger, (total_supply, _operators_table)) : storage) =
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

    let get_allowance : storage ex_view tzresult =
      let open Result_syntax in
      let* name = Script_string.of_string "get_allowance" in
      let implementation (ctxt, _step_constants) (owner, (spender, token_id))
          (storage : storage) =
        let open Lwt_result_syntax in
        if
          Compare.Int.(
            Script_int.compare token_id Clst_contract_storage.token_id = 0)
        then
          let* allowance, ctxt =
            Clst_contract_storage.get_account_operator_allowance
              ctxt
              (Clst_contract_storage.from_clst_storage storage)
              ~owner
              ~spender
          in
          let allowance =
            match allowance with
            | None -> Script_int.zero_n
            | Some None -> Script_int.zero_n
            | Some (Some n) -> n
          in
          return (allowance, ctxt)
        else return (Script_int.zero_n, ctxt)
      in
      let* ty = CLST_types.get_allowance_view_ty in
      return (Ex_view {name; ty; implementation})

    let is_operator : storage ex_view tzresult =
      let open Result_syntax in
      let* name = Script_string.of_string "is_operator" in
      let implementation (ctxt, _step_constants) (owner, (operator, token_id))
          (storage : storage) =
        let open Lwt_result_syntax in
        if
          Compare.Int.(
            Script_int.compare token_id Clst_contract_storage.token_id = 0)
        then
          let* allowance, ctxt =
            Clst_contract_storage.get_account_operator_allowance
              ctxt
              (Clst_contract_storage.from_clst_storage storage)
              ~owner
              ~spender:operator
          in
          (* According to TZIP26, an operator has an infinite
             allowance. *)
          let is_operator =
            match allowance with
            | Some None -> true
            | None | Some (Some _) -> false
          in
          return (is_operator, ctxt)
        else return (false, ctxt)
      in
      let* ty = CLST_types.is_operator_view_ty in
      return (Ex_view {name; ty; implementation})

    let view_map : storage Script_native_types.view_map tzresult =
      let open Result_syntax in
      let* (Ex_view {name = get_balance_name; _} as get_balance) = balance in
      let* (Ex_view {name = get_total_supply_name; _} as get_total_supply) =
        total_supply
      in
      let* (Ex_view {name = is_token_name; _} as is_token) = is_token in
      let* (Ex_view {name = get_allowance_name; _} as get_allowance) =
        get_allowance
      in
      let* (Ex_view {name = is_operator_name; _} as is_operator) =
        is_operator
      in
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
      let view_map =
        Script_map.update get_allowance_name (Some get_allowance) view_map
      in
      let view_map =
        Script_map.update is_operator_name (Some is_operator) view_map
      in
      return view_map
  end
end

let get_views : type arg storage.
    (arg, storage) kind -> storage Script_native_types.view_map tzresult =
  function
  | CLST_kind -> CLST_contract.Views.view_map

let execute (type arg storage) (ctxt, step_constants)
    (kind : (arg, storage) kind) (arg : arg) (storage : storage) :
    ( (operation Script_list.t * storage * Receipt.balance_updates) * context,
      error trace )
    result
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
    ~description:
      "Only implicit contracts can deposit on CLST, or be the origin or \
       destination of token transfers."
    ~pp:(fun ppf address ->
      Format.fprintf
        ppf
        "Only implicit contracts can deposit on CLST, or be the origin or \
         destination of token transfers; %a is not implicit."
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
    (fun (address, amount) -> CLST_contract.Amount_too_large (address, amount)) ;
  register_error_kind
    `Branch
    ~id:"clst.only_owner_can_change_operator"
    ~title:"Only the owner can update operators"
    ~description:"Only the owner can update operators"
    ~pp:(fun ppf (sender, owner) ->
      Format.fprintf
        ppf
        "%a cannot update operators on behalf of %a, only %a can do it."
        Destination.pp
        sender
        Destination.pp
        owner
        Destination.pp
        owner)
    Data_encoding.(
      obj2
        (req "sender" Destination.encoding)
        (req "owner" Destination.encoding))
    (function
      | CLST_contract.Only_owner_can_change_operator (sender, owner) ->
          Some (sender, owner)
      | _ -> None)
    (fun (sender, owner) ->
      CLST_contract.Only_owner_can_change_operator (sender, owner))
