(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Tezos Protocol Implementation - Main Entry Points *)

open Alpha_context

type error +=
  | Faulty_validation_wrong_slot
  | Set_deposits_limit_on_unregistered_delegate of Signature.Public_key_hash.t
  | Set_deposits_limit_when_automated_staking_off
  | Error_while_taking_fees
  | Update_consensus_key_on_unregistered_delegate of
      (Signature.Public_key_hash.t * Operation_repr.consensus_key_kind)
  | Empty_transaction of Contract.t
  | Non_empty_transaction_from of Destination.t
  | Internal_operation_replay of
      Apply_internal_results.packed_internal_operation
  | Multiple_revelation
  | Invalid_transfer_to_sc_rollup
  | Invalid_sender of Destination.t
  | Invalid_self_transaction_destination
  | Staking_to_delegate_that_refuses_external_staking
  | Stake_modification_with_no_delegate_set
  | Invalid_nonzero_transaction_amount of Tez.t
  | Invalid_staking_parameters_sender

let () =
  let description =
    "The consensus operation uses an invalid slot. This error should not \
     happen: the operation validation should have failed earlier."
  in
  register_error_kind
    `Permanent
    ~id:"operation.faulty_validation_wrong_slot"
    ~title:"Faulty validation (wrong slot for consensus operation)"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Faulty_validation_wrong_slot -> Some () | _ -> None)
    (fun () -> Faulty_validation_wrong_slot) ;
  register_error_kind
    `Temporary
    ~id:"operation.set_deposits_limit_on_unregistered_delegate"
    ~title:"Set deposits limit on an unregistered delegate"
    ~description:"Cannot set deposits limit on an unregistered delegate."
    ~pp:(fun ppf c ->
      Format.fprintf
        ppf
        "Cannot set a deposits limit on the unregistered delegate %a."
        Signature.Public_key_hash.pp
        c)
    Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
    (function
      | Set_deposits_limit_on_unregistered_delegate c -> Some c | _ -> None)
    (fun c -> Set_deposits_limit_on_unregistered_delegate c) ;

  register_error_kind
    `Temporary
    ~id:"operation.set_deposits_limit_when_automated_staking_off"
    ~title:"Set deposits limit when automated staking off"
    ~description:
      "Cannot set deposits limit when automated staking is off or Adaptive \
       Issuance is active."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Cannot set a deposits limit when automated staking off.")
    Data_encoding.unit
    (function
      | Set_deposits_limit_when_automated_staking_off -> Some () | _ -> None)
    (fun () -> Set_deposits_limit_when_automated_staking_off) ;

  let error_while_taking_fees_description =
    "There was an error while taking the fees, which should not happen and \
     means that the operation's validation was faulty."
  in
  register_error_kind
    `Permanent
    ~id:"operation.error_while_taking_fees"
    ~title:"Error while taking the fees of a manager operation"
    ~description:error_while_taking_fees_description
    ~pp:(fun ppf () ->
      Format.fprintf ppf "%s" error_while_taking_fees_description)
    Data_encoding.unit
    (function Error_while_taking_fees -> Some () | _ -> None)
    (fun () -> Error_while_taking_fees) ;

  register_error_kind
    `Temporary
    ~id:"operation.update_consensus_key_on_unregistered_delegate"
    ~title:"Update consensus key on an unregistered delegate"
    ~description:"Cannot update consensus key for an unregistered delegate."
    ~pp:(fun ppf (delegate, kind) ->
      Format.fprintf
        ppf
        "Cannot update the %a key on the unregistered delegate %a."
        Operation_repr.pp_consensus_key_kind
        kind
        Signature.Public_key_hash.pp
        delegate)
    Data_encoding.(
      obj2
        (req "delegate" Signature.Public_key_hash.encoding)
        (req "kind" Operation_repr.consensus_key_kind_encoding))
    (function
      | Update_consensus_key_on_unregistered_delegate c -> Some c | _ -> None)
    (fun c -> Update_consensus_key_on_unregistered_delegate c) ;
  register_error_kind
    `Branch
    ~id:"contract.empty_transaction"
    ~title:"Empty transaction"
    ~description:"Forbidden to credit 0ꜩ to a contract without code."
    ~pp:(fun ppf contract ->
      Format.fprintf
        ppf
        "Transactions of 0ꜩ towards a contract without code are forbidden (%a)."
        Contract.pp
        contract)
    Data_encoding.(obj1 (req "contract" Contract.encoding))
    (function Empty_transaction c -> Some c | _ -> None)
    (fun c -> Empty_transaction c) ;
  register_error_kind
    `Branch
    ~id:"contract.non_empty_transaction_from_source"
    ~title:"Unexpected non-empty transaction"
    ~description:"This address cannot initiate non-empty transactions"
    ~pp:(fun ppf contract ->
      Format.fprintf
        ppf
        "%a does not have a balance and cannot initiate non-empty transactions"
        Destination.pp
        contract)
    Data_encoding.(obj1 (req "source" Destination.encoding))
    (function Non_empty_transaction_from c -> Some c | _ -> None)
    (fun c -> Non_empty_transaction_from c) ;
  register_error_kind
    `Permanent
    ~id:"internal_operation_replay"
    ~title:"Internal operation replay"
    ~description:"An internal operation was emitted twice by a script"
    ~pp:(fun ppf (Apply_internal_results.Internal_operation {nonce; _}) ->
      Format.fprintf
        ppf
        "Internal operation %d was emitted twice by a script"
        nonce)
    Apply_internal_results.internal_operation_encoding
    (function Internal_operation_replay op -> Some op | _ -> None)
    (fun op -> Internal_operation_replay op) ;
  register_error_kind
    `Permanent
    ~id:"block.multiple_revelation"
    ~title:"Multiple revelations were included in a manager operation"
    ~description:
      "A manager operation should not contain more than one revelation"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Multiple revelations were included in a manager operation")
    Data_encoding.empty
    (function Multiple_revelation -> Some () | _ -> None)
    (fun () -> Multiple_revelation) ;
  register_error_kind
    `Permanent
    ~id:"operations.invalid_transfer_to_smart_rollup_from_implicit_account"
    ~title:"Invalid transfer to smart rollup"
    ~description:"Invalid transfer to smart rollup from implicit account"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Invalid sender for transfer operation to smart rollup. Only \
         originated accounts are allowed.")
    Data_encoding.empty
    (function Invalid_transfer_to_sc_rollup -> Some () | _ -> None)
    (fun () -> Invalid_transfer_to_sc_rollup) ;
  register_error_kind
    `Permanent
    ~id:"operations.invalid_sender"
    ~title:"Invalid sender for an internal operation"
    ~description:
      "Invalid sender for an internal operation restricted to implicit and \
       originated accounts."
    ~pp:(fun ppf c ->
      Format.fprintf
        ppf
        "Invalid sender (%a) for this internal operation. Only implicit and \
         originated accounts are allowed"
        Destination.pp
        c)
    Data_encoding.(obj1 (req "contract" Destination.encoding))
    (function Invalid_sender c -> Some c | _ -> None)
    (fun c -> Invalid_sender c) ;
  let invalid_self_transaction_destination_description =
    "A pseudo-transaction destination must equal its sender."
  in
  register_error_kind
    `Permanent
    ~id:"operations.invalid_self_transaction_destination"
    ~title:"Invalid destination for a pseudo-transaction"
    ~description:invalid_self_transaction_destination_description
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        invalid_self_transaction_destination_description)
    Data_encoding.unit
    (function Invalid_self_transaction_destination -> Some () | _ -> None)
    (fun () -> Invalid_self_transaction_destination) ;
  let stake_modification_without_delegate_description =
    "(Un)Stake operations are only allowed when delegate is set."
  in
  register_error_kind
    `Permanent
    ~id:"operations.stake_modification_with_no_delegate_set"
    ~title:"(Un)staking without any delegate set"
    ~description:stake_modification_without_delegate_description
    ~pp:(fun ppf () ->
      Format.pp_print_string ppf stake_modification_without_delegate_description)
    Data_encoding.unit
    (function Stake_modification_with_no_delegate_set -> Some () | _ -> None)
    (fun () -> Stake_modification_with_no_delegate_set) ;
  let staking_to_delegate_that_refuses_external_staking_description =
    "The delegate currently does not accept staking operations from sources \
     other than itself: its `limit_of_staking_over_baking` parameter is set to \
     0."
  in
  register_error_kind
    `Permanent
    ~id:"operations.staking_to_delegate_that_refuses_external_staking"
    ~title:"Staking to delegate that does not accept external staking"
    ~description:staking_to_delegate_that_refuses_external_staking_description
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        staking_to_delegate_that_refuses_external_staking_description)
    Data_encoding.unit
    (function
      | Staking_to_delegate_that_refuses_external_staking -> Some () | _ -> None)
    (fun () -> Staking_to_delegate_that_refuses_external_staking) ;
  register_error_kind
    `Permanent
    ~id:"operations.invalid_nonzero_transaction_amount"
    ~title:"Invalid non-zero transaction amount"
    ~description:"A transaction expected a zero-amount but got non-zero."
    ~pp:(fun ppf amount ->
      Format.fprintf
        ppf
        "A transaction expected a zero-amount but got %a."
        Tez.pp
        amount)
    Data_encoding.(obj1 (req "amount" Tez.encoding))
    (function
      | Invalid_nonzero_transaction_amount amount -> Some amount | _ -> None)
    (fun amount -> Invalid_nonzero_transaction_amount amount) ;
  register_error_kind
    `Permanent
    ~id:"operations.invalid_staking_parameters_sender"
    ~title:"Invalid staking parameters sender"
    ~description:"The staking parameters can only be set by delegates."
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid staking parameters sender")
    Data_encoding.empty
    (function Invalid_staking_parameters_sender -> Some () | _ -> None)
    (fun () -> Invalid_staking_parameters_sender)

open Apply_results
open Apply_operation_result
open Apply_internal_results

let update_script_storage_and_ticket_balances ctxt ~self_contract storage
    lazy_storage_diff ticket_diffs operations =
  let open Lwt_result_syntax in
  let* ctxt =
    Contract.update_script_storage ctxt self_contract storage lazy_storage_diff
  in
  let self_contract = Contract.Originated self_contract in
  Ticket_accounting.update_ticket_balances
    ctxt
    ~self_contract
    ~ticket_diffs
    operations

let apply_delegation ~ctxt ~(sender : Contract.t) ~delegate ~before_operation =
  let open Lwt_result_syntax in
  let* ctxt, balance_updates =
    match sender with
    | Originated _ ->
        (* Originated contracts have no stake (yet). *)
        return (ctxt, [])
    | Implicit sender_pkh -> (
        let* sender_delegate_status =
          Contract.get_delegate_status ctxt sender_pkh
        in
        match sender_delegate_status with
        | Undelegated | Delegate ->
            (* No delegate before or re-activation: no unstake request added. *)
            return (ctxt, [])
        | Delegated delegate ->
            (* [request_unstake] bounds to the actual stake. *)
            Staking.request_unstake
              ctxt
              ~sender_contract:sender
              ~delegate
              Tez.max_mutez)
  in
  let+ ctxt = Contract.Delegate.set ctxt sender delegate in
  (ctxt, Gas.consumed ~since:before_operation ~until:ctxt, balance_updates, [])

type 'loc execution_arg =
  | Typed_arg : 'loc * ('a, _) Script_typed_ir.ty * 'a -> 'loc execution_arg
  | Untyped_arg : Script.expr -> _ execution_arg

let apply_transaction_to_implicit ~ctxt ~sender ~amount ~pkh ~before_operation =
  let contract = Contract.Implicit pkh in
  let open Lwt_result_syntax in
  (* Transfers of zero to implicit accounts are forbidden. *)
  let*? () = error_when Tez.(amount = zero) (Empty_transaction contract) in
  (* If the implicit contract is not yet allocated at this point then
     the next transfer of tokens will allocate it. *)
  let*? ctxt = Gas.consume ctxt Michelson_v1_gas.Cost_of.transfer_operation in
  let*! already_allocated = Contract.allocated ctxt contract in
  let* ctxt, balance_updates =
    Token.transfer ctxt (`Contract sender) (`Contract contract) amount
  in
  let result =
    Transaction_to_contract_result
      {
        storage = None;
        lazy_storage_diff = None;
        balance_updates;
        ticket_receipt = [];
        originated_contracts = [];
        consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
        storage_size = Z.zero;
        paid_storage_size_diff = Z.zero;
        allocated_destination_contract = not already_allocated;
        address_registry_diff = [];
      }
  in
  return (ctxt, result, [])

let apply_stake ~ctxt ~sender ~amount ~destination ~before_operation =
  let open Lwt_result_syntax in
  let contract = Contract.Implicit destination in
  (* Staking of zero is forbidden. *)
  let*? () = error_when Tez.(amount = zero) (Empty_transaction contract) in
  let*? () =
    error_unless
      Signature.Public_key_hash.(sender = destination)
      Invalid_self_transaction_destination
  in
  let*? ctxt = Gas.consume ctxt Adaptive_issuance_costs.stake_cost in
  let* delegate_opt = Contract.Delegate.find ctxt contract in
  match delegate_opt with
  | None -> tzfail Stake_modification_with_no_delegate_set
  | Some delegate ->
      let* {limit_of_staking_over_baking_millionth; _} =
        Delegate.Staking_parameters.of_delegate ctxt delegate
      in
      let forbidden =
        Signature.Public_key_hash.(delegate <> sender)
        && Compare.Int32.(limit_of_staking_over_baking_millionth = 0l)
      in
      let*? () =
        error_when forbidden Staking_to_delegate_that_refuses_external_staking
      in
      let* ctxt, balance_updates =
        Staking.stake ctxt ~amount ~sender ~delegate
      in
      (* Since [delegate] is an already existing delegate, it is already allocated. *)
      let allocated_destination_contract = false in
      let result =
        Transaction_to_contract_result
          {
            storage = None;
            lazy_storage_diff = None;
            balance_updates;
            ticket_receipt = [];
            originated_contracts = [];
            consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
            storage_size = Z.zero;
            paid_storage_size_diff = Z.zero;
            allocated_destination_contract;
            address_registry_diff = [];
          }
      in
      return (ctxt, result, [])

let apply_unstake ~ctxt ~sender ~amount ~destination ~before_operation =
  let open Lwt_result_syntax in
  let*? () =
    error_unless
      Signature.Public_key_hash.(sender = destination)
      Invalid_self_transaction_destination
  in
  let sender_contract = Contract.Implicit sender in
  let*? ctxt = Gas.consume ctxt Adaptive_issuance_costs.find_delegate_cost in
  let* delegate_opt = Contract.Delegate.find ctxt sender_contract in
  match delegate_opt with
  | None -> tzfail Stake_modification_with_no_delegate_set
  | Some delegate ->
      let* ctxt, balance_updates =
        Staking.request_unstake ctxt ~sender_contract ~delegate amount
      in
      let result =
        Transaction_to_contract_result
          {
            storage = None;
            lazy_storage_diff = None;
            balance_updates;
            ticket_receipt = [];
            originated_contracts = [];
            consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
            storage_size = Z.zero;
            paid_storage_size_diff = Z.zero;
            allocated_destination_contract = false;
            address_registry_diff = [];
          }
      in
      return (ctxt, result, [])

let apply_finalize_unstake ~ctxt ~amount ~destination ~before_operation =
  let open Lwt_result_syntax in
  let*? () =
    error_when Tez.(amount <> zero) (Invalid_nonzero_transaction_amount amount)
  in
  let contract = Contract.Implicit destination in
  let*? ctxt = Gas.consume ctxt Adaptive_issuance_costs.find_delegate_cost in
  let*! already_allocated = Contract.allocated ctxt contract in
  let* ctxt, balance_updates = Staking.finalize_unstake ctxt contract in
  let result =
    Transaction_to_contract_result
      {
        storage = None;
        lazy_storage_diff = None;
        balance_updates;
        ticket_receipt = [];
        originated_contracts = [];
        consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
        storage_size = Z.zero;
        paid_storage_size_diff = Z.zero;
        allocated_destination_contract = not already_allocated;
        address_registry_diff = [];
      }
  in
  return (ctxt, result, [])

let apply_set_delegate_parameters ~ctxt ~sender ~destination
    ~limit_of_staking_over_baking_millionth
    ~edge_of_baking_over_staking_billionth ~before_operation =
  let open Lwt_result_syntax in
  let*? ctxt =
    Gas.consume ctxt Adaptive_issuance_costs.set_delegate_parameters_cost
  in
  let*? () =
    error_unless
      Signature.Public_key_hash.(sender = destination)
      Invalid_self_transaction_destination
  in
  let* is_delegate = Contract.is_delegate ctxt sender in
  let*? () = error_unless is_delegate Invalid_staking_parameters_sender in
  let*? t =
    Staking_parameters_repr.make
      ~limit_of_staking_over_baking_millionth
      ~edge_of_baking_over_staking_billionth
  in
  let* ctxt = Delegate.Staking_parameters.register_update ctxt sender t in
  let result =
    Transaction_to_contract_result
      {
        storage = None;
        lazy_storage_diff = None;
        balance_updates = [];
        ticket_receipt = [];
        originated_contracts = [];
        consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
        storage_size = Z.zero;
        paid_storage_size_diff = Z.zero;
        allocated_destination_contract = false;
        address_registry_diff = [];
      }
  in
  return (ctxt, result, [])

let transfer_from_any_address ctxt sender destination amount =
  let open Lwt_result_syntax in
  match sender with
  | Destination.Contract sender ->
      Token.transfer ctxt (`Contract sender) (`Contract destination) amount
  | Destination.Sc_rollup _ | Destination.Zk_rollup _ ->
      (* We do not allow transferring tez from rollups to other contracts. *)
      let*? () =
        error_unless Tez.(amount = zero) (Non_empty_transaction_from sender)
      in
      return (ctxt, [])

let apply_transaction_to_implicit_with_ticket ~sender ~destination ~ty ~ticket
    ~amount ~before_operation ctxt =
  let open Lwt_result_syntax in
  let*? ctxt = Gas.consume ctxt Michelson_v1_gas.Cost_of.transfer_operation in
  let destination = Contract.Implicit destination in
  let*! already_allocated = Contract.allocated ctxt destination in
  let ex_token, ticket_amount =
    Ticket_scanner.ex_token_and_amount_of_ex_ticket
    @@ Ticket_scanner.Ex_ticket (ty, ticket)
  in
  let* ticket_token, ctxt = Ticket_token_unparser.unparse ctxt ex_token in
  let* ctxt, balance_updates =
    transfer_from_any_address ctxt sender destination amount
  in
  let ticket_receipt =
    Ticket_receipt.
      [
        {
          ticket_token;
          updates =
            [
              {
                account = Destination.Contract destination;
                amount = Script_int.(to_zint (ticket_amount :> n num));
              };
            ];
        };
      ]
  in
  return
    ( ctxt,
      Transaction_to_contract_result
        {
          storage = None;
          lazy_storage_diff = None;
          balance_updates;
          ticket_receipt;
          originated_contracts = [];
          consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
          storage_size = Z.zero;
          paid_storage_size_diff = Z.zero;
          allocated_destination_contract = not already_allocated;
          address_registry_diff = [];
        },
      [] )

let apply_transaction_to_smart_contract ~ctxt ~sender ~contract_hash ~amount
    ~entrypoint ~before_operation ~payer ~chain_id ~internal ~parameter
    ~(script : Script.t) ~script_ir ~cache_key ?(paid_storage_diff_acc = Z.zero)
    ?(ticket_receipt_acc = []) () =
  let open Lwt_result_syntax in
  let contract = Contract.Originated contract_hash in
  (* We can assume the destination contract is already allocated at this point.
     If the destination contract does not exist, [Script_cache.find],
     which is called earlier, would have failed. *)
  let* ctxt, balance_updates =
    transfer_from_any_address ctxt sender contract amount
  in
  (* [Token.transfer], which is being called before, already loads this value into
     the Irmin cache, so no need to burn gas for it. *)
  let* balance = Contract.get_balance ctxt contract in
  let now = Script_timestamp.now ctxt in
  let level =
    (Level.current ctxt).level |> Raw_level.to_int32 |> Script_int.of_int32
    |> Script_int.abs
  in
  let step_constants =
    let open Script_interpreter in
    {sender; payer; self = contract_hash; amount; chain_id; balance; now; level}
  in
  let execute =
    match parameter with
    | Untyped_arg parameter -> Script_interpreter.execute ~parameter
    | Typed_arg (location, parameter_ty, parameter) ->
        Script_interpreter.execute_with_typed_parameter
          ~location
          ~parameter_ty
          ~parameter
  in
  let cached_script = Some script_ir in
  let* ( {
           script = updated_cached_script;
           code_size = updated_size;
           storage;
           lazy_storage_diff;
           operations;
           ticket_diffs;
           ticket_receipt;
           address_registry_diff;
         },
         ctxt ) =
    execute
      ctxt
      ~cached_script
      Optimized
      step_constants
      ~script
      ~entrypoint
      ~internal
  in
  let* ticket_table_size_diff, ctxt =
    update_script_storage_and_ticket_balances
      ctxt
      ~self_contract:contract_hash
      storage
      lazy_storage_diff
      ticket_diffs
      operations
  in
  let* ticket_paid_storage_diff, ctxt =
    Ticket_balance.adjust_storage_space
      ctxt
      ~storage_diff:ticket_table_size_diff
  in
  let* ctxt, new_size, contract_paid_storage_size_diff =
    Fees.record_paid_storage_space ctxt contract_hash
  in
  let* originated_contracts =
    Contract.originated_from_current_nonce ~since:before_operation ~until:ctxt
  in
  let updated_script =
    match script with
    | Script.Script script ->
        Script.Script {script with storage = Script.lazy_expr storage}
    | Script.Native native ->
        Native {native with storage = Script.lazy_expr storage}
  in
  let*? ctxt =
    Script_cache.update
      ctxt
      cache_key
      (updated_script, updated_cached_script)
      updated_size
  in
  let result =
    Transaction_to_contract_result
      {
        storage = Some storage;
        lazy_storage_diff;
        balance_updates;
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6639
           Currently, if both [ticket_receipt_acc] and [ticket_receipt] contain updates
           for the same ticket token, the token will appear in a non-optimal, but not wrong,
           way in the ticket receipt. See description of #6639 for an example. *)
        ticket_receipt = ticket_receipt_acc @ ticket_receipt;
        originated_contracts;
        consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
        storage_size = new_size;
        paid_storage_size_diff =
          Z.(
            add
              paid_storage_diff_acc
              (add contract_paid_storage_size_diff ticket_paid_storage_diff));
        allocated_destination_contract = false;
        address_registry_diff;
      }
  in
  return (ctxt, result, operations)

let apply_origination ~ctxt ~storage_type ~storage ~unparsed_code
    ~contract:contract_hash ~delegate ~sender ~credit ~before_operation =
  let open Lwt_result_syntax in
  let*? to_duplicate, ctxt =
    Script_ir_translator.collect_lazy_storage ctxt storage_type storage
  in
  let to_update = Script_ir_translator.no_lazy_storage_id in
  let* storage, lazy_storage_diff, ctxt =
    Script_ir_translator.extract_lazy_storage_diff
      ctxt
      Optimized
      storage_type
      storage
      ~to_duplicate
      ~to_update
      ~temporary:false
  in
  let* storage, ctxt =
    Script_ir_translator.unparse_data ctxt Optimized storage_type storage
  in
  let storage = Script.lazy_expr storage in
  (* Normalize code to avoid #843 *)
  let* code, ctxt =
    Script_ir_translator.unparse_code
      ctxt
      Optimized
      (Micheline.root unparsed_code)
  in
  let code = Script.lazy_expr code in
  let script = {Script.code; storage} in
  let* ctxt =
    Contract.raw_originate
      ctxt
      ~prepaid_bootstrap_storage:false
      contract_hash
      ~script:(script, lazy_storage_diff)
  in
  let contract = Contract.Originated contract_hash in
  let* ctxt =
    match delegate with
    | None -> return ctxt
    | Some delegate -> Contract.Delegate.init ctxt contract delegate
  in
  let* ctxt, balance_updates =
    Token.transfer ctxt (`Contract sender) (`Contract contract) credit
  in
  let+ ctxt, size, paid_storage_size_diff =
    Fees.record_paid_storage_space ctxt contract_hash
  in
  let result =
    {
      lazy_storage_diff;
      balance_updates;
      originated_contracts = [contract_hash];
      consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
      storage_size = size;
      paid_storage_size_diff;
    }
  in
  (ctxt, result, [])

(**

   Retrieving the script of a contract from its address is costly
   because it requires I/Os. For this reason, we put the corresponding
   Micheline expression in the cache.

   Elaborating a Micheline node into the well-typed script abstract
   syntax tree is also a costly operation. The result of this operation
   is cached as well.

*)

let assert_sender_is_contract =
  let open Result_syntax in
  function
  | Destination.Contract sender -> return sender
  | sender -> tzfail (Invalid_sender sender)

let find_contract_from_cache ctxt contract_hash =
  let open Lwt_result_syntax in
  let* ctxt, cache_key, script = Script_cache.find ctxt contract_hash in
  match script with
  | None -> tzfail (Contract.Non_existing_contract (Originated contract_hash))
  | Some (script, script_ir) -> return (ctxt, (cache_key, script, script_ir))

let apply_internal_operation_contents : type kind.
    context ->
    payer:public_key_hash ->
    sender:Destination.t ->
    chain_id:Chain_id.t ->
    kind Script_typed_ir.internal_operation_contents ->
    (context
    * kind successful_internal_operation_result
    * Script_typed_ir.packed_internal_operation list)
    tzresult
    Lwt.t =
  let open Lwt_result_syntax in
  fun ctxt_before_op ~payer ~sender ~chain_id operation ->
    let* ctxt = Destination.must_exist ctxt_before_op sender in
    (* There is no signature being checked for internal operations so in
       this case the fixed cost is exactly
       [Michelson_v1_gas.Cost_of.manager_operation]. *)
    let*? ctxt = Gas.consume ctxt Michelson_v1_gas.Cost_of.manager_operation in
    (* Note that [ctxt_before_op] will be used again later to compute
       gas consumption and originations for the operation result (by
       comparing it with the [ctxt] we will have at the end of the
       application). *)
    match operation with
    | Transaction_to_implicit {destination = pkh; amount} ->
        let*? sender = assert_sender_is_contract sender in
        let+ ctxt, res, ops =
          apply_transaction_to_implicit
            ~ctxt
            ~sender
            ~amount
            ~pkh
            ~before_operation:ctxt_before_op
        in
        ( ctxt,
          (ITransaction_result res : kind successful_internal_operation_result),
          ops )
    | Transaction_to_implicit_with_ticket
        {
          destination;
          ticket_ty = Script_typed_ir.Ticket_t (ty, _ty_metadata);
          ticket;
          amount;
          unparsed_ticket = _;
        } ->
        let+ ctxt, res, ops =
          apply_transaction_to_implicit_with_ticket
            ~sender
            ~destination
            ~ty
            ~ticket
            ~amount
            ~before_operation:ctxt_before_op
            ctxt
        in
        ( ctxt,
          (ITransaction_result res : kind successful_internal_operation_result),
          ops )
    | Transaction_to_smart_contract
        {
          amount;
          destination = contract_hash;
          entrypoint;
          location;
          parameters_ty;
          parameters = typed_parameters;
          unparsed_parameters = _;
        } ->
        let* ctxt, (cache_key, script, script_ir) =
          find_contract_from_cache ctxt contract_hash
        in
        let+ ctxt, res, ops =
          apply_transaction_to_smart_contract
            ~ctxt
            ~sender
            ~contract_hash
            ~amount
            ~entrypoint
            ~before_operation:ctxt_before_op
            ~payer
            ~chain_id
            ~internal:true
            ~parameter:(Typed_arg (location, parameters_ty, typed_parameters))
            ~script
            ~script_ir
            ~cache_key
            ()
        in
        (ctxt, ITransaction_result res, ops)
    | Transaction_to_sc_rollup
        {
          destination;
          entrypoint = _;
          parameters_ty;
          parameters;
          unparsed_parameters = payload;
        } ->
        let*? sender =
          match sender with
          | Destination.Contract (Originated hash) -> Ok hash
          | _ -> Result_syntax.tzfail Invalid_transfer_to_sc_rollup
        in
        (* Adding the message to the inbox. Note that it is safe to ignore the
           size diff since only its hash and meta data are stored in the context.
           See #3232. *)
        let* ctxt =
          Sc_rollup.Inbox.add_deposit
            ctxt
            ~destination
            ~payload
            ~sender
            ~source:payer
        in
        let*? has_tickets, ctxt =
          Ticket_scanner.type_has_tickets ctxt parameters_ty
        in
        let* ticket_token_map, ctxt =
          Ticket_accounting.ticket_balances_of_value
            ctxt
            ~include_lazy:true
            has_tickets
            parameters
        in
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/4354
           Factor out function for constructing a ticket receipt.
           There are multiple places where we compute the receipt from a
           ticket-token-map. We should factor out and reuse this logic. *)
        let+ ticket_receipt, ctxt =
          Ticket_token_map.fold_es
            ctxt
            (fun ctxt acc ex_token amount ->
              let* ticket_token, ctxt =
                Ticket_token_unparser.unparse ctxt ex_token
              in
              let item =
                Ticket_receipt.
                  {
                    ticket_token;
                    updates =
                      [{account = Destination.Sc_rollup destination; amount}];
                  }
              in
              return (item :: acc, ctxt))
            []
            ticket_token_map
        in
        let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
        let result =
          Transaction_to_sc_rollup_result {consumed_gas; ticket_receipt}
        in
        (ctxt, ITransaction_result result, [])
    | Event {ty = _; unparsed_data = _; tag = _} ->
        return
          ( ctxt,
            IEvent_result
              {consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt},
            [] )
    | Transaction_to_zk_rollup
        {destination; unparsed_parameters = _; parameters_ty; parameters} ->
        Zk_rollup_apply.transaction_to_zk_rollup
          ~ctxt
          ~parameters_ty
          ~parameters
          ~dst_rollup:destination
          ~since:ctxt_before_op
    | Origination
        {
          delegate;
          code = unparsed_code;
          unparsed_storage = _;
          credit;
          preorigination;
          storage_type;
          storage;
        } ->
        let*? sender = assert_sender_is_contract sender in
        let+ ctxt, origination_result, ops =
          apply_origination
            ~ctxt
            ~storage_type
            ~storage
            ~unparsed_code
            ~contract:preorigination
            ~delegate
            ~sender
            ~credit
            ~before_operation:ctxt_before_op
        in
        (ctxt, IOrigination_result origination_result, ops)
    | Delegation delegate ->
        let*? sender = assert_sender_is_contract sender in
        let+ ctxt, consumed_gas, balance_updates, ops =
          apply_delegation
            ~ctxt
            ~sender
            ~delegate
            ~before_operation:ctxt_before_op
        in
        (ctxt, IDelegation_result {consumed_gas; balance_updates}, ops)

let apply_manager_operation : type kind.
    context ->
    source:public_key_hash ->
    chain_id:Chain_id.t ->
    consume_gas_for_sig_check:Gas.cost option ->
    kind manager_operation ->
    (context
    * kind successful_manager_operation_result
    * Script_typed_ir.packed_internal_operation list)
    tzresult
    Lwt.t =
  let open Lwt_result_syntax in
  fun ctxt_before_op ~source ~chain_id ~consume_gas_for_sig_check operation ->
    let source_contract = Contract.Implicit source in
    (* See the comment above [fixed_gas_cost] in the
       {!Validate.check_contents} function. *)
    let fixed_gas_cost =
      let manager_op_cost = Michelson_v1_gas.Cost_of.manager_operation in
      match consume_gas_for_sig_check with
      | None -> manager_op_cost
      | Some gas_for_sig_check -> Gas.(manager_op_cost +@ gas_for_sig_check)
    in
    let*? ctxt = Gas.consume ctxt_before_op fixed_gas_cost in
    (* Note that [ctxt_before_op] will be used again later to compute
       gas consumption and originations for the operation result (by
       comparing it with the [ctxt] we will have at the end of the
       application). *)
    let consume_deserialization_gas =
      (* Note that we used to set this to [Script.When_needed] because
         the deserialization gas was accounted for in the gas consumed
         by precheck. However, we no longer have access to this precheck
         gas, so we want to always consume the deserialization gas
         again, independently of the internal state of the lazy_exprs in
         the arguments. *)
      Script.Always
    in
    match operation with
    | Reveal {public_key; proof} ->
        (* TODO #2603

           Even if [precheck_manager_contents] has already asserted that
           the implicit contract is allocated, we must re-do this check in
           case the manager has been emptied while collecting fees. This
           should be solved by forking out [validate_operation] from
           [apply_operation]. *)
        let* () = Contract.must_be_allocated ctxt source_contract in
        (* Check proof *)
        let* ctxt =
          match (public_key, proof) with
          | Bls bls_public_key, Some proof ->
              let*? ctxt =
                let gas_cost_for_sig_check =
                  let open Saturation_repr.Syntax in
                  let size = Bls.Public_key.size bls_public_key in
                  Operation_costs.serialization_cost size
                  + Michelson_v1_gas.Cost_of.Interpreter.check_signature_on_algo
                      Bls
                      size
                in
                Gas.consume ctxt gas_cost_for_sig_check
              in
              let* () =
                fail_unless
                  (Signature.pop_verify bls_public_key (Bls.to_bytes proof))
                  (Validate_errors.Manager.Incorrect_bls_proof
                     {kind = Manager_pk; public_key; proof})
              in
              return ctxt
          | _, _ -> return ctxt
        in
        (* TODO tezos/tezos#3070

           We have already asserted the consistency of the supplied public
           key during precheck, so we avoid re-checking that precondition
           with [?check_consistency=false]. This optional parameter is
           temporary, to avoid breaking compatibility with external legacy
           usage of [Contract.reveal_manager_key]. However, the pattern of
           using [Contract.check_public_key] and this usage of
           [Contract.reveal_manager_key] should become the standard. *)
        let* ctxt =
          Contract.reveal_manager_key
            ~check_consistency:false
            ctxt
            source
            public_key
        in
        return
          ( ctxt,
            (Reveal_result
               {consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt}
              : kind successful_manager_operation_result),
            [] )
    | Transaction {amount; parameters; destination = Implicit pkh; entrypoint}
      ->
        let*? parameters, ctxt =
          Script.force_decode_in_context
            ~consume_deserialization_gas
            ctxt
            parameters
        in
        let elab_conf = Script_ir_translator_config.make ~legacy:false () in
        let+ ctxt, res, ops =
          match Entrypoint.to_string entrypoint with
          | "default" ->
              let* () =
                fail_unless
                  (Script.is_unit parameters)
                  (Script_interpreter.Bad_contract_parameter source_contract)
              in
              apply_transaction_to_implicit
                ~ctxt
                ~sender:source_contract
                ~amount
                ~pkh
                ~before_operation:ctxt_before_op
          | "stake" ->
              let* () =
                fail_unless
                  (Script.is_unit parameters)
                  (Script_interpreter.Bad_contract_parameter source_contract)
              in
              apply_stake
                ~ctxt
                ~sender:source
                ~amount
                ~destination:pkh
                ~before_operation:ctxt_before_op
          | "unstake" ->
              let* () =
                fail_unless
                  (Script.is_unit parameters)
                  (Script_interpreter.Bad_contract_parameter source_contract)
              in
              apply_unstake
                ~ctxt
                ~sender:source
                ~amount
                ~destination:pkh
                ~before_operation:ctxt_before_op
          | "finalize_unstake" ->
              let* () =
                fail_unless
                  (Script.is_unit parameters)
                  (Script_interpreter.Bad_contract_parameter source_contract)
              in
              apply_finalize_unstake
                ~ctxt
                ~amount
                ~destination:pkh
                ~before_operation:ctxt_before_op
          | "set_delegate_parameters" ->
              let* ( ( limit_of_staking_over_baking_millionth,
                       (edge_of_baking_over_staking_billionth, ()) ),
                     ctxt ) =
                Script_ir_translator.parse_data
                  ~elab_conf
                  ctxt
                  ~allow_forged_tickets:false
                  ~allow_forged_lazy_storage_id:false
                  Script_typed_ir.pair_int_int_unit_t
                  (Micheline.root parameters)
              in
              apply_set_delegate_parameters
                ~ctxt
                ~sender:source
                ~destination:pkh
                ~limit_of_staking_over_baking_millionth:
                  (Script_int.to_zint limit_of_staking_over_baking_millionth)
                ~edge_of_baking_over_staking_billionth:
                  (Script_int.to_zint edge_of_baking_over_staking_billionth)
                ~before_operation:ctxt_before_op
          | _ -> tzfail (Script_tc_errors.No_such_entrypoint entrypoint)
        in
        (ctxt, Transaction_result res, ops)
    | Transaction
        {amount; parameters; destination = Originated contract_hash; entrypoint}
      ->
        let*? parameters, ctxt =
          Script.force_decode_in_context
            ~consume_deserialization_gas
            ctxt
            parameters
        in
        let* ctxt, (cache_key, script, script_ir) =
          find_contract_from_cache ctxt contract_hash
        in
        let+ ctxt, res, ops =
          if not @@ Constants.direct_ticket_spending_enable ctxt then
            apply_transaction_to_smart_contract
              ~ctxt
              ~sender:(Destination.Contract source_contract)
              ~contract_hash
              ~amount
              ~entrypoint
              ~before_operation:ctxt_before_op
              ~payer:source
              ~chain_id
              ~internal:false
              ~parameter:(Untyped_arg parameters)
              ~script
              ~script_ir
              ~cache_key
              ()
          else
            let (Ex_script (Script {arg_type; entrypoints; _})) = script_ir in
            let*? res, ctxt =
              Gas_monad.run
                ctxt
                (Script_ir_translator.find_entrypoint
                   ~error_details:(Informative ())
                   arg_type
                   entrypoints
                   entrypoint)
            in
            let*? (Ex_ty_cstr {ty = parameters_ty; _}) = res in
            let* typed_arg, ctxt =
              Script_ir_translator.parse_data
                ctxt
                ~elab_conf:Script_ir_translator_config.(make ~legacy:false ())
                ~allow_forged_tickets:true
                ~allow_forged_lazy_storage_id:false
                parameters_ty
                (Micheline.root parameters)
            in
            let* ctxt, ticket_receipt, paid_storage_diff =
              Ticket_transfer.transfer_tickets_in_parameters
                ctxt
                typed_arg
                parameters_ty
                ~source:(Contract source_contract)
                ~dst:(Contract (Originated contract_hash))
            in
            apply_transaction_to_smart_contract
              ~ctxt
              ~sender:(Destination.Contract source_contract)
              ~contract_hash
              ~amount
              ~entrypoint
              ~before_operation:ctxt_before_op
              ~payer:source
              ~chain_id
              ~internal:false
              ~parameter:
                (Typed_arg (Micheline.dummy_location, parameters_ty, typed_arg))
              ~script
              ~script_ir
              ~cache_key
              ~ticket_receipt_acc:ticket_receipt
              ~paid_storage_diff_acc:paid_storage_diff
              ()
        in
        (ctxt, Transaction_result res, ops)
    | Transfer_ticket {contents; ty; ticketer; amount; destination; entrypoint}
      -> (
        match destination with
        | Implicit _ ->
            let*? () =
              error_unless
                Entrypoint.(entrypoint = default)
                (Script_tc_errors.No_such_entrypoint entrypoint)
            in
            let* ctxt, ticket =
              Ticket_transfer.parse_ticket
                ~consume_deserialization_gas
                ~ticketer
                ~contents
                ~ty
                ctxt
            in
            let* ctxt, paid_storage_size_diff =
              Ticket_transfer.transfer_ticket
                ctxt
                ~sender:(Contract source_contract)
                ~dst:(Contract destination)
                ticket
                amount
            in
            let* ticket_token, ctxt =
              Ticket_token_unparser.unparse ctxt ticket
            in
            let amount = Script_int.(to_zint (amount :> n num)) in
            let ticket_receipt =
              Ticket_receipt.
                [
                  {
                    ticket_token;
                    updates =
                      [
                        {
                          account = Contract source_contract;
                          amount = Z.neg amount;
                        };
                        {account = Contract destination; amount};
                      ];
                  };
                ]
            in
            return
              ( ctxt,
                Transfer_ticket_result
                  {
                    balance_updates = [];
                    ticket_receipt;
                    consumed_gas =
                      Gas.consumed ~since:ctxt_before_op ~until:ctxt;
                    paid_storage_size_diff;
                  },
                [] )
        | Originated destination_hash ->
            let* ctxt, token, op =
              Ticket_transfer.parse_ticket_and_operation
                ~consume_deserialization_gas
                ~ticketer
                ~contents
                ~ty
                ~sender:(Destination.Contract source_contract)
                ~destination:destination_hash
                ~entrypoint
                ~amount
                ctxt
            in
            let* ctxt, paid_storage_size_diff =
              Ticket_transfer.transfer_ticket
                ctxt
                ~sender:(Contract source_contract)
                ~dst:(Contract destination)
                token
                amount
            in
            let* ticket_token, ctxt =
              Ticket_token_unparser.unparse ctxt token
            in
            let amount = Script_int.(to_zint (amount :> n num)) in
            let ticket_receipt =
              Ticket_receipt.
                [
                  {
                    ticket_token;
                    updates =
                      [
                        {
                          account = Contract source_contract;
                          amount = Z.neg amount;
                        }
                        (* The transfer of the ticket to [destination] is part of the internal operation [op]. *);
                      ];
                  };
                ]
            in
            return
              ( ctxt,
                Transfer_ticket_result
                  {
                    balance_updates = [];
                    ticket_receipt;
                    consumed_gas =
                      Gas.consumed ~since:ctxt_before_op ~until:ctxt;
                    paid_storage_size_diff;
                  },
                [op] ))
    | Origination {delegate; script; credit} ->
        (* Internal originations have their address generated in the interpreter
           so that the script can use it immediately.
           The address of external originations is generated here. *)
        let*? ctxt, contract =
          Contract.fresh_contract_from_current_nonce ctxt
        in
        let*? _unparsed_storage, ctxt =
          Script.force_decode_in_context
            ~consume_deserialization_gas
            ctxt
            script.Script.storage
        in
        let*? unparsed_code, ctxt =
          Script.force_decode_in_context
            ~consume_deserialization_gas
            ctxt
            script.Script.code
        in
        let* Ex_script parsed_script, ctxt =
          Script_ir_translator.parse_script
            ctxt
            ~elab_conf:Script_ir_translator_config.(make ~legacy:false ())
            ~allow_forged_tickets_in_storage:false
            ~allow_forged_lazy_storage_id_in_storage:false
            (Script script)
        in
        let (Script {storage_type; storage; implementation; _}) =
          parsed_script
        in
        let* ctxt =
          match implementation with
          | Lambda {views; _} ->
              let views_result =
                Script_ir_translator.parse_views
                  ctxt
                  ~elab_conf:Script_ir_translator_config.(make ~legacy:false ())
                  storage_type
                  views
              in
              let* _typed_views, ctxt =
                trace
                  (Script_tc_errors.Ill_typed_contract (unparsed_code, []))
                  views_result
              in
              return ctxt
          (* Native branch is technically dead, a native contract cannot be originated *)
          | Native _ -> return ctxt
        in
        let+ ctxt, origination_result, ops =
          apply_origination
            ~ctxt
            ~storage_type
            ~storage
            ~unparsed_code
            ~contract
            ~delegate
            ~sender:source_contract
            ~credit
            ~before_operation:ctxt_before_op
        in
        (ctxt, Origination_result origination_result, ops)
    | Delegation delegate ->
        let+ ctxt, consumed_gas, balance_updates, ops =
          apply_delegation
            ~ctxt
            ~sender:source_contract
            ~delegate
            ~before_operation:ctxt_before_op
        in
        (ctxt, Delegation_result {consumed_gas; balance_updates}, ops)
    | Register_global_constant {value} ->
        (* Decode the value and consume gas appropriately *)
        let*? expr, ctxt =
          Script.force_decode_in_context ~consume_deserialization_gas ctxt value
        in
        (* Set the key to the value in storage. *)
        let* ctxt, address, size =
          Global_constants_storage.register ctxt expr
        in
        (* The burn and the reporting of the burn are calculated differently.

           [Fees.record_global_constant_storage_space] does the actual burn
           based on the size of the constant registered, and this causes a
           change in account balance.

           On the other hand, the receipt is calculated
           with the help of [Fees.cost_of_bytes], and is included in block metadata
           and the client output. The receipt is also used during simulation,
           letting the client automatically set an appropriate storage limit.
           TODO : is this concern still honored by the token management
           refactoring ? *)
        let ctxt, paid_size =
          Fees.record_global_constant_storage_space ctxt size
        in
        let result =
          Register_global_constant_result
            {
              balance_updates = [];
              consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt;
              size_of_constant = paid_size;
              global_address = address;
            }
        in
        return (ctxt, result, [])
    | Set_deposits_limit _limit ->
        tzfail Set_deposits_limit_when_automated_staking_off
    | Increase_paid_storage {amount_in_bytes; destination} ->
        let* ctxt =
          Contract.increase_paid_storage ctxt destination ~amount_in_bytes
        in
        let payer = `Contract (Contract.Implicit source) in
        let+ ctxt, storage_bus =
          Fees.burn_storage_increase_fees ctxt ~payer amount_in_bytes
        in
        let result =
          Increase_paid_storage_result
            {
              balance_updates = storage_bus;
              consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt;
            }
        in
        (ctxt, result, [])
    | Update_consensus_key {public_key; proof; kind} ->
        let*! is_registered = Delegate.registered ctxt source in
        let*? () =
          error_unless
            is_registered
            (Update_consensus_key_on_unregistered_delegate (source, kind))
        in
        (* Check proof *)
        let* ctxt =
          match (public_key, proof) with
          | Bls bls_public_key, Some proof ->
              let*? ctxt =
                let gas_cost_for_sig_check =
                  let open Saturation_repr.Syntax in
                  let size = Bls.Public_key.size bls_public_key in
                  Operation_costs.serialization_cost size
                  + Michelson_v1_gas.Cost_of.Interpreter.check_signature_on_algo
                      Bls
                      size
                in
                Gas.consume ctxt gas_cost_for_sig_check
              in
              let* () =
                fail_unless
                  (Signature.pop_verify bls_public_key (Bls.to_bytes proof))
                  (Validate_errors.Manager.Incorrect_bls_proof
                     {
                       kind = Operation_repr.consensus_to_public_key_kind kind;
                       public_key;
                       proof;
                     })
              in
              return ctxt
          | _, _ -> return ctxt
        in
        (* Register key *)
        let* ctxt =
          match (public_key, kind) with
          | _, Consensus ->
              Delegate.Consensus_key.register_update ctxt source public_key
          | Bls bls_pk, Companion ->
              Delegate.Consensus_key.register_update_companion
                ctxt
                source
                bls_pk
          | (Ed25519 _ | Secp256k1 _ | P256 _), Companion ->
              (* Should not happen if operation has been validated *)
              tzfail
                (Validate_errors.Manager.Update_companion_key_not_tz4
                   {source; public_key})
        in
        return
          ( ctxt,
            Update_consensus_key_result
              {
                kind;
                consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt;
              },
            [] )
    | Dal_publish_commitment slot_header ->
        let*? ctxt, slot_header =
          Dal_apply.apply_publish_commitment
            ctxt
            slot_header
            ~source:(Contract.Implicit source)
        in
        let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
        let result =
          Dal_publish_commitment_result {slot_header; consumed_gas}
        in
        return (ctxt, result, [])
    | Sc_rollup_originate {kind; boot_sector; parameters_ty; whitelist} ->
        let* {address; size; genesis_commitment_hash}, ctxt =
          Sc_rollup_operations.originate
            ctxt
            ~kind
            ~boot_sector
            ~parameters_ty
            ?whitelist
        in
        let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
        let result =
          Sc_rollup_originate_result
            {
              address;
              genesis_commitment_hash;
              consumed_gas;
              size;
              balance_updates = [];
            }
        in
        return (ctxt, result, [])
    | Sc_rollup_add_messages {messages} ->
        let* ctxt = Sc_rollup.Inbox.add_external_messages ctxt messages in
        let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
        let result = Sc_rollup_add_messages_result {consumed_gas} in
        return (ctxt, result, [])
    | Sc_rollup_cement {rollup} ->
        let* ctxt, commitment, commitment_hash =
          Sc_rollup.Stake_storage.cement_commitment ctxt rollup
        in
        let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
        let result =
          Sc_rollup_cement_result
            {
              consumed_gas;
              inbox_level = commitment.inbox_level;
              commitment_hash;
            }
        in
        return (ctxt, result, [])
    | Sc_rollup_publish {rollup; commitment} ->
        let* staked_hash, published_at_level, ctxt, balance_updates =
          Sc_rollup.Stake_storage.publish_commitment
            ctxt
            rollup
            source
            commitment
        in
        let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
        let result =
          Sc_rollup_publish_result
            {staked_hash; consumed_gas; published_at_level; balance_updates}
        in
        return (ctxt, result, [])
    | Sc_rollup_refute {rollup; opponent; refutation} ->
        let open Sc_rollup.Refutation_storage in
        let player = source in
        let* game_result, ctxt =
          match refutation with
          | Start {player_commitment_hash; opponent_commitment_hash} ->
              let* ctxt =
                start_game
                  ctxt
                  rollup
                  ~player:(player, player_commitment_hash)
                  ~opponent:(opponent, opponent_commitment_hash)
              in
              return (None, ctxt)
          | Move {step; choice} ->
              game_move ctxt rollup ~player ~opponent ~step ~choice
        in
        let* game_status, ctxt, balance_updates =
          match game_result with
          | None -> return (Sc_rollup.Game.Ongoing, ctxt, [])
          | Some game_result ->
              let stakers = Sc_rollup.Game.Index.make source opponent in
              Sc_rollup.Refutation_storage.apply_game_result
                ctxt
                rollup
                stakers
                game_result
        in
        let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
        let result =
          Sc_rollup_refute_result {game_status; consumed_gas; balance_updates}
        in
        return (ctxt, result, [])
    | Sc_rollup_timeout {rollup; stakers} ->
        let* game_result, ctxt =
          Sc_rollup.Refutation_storage.timeout ctxt rollup stakers
        in
        let* game_status, ctxt, balance_updates =
          Sc_rollup.Refutation_storage.apply_game_result
            ctxt
            rollup
            stakers
            game_result
        in
        let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
        let result =
          Sc_rollup_timeout_result {game_status; consumed_gas; balance_updates}
        in
        return (ctxt, result, [])
    | Sc_rollup_execute_outbox_message
        {rollup; cemented_commitment; output_proof} ->
        let+ ( {
                 Sc_rollup_operations.paid_storage_size_diff;
                 ticket_receipt;
                 whitelist_update;
                 operations;
               },
               ctxt ) =
          Sc_rollup_operations.execute_outbox_message
            ctxt
            rollup
            ~cemented_commitment
            ~output_proof
        in
        let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
        let result =
          Sc_rollup_execute_outbox_message_result
            {
              paid_storage_size_diff;
              ticket_receipt;
              whitelist_update;
              balance_updates = [];
              consumed_gas;
            }
        in
        (ctxt, result, operations)
    | Sc_rollup_recover_bond {sc_rollup; staker} ->
        let* ctxt, balance_updates =
          Sc_rollup.Stake_storage.withdraw_stake ctxt sc_rollup staker
        in
        let result =
          Sc_rollup_recover_bond_result
            {
              consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt;
              balance_updates;
            }
        in
        return (ctxt, result, [])
    | Zk_rollup_origination
        {public_parameters; circuits_info; init_state; nb_ops} ->
        Zk_rollup_apply.originate
          ~ctxt_before_op
          ~ctxt
          ~public_parameters
          ~circuits_info
          ~init_state
          ~nb_ops
    | Zk_rollup_publish {zk_rollup; ops} ->
        Zk_rollup_apply.publish ~ctxt_before_op ~ctxt ~zk_rollup ~l2_ops:ops
    | Zk_rollup_update {zk_rollup; update} ->
        Zk_rollup_apply.update ~ctxt_before_op ~ctxt ~zk_rollup ~update

type success_or_failure = Success of context | Failure

let apply_internal_operations ctxt ~payer ~chain_id ops =
  let open Lwt_syntax in
  let rec apply ctxt applied worklist =
    match worklist with
    | [] -> Lwt.return (Success ctxt, List.rev applied)
    | Script_typed_ir.Internal_operation ({sender; operation; nonce} as op)
      :: rest -> (
        let* result =
          if internal_nonce_already_recorded ctxt nonce then
            let op_res = Apply_internal_results.internal_operation op in
            tzfail (Internal_operation_replay (Internal_operation op_res))
          else
            let ctxt = record_internal_nonce ctxt nonce in
            apply_internal_operation_contents
              ctxt
              ~sender
              ~payer
              ~chain_id
              operation
        in
        match result with
        | Error errors ->
            let result =
              pack_internal_operation_result
                op
                (Failed (Script_typed_ir.manager_kind op.operation, errors))
            in
            let skipped =
              List.rev_map
                (fun (Script_typed_ir.Internal_operation op) ->
                  pack_internal_operation_result
                    op
                    (Skipped (Script_typed_ir.manager_kind op.operation)))
                rest
            in
            Lwt.return (Failure, List.rev (skipped @ (result :: applied)))
        | Ok (ctxt, result, emitted) ->
            apply
              ctxt
              (pack_internal_operation_result op (Applied result) :: applied)
              (emitted @ rest))
  in
  apply ctxt [] ops

let burn_transaction_storage_fees ctxt trr ~storage_limit ~payer =
  let open Lwt_result_syntax in
  match trr with
  | Transaction_to_contract_result payload ->
      let consumed = payload.paid_storage_size_diff in
      let* ctxt, storage_limit, storage_bus =
        Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
      in
      let* ctxt, storage_limit, origination_bus =
        if payload.allocated_destination_contract then
          Fees.burn_origination_fees ctxt ~storage_limit ~payer
        else return (ctxt, storage_limit, [])
      in
      let balance_updates =
        storage_bus @ payload.balance_updates @ origination_bus
      in
      return
        ( ctxt,
          storage_limit,
          Transaction_to_contract_result
            {
              storage = payload.storage;
              lazy_storage_diff = payload.lazy_storage_diff;
              balance_updates;
              ticket_receipt = payload.ticket_receipt;
              originated_contracts = payload.originated_contracts;
              consumed_gas = payload.consumed_gas;
              storage_size = payload.storage_size;
              paid_storage_size_diff = payload.paid_storage_size_diff;
              allocated_destination_contract =
                payload.allocated_destination_contract;
              address_registry_diff = payload.address_registry_diff;
            } )
  | Transaction_to_sc_rollup_result _ -> return (ctxt, storage_limit, trr)
  | Transaction_to_zk_rollup_result payload ->
      let consumed = payload.paid_storage_size_diff in
      let* ctxt, storage_limit, storage_bus =
        Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
      in
      let balance_updates = storage_bus @ payload.balance_updates in
      return
        ( ctxt,
          storage_limit,
          Transaction_to_zk_rollup_result {payload with balance_updates} )

let burn_origination_storage_fees ctxt
    {
      lazy_storage_diff;
      balance_updates;
      originated_contracts;
      consumed_gas;
      storage_size;
      paid_storage_size_diff;
    } ~storage_limit ~payer =
  let open Lwt_result_syntax in
  let consumed = paid_storage_size_diff in
  let* ctxt, storage_limit, storage_bus =
    Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
  in
  let* ctxt, storage_limit, origination_bus =
    Fees.burn_origination_fees ctxt ~storage_limit ~payer
  in
  let balance_updates = storage_bus @ origination_bus @ balance_updates in
  return
    ( ctxt,
      storage_limit,
      {
        lazy_storage_diff;
        balance_updates;
        originated_contracts;
        consumed_gas;
        storage_size;
        paid_storage_size_diff;
      } )

(** [burn_manager_storage_fees ctxt smopr storage_limit payer] burns the
    storage fees associated to an external operation result [smopr].
    Returns an updated context, an updated storage limit with the space consumed
    by the operation subtracted, and [smopr] with the relevant balance updates
    included. *)
let burn_manager_storage_fees : type kind.
    context ->
    kind successful_manager_operation_result ->
    storage_limit:Z.t ->
    payer:public_key_hash ->
    (context * Z.t * kind successful_manager_operation_result) tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun ctxt smopr ~storage_limit ~payer ->
    let payer = `Contract (Contract.Implicit payer) in
    match smopr with
    | Transaction_result transaction_result ->
        let+ ctxt, storage_limit, transaction_result =
          burn_transaction_storage_fees
            ctxt
            transaction_result
            ~storage_limit
            ~payer
        in
        (ctxt, storage_limit, Transaction_result transaction_result)
    | Origination_result origination_result ->
        let+ ctxt, storage_limit, origination_result =
          burn_origination_storage_fees
            ctxt
            origination_result
            ~storage_limit
            ~payer
        in
        (ctxt, storage_limit, Origination_result origination_result)
    | Reveal_result _ | Delegation_result _ ->
        return (ctxt, storage_limit, smopr)
    | Register_global_constant_result payload ->
        let consumed = payload.size_of_constant in
        let+ ctxt, storage_limit, storage_bus =
          Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
        in
        let balance_updates = storage_bus @ payload.balance_updates in
        ( ctxt,
          storage_limit,
          Register_global_constant_result
            {
              balance_updates;
              consumed_gas = payload.consumed_gas;
              size_of_constant = payload.size_of_constant;
              global_address = payload.global_address;
            } )
    | Set_deposits_limit_result _ | Update_consensus_key_result _ ->
        return (ctxt, storage_limit, smopr)
    | Increase_paid_storage_result _ -> return (ctxt, storage_limit, smopr)
    | Transfer_ticket_result payload ->
        let consumed = payload.paid_storage_size_diff in
        let+ ctxt, storage_limit, storage_bus =
          Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
        in
        let balance_updates = payload.balance_updates @ storage_bus in
        ( ctxt,
          storage_limit,
          Transfer_ticket_result {payload with balance_updates} )
    | Dal_publish_commitment_result _ -> return (ctxt, storage_limit, smopr)
    | Sc_rollup_originate_result payload ->
        let+ ctxt, storage_limit, balance_updates =
          Fees.burn_sc_rollup_origination_fees
            ctxt
            ~storage_limit
            ~payer
            payload.size
        in
        let result =
          Sc_rollup_originate_result {payload with balance_updates}
        in
        (ctxt, storage_limit, result)
    | Sc_rollup_add_messages_result _ -> return (ctxt, storage_limit, smopr)
    | Sc_rollup_cement_result _ -> return (ctxt, storage_limit, smopr)
    | Sc_rollup_publish_result _ -> return (ctxt, storage_limit, smopr)
    | Sc_rollup_refute_result _ -> return (ctxt, storage_limit, smopr)
    | Sc_rollup_timeout_result _ -> return (ctxt, storage_limit, smopr)
    | Sc_rollup_execute_outbox_message_result
        ({paid_storage_size_diff; balance_updates; _} as payload) ->
        let consumed = paid_storage_size_diff in
        let+ ctxt, storage_limit, storage_bus =
          Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
        in
        let balance_updates = storage_bus @ balance_updates in
        ( ctxt,
          storage_limit,
          Sc_rollup_execute_outbox_message_result {payload with balance_updates}
        )
    | Sc_rollup_recover_bond_result _ -> return (ctxt, storage_limit, smopr)
    | Zk_rollup_origination_result payload ->
        let* ctxt, storage_limit, balance_updates =
          Fees.burn_zk_rollup_origination_fees
            ctxt
            ~storage_limit
            ~payer
            payload.storage_size
        in
        let result =
          Zk_rollup_origination_result {payload with balance_updates}
        in
        return (ctxt, storage_limit, result)
    | Zk_rollup_publish_result payload ->
        let consumed = payload.paid_storage_size_diff in
        let+ ctxt, storage_limit, storage_bus =
          Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
        in
        let balance_updates = storage_bus @ payload.balance_updates in
        ( ctxt,
          storage_limit,
          Zk_rollup_publish_result {payload with balance_updates} )
    | Zk_rollup_update_result
        ({paid_storage_size_diff; balance_updates; _} as payload) ->
        let consumed = paid_storage_size_diff in
        let+ ctxt, storage_limit, storage_bus =
          Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
        in
        let balance_updates = storage_bus @ balance_updates in
        ( ctxt,
          storage_limit,
          Zk_rollup_update_result {payload with balance_updates} )

(** [burn_internal_storage_fees ctxt smopr storage_limit payer] burns the
    storage fees associated to an internal operation result [smopr].
    Returns an updated context, an updated storage limit with the space consumed
    by the operation subtracted, and [smopr] with the relevant balance updates
    included. *)
let burn_internal_storage_fees : type kind.
    context ->
    kind successful_internal_operation_result ->
    storage_limit:Z.t ->
    payer:public_key_hash ->
    (context * Z.t * kind successful_internal_operation_result) tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun ctxt smopr ~storage_limit ~payer ->
    let payer = `Contract (Contract.Implicit payer) in
    match smopr with
    | ITransaction_result transaction_result ->
        let+ ctxt, storage_limit, transaction_result =
          burn_transaction_storage_fees
            ctxt
            transaction_result
            ~storage_limit
            ~payer
        in
        (ctxt, storage_limit, ITransaction_result transaction_result)
    | IOrigination_result origination_result ->
        let+ ctxt, storage_limit, origination_result =
          burn_origination_storage_fees
            ctxt
            origination_result
            ~storage_limit
            ~payer
        in
        (ctxt, storage_limit, IOrigination_result origination_result)
    | IDelegation_result _ -> return (ctxt, storage_limit, smopr)
    | IEvent_result _ -> return (ctxt, storage_limit, smopr)

let apply_manager_contents (type kind) ctxt chain_id ~consume_gas_for_sig_check
    (op : kind Kind.manager contents) :
    (success_or_failure
    * kind manager_operation_result
    * packed_internal_operation_result list)
    Lwt.t =
  let open Lwt_result_syntax in
  let (Manager_operation {source; operation; gas_limit; storage_limit; _}) =
    op
  in
  (* We do not expose the internal scaling to the users. Instead, we multiply
       the specified gas limit by the internal scaling. *)
  let ctxt = Gas.set_limit ctxt gas_limit in
  let*! result =
    apply_manager_operation
      ctxt
      ~source
      ~chain_id
      ~consume_gas_for_sig_check
      operation
  in
  match result with
  | Ok (ctxt, operation_results, internal_operations) -> (
      let*! result =
        apply_internal_operations
          ctxt
          ~payer:source
          ~chain_id
          internal_operations
      in
      match result with
      | Success ctxt, internal_operations_results -> (
          let*! result =
            burn_manager_storage_fees
              ctxt
              operation_results
              ~storage_limit
              ~payer:source
          in
          match result with
          | Ok (ctxt, storage_limit, operation_results) ->
              let*! result =
                List.fold_left_es
                  (fun (ctxt, storage_limit, res) imopr ->
                    let (Internal_operation_result (op, mopr)) = imopr in
                    match mopr with
                    | Applied smopr ->
                        let* ctxt, storage_limit, smopr =
                          burn_internal_storage_fees
                            ctxt
                            smopr
                            ~storage_limit
                            ~payer:source
                        in
                        let imopr =
                          Internal_operation_result (op, Applied smopr)
                        in
                        return (ctxt, storage_limit, imopr :: res)
                    | _ -> return (ctxt, storage_limit, imopr :: res))
                  (ctxt, storage_limit, [])
                  internal_operations_results
              in
              Lwt.return
                (match result with
                | Ok (ctxt, _, internal_operations_results) ->
                    ( Success ctxt,
                      Applied operation_results,
                      List.rev internal_operations_results )
                | Error errors ->
                    ( Failure,
                      Backtracked (operation_results, Some errors),
                      internal_operations_results ))
          | Error errors ->
              Lwt.return
                ( Failure,
                  Backtracked (operation_results, Some errors),
                  internal_operations_results ))
      | Failure, internal_operations_results ->
          Lwt.return
            (Failure, Applied operation_results, internal_operations_results))
  | Error errors ->
      Lwt.return (Failure, Failed (manager_kind operation, errors), [])

(** An individual manager operation (either standalone or inside a
    batch) together with the balance update corresponding to the
    transfer of its fee. *)
type 'kind fees_updated_contents = {
  contents : 'kind contents;
  balance_updates : Receipt.balance_updates;
}

type _ fees_updated_contents_list =
  | FeesUpdatedSingle :
      'kind fees_updated_contents
      -> 'kind fees_updated_contents_list
  | FeesUpdatedCons :
      'kind Kind.manager fees_updated_contents
      * 'rest Kind.manager fees_updated_contents_list
      -> ('kind * 'rest) Kind.manager fees_updated_contents_list

let rec mark_skipped : type kind.
    payload_producer:Consensus_key.t ->
    Level.t ->
    kind Kind.manager fees_updated_contents_list ->
    kind Kind.manager contents_result_list =
 fun ~payload_producer level fees_updated_contents_list ->
  match fees_updated_contents_list with
  | FeesUpdatedSingle
      {contents = Manager_operation {operation; _}; balance_updates} ->
      Single_result
        (Manager_operation_result
           {
             balance_updates;
             operation_result = Skipped (manager_kind operation);
             internal_operation_results = [];
           })
  | FeesUpdatedCons
      ({contents = Manager_operation {operation; _}; balance_updates}, rest) ->
      Cons_result
        ( Manager_operation_result
            {
              balance_updates;
              operation_result = Skipped (manager_kind operation);
              internal_operation_results = [];
            },
          mark_skipped ~payload_producer level rest )

(** Return balance updates for fees, and an updated context that
   accounts for:

    - fees spending,

    - counter incrementation,

    - consumption of each operation's [gas_limit] from the available
   block gas.

    The operation should already have been validated by
   {!Validate.validate_operation}. The latter is responsible for ensuring that
   the operation is solvable, i.e. its fees can be taken, i.e.
   [take_fees] cannot return an error. *)
let take_fees ctxt contents_list =
  let open Lwt_result_syntax in
  let rec take_fees_rec : type kind.
      context ->
      kind Kind.manager contents_list ->
      (context * kind Kind.manager fees_updated_contents_list) tzresult Lwt.t =
   fun ctxt contents_list ->
    let contents_effects contents =
      let (Manager_operation {source; fee; gas_limit; _}) = contents in
      let*? ctxt = Gas.consume_limit_in_block ctxt gas_limit in
      let* ctxt = Contract.increment_counter ctxt source in
      let+ ctxt, balance_updates =
        Token.transfer
          ctxt
          (`Contract (Contract.Implicit source))
          `Block_fees
          fee
      in
      (ctxt, {contents; balance_updates})
    in
    match contents_list with
    | Single contents ->
        let+ ctxt, fees_updated_contents = contents_effects contents in
        (ctxt, FeesUpdatedSingle fees_updated_contents)
    | Cons (contents, rest) ->
        let* ctxt, fees_updated_contents = contents_effects contents in
        let+ ctxt, result_rest = take_fees_rec ctxt rest in
        (ctxt, FeesUpdatedCons (fees_updated_contents, result_rest))
  in
  let*! result = take_fees_rec ctxt contents_list in
  Lwt.return (record_trace Error_while_taking_fees result)

let rec apply_manager_contents_list_rec : type kind.
    context ->
    payload_producer:Consensus_key.t ->
    Chain_id.t ->
    consume_gas_for_sig_check:Gas.cost option ->
    kind Kind.manager fees_updated_contents_list ->
    (success_or_failure * kind Kind.manager contents_result_list) Lwt.t =
  let open Lwt_syntax in
  fun ctxt
      ~payload_producer
      chain_id
      ~consume_gas_for_sig_check
      fees_updated_contents_list
    ->
    let level = Level.current ctxt in
    match fees_updated_contents_list with
    | FeesUpdatedSingle {contents = Manager_operation _ as op; balance_updates}
      ->
        let+ ctxt_result, operation_result, internal_operation_results =
          apply_manager_contents ctxt chain_id ~consume_gas_for_sig_check op
        in
        let result =
          Manager_operation_result
            {balance_updates; operation_result; internal_operation_results}
        in
        (ctxt_result, Single_result result)
    | FeesUpdatedCons
        ({contents = Manager_operation _ as op; balance_updates}, rest) -> (
        let* result =
          apply_manager_contents ctxt chain_id ~consume_gas_for_sig_check op
        in
        match result with
        | Failure, operation_result, internal_operation_results ->
            let result =
              Manager_operation_result
                {balance_updates; operation_result; internal_operation_results}
            in
            Lwt.return
              ( Failure,
                Cons_result (result, mark_skipped ~payload_producer level rest)
              )
        | Success ctxt, operation_result, internal_operation_results ->
            let result =
              Manager_operation_result
                {balance_updates; operation_result; internal_operation_results}
            in
            let+ ctxt_result, results =
              apply_manager_contents_list_rec
                ctxt
                ~payload_producer
                chain_id
                ~consume_gas_for_sig_check:None
                rest
            in
            (ctxt_result, Cons_result (result, results)))

let mark_backtracked results =
  let mark_results : type kind.
      kind Kind.manager contents_result -> kind Kind.manager contents_result =
   fun results ->
    let mark_manager_operation_result : type kind.
        kind manager_operation_result -> kind manager_operation_result =
      function
      | (Failed _ | Skipped _ | Backtracked _) as result -> result
      | Applied result -> Backtracked (result, None)
    in
    let mark_internal_operation_result : type kind.
        kind internal_operation_result -> kind internal_operation_result =
      function
      | (Failed _ | Skipped _ | Backtracked _) as result -> result
      | Applied result -> Backtracked (result, None)
    in
    let mark_internal_operation_results
        (Internal_operation_result (kind, result)) =
      Internal_operation_result (kind, mark_internal_operation_result result)
    in
    match results with
    | Manager_operation_result op ->
        Manager_operation_result
          {
            balance_updates = op.balance_updates;
            operation_result = mark_manager_operation_result op.operation_result;
            internal_operation_results =
              List.map
                mark_internal_operation_results
                op.internal_operation_results;
          }
  in
  let rec traverse_apply_results : type kind.
      kind Kind.manager contents_result_list ->
      kind Kind.manager contents_result_list = function
    | Single_result res -> Single_result (mark_results res)
    | Cons_result (res, rest) ->
        Cons_result (mark_results res, traverse_apply_results rest)
  in
  traverse_apply_results results

type mode =
  | Application of {
      block_header : Block_header.t;
      fitness : Fitness.t;
      payload_producer : Consensus_key.t;
      block_producer : Consensus_key.t;
      predecessor_level : Level.t;
      predecessor_round : Round.t;
    }
  | Full_construction of {
      block_data_contents : Block_header.contents;
      predecessor_hash : Block_hash.t;
      payload_producer : Consensus_key.t;
      block_producer : Consensus_key.t;
      round : Round.t;
      predecessor_level : Level.t;
      predecessor_round : Round.t;
    }
  | Partial_construction of {predecessor_fitness : Fitness.raw}

type application_state = {
  ctxt : t;
  chain_id : Chain_id.t;
  mode : mode;
  op_count : int;
  migration_balance_updates : Receipt.balance_updates;
  liquidity_baking_toggle_ema : Per_block_votes.Liquidity_baking_toggle_EMA.t;
  implicit_operations_results :
    Apply_results.packed_successful_manager_operation_result list;
}

let record_operation (type kind) ctxt hash (operation : kind operation) :
    context =
  match operation.protocol_data.contents with
  | Single (Preattestation _) -> ctxt
  | Single (Attestation _) -> ctxt
  | Single (Preattestations_aggregate _) -> ctxt
  | Single (Attestations_aggregate _) -> ctxt
  | Single
      ( Failing_noop _ | Proposals _ | Ballot _ | Seed_nonce_revelation _
      | Vdf_revelation _ | Double_consensus_operation_evidence _
      | Double_baking_evidence _ | Dal_entrapment_evidence _
      | Activate_account _ | Drain_delegate _ | Manager_operation _ )
  | Cons (Manager_operation _, _) ->
      record_non_consensus_operation_hash ctxt hash

let find_in_slot_map slot slot_map =
  let open Result_syntax in
  match slot_map with
  | None -> tzfail (Consensus.Slot_map_not_found {loc = __LOC__})
  | Some slot_map -> (
      match Slot.Map.find slot slot_map with
      | None ->
          (* This should not happen: operation validation should have failed. *)
          tzfail Faulty_validation_wrong_slot
      | Some x -> return x)

let record_preattestation ctxt (mode : mode) (content : consensus_content) :
    (context * Kind.preattestation contents_result_list) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let ctxt =
    match mode with
    | Full_construction _ -> (
        match Consensus.get_preattestations_quorum_round ctxt with
        | None -> Consensus.set_preattestations_quorum_round ctxt content.round
        | Some _ -> ctxt)
    | Application _ | Partial_construction _ -> ctxt
  in
  let mk_preattestation_result ({delegate; consensus_pkh; _} : Consensus_key.pk)
      consensus_power =
    let consensus_power =
      Attesting_power.to_result
        ctxt
        ~attested_level:content.level
        consensus_power
    in
    Single_result
      (Preattestation_result
         {
           balance_updates = [];
           delegate;
           consensus_key = consensus_pkh;
           consensus_power;
         })
  in
  match mode with
  | Application _ | Full_construction _ ->
      let*? {consensus_key; attesting_power; dal_power = _} =
        find_in_slot_map content.slot (Consensus.allowed_preattestations ctxt)
      in
      let*? ctxt =
        Consensus.record_preattestation
          ctxt
          ~initial_slot:content.slot
          ~power:attesting_power
          content.round
      in
      return (ctxt, mk_preattestation_result consensus_key attesting_power)
  | Partial_construction _ ->
      (* In mempool mode, preattestations are allowed for various levels
         and rounds. We do not record preattestations because we could get
         false-positive conflicts for preattestations with the same slot
         but different levels/rounds. We could record just preattestations
         for the mempool head's level and round (the most usual
         preattestations), but we don't need to, because there is no block
         to finalize anyway in this mode. *)
      let* ctxt, consensus_key =
        Stake_distribution.attestation_slot_owner
          ctxt
          ~attested_level:(Level.from_raw ctxt content.level)
          content.slot
      in
      return
        ( ctxt,
          mk_preattestation_result
            consensus_key
            Attesting_power.zero (* Fake power. *) )

let record_dal_content ctxt ~delegate level slot ~dal_power dal_content_opt =
  let open Lwt_result_syntax in
  match dal_content_opt with
  | None -> return ctxt
  | Some {attestation} ->
      let* proto_activation_level = Protocol_activation_level.get ctxt in
      let lag = Constants.dal_attestation_lag ctxt in
      let attestation =
        if Raw_level.(level < add proto_activation_level lag) then
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/8065
           CODE TO BE REVERTED IN PROTOCOL V *)
          Dal.Attestation.empty
        else attestation
      in
      let*? ctxt =
        Dal_apply.apply_attestation
          ctxt
          ~delegate
          ~tb_slot:slot
          attestation
          ~power:dal_power
      in
      return ctxt

let record_attestation ctxt (mode : mode) (consensus : consensus_content)
    (dal_content_opt : dal_content option) :
    (context * Kind.attestation contents_result_list) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let mk_attestation_result ({delegate; consensus_pkh; _} : Consensus_key.pk)
      consensus_power =
    let consensus_power =
      Attesting_power.to_result
        ctxt
        ~attested_level:consensus.level
        consensus_power
    in
    Single_result
      (Attestation_result
         {
           balance_updates = [];
           delegate;
           consensus_key = consensus_pkh;
           consensus_power;
         })
  in
  match mode with
  | Application _ | Full_construction _ ->
      let*? {consensus_key; attesting_power; dal_power} =
        find_in_slot_map consensus.slot (Consensus.allowed_attestations ctxt)
      in
      let*? ctxt =
        Consensus.record_attestation
          ctxt
          ~initial_slot:consensus.slot
          ~power:attesting_power
      in
      let* ctxt =
        record_dal_content
          ctxt
          ~delegate:consensus_key.delegate
          consensus.level
          consensus.slot
          ~dal_power
          dal_content_opt
      in
      return (ctxt, mk_attestation_result consensus_key attesting_power)
  | Partial_construction _ ->
      (* In mempool mode, attestations are allowed for various levels
         and rounds. We do not record attestations because we could get
         false-positive conflicts for attestations with the same slot
         but different levels/rounds. We could record just attestations
         for the predecessor's level and round (the most usual
         attestations), but we don't need to, because there is no block
         to finalize anyway in this mode. *)
      let* ctxt, consensus_key =
        Stake_distribution.attestation_slot_owner
          ctxt
          ~attested_level:(Level.from_raw ctxt consensus.level)
          consensus.slot
      in
      return
        ( ctxt,
          mk_attestation_result
            consensus_key
            Attesting_power.zero (* Fake power. *) )

let record_attestations_aggregate ctxt (mode : mode)
    (content : consensus_aggregate_content) committee :
    (context * Kind.attestations_aggregate contents_result_list) tzresult Lwt.t
    =
  let open Lwt_result_syntax in
  match mode with
  | Application _ | Full_construction _ ->
      let slot_map = Consensus.allowed_attestations ctxt in
      let* ctxt, rev_committee, total_consensus_power =
        List.fold_left_es
          (fun (ctxt, consensus_keys, consensus_power) (slot, dal) ->
            let*? {
                    consensus_key = {delegate; consensus_pkh; _};
                    attesting_power;
                    dal_power;
                  } =
              find_in_slot_map slot slot_map
            in
            let*? ctxt =
              Consensus.record_attestation
                ctxt
                ~initial_slot:slot
                ~power:attesting_power
            in
            let* ctxt =
              record_dal_content
                ctxt
                ~delegate
                content.level
                slot
                ~dal_power
                dal
            in
            let key = ({delegate; consensus_pkh} : Consensus_key.t) in
            return
              ( ctxt,
                ( key,
                  Attesting_power.to_result
                    ctxt
                    ~attested_level:content.level
                    attesting_power )
                :: consensus_keys,
                Attesting_power.add attesting_power consensus_power ))
          (ctxt, [], Attesting_power.zero)
          committee
      in
      let total_consensus_power =
        Attesting_power.to_result
          ctxt
          ~attested_level:content.level
          total_consensus_power
      in
      let result =
        Attestations_aggregate_result
          {
            balance_updates = [];
            committee = List.rev rev_committee;
            total_consensus_power;
          }
      in
      return (ctxt, Single_result result)
  | Partial_construction _ ->
      (* Attestations_aggregate are built at baking time and shouldn't be
         propagated between mempools.*)
      tzfail Validate_errors.Consensus.Aggregate_in_mempool

let record_preattestations_aggregate ctxt (mode : mode)
    (content : consensus_aggregate_content) (committee : Slot.t list) :
    (context * Kind.preattestations_aggregate contents_result_list) tzresult
    Lwt.t =
  let open Lwt_result_syntax in
  match mode with
  | Application _ | Full_construction _ ->
      let round = content.round in
      let ctxt =
        match mode with
        | Full_construction _ -> (
            match Consensus.get_preattestations_quorum_round ctxt with
            | None -> Consensus.set_preattestations_quorum_round ctxt round
            | Some _ -> ctxt)
        | Application _ | Partial_construction _ -> ctxt
      in
      let slot_map = Consensus.allowed_preattestations ctxt in
      (* Accumulate the list of delegates and the total attesting power *)
      let*? ctxt, rev_committee, total_consensus_power =
        let open Result_syntax in
        List.fold_left_e
          (fun (ctxt, consensus_keys, consensus_power) slot ->
            let* {
                   consensus_key = {delegate; consensus_pkh; _};
                   attesting_power;
                   dal_power = _;
                 } =
              find_in_slot_map slot slot_map
            in
            let* ctxt =
              Consensus.record_preattestation
                ctxt
                ~initial_slot:slot
                ~power:attesting_power
                round
            in
            let key = ({delegate; consensus_pkh} : Consensus_key.t) in
            return
              ( ctxt,
                ( key,
                  Attesting_power.to_result
                    ctxt
                    ~attested_level:content.level
                    attesting_power )
                :: consensus_keys,
                Attesting_power.add attesting_power consensus_power ))
          (ctxt, [], Attesting_power.zero)
          committee
      in
      let total_consensus_power =
        Attesting_power.to_result
          ctxt
          ~attested_level:content.level
          total_consensus_power
      in
      let result =
        Preattestations_aggregate_result
          {
            balance_updates = [];
            committee = List.rev rev_committee;
            total_consensus_power;
          }
      in
      return (ctxt, Single_result result)
  | Partial_construction _ ->
      (* Attestations_aggregate are built at baking time and shouldn't be
         propagated between mempools.*)
      tzfail Validate_errors.Consensus.Aggregate_in_mempool

let apply_manager_contents_list ctxt ~payload_producer chain_id
    ~gas_cost_for_sig_check fees_updated_contents_list =
  let open Lwt_syntax in
  let* ctxt_result, results =
    apply_manager_contents_list_rec
      ctxt
      ~payload_producer
      chain_id
      ~consume_gas_for_sig_check:(Some gas_cost_for_sig_check)
      fees_updated_contents_list
  in
  match ctxt_result with
  | Failure -> Lwt.return (ctxt (* backtracked *), mark_backtracked results)
  | Success ctxt ->
      let+ ctxt = Lazy_storage.cleanup_temporaries ctxt in
      (ctxt, results)

let apply_manager_operations ctxt ~payload_producer chain_id ~mempool_mode
    ~source ~operation contents_list =
  let open Lwt_result_syntax in
  let ctxt = if mempool_mode then Gas.reset_block_gas ctxt else ctxt in
  let* ctxt, fees_updated_contents_list = take_fees ctxt contents_list in
  let gas_cost_for_sig_check =
    let algo =
      Michelson_v1_gas.Cost_of.Interpreter.algo_of_public_key_hash source
    in
    Operation_costs.check_signature_cost algo operation
  in
  let*! ctxt, contents_result_list =
    apply_manager_contents_list
      ctxt
      ~payload_producer
      chain_id
      ~gas_cost_for_sig_check
      fees_updated_contents_list
  in
  return (ctxt, contents_result_list)

let punish_double_signing ctxt ~operation_hash delegate misbehaviour
    ~payload_producer =
  let open Lwt_result_syntax in
  let rewarded_delegate = payload_producer.Consensus_key.delegate in
  let* ctxt =
    Delegate.punish_double_signing
      ctxt
      ~operation_hash
      misbehaviour
      delegate
      ~rewarded:rewarded_delegate
  in
  let contents_result =
    {punished_delegate = delegate; rewarded_delegate; misbehaviour}
  in
  return (ctxt, contents_result)

let punish_double_consensus_operation ctxt ~operation_hash ~payload_producer
    contents =
  let open Lwt_result_syntax in
  let (Double_consensus_operation_evidence {slot; op1; op2 = _}) = contents in
  let misbehaviour =
    match op1.protocol_data.contents with
    | Single
        ( Preattestation {level; round; _}
        | Preattestations_aggregate {consensus_content = {level; round; _}; _}
          ) ->
        Misbehaviour.{level; round; kind = Double_preattesting}
    | Single
        ( Attestation {consensus_content = {level; round; _}; _}
        | Attestations_aggregate {consensus_content = {level; round; _}; _} ) ->
        Misbehaviour.{level; round; kind = Double_attesting}
  in
  let* ctxt, {delegate; _} =
    Stake_distribution.attestation_slot_owner
      ctxt
      ~attested_level:(Level.from_raw ctxt misbehaviour.level)
      slot
  in
  let* ctxt, contents_result =
    punish_double_signing
      ctxt
      ~operation_hash
      delegate
      misbehaviour
      ~payload_producer
  in
  return
    ( ctxt,
      Single_result (Double_consensus_operation_evidence_result contents_result)
    )

let punish_double_baking ctxt ~operation_hash (bh1 : Block_header.t)
    ~payload_producer =
  let open Lwt_result_syntax in
  let*? bh1_fitness = Fitness.from_raw bh1.shell.fitness in
  let round = Fitness.round bh1_fitness in
  let*? raw_level = Raw_level.of_int32 bh1.shell.level in
  let level = Level.from_raw ctxt raw_level in
  let* ctxt, _, {delegate; _} =
    Stake_distribution.baking_rights_owner ctxt level ~round
  in
  let* ctxt, contents_result =
    punish_double_signing
      ctxt
      ~operation_hash
      delegate
      {level = raw_level; round; kind = Double_baking}
      ~payload_producer
  in
  return (ctxt, Single_result (Double_baking_evidence_result contents_result))

let apply_contents_list (type kind) ctxt chain_id (mode : mode)
    ~payload_producer ~operation ~operation_hash
    (contents_list : kind contents_list) :
    (context * kind contents_result_list) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let mempool_mode =
    match mode with
    | Partial_construction _ -> true
    | Full_construction _ | Application _ -> false
  in
  match contents_list with
  | Single (Preattestation consensus_content) ->
      record_preattestation ctxt mode consensus_content
  | Single (Attestation {consensus_content; dal_content}) ->
      record_attestation ctxt mode consensus_content dal_content
  | Single (Preattestations_aggregate {consensus_content; committee}) ->
      let*? () =
        error_unless
          (Constants.aggregate_attestation ctxt)
          Validate_errors.Consensus.(Aggregate_disabled)
      in
      record_preattestations_aggregate ctxt mode consensus_content committee
  | Single (Attestations_aggregate {consensus_content; committee}) ->
      let*? () =
        error_unless
          (Constants.aggregate_attestation ctxt)
          Validate_errors.Consensus.(Aggregate_disabled)
      in
      record_attestations_aggregate ctxt mode consensus_content committee
  | Single (Seed_nonce_revelation {level; nonce}) ->
      let level = Level.from_raw ctxt level in
      let* ctxt = Nonce.reveal ctxt level nonce in
      let*? tip = Delegate.Rewards.seed_nonce_revelation_tip ctxt in
      let delegate = payload_producer.Consensus_key.delegate in
      let+ ctxt, balance_updates =
        Delegate.Shared_stake.pay_rewards
          ctxt
          ~source:`Revelation_rewards
          ~delegate
          tip
      in
      (ctxt, Single_result (Seed_nonce_revelation_result balance_updates))
  | Single (Vdf_revelation {solution}) ->
      let* ctxt = Seed.update_seed ctxt solution in
      let*? tip = Delegate.Rewards.vdf_revelation_tip ctxt in
      let delegate = payload_producer.Consensus_key.delegate in
      let+ ctxt, balance_updates =
        Delegate.Shared_stake.pay_rewards
          ctxt
          ~source:`Revelation_rewards
          ~delegate
          tip
      in
      (ctxt, Single_result (Vdf_revelation_result balance_updates))
  | Single (Double_consensus_operation_evidence _ as contents) ->
      punish_double_consensus_operation
        ctxt
        ~operation_hash
        ~payload_producer
        contents
  | Single (Double_baking_evidence {bh1; bh2 = _}) ->
      punish_double_baking ctxt ~operation_hash bh1 ~payload_producer
  | Single
      (Dal_entrapment_evidence {attestation; consensus_slot; slot_index; _}) ->
      let (Single
             (* Note that since the evidence has been successfully
               validated, [attestation] cannot be a preattestation or
               preattestations aggregate (also [consensus_slot] is
               consistent, and [slot_index] is an attested trap, etc.) *)
             ( Preattestation {level; _}
             | Attestation {consensus_content = {level; _}; _}
             | Preattestations_aggregate {consensus_content = {level; _}; _}
             | Attestations_aggregate {consensus_content = {level; _}; _} )) =
        attestation.protocol_data.contents
      in
      let level = Level.from_raw ctxt level in
      let* ctxt, consensus_pk =
        Stake_distribution.attestation_slot_owner
          ctxt
          ~attested_level:level
          consensus_slot
      in
      let delegate = consensus_pk.delegate in
      let*! ctxt, _already_denounced =
        Dal.Delegate.add_denunciation ctxt delegate level slot_index
      in
      (* We could assert that the delegate has not been already denounced, given
         that the operation has already been validated. *)
      return
        ( ctxt,
          Single_result (Dal_entrapment_evidence_result {balance_updates = []})
        )
  | Single (Activate_account {id = pkh; activation_code}) ->
      let blinded_pkh =
        Blinded_public_key_hash.of_ed25519_pkh activation_code pkh
      in
      let sender = `Collected_commitments blinded_pkh in
      let contract = Contract.Implicit (Signature.Ed25519 pkh) in
      let* ctxt, amount = Token.balance ctxt sender in
      let* ctxt, bupds =
        Token.transfer ctxt sender (`Contract contract) amount
      in
      return (ctxt, Single_result (Activate_account_result bupds))
  | Single (Proposals _ as contents) ->
      Amendment.apply_proposals ctxt chain_id contents
  | Single (Ballot _ as contents) -> Amendment.apply_ballot ctxt contents
  | Single (Drain_delegate {delegate; destination; consensus_key = _}) ->
      let* ctxt, allocated_destination_contract, fees, drain_balance_updates =
        Delegate.drain ctxt ~delegate ~destination
      in
      let* ctxt, fees_balance_updates =
        Token.transfer
          ctxt
          (`Contract (Contract.Implicit delegate))
          (`Contract (Contract.Implicit payload_producer.Consensus_key.delegate))
          fees
      in
      let balance_updates = drain_balance_updates @ fees_balance_updates in
      return
        ( ctxt,
          Single_result
            (Drain_delegate_result
               {balance_updates; allocated_destination_contract}) )
  | Single (Failing_noop _) ->
      (* This operation always fails. It should already have been
         rejected by {!Validate.validate_operation}. *)
      tzfail Validate_errors.Failing_noop_error
  | Single (Manager_operation {source; _}) ->
      apply_manager_operations
        ctxt
        ~payload_producer
        chain_id
        ~mempool_mode
        ~source
        ~operation
        contents_list
  | Cons (Manager_operation {source; _}, _) ->
      apply_manager_operations
        ctxt
        ~payload_producer
        chain_id
        ~mempool_mode
        ~source
        ~operation
        contents_list

let apply_operation application_state operation_hash operation =
  let open Lwt_result_syntax in
  let apply_operation application_state packed_operation ~payload_producer =
    let {shell; protocol_data = Operation_data unpacked_protocol_data} =
      packed_operation
    in
    let operation : _ Operation.t =
      {shell; protocol_data = unpacked_protocol_data}
    in
    let ctxt = Origination_nonce.init application_state.ctxt operation_hash in
    let ctxt = record_operation ctxt operation_hash operation in
    let* ctxt, result =
      apply_contents_list
        ctxt
        application_state.chain_id
        application_state.mode
        ~payload_producer
        ~operation
        ~operation_hash
        operation.protocol_data.contents
    in
    let ctxt = Gas.set_unlimited ctxt in
    let ctxt = Origination_nonce.unset ctxt in
    let op_count = succ application_state.op_count in
    return
      ( {application_state with ctxt; op_count},
        Operation_metadata {contents = result} )
  in
  match application_state.mode with
  | Application {payload_producer; _} ->
      apply_operation application_state operation ~payload_producer
  | Full_construction {payload_producer; _} ->
      apply_operation application_state operation ~payload_producer
  | Partial_construction _ ->
      apply_operation
        application_state
        operation
        ~payload_producer:Consensus_key.zero

let may_start_new_cycle ctxt =
  let open Lwt_result_syntax in
  match Level.dawn_of_a_new_cycle ctxt with
  | None -> return (ctxt, [], [])
  | Some last_cycle ->
      let* ctxt, balance_updates, deactivated =
        Delegate.cycle_end ctxt last_cycle
      in
      let+ ctxt = Bootstrap.cycle_end ctxt last_cycle in
      (ctxt, balance_updates, deactivated)

let apply_liquidity_baking_subsidy ctxt ~per_block_vote =
  let open Lwt_result_syntax in
  Liquidity_baking.on_subsidy_allowed
    ctxt
    ~per_block_vote
    (fun ctxt liquidity_baking_cpmm_contract_hash ->
      let liquidity_baking_cpmm_contract =
        Contract.Originated liquidity_baking_cpmm_contract_hash
      in
      let ctxt =
        (* We set a gas limit of 1/20th the block limit, which is ~10x
           actual usage here in Granada. Gas consumed is reported in
           the Transaction receipt, but not counted towards the block
           limit. The gas limit is reset to unlimited at the end of
           this function.*)
        Gas.set_limit
          ctxt
          (Gas.Arith.integral_exn
             (Z.div
                (Gas.Arith.integral_to_z
                   (Constants.hard_gas_limit_per_block ctxt))
                (Z.of_int 20)))
      in
      let backtracking_ctxt = ctxt in
      let*! result =
        let*? liquidity_baking_subsidy =
          Delegate.Rewards.liquidity_baking_subsidy ctxt
        in
        (* credit liquidity baking subsidy to CPMM contract *)
        let* ctxt, balance_updates =
          Token.transfer
            ~origin:Subsidy
            ctxt
            `Liquidity_baking_subsidies
            (`Contract liquidity_baking_cpmm_contract)
            liquidity_baking_subsidy
        in
        let* ctxt, cache_key, script =
          Script_cache.find ctxt liquidity_baking_cpmm_contract_hash
        in
        match script with
        | None ->
            tzfail (Script_tc_errors.No_such_entrypoint Entrypoint.default)
        | Some (script, script_ir) -> (
            (* Token.transfer which is being called above already loads this
               value into the Irmin cache, so no need to burn gas for it. *)
            let* balance =
              Contract.get_balance ctxt liquidity_baking_cpmm_contract
            in
            let now = Script_timestamp.now ctxt in
            let level =
              (Level.current ctxt).level |> Raw_level.to_int32
              |> Script_int.of_int32 |> Script_int.abs
            in
            let step_constants =
              let open Script_interpreter in
              (* Using dummy values for source, payer, and chain_id
                 since they are not used within the CPMM default
                 entrypoint. *)
              {
                sender = Destination.Contract liquidity_baking_cpmm_contract;
                payer = Signature.Public_key_hash.zero;
                self = liquidity_baking_cpmm_contract_hash;
                amount = liquidity_baking_subsidy;
                balance;
                chain_id = Chain_id.zero;
                now;
                level;
              }
            in
            (*
                 Call CPPM default entrypoint with parameter Unit.
                 This is necessary for the CPMM's xtz_pool in storage to
                 increase since it cannot use BALANCE due to a transfer attack.

                 Mimicks a transaction.

                 There is no:
                 - storage burn (extra storage is free)
                 - fees (the operation is mandatory)
          *)
            let* ( {
                     script = updated_cached_script;
                     code_size = updated_size;
                     storage;
                     lazy_storage_diff;
                     operations;
                     ticket_diffs;
                     ticket_receipt;
                     address_registry_diff;
                   },
                   ctxt ) =
              Script_interpreter.execute_with_typed_parameter
                ctxt
                Optimized
                step_constants
                ~script
                ~parameter:()
                ~parameter_ty:Unit_t
                ~cached_script:(Some script_ir)
                ~location:Micheline.dummy_location
                ~entrypoint:Entrypoint.default
                ~internal:false
            in
            match operations with
            | _ :: _ ->
                (* No internal operations are expected here. Something bad may be happening. *)
                return (backtracking_ctxt, [])
            | [] ->
                (* update CPMM storage *)
                let* ticket_table_size_diff, ctxt =
                  update_script_storage_and_ticket_balances
                    ctxt
                    ~self_contract:liquidity_baking_cpmm_contract_hash
                    storage
                    lazy_storage_diff
                    ticket_diffs
                    operations
                in
                let* ctxt, new_size, paid_storage_size_diff =
                  Fees.record_paid_storage_space
                    ctxt
                    liquidity_baking_cpmm_contract_hash
                in
                let* ticket_paid_storage_diff, ctxt =
                  Ticket_balance.adjust_storage_space
                    ctxt
                    ~storage_diff:ticket_table_size_diff
                in
                let consumed_gas =
                  Gas.consumed ~since:backtracking_ctxt ~until:ctxt
                in
                let updated_script =
                  match script with
                  | Script.Script script ->
                      Script.Script
                        {script with storage = Script.lazy_expr storage}
                  | Script.Native native ->
                      Native {native with storage = Script.lazy_expr storage}
                in
                let*? ctxt =
                  Script_cache.update
                    ctxt
                    cache_key
                    (updated_script, updated_cached_script)
                    updated_size
                in
                let result =
                  Transaction_result
                    (Transaction_to_contract_result
                       {
                         storage = Some storage;
                         lazy_storage_diff;
                         balance_updates;
                         ticket_receipt;
                         (* At this point in application the
                            origination nonce has not been initialized
                            so it's not possible to originate new
                            contracts. We've checked above that none
                            were originated. *)
                         originated_contracts = [];
                         consumed_gas;
                         storage_size = new_size;
                         paid_storage_size_diff =
                           Z.add paid_storage_size_diff ticket_paid_storage_diff;
                         allocated_destination_contract = false;
                         address_registry_diff;
                       })
                in
                let ctxt = Gas.set_unlimited ctxt in
                return (ctxt, [Successful_manager_result result]))
      in
      return
        (match result with
        | Ok (ctxt, results) -> (ctxt, results)
        | Error _ ->
            (* Do not fail if something bad happens during CPMM contract call. *)
            let ctxt = Gas.set_unlimited backtracking_ctxt in
            (ctxt, [])))

let are_attestations_required ctxt ~level =
  let open Lwt_result_syntax in
  let+ first_level = Protocol_activation_level.get ctxt in
  (* NB: the first level is the level of the migration block. There
     are no attestations for this block. Therefore the block at the
     next level cannot contain attestations. *)
  let level_position_in_protocol = Raw_level.diff level first_level in
  Compare.Int32.(level_position_in_protocol > 1l)

(* It also records participation in the DAL. *)
let record_attesting_participation ctxt dal_slot_availability =
  let open Lwt_result_syntax in
  match Consensus.allowed_attestations ctxt with
  | None -> tzfail (Consensus.Slot_map_not_found {loc = __LOC__})
  | Some validators ->
      Slot.Map.fold_es
        (fun initial_slot
             ({consensus_key; attesting_power; dal_power} : Consensus_key.power)
             ctxt
           ->
          let participation =
            if Slot.Set.mem initial_slot (Consensus.attestations_seen ctxt) then
              Delegate.Participated
            else Delegate.Didn't_participate
          in
          let* ctxt =
            Delegate.record_attesting_participation
              ctxt
              ~delegate:consensus_key.delegate
              ~participation
              ~attesting_slots:(Attesting_power.get_slots attesting_power)
          in
          Dal_apply.record_participation
            ctxt
            consensus_key.delegate
            initial_slot
            ~dal_power
            dal_slot_availability)
        validators
        ctxt

let begin_application ctxt chain_id ~migration_balance_updates
    ~migration_operation_results ~(predecessor_fitness : Fitness.raw)
    (block_header : Block_header.t) : application_state tzresult Lwt.t =
  let open Lwt_result_syntax in
  let*? fitness = Fitness.from_raw block_header.shell.fitness in
  let level = block_header.shell.level in
  let*? predecessor_round = Fitness.round_from_raw predecessor_fitness in
  let*? predecessor_level = Raw_level.of_int32 (Int32.pred level) in
  let predecessor_level = Level.from_raw ctxt predecessor_level in
  let round = Fitness.round fitness in
  let current_level = Level.current ctxt in
  let* ctxt, _slot, block_producer =
    Stake_distribution.baking_rights_owner ctxt current_level ~round
  in
  let* ctxt, _slot, payload_producer =
    Stake_distribution.baking_rights_owner
      ctxt
      current_level
      ~round:block_header.protocol_data.contents.payload_round
  in
  let per_block_vote =
    block_header.Block_header.protocol_data.contents.per_block_votes
      .liquidity_baking_vote
  in
  let* ctxt, liquidity_baking_operations_results, liquidity_baking_toggle_ema =
    apply_liquidity_baking_subsidy ctxt ~per_block_vote
  in
  let* ctxt =
    Sc_rollup.Inbox.add_level_info
      ~predecessor:block_header.shell.predecessor
      ctxt
  in
  let mode =
    Application
      {
        block_header;
        fitness;
        predecessor_round;
        predecessor_level;
        payload_producer = Consensus_key.pkh payload_producer;
        block_producer = Consensus_key.pkh block_producer;
      }
  in
  return
    {
      mode;
      chain_id;
      ctxt;
      op_count = 0;
      migration_balance_updates;
      liquidity_baking_toggle_ema;
      implicit_operations_results =
        Apply_results.pack_migration_operation_results
          migration_operation_results
        @ liquidity_baking_operations_results;
    }

let begin_full_construction ctxt chain_id ~migration_balance_updates
    ~migration_operation_results ~predecessor_timestamp ~predecessor_level
    ~predecessor_round ~predecessor_hash ~timestamp
    (block_data_contents : Block_header.contents) =
  let open Lwt_result_syntax in
  let round_durations = Constants.round_durations ctxt in
  let*? round =
    Round.round_of_timestamp
      round_durations
      ~predecessor_timestamp
      ~predecessor_round
      ~timestamp
  in
  (* The attestation/preattestation validation rules for construction are the
     same as for application. *)
  let current_level = Level.current ctxt in
  let* ctxt, _slot, block_producer =
    Stake_distribution.baking_rights_owner ctxt current_level ~round
  in
  let* ctxt, _slot, payload_producer =
    Stake_distribution.baking_rights_owner
      ctxt
      current_level
      ~round:block_data_contents.payload_round
  in
  let per_block_vote =
    block_data_contents.per_block_votes.liquidity_baking_vote
  in
  let* ctxt, liquidity_baking_operations_results, liquidity_baking_toggle_ema =
    apply_liquidity_baking_subsidy ctxt ~per_block_vote
  in
  let* ctxt =
    Sc_rollup.Inbox.add_level_info ~predecessor:predecessor_hash ctxt
  in
  let mode =
    Full_construction
      {
        block_data_contents;
        predecessor_hash;
        payload_producer = Consensus_key.pkh payload_producer;
        block_producer = Consensus_key.pkh block_producer;
        round;
        predecessor_round;
        predecessor_level;
      }
  in
  return
    {
      mode;
      chain_id;
      ctxt;
      op_count = 0;
      migration_balance_updates;
      liquidity_baking_toggle_ema;
      implicit_operations_results =
        Apply_results.pack_migration_operation_results
          migration_operation_results
        @ liquidity_baking_operations_results;
    }

let begin_partial_construction ctxt chain_id ~migration_balance_updates
    ~migration_operation_results ~predecessor_hash
    ~(predecessor_fitness : Fitness.raw) : application_state tzresult Lwt.t =
  let open Lwt_result_syntax in
  let per_block_vote = Per_block_votes.Per_block_vote_pass in
  let* ctxt, liquidity_baking_operations_results, liquidity_baking_toggle_ema =
    apply_liquidity_baking_subsidy ctxt ~per_block_vote
  in
  let* ctxt =
    (* The mode [Partial_construction] is used in simulation. We try to
       put a realistic value of the block's timestamp. Even though, it should
       not have an impact on the simulation of the following smart rollup
       operations.
    *)
    let predecessor = predecessor_hash in
    Sc_rollup.Inbox.add_level_info ~predecessor ctxt
  in
  let mode = Partial_construction {predecessor_fitness} in
  return
    {
      mode;
      chain_id;
      ctxt;
      op_count = 0;
      migration_balance_updates;
      liquidity_baking_toggle_ema;
      implicit_operations_results =
        Apply_results.pack_migration_operation_results
          migration_operation_results
        @ liquidity_baking_operations_results;
    }

let finalize_application ctxt block_data_contents ~round ~predecessor_hash
    ~liquidity_baking_toggle_ema ~implicit_operations_results
    ~migration_balance_updates ~(block_producer : Consensus_key.t)
    ~(payload_producer : Consensus_key.t) =
  let open Lwt_result_syntax in
  (* Compute consumed gas earlier, in case maintenance actions performed below
     are carbonated. *)
  let consumed_gas =
    Gas.Arith.sub
      (Gas.Arith.fp @@ Constants.hard_gas_limit_per_block ctxt)
      (Gas.block_level ctxt)
  in
  let current_level = Level.current ctxt in
  let attesting_power = Consensus.current_attesting_power ctxt in
  let block_payload_hash =
    Block_payload.hash
      ~predecessor_hash
      ~payload_round:block_data_contents.Block_header.payload_round
      (non_consensus_operations ctxt)
  in
  (* from this point nothing should fail *)
  (* We mark the attestation branch as the grand parent branch when
     accessible. This will not be present before the first two blocks
     of tenderbake. *)
  (* We mark the current payload hash as the predecessor one => this
     will only be accessed by the successor block now. *)
  let*! ctxt =
    Consensus.store_attestation_branch
      ctxt
      (predecessor_hash, block_payload_hash)
  in
  let* ctxt = Round.update ctxt round in
  let* ctxt =
    if Round.(round = zero) then Consecutive_round_zero.incr ctxt
    else Consecutive_round_zero.reset ctxt
  in
  (* end of level *)
  let* ctxt =
    match block_data_contents.Block_header.seed_nonce_hash with
    | None -> return ctxt
    | Some nonce_hash ->
        Nonce.record_hash ctxt {nonce_hash; delegate = block_producer.delegate}
  in
  let* ctxt, dal_slot_availability = Dal_apply.finalisation ctxt in
  let* ctxt, reward_bonus, attestation_result =
    let* required_attestations =
      are_attestations_required ctxt ~level:current_level.level
    in
    if required_attestations then
      let* ctxt = record_attesting_participation ctxt dal_slot_availability in
      (* The attested level is the predecessor of the block's level. *)
      match Level.pred ctxt current_level with
      | None ->
          (* This cannot happen because [required_attestations = true]
             ensures that [current_level >= 2]. *)
          assert false
      | Some attested_level ->
          let* ctxt, rewards_bonus =
            Baking.bonus_baking_reward ctxt ~attested_level ~attesting_power
          in
          let* ctxt, consensus_committee =
            Attesting_power.consensus_committee ctxt ~attested_level
          in
          let* ctxt, consensus_threshold =
            Attesting_power.consensus_threshold ctxt ~attested_level
          in
          let consensus_recorded_power =
            Attesting_power.get ctxt ~attested_level attesting_power
          in
          return
            ( ctxt,
              Some rewards_bonus,
              Some
                {
                  consensus_committee;
                  consensus_threshold;
                  consensus_recorded_power;
                } )
    else return (ctxt, None, None)
  in
  let* ctxt, preattestation_result =
    let lre = Consensus.locked_round_evidence ctxt in
    match lre with
    | None -> return (ctxt, None)
    | Some (_round, preattesting_power) ->
        let attested_level = current_level in
        let* ctxt, consensus_committee =
          Attesting_power.consensus_committee ctxt ~attested_level
        in
        let* ctxt, consensus_threshold =
          Attesting_power.consensus_threshold ctxt ~attested_level
        in
        let consensus_recorded_power =
          Attesting_power.get ctxt ~attested_level preattesting_power
        in
        return
          ( ctxt,
            Some
              {
                consensus_committee;
                consensus_threshold;
                consensus_recorded_power;
              } )
  in
  let*? baking_reward = Delegate.Rewards.baking_reward_fixed_portion ctxt in
  let* ctxt, baking_receipts =
    Delegate.record_baking_activity_and_pay_rewards_and_fees
      ctxt
      ~payload_producer:payload_producer.delegate
      ~block_producer:block_producer.delegate
      ~baking_reward
      ~reward_bonus
  in
  (* if end of nonce revelation period, compute seed *)
  let* ctxt =
    if Level.may_compute_randao ctxt then Seed.compute_randao ctxt
    else return ctxt
  in
  let* ctxt, cycle_end_balance_updates, deactivated =
    may_start_new_cycle ctxt
  in
  let* ctxt = Amendment.may_start_new_voting_period ctxt in
  let* ctxt = Sc_rollup.Inbox.finalize_inbox_level ctxt in
  let balance_updates =
    migration_balance_updates @ baking_receipts @ cycle_end_balance_updates
  in
  let+ voting_period_info = Voting_period.get_rpc_current_info ctxt in
  let abaab_activation_level =
    Attesting_power.all_bakers_attest_activation_level ctxt
  in
  let receipt =
    Apply_results.
      {
        proposer = payload_producer;
        baker = block_producer;
        level_info = current_level;
        voting_period_info;
        nonce_hash = block_data_contents.seed_nonce_hash;
        consumed_gas;
        deactivated;
        balance_updates;
        liquidity_baking_toggle_ema;
        implicit_operations_results;
        dal_slot_availability;
        abaab_activation_level;
        attestations = attestation_result;
        preattestations = preattestation_result;
      }
  in
  (ctxt, receipt)

type error += Missing_shell_header

let () =
  register_error_kind
    `Permanent
    ~id:"apply.missing_shell_header"
    ~title:"Missing shell_header during finalisation of a block"
    ~description:
      "During finalisation of a block header in Application mode or Full \
       construction mode, a shell header should be provided so that a cache \
       nonce can be computed."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "No shell header provided during the finalisation of a block.")
    Data_encoding.unit
    (function Missing_shell_header -> Some () | _ -> None)
    (fun () -> Missing_shell_header)

let finalize_with_commit_message ctxt ~cache_nonce fitness round op_count =
  let open Lwt_syntax in
  let* ctxt = Cache.Admin.sync ctxt cache_nonce in
  let raw_level = Raw_level.to_int32 (Level.current ctxt).level in
  let commit_message =
    Format.asprintf
      "lvl %ld, fit:%a, round %a, %d ops"
      raw_level
      Fitness.pp
      fitness
      Round.pp
      round
      op_count
  in
  let validation_result =
    finalize ~commit_message ctxt (Fitness.to_raw fitness)
  in
  return validation_result

let finalize_block (application_state : application_state) shell_header_opt =
  let open Lwt_result_syntax in
  let {
    ctxt;
    liquidity_baking_toggle_ema;
    implicit_operations_results;
    migration_balance_updates;
    op_count;
    _;
  } =
    application_state
  in
  match application_state.mode with
  | Full_construction
      {
        block_data_contents;
        predecessor_hash;
        predecessor_level = _;
        predecessor_round;
        block_producer;
        payload_producer;
        round;
      } ->
      let*? (shell_header : Block_header.shell_header) =
        Option.value_e
          shell_header_opt
          ~error:(Error_monad.trace_of_error Missing_shell_header)
      in
      let cache_nonce =
        Cache.cache_nonce_from_block_header shell_header block_data_contents
      in
      let locked_round =
        Option.map fst (Consensus.locked_round_evidence ctxt)
      in
      let level = (Level.current ctxt).level in
      let*? fitness =
        Fitness.create ~level ~round ~predecessor_round ~locked_round
      in
      let* ctxt, receipt =
        finalize_application
          ctxt
          block_data_contents
          ~round
          ~predecessor_hash
          ~liquidity_baking_toggle_ema
          ~implicit_operations_results
          ~migration_balance_updates
          ~block_producer
          ~payload_producer
      in
      let*! result =
        finalize_with_commit_message ctxt ~cache_nonce fitness round op_count
      in
      return (result, receipt)
  | Partial_construction {predecessor_fitness; _} ->
      (* Fake finalization to return a correct type, because there is no
         block to finalize in mempool mode. If this changes in the
         future, beware that consensus operations are not recorded by
         {!record_preattestation} and {!record_attestation} in this mode. *)
      let* voting_period_info = Voting_period.get_rpc_current_info ctxt in
      let level_info = Level.current ctxt in
      let result = finalize ctxt predecessor_fitness in
      return
        ( result,
          Apply_results.
            {
              proposer = Consensus_key.zero;
              baker = Consensus_key.zero;
              level_info;
              voting_period_info;
              nonce_hash = None;
              consumed_gas = Gas.Arith.zero;
              deactivated = [];
              balance_updates = migration_balance_updates;
              liquidity_baking_toggle_ema;
              implicit_operations_results;
              dal_slot_availability = Dal.Slot_availability.empty;
              abaab_activation_level = None;
              attestations = None;
              preattestations = None;
            } )
  | Application
      {
        fitness;
        block_header = {shell; protocol_data};
        payload_producer;
        block_producer;
        _;
      } ->
      let round = Fitness.round fitness in
      let cache_nonce =
        Cache.cache_nonce_from_block_header shell protocol_data.contents
      in
      let* ctxt, receipt =
        finalize_application
          ctxt
          protocol_data.contents
          ~round
          ~predecessor_hash:shell.predecessor
          ~liquidity_baking_toggle_ema
          ~implicit_operations_results
          ~migration_balance_updates
          ~block_producer
          ~payload_producer
      in
      let*! result =
        finalize_with_commit_message ctxt ~cache_nonce fitness round op_count
      in
      return (result, receipt)

let value_of_key ctxt k = Cache.Admin.value_of_key ctxt k

module Internal_for_benchmark = struct
  let take_fees ctxt batch = ignore (take_fees ctxt batch)
end
