(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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
  | Not_enough_endorsements of {required : int; provided : int}
  | Faulty_validation_wrong_slot
  | Set_deposits_limit_on_unregistered_delegate of Signature.Public_key_hash.t
  | Error_while_taking_fees
  | Update_consensus_key_on_unregistered_delegate of Signature.Public_key_hash.t
  | Empty_transaction of Contract.t
  | Non_empty_transaction_from of Destination.t
  | Sc_rollup_feature_disabled
  | Internal_operation_replay of
      Apply_internal_results.packed_internal_operation
  | Multiple_revelation
  | Invalid_transfer_to_sc_rollup
  | Invalid_sender of Destination.t
  | Invalid_staking_destination of public_key_hash

let () =
  register_error_kind
    `Permanent
    ~id:"operation.not_enough_endorsements"
    ~title:"Not enough endorsements"
    ~description:
      "The block being validated does not include the required minimum number \
       of endorsements."
    ~pp:(fun ppf (required, provided) ->
      Format.fprintf
        ppf
        "Wrong number of endorsements (%i), at least %i are expected"
        provided
        required)
    Data_encoding.(obj2 (req "required" int31) (req "provided" int31))
    (function
      | Not_enough_endorsements {required; provided} -> Some (required, provided)
      | _ -> None)
    (fun (required, provided) -> Not_enough_endorsements {required; provided}) ;
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
    ~description:"Cannot update consensus key an unregistered delegate."
    ~pp:(fun ppf c ->
      Format.fprintf
        ppf
        "Cannot update the consensus key on the unregistered delegate %a."
        Signature.Public_key_hash.pp
        c)
    Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
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

  let description = "Smart rollups are disabled." in
  register_error_kind
    `Permanent
    ~id:"operation.smart_rollup_disabled"
    ~title:"Smart rollups are disabled"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Sc_rollup_feature_disabled -> Some () | _ -> None)
    (fun () -> Sc_rollup_feature_disabled) ;

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
  register_error_kind
    `Permanent
    ~id:"operations.invalid_staking_destination"
    ~title:"Invalid destination for a stake operation"
    ~description:
      "The transaction destination must be a registered delegate. Additionally \
       it must equal the operation source for now."
    ~pp:(fun ppf pkh ->
      Format.fprintf
        ppf
        "Invalid destination (%a) for this stake operation. Only registered \
         delegate are allowed."
        Signature.Public_key_hash.pp
        pkh)
    Data_encoding.(obj1 (req "destination" Contract.implicit_encoding))
    (function Invalid_staking_destination pkh -> Some pkh | _ -> None)
    (fun pkh -> Invalid_staking_destination pkh)

open Apply_results
open Apply_operation_result
open Apply_internal_results

let assert_sc_rollup_feature_enabled ctxt =
  error_unless (Constants.sc_rollup_enable ctxt) Sc_rollup_feature_disabled

let update_script_storage_and_ticket_balances ctxt ~self_contract storage
    lazy_storage_diff ticket_diffs operations =
  Contract.update_script_storage ctxt self_contract storage lazy_storage_diff
  >>=? fun ctxt ->
  let self_contract = Contract.Originated self_contract in
  Ticket_accounting.update_ticket_balances
    ctxt
    ~self_contract
    ~ticket_diffs
    operations

let apply_delegation ~ctxt ~sender ~delegate ~before_operation =
  Contract.Delegate.set ctxt sender delegate >|=? fun ctxt ->
  (ctxt, Gas.consumed ~since:before_operation ~until:ctxt, [])

type 'loc execution_arg =
  | Typed_arg : 'loc * ('a, _) Script_typed_ir.ty * 'a -> 'loc execution_arg
  | Untyped_arg : Script.expr -> _ execution_arg

let apply_transaction_to_implicit ~ctxt ~sender ~amount ~pkh ~before_operation =
  let contract = Contract.Implicit pkh in
  (* Transfers of zero to implicit accounts are forbidden. *)
  error_when Tez.(amount = zero) (Empty_transaction contract) >>?= fun () ->
  (* If the implicit contract is not yet allocated at this point then
     the next transfer of tokens will allocate it. *)
  Contract.allocated ctxt contract >>= fun already_allocated ->
  Token.transfer ctxt (`Contract sender) (`Contract contract) amount
  >>=? fun (ctxt, balance_updates) ->
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
      }
  in
  return (ctxt, result, [])

let apply_stake ~ctxt ~sender ~amount ~delegate ~before_operation =
  let contract = Contract.Implicit delegate in
  (* Staking of zero is forbidden. *)
  error_when Tez.(amount = zero) (Empty_transaction contract) >>?= fun () ->
  error_unless
    Signature.Public_key_hash.(sender = delegate)
    (Invalid_staking_destination delegate)
  >>?= fun () ->
  Contract.is_delegate ctxt delegate >>=? fun is_delegate ->
  error_unless is_delegate (Invalid_staking_destination delegate) >>?= fun () ->
  Token.transfer
    ctxt
    (`Contract (Contract.Implicit sender))
    (`Frozen_deposits delegate)
    amount
  >>=? fun (ctxt, balance_updates) ->
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
      }
  in
  return (ctxt, result, [])

let transfer_from_any_address ctxt sender destination amount =
  match sender with
  | Destination.Contract sender ->
      Token.transfer ctxt (`Contract sender) (`Contract destination) amount
  | Destination.Sc_rollup _ | Destination.Zk_rollup _ ->
      (* We do not allow transferring tez from rollups to other contracts. *)
      error_unless Tez.(amount = zero) (Non_empty_transaction_from sender)
      >>?= fun () -> return (ctxt, [])

let apply_transaction_to_implicit_with_ticket ~sender ~destination ~ty ~ticket
    ~amount ~before_operation ctxt =
  let destination = Contract.Implicit destination in
  Contract.allocated ctxt destination >>= fun already_allocated ->
  let ex_token, ticket_amount =
    Ticket_scanner.ex_token_and_amount_of_ex_ticket
    @@ Ticket_scanner.Ex_ticket (ty, ticket)
  in
  Ticket_token_unparser.unparse ctxt ex_token >>=? fun (ticket_token, ctxt) ->
  transfer_from_any_address ctxt sender destination amount
  >>=? fun (ctxt, balance_updates) ->
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
        },
      [] )

let apply_transaction_to_smart_contract ~ctxt ~sender ~contract_hash ~amount
    ~entrypoint ~before_operation ~payer ~chain_id ~internal ~parameter =
  let contract = Contract.Originated contract_hash in
  (* Since the contract is originated, nothing will be allocated or this
     transfer of tokens will fail.  [Token.transfer] will succeed even on
     non-existing contracts, if the amount is zero.  Then if the destination
     does not exist, [Script_cache.find] will signal that by returning [None]
     and we'll fail.
  *)
  transfer_from_any_address ctxt sender contract amount
  >>=? fun (ctxt, balance_updates) ->
  Script_cache.find ctxt contract_hash >>=? fun (ctxt, cache_key, script) ->
  match script with
  | None -> tzfail (Contract.Non_existing_contract contract)
  | Some (script, script_ir) ->
      (* Token.transfer which is being called before already loads this value into
         the Irmin cache, so no need to burn gas for it. *)
      Contract.get_balance ctxt contract >>=? fun balance ->
      let now = Script_timestamp.now ctxt in
      let level =
        (Level.current ctxt).level |> Raw_level.to_int32 |> Script_int.of_int32
        |> Script_int.abs
      in
      let step_constants =
        let open Script_interpreter in
        {
          sender;
          payer;
          self = contract_hash;
          amount;
          chain_id;
          balance;
          now;
          level;
        }
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
      execute
        ctxt
        ~cached_script
        Optimized
        step_constants
        ~script
        ~entrypoint
        ~internal
      >>=? fun ( {
                   script = updated_cached_script;
                   code_size = updated_size;
                   storage;
                   lazy_storage_diff;
                   operations;
                   ticket_diffs;
                   ticket_receipt;
                 },
                 ctxt ) ->
      update_script_storage_and_ticket_balances
        ctxt
        ~self_contract:contract_hash
        storage
        lazy_storage_diff
        ticket_diffs
        operations
      >>=? fun (ticket_table_size_diff, ctxt) ->
      Ticket_balance.adjust_storage_space
        ctxt
        ~storage_diff:ticket_table_size_diff
      >>=? fun (ticket_paid_storage_diff, ctxt) ->
      Fees.record_paid_storage_space ctxt contract_hash
      >>=? fun (ctxt, new_size, contract_paid_storage_size_diff) ->
      Contract.originated_from_current_nonce ~since:before_operation ~until:ctxt
      >>=? fun originated_contracts ->
      Lwt.return
        ( Script_cache.update
            ctxt
            cache_key
            ( {script with storage = Script.lazy_expr storage},
              updated_cached_script )
            updated_size
        >|? fun ctxt ->
          let result =
            Transaction_to_contract_result
              {
                storage = Some storage;
                lazy_storage_diff;
                balance_updates;
                ticket_receipt;
                originated_contracts;
                consumed_gas = Gas.consumed ~since:before_operation ~until:ctxt;
                storage_size = new_size;
                paid_storage_size_diff =
                  Z.add contract_paid_storage_size_diff ticket_paid_storage_diff;
                allocated_destination_contract = false;
              }
          in
          (ctxt, result, operations) )

let apply_origination ~ctxt ~storage_type ~storage ~unparsed_code
    ~contract:contract_hash ~delegate ~sender ~credit ~before_operation =
  Script_ir_translator.collect_lazy_storage ctxt storage_type storage
  >>?= fun (to_duplicate, ctxt) ->
  let to_update = Script_ir_translator.no_lazy_storage_id in
  Script_ir_translator.extract_lazy_storage_diff
    ctxt
    Optimized
    storage_type
    storage
    ~to_duplicate
    ~to_update
    ~temporary:false
  >>=? fun (storage, lazy_storage_diff, ctxt) ->
  Script_ir_translator.unparse_data ctxt Optimized storage_type storage
  >>=? fun (storage, ctxt) ->
  let storage = Script.lazy_expr storage in
  (* Normalize code to avoid #843 *)
  Script_ir_translator.unparse_code
    ctxt
    Optimized
    (Micheline.root unparsed_code)
  >>=? fun (code, ctxt) ->
  let code = Script.lazy_expr code in
  let script = {Script.code; storage} in
  Contract.raw_originate
    ctxt
    ~prepaid_bootstrap_storage:false
    contract_hash
    ~script:(script, lazy_storage_diff)
  >>=? fun ctxt ->
  let contract = Contract.Originated contract_hash in
  (match delegate with
  | None -> return ctxt
  | Some delegate -> Contract.Delegate.init ctxt contract delegate)
  >>=? fun ctxt ->
  Token.transfer ctxt (`Contract sender) (`Contract contract) credit
  >>=? fun (ctxt, balance_updates) ->
  Fees.record_paid_storage_space ctxt contract_hash
  >|=? fun (ctxt, size, paid_storage_size_diff) ->
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

let assert_sender_is_contract = function
  | Destination.Contract sender -> ok sender
  | sender -> error (Invalid_sender sender)

let apply_internal_operation_contents :
    type kind.
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
 fun ctxt_before_op ~payer ~sender ~chain_id operation ->
  Destination.must_exist ctxt_before_op sender >>=? fun ctxt ->
  (* There is no signature being checked for internal operations so in
     this case the fixed cost is exactly
     [Michelson_v1_gas.Cost_of.manager_operation]. *)
  Gas.consume ctxt Michelson_v1_gas.Cost_of.manager_operation >>?= fun ctxt ->
  (* Note that [ctxt_before_op] will be used again later to compute
     gas consumption and originations for the operation result (by
     comparing it with the [ctxt] we will have at the end of the
     application). *)
  match operation with
  | Transaction_to_implicit {destination = pkh; amount} ->
      assert_sender_is_contract sender >>?= fun sender ->
      apply_transaction_to_implicit
        ~ctxt
        ~sender
        ~amount
        ~pkh
        ~before_operation:ctxt_before_op
      >|=? fun (ctxt, res, ops) ->
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
      apply_transaction_to_implicit_with_ticket
        ~sender
        ~destination
        ~ty
        ~ticket
        ~amount
        ~before_operation:ctxt_before_op
        ctxt
      >|=? fun (ctxt, res, ops) ->
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
      >|=? fun (ctxt, res, ops) -> (ctxt, ITransaction_result res, ops)
  | Transaction_to_sc_rollup
      {
        destination;
        entrypoint = _;
        parameters_ty;
        parameters;
        unparsed_parameters = payload;
      } ->
      assert_sc_rollup_feature_enabled ctxt >>?= fun () ->
      (match sender with
      | Destination.Contract (Originated hash) -> ok hash
      | _ -> error Invalid_transfer_to_sc_rollup)
      >>?= fun sender ->
      (* Adding the message to the inbox. Note that it is safe to ignore the
         size diff since only its hash and meta data are stored in the context.
         See #3232. *)
      Sc_rollup.Inbox.add_deposit
        ctxt
        ~destination
        ~payload
        ~sender
        ~source:payer
      >>=? fun ctxt ->
      Ticket_scanner.type_has_tickets ctxt parameters_ty
      >>?= fun (has_tickets, ctxt) ->
      Ticket_accounting.ticket_balances_of_value
        ctxt
        ~include_lazy:true
        has_tickets
        parameters
      >>=? fun (ticket_token_map, ctxt) ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4354
         Factor out function for constructing a ticket receipt.
         There are multiple places where we compute the receipt from a
         ticket-token-map. We should factor out and reuse this logic. *)
      Ticket_token_map.fold_es
        ctxt
        (fun ctxt acc ex_token amount ->
          Ticket_token_unparser.unparse ctxt ex_token
          >>=? fun (ticket_token, ctxt) ->
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
      >|=? fun (ticket_receipt, ctxt) ->
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
      assert_sender_is_contract sender >>?= fun sender ->
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
      >|=? fun (ctxt, origination_result, ops) ->
      (ctxt, IOrigination_result origination_result, ops)
  | Delegation delegate ->
      assert_sender_is_contract sender >>?= fun sender ->
      apply_delegation ~ctxt ~sender ~delegate ~before_operation:ctxt_before_op
      >|=? fun (ctxt, consumed_gas, ops) ->
      (ctxt, IDelegation_result {consumed_gas}, ops)

let apply_manager_operation :
    type kind.
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
  Gas.consume ctxt_before_op fixed_gas_cost >>?= fun ctxt ->
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
  | Reveal pk ->
      (* TODO #2603

         Even if [precheck_manager_contents] has already asserted that
         the implicit contract is allocated, we must re-do this check in
         case the manager has been emptied while collecting fees. This
         should be solved by forking out [validate_operation] from
         [apply_operation]. *)
      Contract.must_be_allocated ctxt source_contract >>=? fun () ->
      (* TODO tezos/tezos#3070

         We have already asserted the consistency of the supplied public
         key during precheck, so we avoid re-checking that precondition
         with [?check_consistency=false]. This optional parameter is
         temporary, to avoid breaking compatibility with external legacy
         usage of [Contract.reveal_manager_key]. However, the pattern of
         using [Contract.check_public_key] and this usage of
         [Contract.reveal_manager_key] should become the standard. *)
      Contract.reveal_manager_key ~check_consistency:false ctxt source pk
      >>=? fun ctxt ->
      return
        ( ctxt,
          (Reveal_result
             {consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt}
            : kind successful_manager_operation_result),
          [] )
  | Transaction {amount; parameters; destination = Implicit pkh; entrypoint} ->
      Script.force_decode_in_context
        ~consume_deserialization_gas
        ctxt
        parameters
      >>?= fun (parameters, ctxt) ->
      (match (Entrypoint.to_string entrypoint, Micheline.root parameters) with
      | "default", Prim (_, D_Unit, [], _) ->
          apply_transaction_to_implicit
            ~ctxt
            ~sender:source_contract
            ~amount
            ~pkh
            ~before_operation:ctxt_before_op
      | "stake", Prim (_, D_Unit, [], _) ->
          apply_stake
            ~ctxt
            ~sender:source
            ~amount
            ~delegate:pkh
            ~before_operation:ctxt_before_op
      | ("default" | "stake"), _ ->
          (* Only allow [Unit] parameter to implicit accounts' default and stake entrypoints. *)
          tzfail (Script_interpreter.Bad_contract_parameter source_contract)
      | _ -> tzfail (Script_tc_errors.No_such_entrypoint entrypoint))
      >|=? fun (ctxt, res, ops) -> (ctxt, Transaction_result res, ops)
  | Transaction
      {amount; parameters; destination = Originated contract_hash; entrypoint}
    ->
      Script.force_decode_in_context
        ~consume_deserialization_gas
        ctxt
        parameters
      >>?= fun (parameters, ctxt) ->
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
      >|=? fun (ctxt, res, ops) -> (ctxt, Transaction_result res, ops)
  | Transfer_ticket {contents; ty; ticketer; amount; destination; entrypoint}
    -> (
      match destination with
      | Implicit _ ->
          error_unless
            Entrypoint.(entrypoint = default)
            (Script_tc_errors.No_such_entrypoint entrypoint)
          >>?= fun () ->
          Ticket_transfer.parse_ticket
            ~consume_deserialization_gas
            ~ticketer
            ~contents
            ~ty
            ctxt
          >>=? fun (ctxt, ticket) ->
          Ticket_transfer.transfer_ticket
            ctxt
            ~sender:(Contract source_contract)
            ~dst:(Contract destination)
            ticket
            amount
          >>=? fun (ctxt, paid_storage_size_diff) ->
          Ticket_token_unparser.unparse ctxt ticket
          >>=? fun (ticket_token, ctxt) ->
          let amount = Script_int.(to_zint (amount :> n num)) in
          let ticket_receipt =
            Ticket_receipt.
              [
                {
                  ticket_token;
                  updates =
                    [
                      {account = Contract source_contract; amount = Z.neg amount};
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
                  consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt;
                  paid_storage_size_diff;
                },
              [] )
      | Originated destination_hash ->
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
          >>=? fun (ctxt, token, op) ->
          Ticket_transfer.transfer_ticket
            ctxt
            ~sender:(Contract source_contract)
            ~dst:(Contract destination)
            token
            amount
          >>=? fun (ctxt, paid_storage_size_diff) ->
          Ticket_token_unparser.unparse ctxt token
          >>=? fun (ticket_token, ctxt) ->
          let amount = Script_int.(to_zint (amount :> n num)) in
          let ticket_receipt =
            Ticket_receipt.
              [
                {
                  ticket_token;
                  updates =
                    [
                      {account = Contract source_contract; amount = Z.neg amount}
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
                  consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt;
                  paid_storage_size_diff;
                },
              [op] ))
  | Origination {delegate; script; credit} ->
      (* Internal originations have their address generated in the interpreter
         so that the script can use it immediately.
         The address of external originations is generated here. *)
      Contract.fresh_contract_from_current_nonce ctxt
      >>?= fun (ctxt, contract) ->
      Script.force_decode_in_context
        ~consume_deserialization_gas
        ctxt
        script.Script.storage
      >>?= fun (_unparsed_storage, ctxt) ->
      Script.force_decode_in_context
        ~consume_deserialization_gas
        ctxt
        script.Script.code
      >>?= fun (unparsed_code, ctxt) ->
      Script_ir_translator.parse_script
        ctxt
        ~elab_conf:Script_ir_translator_config.(make ~legacy:false ())
        ~allow_forged_in_storage:false
        script
      >>=? fun (Ex_script parsed_script, ctxt) ->
      let (Script {storage_type; views; storage; _}) = parsed_script in
      let views_result =
        Script_ir_translator.parse_views
          ctxt
          ~elab_conf:Script_ir_translator_config.(make ~legacy:false ())
          storage_type
          views
      in
      trace
        (Script_tc_errors.Ill_typed_contract (unparsed_code, []))
        views_result
      >>=? fun (_typed_views, ctxt) ->
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
      >|=? fun (ctxt, origination_result, ops) ->
      (ctxt, Origination_result origination_result, ops)
  | Delegation delegate ->
      apply_delegation
        ~ctxt
        ~sender:source_contract
        ~delegate
        ~before_operation:ctxt_before_op
      >|=? fun (ctxt, consumed_gas, ops) ->
      (ctxt, Delegation_result {consumed_gas}, ops)
  | Register_global_constant {value} ->
      (* Decode the value and consume gas appropriately *)
      Script.force_decode_in_context ~consume_deserialization_gas ctxt value
      >>?= fun (expr, ctxt) ->
      (* Set the key to the value in storage. *)
      Global_constants_storage.register ctxt expr
      >>=? fun (ctxt, address, size) ->
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
  | Set_deposits_limit limit ->
      Delegate.registered ctxt source >>= fun is_registered ->
      error_unless
        is_registered
        (Set_deposits_limit_on_unregistered_delegate source)
      >>?= fun () ->
      Delegate.set_frozen_deposits_limit ctxt source limit >>= fun ctxt ->
      return
        ( ctxt,
          Set_deposits_limit_result
            {consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt},
          [] )
  | Increase_paid_storage {amount_in_bytes; destination} ->
      Contract.increase_paid_storage ctxt destination ~amount_in_bytes
      >>=? fun ctxt ->
      let payer = `Contract (Contract.Implicit source) in
      Fees.burn_storage_increase_fees ctxt ~payer amount_in_bytes
      >|=? fun (ctxt, storage_bus) ->
      let result =
        Increase_paid_storage_result
          {
            balance_updates = storage_bus;
            consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt;
          }
      in
      (ctxt, result, [])
  | Update_consensus_key pk ->
      Delegate.registered ctxt source >>= fun is_registered ->
      error_unless
        is_registered
        (Update_consensus_key_on_unregistered_delegate source)
      >>?= fun () ->
      Delegate.Consensus_key.register_update ctxt source pk >>=? fun ctxt ->
      return
        ( ctxt,
          Update_consensus_key_result
            {consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt},
          [] )
  | Dal_publish_slot_header slot_header ->
      Dal_apply.apply_publish_slot_header ctxt slot_header
      >>?= fun (ctxt, slot_header) ->
      let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
      let result = Dal_publish_slot_header_result {slot_header; consumed_gas} in
      return (ctxt, result, [])
  | Sc_rollup_originate {kind; boot_sector; parameters_ty} ->
      Sc_rollup_operations.originate ctxt ~kind ~boot_sector ~parameters_ty
      >>=? fun ({address; size; genesis_commitment_hash}, ctxt) ->
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
      Sc_rollup.Inbox.add_external_messages ctxt messages >>=? fun ctxt ->
      let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
      let result = Sc_rollup_add_messages_result {consumed_gas} in
      return (ctxt, result, [])
  | Sc_rollup_cement {rollup; commitment = _commitment_hash} ->
      (* The field [commitment] is deprecated. It is not used during the
         application of [Sc_rollup_cement]. The commitment to cement is
         computed by the protocol.

         It is deprecated starting N, and can be removed in O. *)
      Sc_rollup.Stake_storage.cement_commitment ctxt rollup
      >>=? fun (ctxt, commitment, commitment_hash) ->
      let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
      let result =
        Sc_rollup_cement_result
          {consumed_gas; inbox_level = commitment.inbox_level; commitment_hash}
      in
      return (ctxt, result, [])
  | Sc_rollup_publish {rollup; commitment} ->
      Sc_rollup.Stake_storage.publish_commitment ctxt rollup source commitment
      >>=? fun (staked_hash, published_at_level, ctxt, balance_updates) ->
      let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
      let result =
        Sc_rollup_publish_result
          {staked_hash; consumed_gas; published_at_level; balance_updates}
      in
      return (ctxt, result, [])
  | Sc_rollup_refute {rollup; opponent; refutation} ->
      let open Sc_rollup.Refutation_storage in
      let player = source in
      (match refutation with
      | Start {player_commitment_hash; opponent_commitment_hash} ->
          start_game
            ctxt
            rollup
            ~player:(player, player_commitment_hash)
            ~opponent:(opponent, opponent_commitment_hash)
          >>=? fun ctxt -> return (None, ctxt)
      | Move {step; choice} ->
          game_move ctxt rollup ~player ~opponent ~step ~choice)
      >>=? fun (game_result, ctxt) ->
      (match game_result with
      | None -> return (Sc_rollup.Game.Ongoing, ctxt, [])
      | Some game_result ->
          let stakers = Sc_rollup.Game.Index.make source opponent in
          Sc_rollup.Refutation_storage.apply_game_result
            ctxt
            rollup
            stakers
            game_result)
      >>=? fun (game_status, ctxt, balance_updates) ->
      let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
      let result =
        Sc_rollup_refute_result {game_status; consumed_gas; balance_updates}
      in
      return (ctxt, result, [])
  | Sc_rollup_timeout {rollup; stakers} ->
      Sc_rollup.Refutation_storage.timeout ctxt rollup stakers
      >>=? fun (game_result, ctxt) ->
      Sc_rollup.Refutation_storage.apply_game_result
        ctxt
        rollup
        stakers
        game_result
      >>=? fun (game_status, ctxt, balance_updates) ->
      let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
      let result =
        Sc_rollup_timeout_result {game_status; consumed_gas; balance_updates}
      in
      return (ctxt, result, [])
  | Sc_rollup_execute_outbox_message {rollup; cemented_commitment; output_proof}
    ->
      Sc_rollup_operations.execute_outbox_message
        ctxt
        rollup
        ~cemented_commitment
        ~output_proof
      >|=? fun ( {
                   Sc_rollup_operations.paid_storage_size_diff;
                   ticket_receipt;
                   operations;
                 },
                 ctxt ) ->
      let consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt in
      let result =
        Sc_rollup_execute_outbox_message_result
          {
            paid_storage_size_diff;
            ticket_receipt;
            balance_updates = [];
            consumed_gas;
          }
      in
      (ctxt, result, operations)
  | Sc_rollup_recover_bond {sc_rollup; staker} ->
      Sc_rollup.Stake_storage.withdraw_stake ctxt sc_rollup staker
      >>=? fun (ctxt, balance_updates) ->
      let result =
        Sc_rollup_recover_bond_result
          {
            consumed_gas = Gas.consumed ~since:ctxt_before_op ~until:ctxt;
            balance_updates;
          }
      in
      return (ctxt, result, [])
  | Zk_rollup_origination {public_parameters; circuits_info; init_state; nb_ops}
    ->
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
  let rec apply ctxt applied worklist =
    match worklist with
    | [] -> Lwt.return (Success ctxt, List.rev applied)
    | Script_typed_ir.Internal_operation ({sender; operation; nonce} as op)
      :: rest -> (
        (if internal_nonce_already_recorded ctxt nonce then
         let op_res = Apply_internal_results.internal_operation op in
         tzfail (Internal_operation_replay (Internal_operation op_res))
        else
          let ctxt = record_internal_nonce ctxt nonce in
          apply_internal_operation_contents
            ctxt
            ~sender
            ~payer
            ~chain_id
            operation)
        >>= function
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
  match trr with
  | Transaction_to_contract_result payload ->
      let consumed = payload.paid_storage_size_diff in
      Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
      >>=? fun (ctxt, storage_limit, storage_bus) ->
      (if payload.allocated_destination_contract then
       Fees.burn_origination_fees ctxt ~storage_limit ~payer
      else return (ctxt, storage_limit, []))
      >>=? fun (ctxt, storage_limit, origination_bus) ->
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
            } )
  | Transaction_to_sc_rollup_result _ -> return (ctxt, storage_limit, trr)
  | Transaction_to_zk_rollup_result payload ->
      let consumed = payload.paid_storage_size_diff in
      Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
      >>=? fun (ctxt, storage_limit, storage_bus) ->
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
  let consumed = paid_storage_size_diff in
  Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
  >>=? fun (ctxt, storage_limit, storage_bus) ->
  Fees.burn_origination_fees ctxt ~storage_limit ~payer
  >>=? fun (ctxt, storage_limit, origination_bus) ->
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
let burn_manager_storage_fees :
    type kind.
    context ->
    kind successful_manager_operation_result ->
    storage_limit:Z.t ->
    payer:public_key_hash ->
    (context * Z.t * kind successful_manager_operation_result) tzresult Lwt.t =
 fun ctxt smopr ~storage_limit ~payer ->
  let payer = `Contract (Contract.Implicit payer) in
  match smopr with
  | Transaction_result transaction_result ->
      burn_transaction_storage_fees
        ctxt
        transaction_result
        ~storage_limit
        ~payer
      >>=? fun (ctxt, storage_limit, transaction_result) ->
      return (ctxt, storage_limit, Transaction_result transaction_result)
  | Origination_result origination_result ->
      burn_origination_storage_fees
        ctxt
        origination_result
        ~storage_limit
        ~payer
      >>=? fun (ctxt, storage_limit, origination_result) ->
      return (ctxt, storage_limit, Origination_result origination_result)
  | Reveal_result _ | Delegation_result _ -> return (ctxt, storage_limit, smopr)
  | Register_global_constant_result payload ->
      let consumed = payload.size_of_constant in
      Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
      >|=? fun (ctxt, storage_limit, storage_bus) ->
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
      Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
      >|=? fun (ctxt, storage_limit, storage_bus) ->
      let balance_updates = payload.balance_updates @ storage_bus in
      ( ctxt,
        storage_limit,
        Transfer_ticket_result {payload with balance_updates} )
  | Dal_publish_slot_header_result _ -> return (ctxt, storage_limit, smopr)
  | Sc_rollup_originate_result payload ->
      Fees.burn_sc_rollup_origination_fees
        ctxt
        ~storage_limit
        ~payer
        payload.size
      >|=? fun (ctxt, storage_limit, balance_updates) ->
      let result = Sc_rollup_originate_result {payload with balance_updates} in
      (ctxt, storage_limit, result)
  | Sc_rollup_add_messages_result _ -> return (ctxt, storage_limit, smopr)
  | Sc_rollup_cement_result _ -> return (ctxt, storage_limit, smopr)
  | Sc_rollup_publish_result _ -> return (ctxt, storage_limit, smopr)
  | Sc_rollup_refute_result _ -> return (ctxt, storage_limit, smopr)
  | Sc_rollup_timeout_result _ -> return (ctxt, storage_limit, smopr)
  | Sc_rollup_execute_outbox_message_result
      ({paid_storage_size_diff; balance_updates; _} as payload) ->
      let consumed = paid_storage_size_diff in
      Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
      >|=? fun (ctxt, storage_limit, storage_bus) ->
      let balance_updates = storage_bus @ balance_updates in
      ( ctxt,
        storage_limit,
        Sc_rollup_execute_outbox_message_result {payload with balance_updates}
      )
  | Sc_rollup_recover_bond_result _ -> return (ctxt, storage_limit, smopr)
  | Zk_rollup_origination_result payload ->
      Fees.burn_zk_rollup_origination_fees
        ctxt
        ~storage_limit
        ~payer
        payload.storage_size
      >>=? fun (ctxt, storage_limit, balance_updates) ->
      let result =
        Zk_rollup_origination_result {payload with balance_updates}
      in
      return (ctxt, storage_limit, result)
  | Zk_rollup_publish_result payload ->
      let consumed = payload.paid_storage_size_diff in
      Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
      >|=? fun (ctxt, storage_limit, storage_bus) ->
      let balance_updates = storage_bus @ payload.balance_updates in
      ( ctxt,
        storage_limit,
        Zk_rollup_publish_result {payload with balance_updates} )
  | Zk_rollup_update_result
      ({paid_storage_size_diff; balance_updates; _} as payload) ->
      let consumed = paid_storage_size_diff in
      Fees.burn_storage_fees ctxt ~storage_limit ~payer consumed
      >|=? fun (ctxt, storage_limit, storage_bus) ->
      let balance_updates = storage_bus @ balance_updates in
      ( ctxt,
        storage_limit,
        Zk_rollup_update_result {payload with balance_updates} )

(** [burn_internal_storage_fees ctxt smopr storage_limit payer] burns the
    storage fees associated to an internal operation result [smopr].
    Returns an updated context, an updated storage limit with the space consumed
    by the operation subtracted, and [smopr] with the relevant balance updates
    included. *)
let burn_internal_storage_fees :
    type kind.
    context ->
    kind successful_internal_operation_result ->
    storage_limit:Z.t ->
    payer:public_key_hash ->
    (context * Z.t * kind successful_internal_operation_result) tzresult Lwt.t =
 fun ctxt smopr ~storage_limit ~payer ->
  let payer = `Contract (Contract.Implicit payer) in
  match smopr with
  | ITransaction_result transaction_result ->
      burn_transaction_storage_fees
        ctxt
        transaction_result
        ~storage_limit
        ~payer
      >|=? fun (ctxt, storage_limit, transaction_result) ->
      (ctxt, storage_limit, ITransaction_result transaction_result)
  | IOrigination_result origination_result ->
      burn_origination_storage_fees
        ctxt
        origination_result
        ~storage_limit
        ~payer
      >|=? fun (ctxt, storage_limit, origination_result) ->
      (ctxt, storage_limit, IOrigination_result origination_result)
  | IDelegation_result _ -> return (ctxt, storage_limit, smopr)
  | IEvent_result _ -> return (ctxt, storage_limit, smopr)

let apply_manager_contents (type kind) ctxt chain_id ~consume_gas_for_sig_check
    (op : kind Kind.manager contents) :
    (success_or_failure
    * kind manager_operation_result
    * packed_internal_operation_result list)
    Lwt.t =
  let (Manager_operation {source; operation; gas_limit; storage_limit; _}) =
    op
  in
  (* We do not expose the internal scaling to the users. Instead, we multiply
       the specified gas limit by the internal scaling. *)
  let ctxt = Gas.set_limit ctxt gas_limit in
  apply_manager_operation
    ctxt
    ~source
    ~chain_id
    ~consume_gas_for_sig_check
    operation
  >>= function
  | Ok (ctxt, operation_results, internal_operations) -> (
      apply_internal_operations ctxt ~payer:source ~chain_id internal_operations
      >>= function
      | Success ctxt, internal_operations_results -> (
          burn_manager_storage_fees
            ctxt
            operation_results
            ~storage_limit
            ~payer:source
          >>= function
          | Ok (ctxt, storage_limit, operation_results) -> (
              List.fold_left_es
                (fun (ctxt, storage_limit, res) imopr ->
                  let (Internal_operation_result (op, mopr)) = imopr in
                  match mopr with
                  | Applied smopr ->
                      burn_internal_storage_fees
                        ctxt
                        smopr
                        ~storage_limit
                        ~payer:source
                      >>=? fun (ctxt, storage_limit, smopr) ->
                      let imopr =
                        Internal_operation_result (op, Applied smopr)
                      in
                      return (ctxt, storage_limit, imopr :: res)
                  | _ -> return (ctxt, storage_limit, imopr :: res))
                (ctxt, storage_limit, [])
                internal_operations_results
              >|= function
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

let rec mark_skipped :
    type kind.
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
  let rec take_fees_rec :
      type kind.
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

let rec apply_manager_contents_list_rec :
    type kind.
    context ->
    payload_producer:Consensus_key.t ->
    Chain_id.t ->
    consume_gas_for_sig_check:Gas.cost option ->
    kind Kind.manager fees_updated_contents_list ->
    (success_or_failure * kind Kind.manager contents_result_list) Lwt.t =
 fun ctxt
     ~payload_producer
     chain_id
     ~consume_gas_for_sig_check
     fees_updated_contents_list ->
  let level = Level.current ctxt in
  match fees_updated_contents_list with
  | FeesUpdatedSingle {contents = Manager_operation _ as op; balance_updates} ->
      apply_manager_contents ctxt chain_id ~consume_gas_for_sig_check op
      >|= fun (ctxt_result, operation_result, internal_operation_results) ->
      let result =
        Manager_operation_result
          {balance_updates; operation_result; internal_operation_results}
      in
      (ctxt_result, Single_result result)
  | FeesUpdatedCons
      ({contents = Manager_operation _ as op; balance_updates}, rest) -> (
      apply_manager_contents ctxt chain_id ~consume_gas_for_sig_check op
      >>= function
      | Failure, operation_result, internal_operation_results ->
          let result =
            Manager_operation_result
              {balance_updates; operation_result; internal_operation_results}
          in
          Lwt.return
            ( Failure,
              Cons_result (result, mark_skipped ~payload_producer level rest) )
      | Success ctxt, operation_result, internal_operation_results ->
          let result =
            Manager_operation_result
              {balance_updates; operation_result; internal_operation_results}
          in
          apply_manager_contents_list_rec
            ctxt
            ~payload_producer
            chain_id
            ~consume_gas_for_sig_check:None
            rest
          >|= fun (ctxt_result, results) ->
          (ctxt_result, Cons_result (result, results)))

let mark_backtracked results =
  let mark_results :
      type kind.
      kind Kind.manager contents_result -> kind Kind.manager contents_result =
   fun results ->
    let mark_manager_operation_result :
        type kind.
        kind manager_operation_result -> kind manager_operation_result =
      function
      | (Failed _ | Skipped _ | Backtracked _) as result -> result
      | Applied result -> Backtracked (result, None)
    in
    let mark_internal_operation_result :
        type kind.
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
  let rec traverse_apply_results :
      type kind.
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
  liquidity_baking_toggle_ema : Liquidity_baking.Toggle_EMA.t;
  implicit_operations_results :
    Apply_results.packed_successful_manager_operation_result list;
}

let record_operation (type kind) ctxt hash (operation : kind operation) :
    context =
  match operation.protocol_data.contents with
  | Single (Preendorsement _) -> ctxt
  | Single (Endorsement _) -> ctxt
  | Single (Dal_attestation _) -> ctxt
  | Single
      ( Failing_noop _ | Proposals _ | Ballot _ | Seed_nonce_revelation _
      | Vdf_revelation _ | Double_endorsement_evidence _
      | Double_preendorsement_evidence _ | Double_baking_evidence _
      | Activate_account _ | Drain_delegate _ | Manager_operation _ )
  | Cons (Manager_operation _, _) ->
      record_non_consensus_operation_hash ctxt hash

let find_in_slot_map consensus_content slot_map =
  match slot_map with
  | None -> error (Consensus.Slot_map_not_found {loc = __LOC__})
  | Some slot_map -> (
      match Slot.Map.find consensus_content.slot slot_map with
      | None ->
          (* This should not happen: operation validation should have failed. *)
          error Faulty_validation_wrong_slot
      | Some (consensus_key, power) -> ok (consensus_key, power))

let record_preendorsement ctxt (mode : mode) (content : consensus_content) :
    (context * Kind.preendorsement contents_result_list) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let ctxt =
    match mode with
    | Full_construction _ -> (
        match Consensus.get_preendorsements_quorum_round ctxt with
        | None -> Consensus.set_preendorsements_quorum_round ctxt content.round
        | Some _ -> ctxt)
    | Application _ | Partial_construction _ -> ctxt
  in
  let mk_preendorsement_result ({delegate; consensus_pkh; _} : Consensus_key.pk)
      preendorsement_power =
    Single_result
      (Preendorsement_result
         {
           balance_updates = [];
           delegate;
           consensus_key = consensus_pkh;
           preendorsement_power;
         })
  in
  match mode with
  | Application _ | Full_construction _ ->
      let*? consensus_key, power =
        find_in_slot_map content (Consensus.allowed_preendorsements ctxt)
      in
      let*? ctxt =
        Consensus.record_preendorsement
          ctxt
          ~initial_slot:content.slot
          ~power
          content.round
      in
      return (ctxt, mk_preendorsement_result consensus_key power)
  | Partial_construction _ ->
      (* In mempool mode, preendorsements are allowed for various levels
         and rounds. We do not record preendorsements because we could get
         false-positive conflicts for preendorsements with the same slot
         but different levels/rounds. We could record just preendorsements
         for the mempool head's level and round (the most usual
         preendorsements), but we don't need to, because there is no block
         to finalize anyway in this mode. *)
      let* ctxt, consensus_key =
        let level = Level.from_raw ctxt content.level in
        Stake_distribution.slot_owner ctxt level content.slot
      in
      return (ctxt, mk_preendorsement_result consensus_key 0 (* Fake power. *))

let record_endorsement ctxt (mode : mode) (content : consensus_content) :
    (context * Kind.endorsement contents_result_list) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let mk_endorsement_result ({delegate; consensus_pkh; _} : Consensus_key.pk)
      endorsement_power =
    Single_result
      (Endorsement_result
         {
           balance_updates = [];
           delegate;
           consensus_key = consensus_pkh;
           endorsement_power;
         })
  in
  match mode with
  | Application _ | Full_construction _ ->
      let*? consensus_key, power =
        find_in_slot_map content (Consensus.allowed_endorsements ctxt)
      in
      let*? ctxt =
        Consensus.record_endorsement ctxt ~initial_slot:content.slot ~power
      in
      return (ctxt, mk_endorsement_result consensus_key power)
  | Partial_construction _ ->
      (* In mempool mode, endorsements are allowed for various levels
         and rounds. We do not record endorsements because we could get
         false-positive conflicts for endorsements with the same slot
         but different levels/rounds. We could record just endorsements
         for the predecessor's level and round (the most usual
         endorsements), but we don't need to, because there is no block
         to finalize anyway in this mode. *)
      let* ctxt, consensus_key =
        let level = Level.from_raw ctxt content.level in
        Stake_distribution.slot_owner ctxt level content.slot
      in
      return (ctxt, mk_endorsement_result consensus_key 0 (* Fake power. *))

let apply_manager_contents_list ctxt ~payload_producer chain_id
    ~gas_cost_for_sig_check fees_updated_contents_list =
  apply_manager_contents_list_rec
    ctxt
    ~payload_producer
    chain_id
    ~consume_gas_for_sig_check:(Some gas_cost_for_sig_check)
    fees_updated_contents_list
  >>= fun (ctxt_result, results) ->
  match ctxt_result with
  | Failure -> Lwt.return (ctxt (* backtracked *), mark_backtracked results)
  | Success ctxt ->
      Lazy_storage.cleanup_temporaries ctxt >|= fun ctxt -> (ctxt, results)

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

let punish_delegate ctxt delegate level mistake mk_result ~payload_producer =
  let punish =
    match mistake with
    | `Double_baking -> Delegate.punish_double_baking
    | `Double_endorsing -> Delegate.punish_double_endorsing
  in
  punish ctxt delegate level >>=? fun (ctxt, {reward; amount_to_burn}) ->
  Token.transfer
    ctxt
    (`Frozen_deposits delegate)
    `Double_signing_punishments
    amount_to_burn
  >>=? fun (ctxt, punish_balance_updates) ->
  Token.transfer
    ctxt
    (`Frozen_deposits delegate)
    (`Contract (Contract.Implicit payload_producer.Consensus_key.delegate))
    reward
  >|=? fun (ctxt, reward_balance_updates) ->
  let balance_updates = reward_balance_updates @ punish_balance_updates in
  (ctxt, Single_result (mk_result balance_updates))

let punish_double_endorsement_or_preendorsement (type kind) ctxt
    ~(op1 : kind Kind.consensus Operation.t) ~payload_producer :
    (context
    * kind Kind.double_consensus_operation_evidence contents_result_list)
    tzresult
    Lwt.t =
  let mk_result (balance_updates : Receipt.balance_updates) :
      kind Kind.double_consensus_operation_evidence contents_result =
    match op1.protocol_data.contents with
    | Single (Preendorsement _) ->
        Double_preendorsement_evidence_result balance_updates
    | Single (Endorsement _) ->
        Double_endorsement_evidence_result balance_updates
  in
  match op1.protocol_data.contents with
  | Single (Preendorsement e1) | Single (Endorsement e1) ->
      let level = Level.from_raw ctxt e1.level in
      Stake_distribution.slot_owner ctxt level e1.slot
      >>=? fun (ctxt, consensus_pk1) ->
      punish_delegate
        ctxt
        consensus_pk1.delegate
        level
        `Double_endorsing
        mk_result
        ~payload_producer

let punish_double_baking ctxt (bh1 : Block_header.t) ~payload_producer =
  Fitness.from_raw bh1.shell.fitness >>?= fun bh1_fitness ->
  let round1 = Fitness.round bh1_fitness in
  Raw_level.of_int32 bh1.shell.level >>?= fun raw_level ->
  let level = Level.from_raw ctxt raw_level in
  let committee_size = Constants.consensus_committee_size ctxt in
  Round.to_slot round1 ~committee_size >>?= fun slot1 ->
  Stake_distribution.slot_owner ctxt level slot1
  >>=? fun (ctxt, consensus_pk1) ->
  punish_delegate
    ctxt
    consensus_pk1.delegate
    level
    `Double_baking
    ~payload_producer
    (fun balance_updates -> Double_baking_evidence_result balance_updates)

let apply_contents_list (type kind) ctxt chain_id (mode : mode)
    ~payload_producer ~operation (contents_list : kind contents_list) :
    (context * kind contents_result_list) tzresult Lwt.t =
  let mempool_mode =
    match mode with
    | Partial_construction _ -> true
    | Full_construction _ | Application _ -> false
  in
  match contents_list with
  | Single (Preendorsement consensus_content) ->
      record_preendorsement ctxt mode consensus_content
  | Single (Endorsement consensus_content) ->
      record_endorsement ctxt mode consensus_content
  | Single (Dal_attestation op) ->
      (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3115

         This is a temporary operation. We do no check for the
         moment. In particular, this means we do not check the
         signature. Consequently, it is really important to ensure this
         operation cannot be included into a block when the feature flag
         is not set. This is done in order to avoid modifying the
         endorsement encoding. However, once the DAL will be ready, this
         operation should be merged with an endorsement or at least
         refined. *)
      Dal_apply.apply_attestation ctxt op >>?= fun ctxt ->
      return
        (ctxt, Single_result (Dal_attestation_result {delegate = op.attestor}))
  | Single (Seed_nonce_revelation {level; nonce}) ->
      let level = Level.from_raw ctxt level in
      Nonce.reveal ctxt level nonce >>=? fun ctxt ->
      let tip = Delegate.Rewards.seed_nonce_revelation_tip ctxt in
      let delegate = payload_producer.Consensus_key.delegate in
      let receiver =
        if Constants.freeze_rewards ctxt then `Frozen_deposits delegate
        else `Contract (Contract.Implicit delegate)
      in
      Token.transfer ctxt `Revelation_rewards receiver tip
      >|=? fun (ctxt, balance_updates) ->
      (ctxt, Single_result (Seed_nonce_revelation_result balance_updates))
  | Single (Vdf_revelation {solution}) ->
      Seed.update_seed ctxt solution >>=? fun ctxt ->
      let tip = Delegate.Rewards.vdf_revelation_tip ctxt in
      let delegate = payload_producer.Consensus_key.delegate in
      let receiver =
        if Constants.freeze_rewards ctxt then `Frozen_deposits delegate
        else `Contract (Contract.Implicit delegate)
      in
      Token.transfer ctxt `Revelation_rewards receiver tip
      >|=? fun (ctxt, balance_updates) ->
      (ctxt, Single_result (Vdf_revelation_result balance_updates))
  | Single (Double_preendorsement_evidence {op1; op2 = _}) ->
      punish_double_endorsement_or_preendorsement ctxt ~op1 ~payload_producer
  | Single (Double_endorsement_evidence {op1; op2 = _}) ->
      punish_double_endorsement_or_preendorsement ctxt ~op1 ~payload_producer
  | Single (Double_baking_evidence {bh1; bh2 = _}) ->
      punish_double_baking ctxt bh1 ~payload_producer
  | Single (Activate_account {id = pkh; activation_code}) ->
      let blinded_pkh =
        Blinded_public_key_hash.of_ed25519_pkh activation_code pkh
      in
      let sender = `Collected_commitments blinded_pkh in
      let contract = Contract.Implicit (Signature.Ed25519 pkh) in
      Token.balance ctxt sender >>=? fun (ctxt, amount) ->
      Token.transfer ctxt sender (`Contract contract) amount
      >>=? fun (ctxt, bupds) ->
      return (ctxt, Single_result (Activate_account_result bupds))
  | Single (Proposals _ as contents) ->
      Amendment.apply_proposals ctxt chain_id contents
  | Single (Ballot _ as contents) -> Amendment.apply_ballot ctxt contents
  | Single (Drain_delegate {delegate; destination; consensus_key = _}) ->
      Delegate.drain ctxt ~delegate ~destination
      >>=? fun ( ctxt,
                 allocated_destination_contract,
                 fees,
                 drain_balance_updates ) ->
      Token.transfer
        ctxt
        (`Contract (Contract.Implicit delegate))
        (`Contract (Contract.Implicit payload_producer.Consensus_key.delegate))
        fees
      >>=? fun (ctxt, fees_balance_updates) ->
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
  match Level.dawn_of_a_new_cycle ctxt with
  | None -> return (ctxt, [], [])
  | Some last_cycle ->
      Delegate.cycle_end ctxt last_cycle
      >>=? fun (ctxt, balance_updates, deactivated) ->
      Bootstrap.cycle_end ctxt last_cycle >|=? fun ctxt ->
      (ctxt, balance_updates, deactivated)

let apply_liquidity_baking_subsidy ctxt ~toggle_vote =
  Liquidity_baking.on_subsidy_allowed
    ctxt
    ~toggle_vote
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
      (let liquidity_baking_subsidy =
         Delegate.Rewards.liquidity_baking_subsidy ctxt
       in
       (* credit liquidity baking subsidy to CPMM contract *)
       Token.transfer
         ~origin:Subsidy
         ctxt
         `Liquidity_baking_subsidies
         (`Contract liquidity_baking_cpmm_contract)
         liquidity_baking_subsidy
       >>=? fun (ctxt, balance_updates) ->
       Script_cache.find ctxt liquidity_baking_cpmm_contract_hash
       >>=? fun (ctxt, cache_key, script) ->
       match script with
       | None -> tzfail (Script_tc_errors.No_such_entrypoint Entrypoint.default)
       | Some (script, script_ir) -> (
           (* Token.transfer which is being called above already loads this
              value into the Irmin cache, so no need to burn gas for it. *)
           Contract.get_balance ctxt liquidity_baking_cpmm_contract
           >>=? fun balance ->
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
           >>=? fun ( {
                        script = updated_cached_script;
                        code_size = updated_size;
                        storage;
                        lazy_storage_diff;
                        operations;
                        ticket_diffs;
                        ticket_receipt;
                      },
                      ctxt ) ->
           match operations with
           | _ :: _ ->
               (* No internal operations are expected here. Something bad may be happening. *)
               return (backtracking_ctxt, [])
           | [] ->
               (* update CPMM storage *)
               update_script_storage_and_ticket_balances
                 ctxt
                 ~self_contract:liquidity_baking_cpmm_contract_hash
                 storage
                 lazy_storage_diff
                 ticket_diffs
                 operations
               >>=? fun (ticket_table_size_diff, ctxt) ->
               Fees.record_paid_storage_space
                 ctxt
                 liquidity_baking_cpmm_contract_hash
               >>=? fun (ctxt, new_size, paid_storage_size_diff) ->
               Ticket_balance.adjust_storage_space
                 ctxt
                 ~storage_diff:ticket_table_size_diff
               >>=? fun (ticket_paid_storage_diff, ctxt) ->
               let consumed_gas =
                 Gas.consumed ~since:backtracking_ctxt ~until:ctxt
               in
               Script_cache.update
                 ctxt
                 cache_key
                 ( {script with storage = Script.lazy_expr storage},
                   updated_cached_script )
                 updated_size
               >>?= fun ctxt ->
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
                      })
               in
               let ctxt = Gas.set_unlimited ctxt in
               return (ctxt, [Successful_manager_result result])))
      >|= function
      | Ok (ctxt, results) -> Ok (ctxt, results)
      | Error _ ->
          (* Do not fail if something bad happens during CPMM contract call. *)
          let ctxt = Gas.set_unlimited backtracking_ctxt in
          Ok (ctxt, []))

let are_endorsements_required ctxt ~level =
  First_level_of_protocol.get ctxt >|=? fun first_level ->
  (* NB: the first level is the level of the migration block. There
     are no endorsements for this block. Therefore the block at the
     next level cannot contain endorsements. *)
  let level_position_in_protocol = Raw_level.diff level first_level in
  Compare.Int32.(level_position_in_protocol > 1l)

let record_endorsing_participation ctxt =
  let open Lwt_result_syntax in
  let*? validators =
    match Consensus.allowed_endorsements ctxt with
    | Some x -> ok x
    | None -> error (Consensus.Slot_map_not_found {loc = __LOC__})
  in
  Slot.Map.fold_es
    (fun initial_slot ((consensus_pk : Consensus_key.pk), power) ctxt ->
      let participation =
        if Slot.Set.mem initial_slot (Consensus.endorsements_seen ctxt) then
          Delegate.Participated
        else Delegate.Didn't_participate
      in
      Delegate.record_endorsing_participation
        ctxt
        ~delegate:consensus_pk.delegate
        ~participation
        ~endorsing_power:power)
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
  let toggle_vote =
    block_header.Block_header.protocol_data.contents
      .liquidity_baking_toggle_vote
  in
  let* ctxt, liquidity_baking_operations_results, liquidity_baking_toggle_ema =
    apply_liquidity_baking_subsidy ctxt ~toggle_vote
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
  (* The endorsement/preendorsement validation rules for construction are the
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
  let toggle_vote = block_data_contents.liquidity_baking_toggle_vote in
  let* ctxt, liquidity_baking_operations_results, liquidity_baking_toggle_ema =
    apply_liquidity_baking_subsidy ctxt ~toggle_vote
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
  let toggle_vote = Liquidity_baking.LB_pass in
  let* ctxt, liquidity_baking_operations_results, liquidity_baking_toggle_ema =
    apply_liquidity_baking_subsidy ctxt ~toggle_vote
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
  let level = Level.current ctxt in
  let endorsing_power = Consensus.current_endorsement_power ctxt in
  let* required_endorsements =
    are_endorsements_required ctxt ~level:level.level
  in
  let block_payload_hash =
    Block_payload.hash
      ~predecessor_hash
      ~payload_round:block_data_contents.Block_header.payload_round
      (non_consensus_operations ctxt)
  in
  (* from this point nothing should fail *)
  (* We mark the endorsement branch as the grand parent branch when
     accessible. This will not be present before the first two blocks
     of tenderbake. *)
  let level = Level.current ctxt in
  (* We mark the current payload hash as the predecessor one => this
     will only be accessed by the successor block now. *)
  let*! ctxt =
    Consensus.store_endorsement_branch
      ctxt
      (predecessor_hash, block_payload_hash)
  in
  let* ctxt = Round.update ctxt round in
  (* end of level  *)
  let* ctxt =
    match block_data_contents.Block_header.seed_nonce_hash with
    | None -> return ctxt
    | Some nonce_hash ->
        Nonce.record_hash ctxt {nonce_hash; delegate = block_producer.delegate}
  in
  let* ctxt, reward_bonus =
    if required_endorsements then
      let* ctxt = record_endorsing_participation ctxt in
      let*? rewards_bonus = Baking.bonus_baking_reward ctxt ~endorsing_power in
      return (ctxt, Some rewards_bonus)
    else return (ctxt, None)
  in
  let baking_reward = Delegate.Rewards.baking_reward_fixed_portion ctxt in
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
  let* ctxt =
    if Level.may_snapshot_stake_distribution ctxt then
      Stake_distribution.snapshot ctxt
    else return ctxt
  in
  let* ctxt, cycle_end_balance_updates, deactivated =
    may_start_new_cycle ctxt
  in
  let* ctxt = Amendment.may_start_new_voting_period ctxt in
  let* ctxt, dal_attestation = Dal_apply.finalisation ctxt in
  let* ctxt = Sc_rollup.Inbox.finalize_inbox_level ctxt in
  let balance_updates =
    migration_balance_updates @ baking_receipts @ cycle_end_balance_updates
  in
  let consumed_gas =
    Gas.Arith.sub
      (Gas.Arith.fp @@ Constants.hard_gas_limit_per_block ctxt)
      (Gas.block_level ctxt)
  in
  let+ voting_period_info = Voting_period.get_rpc_current_info ctxt in
  let receipt =
    Apply_results.
      {
        proposer = payload_producer;
        baker = block_producer;
        level_info = level;
        voting_period_info;
        nonce_hash = block_data_contents.seed_nonce_hash;
        consumed_gas;
        deactivated;
        balance_updates;
        liquidity_baking_toggle_ema;
        implicit_operations_results;
        dal_attestation;
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
         {!record_preendorsement} and {!record_endorsement} in this mode. *)
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
              dal_attestation = None;
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
