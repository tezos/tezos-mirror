(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Alpha_context

(** {2 Definition and initialization of [validate_operation_info] and
    [validate_operation_state]}

    These live in memory during the validation of a block, or until a
    change of head block in mempool mode; they are never put in the
    storage. *)

(** Static information used to validate manager operations. *)
type manager_info = {
  hard_storage_limit_per_operation : Z.t;
  hard_gas_limit_per_operation : Gas.Arith.integral;
}

let init_manager_info ctxt =
  {
    hard_storage_limit_per_operation =
      Constants.hard_storage_limit_per_operation ctxt;
    hard_gas_limit_per_operation = Constants.hard_gas_limit_per_operation ctxt;
  }

(** State used and modified when validating manager operations. *)
type manager_state = {
  managers_seen : Operation_hash.t Signature.Public_key_hash.Map.t;
      (** To enforce the one-operation-per manager-per-block restriction
          (1M). The operation hash lets us indicate the conflicting
          operation in the {!Manager_restriction} error.

          Note that as part of {!validate_operation_state}, this map
          lives in memory. It is not explicitly bounded here, however:

          - In block validation mode, it is bounded by the number of
            manager operations allowed in the block.

          - In mempool mode, bounding the number of operations in this
            map is the responsability of the mempool. (E.g. the plugin used
            by Octez has a [max_prechecked_manager_operations] parameter to
            ensure this.) *)
  remaining_block_gas : Gas.Arith.fp;
}

let init_manager_state ctxt =
  {
    managers_seen = Signature.Public_key_hash.Map.empty;
    remaining_block_gas = Gas.Arith.fp (Constants.hard_gas_limit_per_block ctxt);
  }

(* If you add a new mode, please make sure that it has a way to bound
   the size of the map {!recfield:managers_seen}. *)
type mode = Block | Mempool

type validate_operation_info = {
  ctxt : t;  (** The context at the beginning of the block. *)
  mode : mode;
  chain_id : Chain_id.t;  (** Needed for signature checks. *)
  current_level : Level.t;
  manager_info : manager_info;
}

type validate_operation_state = {manager_state : manager_state}

let init_validate_operation_info ctxt mode chain_id =
  {
    ctxt;
    mode;
    chain_id;
    current_level = Level.current ctxt;
    manager_info = init_manager_info ctxt;
  }

let init_validate_operation_state ctxt =
  {manager_state = init_manager_state ctxt}

let init_info_and_state ctxt mode chain_id =
  let vi = init_validate_operation_info ctxt mode chain_id in
  let vs = init_validate_operation_state ctxt in
  (vi, vs)

(* See mli file. *)
type stamp = Operation_validated_stamp

module Manager = struct
  type error +=
    | Manager_restriction of Signature.Public_key_hash.t * Operation_hash.t
    | Inconsistent_sources
    | Inconsistent_counters
    | Incorrect_reveal_position
    | Insufficient_gas_for_manager
    | Gas_quota_exceeded_init_deserialize
    | Tx_rollup_feature_disabled
    | Sc_rollup_feature_disabled

  let () =
    register_error_kind
      `Temporary
      ~id:"validate_operation.manager_restriction"
      ~title:"Manager restriction"
      ~description:
        "An operation with the same manager has already been validated in the \
         current block."
      ~pp:(fun ppf (d, hash) ->
        Format.fprintf
          ppf
          "Manager %a already has the operation %a in the current block."
          Signature.Public_key_hash.pp
          d
          Operation_hash.pp
          hash)
      Data_encoding.(
        obj2
          (req "manager" Signature.Public_key_hash.encoding)
          (req "hash" Operation_hash.encoding))
      (function
        | Manager_restriction (manager, hash) -> Some (manager, hash)
        | _ -> None)
      (fun (manager, hash) -> Manager_restriction (manager, hash)) ;
    let inconsistent_sources_description =
      "The operation batch includes operations from different sources."
    in
    register_error_kind
      `Permanent
      ~id:"validate_operation.inconsistent_sources"
      ~title:"Inconsistent sources in operation batch"
      ~description:inconsistent_sources_description
      ~pp:(fun ppf () ->
        Format.fprintf ppf "%s" inconsistent_sources_description)
      Data_encoding.empty
      (function Inconsistent_sources -> Some () | _ -> None)
      (fun () -> Inconsistent_sources) ;
    let inconsistent_counters_description =
      "Inconsistent counters in operation. Counters of an operation must be \
       successive."
    in
    register_error_kind
      `Permanent
      ~id:"validate_operation.inconsistent_counters"
      ~title:"Inconsistent counters in operation"
      ~description:inconsistent_counters_description
      ~pp:(fun ppf () ->
        Format.fprintf ppf "%s" inconsistent_counters_description)
      Data_encoding.empty
      (function Inconsistent_counters -> Some () | _ -> None)
      (fun () -> Inconsistent_counters) ;
    let incorrect_reveal_description =
      "Incorrect reveal operation position in batch: only allowed in first \
       position."
    in
    register_error_kind
      `Permanent
      ~id:"validate_operation.incorrect_reveal_position"
      ~title:"Incorrect reveal position"
      ~description:incorrect_reveal_description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" incorrect_reveal_description)
      Data_encoding.empty
      (function Incorrect_reveal_position -> Some () | _ -> None)
      (fun () -> Incorrect_reveal_position) ;
    register_error_kind
      `Permanent
      ~id:"validate_operation.insufficient_gas_for_manager"
      ~title:"Not enough gas for initial manager cost"
      ~description:
        (Format.asprintf
           "Gas limit is too low to cover the initial cost of manager \
            operations: at least %a gas required."
           Gas.pp_cost
           Michelson_v1_gas.Cost_of.manager_operation)
      Data_encoding.empty
      (function Insufficient_gas_for_manager -> Some () | _ -> None)
      (fun () -> Insufficient_gas_for_manager) ;
    let gas_deserialize_description =
      "Gas limit was not high enough to deserialize the transaction parameters \
       or origination script code or initial storage etc., making the \
       operation impossible to parse within the provided gas bounds."
    in
    register_error_kind
      `Permanent
      ~id:"validate_operation.gas_quota_exceeded_init_deserialize"
      ~title:"Not enough gas for initial deserialization of script expressions"
      ~description:gas_deserialize_description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" gas_deserialize_description)
      Data_encoding.empty
      (function Gas_quota_exceeded_init_deserialize -> Some () | _ -> None)
      (fun () -> Gas_quota_exceeded_init_deserialize) ;
    register_error_kind
      `Permanent
      ~id:"validate_operation.tx_rollup_is_disabled"
      ~title:"Tx rollup is disabled"
      ~description:"Cannot originate a tx rollup as it is disabled."
      ~pp:(fun ppf () ->
        Format.fprintf
          ppf
          "Cannot apply a tx rollup operation as it is disabled. This feature \
           will be enabled in a future proposal")
      Data_encoding.unit
      (function Tx_rollup_feature_disabled -> Some () | _ -> None)
      (fun () -> Tx_rollup_feature_disabled) ;
    let scoru_disabled_description =
      "Smart contract rollups will be enabled in a future proposal."
    in
    register_error_kind
      `Permanent
      ~id:"validate_operation.sc_rollup_disabled"
      ~title:"Smart contract rollups are disabled"
      ~description:scoru_disabled_description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" scoru_disabled_description)
      Data_encoding.unit
      (function Sc_rollup_feature_disabled -> Some () | _ -> None)
      (fun () -> Sc_rollup_feature_disabled)

  (** State that simulates changes from individual operations that have
      an effect on future operations inside the same batch. *)
  type batch_state = {
    balance : Tez.t;
        (** Remaining balance in the contract, used to simulate the
            payment of fees by each operation in the batch. *)
    is_allocated : bool;
        (** Track whether the contract is still allocated. Indeed,
            previous operations' fee payment may empty the contract and
            this may deallocate the contract.

            TODO: https://gitlab.com/tezos/tezos/-/issues/3209 Change
            empty account cleanup mechanism to avoid the need for this
            field. *)
    remaining_block_gas : Gas.Arith.fp;
        (** In Block_validation mode, this is what remains of the block gas
            quota after subtracting the gas_limit of all previously
            validated operations in the block. In Mempool mode, only
            previous gas for previous operations in the same batch has been
            subtracted from the block quota. Cf
            {!maybe_update_remaining_block_gas}:
            [vs.manager_state.remaining_block_gas] is updated only in
            Block_validation mode. *)
  }

  (** Check a few simple properties of the batch, and return the
      initial {!batch_state} and the contract public key.

      Invariants checked:

      - All operations in a batch have the same source.

      - The source's contract is allocated.

      - The counters in a batch are successive, and the first of them
        is the source's next expected counter.

      - A batch contains at most one Reveal operation that must occur
        in first position.

      - The source's public key has been revealed (either before the
        considered batch, or during its first operation).

      Note that currently, the [op] batch contains only one signature,
      so all operations in the batch are required to originate from the
      same manager. This may change in the future, in order to allow
      several managers to group-sign a sequence of operations. *)
  let check_sanity_and_find_public_key vi vs
      (contents_list : _ Kind.manager contents_list) =
    let open Result_syntax in
    let check_source_and_counter ~expected_source ~source ~previous_counter
        ~counter =
      let* () =
        error_unless
          (Signature.Public_key_hash.equal expected_source source)
          Inconsistent_sources
      in
      error_unless
        Compare.Z.(Z.succ previous_counter = counter)
        Inconsistent_counters
    in
    let rec check_batch_tail_sanity :
        type kind.
        public_key_hash ->
        counter ->
        kind Kind.manager contents_list ->
        unit tzresult =
     fun expected_source previous_counter -> function
      | Single (Manager_operation {operation = Reveal _key; _}) ->
          error Incorrect_reveal_position
      | Cons (Manager_operation {operation = Reveal _key; _}, _res) ->
          error Incorrect_reveal_position
      | Single (Manager_operation {source; counter; _}) ->
          check_source_and_counter
            ~expected_source
            ~source
            ~previous_counter
            ~counter
      | Cons (Manager_operation {source; counter; _}, rest) ->
          let open Result_syntax in
          let* () =
            check_source_and_counter
              ~expected_source
              ~source
              ~previous_counter
              ~counter
          in
          check_batch_tail_sanity source counter rest
    in
    let check_batch :
        type kind.
        kind Kind.manager contents_list ->
        (public_key_hash * public_key option * counter) tzresult =
     fun contents_list ->
      match contents_list with
      | Single (Manager_operation {source; operation = Reveal key; counter; _})
        ->
          ok (source, Some key, counter)
      | Single (Manager_operation {source; counter; _}) ->
          ok (source, None, counter)
      | Cons
          (Manager_operation {source; operation = Reveal key; counter; _}, rest)
        ->
          check_batch_tail_sanity source counter rest >>? fun () ->
          ok (source, Some key, counter)
      | Cons (Manager_operation {source; counter; _}, rest) ->
          check_batch_tail_sanity source counter rest >>? fun () ->
          ok (source, None, counter)
    in
    let open Lwt_result_syntax in
    let*? source, revealed_key, first_counter = check_batch contents_list in
    let* balance = Contract.check_allocated_and_get_balance vi.ctxt source in
    let* () = Contract.check_counter_increment vi.ctxt source first_counter in
    let* pk =
      match revealed_key with
      | Some pk -> return pk
      | None -> Contract.get_manager_key vi.ctxt source
    in
    let initial_batch_state =
      {
        balance;
        (* Initial contract allocation is ensured by the success of
           the call to {!Contract.check_allocated_and_get_balance}
           above. *)
        is_allocated = true;
        remaining_block_gas = vs.manager_state.remaining_block_gas;
      }
    in
    return (initial_batch_state, pk)

  let check_gas_limit_and_consume_from_block_gas vi ~remaining_block_gas
      ~gas_limit =
    (match vi.mode with
    | Block -> fun res -> res
    | Mempool ->
        (* [Gas.check_limit_and_consume_from_block_gas] will only
           raise a "temporary" error, however when
           {!validate_operation} is called on a batch in isolation
           (like e.g. in the mempool) it must "refuse" operations
           whose total gas limit (the sum of the [gas_limit]s of each
           operation) is already above the block limit. We add the
           "permanent" error [Gas.Gas_limit_too_high] on top of the
           trace to this effect. *)
        record_trace Gas.Gas_limit_too_high)
      (Gas.check_limit_and_consume_from_block_gas
         ~hard_gas_limit_per_operation:
           vi.manager_info.hard_gas_limit_per_operation
         ~remaining_block_gas
         ~gas_limit)

  let check_storage_limit vi storage_limit =
    error_unless
      Compare.Z.(
        storage_limit <= vi.manager_info.hard_storage_limit_per_operation
        && storage_limit >= Z.zero)
      Fees.Storage_limit_too_high

  let assert_tx_rollup_feature_enabled vi =
    let open Result_syntax in
    let* sunset =
      Raw_level.of_int32 (Constants.tx_rollup_sunset_level vi.ctxt)
    in
    error_unless
      (Constants.tx_rollup_enable vi.ctxt
      && Raw_level.(vi.current_level.level < sunset))
      Tx_rollup_feature_disabled

  let assert_sc_rollup_feature_enabled vi =
    error_unless (Constants.sc_rollup_enable vi.ctxt) Sc_rollup_feature_disabled

  let assert_dal_feature_enabled vi =
    error_unless (Constants.dal_enable vi.ctxt) Dal_errors.Dal_feature_disabled

  let consume_decoding_gas ctxt lexpr =
    record_trace Gas_quota_exceeded_init_deserialize
    @@ (* Fail early if the operation does not have enough gas to
          cover the deserialization cost. We always consider the full
          deserialization cost, independently from the internal state
          of the lazy_expr. Otherwise we might risk getting different
          results if the operation has already been deserialized
          before (e.g. when retrieved in JSON format). Note that the
          lazy_expr is not actually decoded here; its deserialization
          cost is estimated from the size of its bytes. *)
    Script.consume_decoding_gas ctxt lexpr

  let validate_tx_rollup_submit_batch vi remaining_gas content =
    let open Result_syntax in
    let* () = assert_tx_rollup_feature_enabled vi in
    let size_limit = Constants.tx_rollup_hard_size_limit_per_message vi.ctxt in
    let _message, message_size = Tx_rollup_message.make_batch content in
    let* cost = Tx_rollup_gas.hash_cost message_size in
    let* remaining_gas = Gas.consume_from remaining_gas cost in
    let* () =
      error_unless
        Compare.Int.(message_size <= size_limit)
        Tx_rollup_errors.Message_size_exceeds_limit
    in
    return remaining_gas

  let validate_tx_rollup_dispatch_tickets vi remaining_gas operation =
    let open Result_syntax in
    let* () = assert_tx_rollup_feature_enabled vi in
    let (Tx_rollup_dispatch_tickets {tickets_info; message_result_path; _}) =
      operation
    in
    let Constants.Parametric.
          {max_messages_per_inbox; max_withdrawals_per_batch; _} =
      Constants.tx_rollup vi.ctxt
    in
    let* () =
      Tx_rollup_errors.check_path_depth
        `Commitment
        (Tx_rollup_commitment.Merkle.path_depth message_result_path)
        ~count_limit:max_messages_per_inbox
    in
    let* () =
      error_when
        Compare.List_length_with.(tickets_info = 0)
        Tx_rollup_errors.No_withdrawals_to_dispatch
    in
    let* () =
      error_when
        Compare.List_length_with.(tickets_info > max_withdrawals_per_batch)
        Tx_rollup_errors.Too_many_withdrawals
    in
    record_trace
      Gas_quota_exceeded_init_deserialize
      (List.fold_left_e
         (fun remaining_gas Tx_rollup_reveal.{contents; ty; _} ->
           let* remaining_gas =
             Script.consume_decoding_gas remaining_gas contents
           in
           Script.consume_decoding_gas remaining_gas ty)
         remaining_gas
         tickets_info)

  let validate_tx_rollup_rejection vi operation =
    let open Result_syntax in
    let* () = assert_tx_rollup_feature_enabled vi in
    let (Tx_rollup_rejection
          {message_path; message_result_path; previous_message_result_path; _})
        =
      operation
    in
    let Constants.Parametric.{max_messages_per_inbox; _} =
      Constants.tx_rollup vi.ctxt
    in
    let* () =
      Tx_rollup_errors.check_path_depth
        `Inbox
        (Tx_rollup_inbox.Merkle.path_depth message_path)
        ~count_limit:max_messages_per_inbox
    in
    let* () =
      Tx_rollup_errors.check_path_depth
        `Commitment
        (Tx_rollup_commitment.Merkle.path_depth message_result_path)
        ~count_limit:max_messages_per_inbox
    in
    Tx_rollup_errors.check_path_depth
      `Commitment
      (Tx_rollup_commitment.Merkle.path_depth previous_message_result_path)
      ~count_limit:max_messages_per_inbox

  let validate_contents (type kind) vi batch_state
      (contents : kind Kind.manager contents) =
    let open Lwt_result_syntax in
    let (Manager_operation
          {source; fee; counter = _; operation; gas_limit; storage_limit}) =
      contents
    in
    let*? remaining_block_gas =
      check_gas_limit_and_consume_from_block_gas
        vi
        ~remaining_block_gas:batch_state.remaining_block_gas
        ~gas_limit
    in
    let*? remaining_gas =
      record_trace
        Insufficient_gas_for_manager
        (Gas.consume_from
           (Gas.Arith.fp gas_limit)
           Michelson_v1_gas.Cost_of.manager_operation)
    in
    let*? () = check_storage_limit vi storage_limit in
    let*? () =
      (* {!Contract.must_be_allocated} has already been called while
         initializing [batch_state]. This checks that the contract has
         not been emptied by spending fees for previous operations in
         the batch. *)
      error_unless
        batch_state.is_allocated
        (Contract_storage.Empty_implicit_contract source)
    in
    let*? (_remaining_gas : Gas.Arith.fp) =
      let open Result_syntax in
      match operation with
      | Reveal pk ->
          let* () = Contract.check_public_key pk source in
          return remaining_gas
      | Transaction {parameters; _} ->
          consume_decoding_gas remaining_gas parameters
      | Origination {script; _} ->
          let* remaining_gas = consume_decoding_gas remaining_gas script.code in
          consume_decoding_gas remaining_gas script.storage
      | Register_global_constant {value} ->
          consume_decoding_gas remaining_gas value
      | Delegation _ | Set_deposits_limit _ | Increase_paid_storage _ ->
          return remaining_gas
      | Tx_rollup_origination ->
          let* () = assert_tx_rollup_feature_enabled vi in
          return remaining_gas
      | Tx_rollup_submit_batch {content; _} ->
          validate_tx_rollup_submit_batch vi remaining_gas content
      | Tx_rollup_commit _ | Tx_rollup_return_bond _
      | Tx_rollup_finalize_commitment _ | Tx_rollup_remove_commitment _ ->
          let* () = assert_tx_rollup_feature_enabled vi in
          return remaining_gas
      | Transfer_ticket {contents; ty; _} ->
          let* () = assert_tx_rollup_feature_enabled vi in
          let* remaining_gas = consume_decoding_gas remaining_gas contents in
          consume_decoding_gas remaining_gas ty
      | Tx_rollup_dispatch_tickets _ ->
          validate_tx_rollup_dispatch_tickets vi remaining_gas operation
      | Tx_rollup_rejection _ ->
          let* () = validate_tx_rollup_rejection vi operation in
          return remaining_gas
      | Sc_rollup_originate _ | Sc_rollup_add_messages _ | Sc_rollup_cement _
      | Sc_rollup_publish _ | Sc_rollup_refute _ | Sc_rollup_timeout _
      | Sc_rollup_execute_outbox_message _ ->
          let* () = assert_sc_rollup_feature_enabled vi in
          return remaining_gas
      | Sc_rollup_recover_bond _ ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/3063
             Should we successfully precheck Sc_rollup_recover_bond and any
             (simple) Sc rollup operation, or should we add some some checks to make
             the operations Branch_delayed if they cannot be successfully
             prechecked? *)
          let* () = assert_sc_rollup_feature_enabled vi in
          return remaining_gas
      | Sc_rollup_dal_slot_subscribe _ ->
          let* () = assert_sc_rollup_feature_enabled vi in
          let* () = assert_dal_feature_enabled vi in
          return remaining_gas
      | Dal_publish_slot_header {slot} ->
          let* () = Dal_apply.validate_publish_slot_header vi.ctxt slot in
          return remaining_gas
    in
    let* balance, is_allocated =
      Contract.simulate_spending
        vi.ctxt
        ~balance:batch_state.balance
        ~amount:fee
        source
    in
    return {remaining_block_gas; balance; is_allocated}

  (** This would be [fold_left_es (validate_contents vi) batch_state
      contents_list] if [contents_list] were an ordinary [list]. *)
  let rec validate_contents_list :
      type kind.
      validate_operation_info ->
      batch_state ->
      kind Kind.manager contents_list ->
      batch_state tzresult Lwt.t =
   fun vi batch_state contents_list ->
    let open Lwt_result_syntax in
    match contents_list with
    | Single contents -> validate_contents vi batch_state contents
    | Cons (contents, tail) ->
        let* batch_state = validate_contents vi batch_state contents in
        validate_contents_list vi batch_state tail

  (** Return the new value that [remaining_block_gas] should have in
      [validate_operation_state] after the validation of a manager
      operation:

      - In [Block] (ie. block validation or block full construction)
        mode, this value is [batch_state.remaining_block_gas], in which
        the gas from the validated operation has been subtracted.

      - In [Mempool] mode, the [remaining_block_gas] in
        [validate_operation_state] should remain unchanged. Indeed, we
        only want each batch to not exceed the block limit individually,
        without taking other operations into account. *)
  let maybe_update_remaining_block_gas vi vs batch_state =
    match vi.mode with
    | Block -> batch_state.remaining_block_gas
    | Mempool -> vs.manager_state.remaining_block_gas

  let validate_manager_operation vi vs source oph (type kind)
      (operation : kind Kind.manager operation) =
    let open Lwt_result_syntax in
    let*? () =
      (* One-operation-per-manager-per-block restriction (1M).

         We want to check 1M before we call
         {!Contract.check_counter_increment} in
         {!check_batch_sanity_and_find_public_key}. Indeed, if a block
         contains two operations from the same manager, it is more
         relevant to fail the second one with {!Manager_restriction}
         than with {!Contract_storage.Counter_in_the_future}. *)
      match
        Signature.Public_key_hash.Map.find source vs.manager_state.managers_seen
      with
      | None -> Result.return_unit
      | Some conflicting_oph ->
          error (Manager_restriction (source, conflicting_oph))
    in
    let contents_list = operation.protocol_data.contents in
    let* batch_state, source_pk =
      check_sanity_and_find_public_key vi vs contents_list
    in
    let* batch_state = validate_contents_list vi batch_state contents_list in
    let*? () = Operation.check_signature source_pk vi.chain_id operation in
    let managers_seen =
      Signature.Public_key_hash.Map.add
        source
        oph
        vs.manager_state.managers_seen
    in
    let remaining_block_gas =
      maybe_update_remaining_block_gas vi vs batch_state
    in
    return {manager_state = {managers_seen; remaining_block_gas}}
end

let validate_operation (vi : validate_operation_info)
    (vs : validate_operation_state) oph (type kind) (operation : kind operation)
    =
  let open Lwt_result_syntax in
  let* vs =
    match operation.protocol_data.contents with
    | Single (Manager_operation {source; _}) ->
        Manager.validate_manager_operation vi vs source oph operation
    | Cons (Manager_operation {source; _}, _) ->
        Manager.validate_manager_operation vi vs source oph operation
    | Single (Preendorsement _)
    | Single (Endorsement _)
    | Single (Dal_slot_availability _)
    | Single (Seed_nonce_revelation _)
    | Single (Vdf_revelation _)
    | Single (Proposals _)
    | Single (Ballot _)
    | Single (Activate_account _)
    | Single (Double_preendorsement_evidence _)
    | Single (Double_endorsement_evidence _)
    | Single (Double_baking_evidence _)
    | Single (Failing_noop _) ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/2603

           There is no separate validation phase for non-manager
           operations yet: all checks are currently done during
           application in {!Apply}.

           When the validation of other operations is implemented, we
           should also update
           {!TMP_for_plugin.precheck_manager__do_nothing_on_non_manager_op}
           (if has not been removed yet). *)
        return vs
  in
  return (vs, Operation_validated_stamp)

module TMP_for_plugin = struct
  type 'a should_check_signature =
    | Check_signature of 'a operation
    | Skip_signature_check

  let precheck_manager vi vs contents_list should_check_signature =
    let open Lwt_result_syntax in
    let open Manager in
    let* batch_state, source_pk =
      check_sanity_and_find_public_key vi vs contents_list
    in
    let* _batch_state = validate_contents_list vi batch_state contents_list in
    let*? () =
      match should_check_signature with
      | Check_signature operation ->
          Operation.check_signature source_pk vi.chain_id operation
      | Skip_signature_check -> ok ()
    in
    return Operation_validated_stamp

  let precheck_manager__do_nothing_on_non_manager_op ctxt chain_id (type kind)
      (contents_list : kind contents_list) should_check_signature =
    let handle_manager (type a) (contents_list : a Kind.manager contents_list) =
      let vi, vs = init_info_and_state ctxt Mempool chain_id in
      precheck_manager vi vs contents_list should_check_signature
    in
    match contents_list with
    | Single (Manager_operation _) -> handle_manager contents_list
    | Cons (Manager_operation _, _) -> handle_manager contents_list
    | Single (Preendorsement _)
    | Single (Endorsement _)
    | Single (Dal_slot_availability _)
    | Single (Seed_nonce_revelation _)
    | Single (Vdf_revelation _)
    | Single (Proposals _)
    | Single (Ballot _)
    | Single (Activate_account _)
    | Single (Double_preendorsement_evidence _)
    | Single (Double_endorsement_evidence _)
    | Single (Double_baking_evidence _)
    | Single (Failing_noop _) ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/2603

           This should be updated when {!validate_operation} is
           implemented on non-manager operations. (Alternatively, this
           function might be removed first:
           https://gitlab.com/tezos/tezos/-/issues/3245) *)
        return Operation_validated_stamp
end
