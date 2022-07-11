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

module Double_evidence = Map.Make (struct
  type t = Signature.Public_key_hash.t * Level.t

  let compare (d1, l1) (d2, l2) =
    let res = Signature.Public_key_hash.compare d1 d2 in
    if Compare.Int.equal res 0 then Level.compare l1 l2 else res
end)

(** State used and modified when validating anonymous operations.
    These fields are used to enforce that we do not validate the same
    operation multiple times.

    Note that as part of {!validate_operation_state}, these maps live
    in memory. They are not explicitly bounded here, however:

    - In block validation mode, they are bounded by the number of
    anonymous operations allowed in the block.

    - In mempool mode, bounding the number of operations in this map
    is the responsability of the prevalidator on the shell side. *)
type anonymous_state = {
  blinded_pkhs_seen : Operation_hash.t Blinded_public_key_hash.Map.t;
  double_baking_evidences_seen : Operation_hash.t Double_evidence.t;
  double_consensus_evidences_seen : Operation_hash.t Double_evidence.t;
  seed_nonce_levels_seen : Raw_level.Set.t;
}

let empty_anonymous_state =
  {
    blinded_pkhs_seen = Blinded_public_key_hash.Map.empty;
    double_baking_evidences_seen = Double_evidence.empty;
    double_consensus_evidences_seen = Double_evidence.empty;
    seed_nonce_levels_seen = Raw_level.Set.empty;
  }

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

type validate_operation_state = {
  anonymous_state : anonymous_state;
  manager_state : manager_state;
}

let init_validate_operation_info ctxt mode chain_id =
  {
    ctxt;
    mode;
    chain_id;
    current_level = Level.current ctxt;
    manager_info = init_manager_info ctxt;
  }

let init_validate_operation_state ctxt =
  {
    anonymous_state = empty_anonymous_state;
    manager_state = init_manager_state ctxt;
  }

let init_info_and_state ctxt mode chain_id =
  let vi = init_validate_operation_info ctxt mode chain_id in
  let vs = init_validate_operation_state ctxt in
  (vi, vs)

(* See mli file. *)
type stamp = Operation_validated_stamp

module Anonymous = struct
  open Validate_errors.Anonymous

  let validate_activate_account vi vs oph
      (Activate_account {id = edpkh; activation_code}) =
    let open Lwt_result_syntax in
    let blinded_pkh =
      Blinded_public_key_hash.of_ed25519_pkh activation_code edpkh
    in
    let*? () =
      match
        Blinded_public_key_hash.Map.find
          blinded_pkh
          vs.anonymous_state.blinded_pkhs_seen
      with
      | None -> ok ()
      | Some oph' -> error (Conflicting_activation (edpkh, oph'))
    in
    let*! exists = Commitment.exists vi.ctxt blinded_pkh in
    let*? () = error_unless exists (Invalid_activation {pkh = edpkh}) in
    let blinded_pkhs_seen =
      Blinded_public_key_hash.Map.add
        blinded_pkh
        oph
        vs.anonymous_state.blinded_pkhs_seen
    in
    return
      {vs with anonymous_state = {vs.anonymous_state with blinded_pkhs_seen}}

  let check_denunciation_age vi kind given_level =
    let open Result_syntax in
    let current_cycle = vi.current_level.cycle in
    let given_cycle = (Level.from_raw vi.ctxt given_level).cycle in
    let max_slashing_period = Constants.max_slashing_period vi.ctxt in
    let last_slashable_cycle = Cycle.add given_cycle max_slashing_period in
    let* () =
      error_unless
        Cycle.(given_cycle <= current_cycle)
        (Too_early_denunciation
           {kind; level = given_level; current = vi.current_level.level})
    in
    error_unless
      Cycle.(last_slashable_cycle > current_cycle)
      (Outdated_denunciation
         {kind; level = given_level; last_cycle = last_slashable_cycle})

  let validate_double_consensus (type kind)
      ~consensus_operation:denunciation_kind vi vs oph
      (op1 : kind Kind.consensus Operation.t)
      (op2 : kind Kind.consensus Operation.t) =
    let open Lwt_result_syntax in
    match (op1.protocol_data.contents, op2.protocol_data.contents) with
    | Single (Preendorsement e1), Single (Preendorsement e2)
    | Single (Endorsement e1), Single (Endorsement e2) ->
        let op1_hash = Operation.hash op1 in
        let op2_hash = Operation.hash op2 in
        let*? () =
          error_unless
            (Raw_level.(e1.level = e2.level)
            && Round.(e1.round = e2.round)
            && (not
                  (Block_payload_hash.equal
                     e1.block_payload_hash
                     e2.block_payload_hash))
            && (* we require an order on hashes to avoid the existence of
                  equivalent evidences *)
            Operation_hash.(op1_hash < op2_hash))
            (Invalid_denunciation denunciation_kind)
        in
        (* Disambiguate: levels are equal *)
        let level = Level.from_raw vi.ctxt e1.level in
        let*? () = check_denunciation_age vi denunciation_kind level.level in
        let* ctxt, (delegate1_pk, delegate1) =
          Stake_distribution.slot_owner vi.ctxt level e1.slot
        in
        let* ctxt, (_delegate2_pk, delegate2) =
          Stake_distribution.slot_owner ctxt level e2.slot
        in
        let*? () =
          error_unless
            (Signature.Public_key_hash.equal delegate1 delegate2)
            (Inconsistent_denunciation
               {kind = denunciation_kind; delegate1; delegate2})
        in
        let delegate_pk, delegate = (delegate1_pk, delegate1) in
        let*? () =
          match
            Double_evidence.find
              (delegate, level)
              vs.anonymous_state.double_consensus_evidences_seen
          with
          | None -> ok ()
          | Some oph' ->
              error
                (Conflicting_denunciation
                   {kind = denunciation_kind; delegate; level; hash = oph'})
        in
        let* already_slashed =
          Delegate.already_slashed_for_double_endorsing ctxt delegate level
        in
        let*? () =
          error_unless
            (not already_slashed)
            (Already_denounced {kind = denunciation_kind; delegate; level})
        in
        let*? () = Operation.check_signature delegate_pk vi.chain_id op1 in
        let*? () = Operation.check_signature delegate_pk vi.chain_id op2 in
        let double_consensus_evidences_seen =
          Double_evidence.add
            (delegate, level)
            oph
            vs.anonymous_state.double_consensus_evidences_seen
        in
        return
          {
            vs with
            anonymous_state =
              {vs.anonymous_state with double_consensus_evidences_seen};
          }

  let validate_double_preendorsement_evidence vi vs oph
      (Double_preendorsement_evidence {op1; op2}) =
    validate_double_consensus
      ~consensus_operation:Preendorsement
      vi
      vs
      oph
      op1
      op2

  let validate_double_endorsement_evidence vi vs oph
      (Double_endorsement_evidence {op1; op2}) =
    validate_double_consensus ~consensus_operation:Endorsement vi vs oph op1 op2

  let validate_double_baking_evidence vi vs oph
      (Double_baking_evidence {bh1; bh2}) =
    let open Lwt_result_syntax in
    let hash1 = Block_header.hash bh1 in
    let hash2 = Block_header.hash bh2 in
    let*? bh1_fitness = Fitness.from_raw bh1.shell.fitness in
    let round1 = Fitness.round bh1_fitness in
    let*? bh2_fitness = Fitness.from_raw bh2.shell.fitness in
    let round2 = Fitness.round bh2_fitness in
    let*? level1 = Raw_level.of_int32 bh1.shell.level in
    let*? level2 = Raw_level.of_int32 bh2.shell.level in
    let*? () =
      error_unless
        (Raw_level.(level1 = level2)
        && Round.(round1 = round2)
        && (* we require an order on hashes to avoid the existence of
              equivalent evidences *)
        Block_hash.(hash1 < hash2))
        (Invalid_double_baking_evidence
           {hash1; level1; round1; hash2; level2; round2})
    in
    let*? () = check_denunciation_age vi Block level1 in
    let level = Level.from_raw vi.ctxt level1 in
    let committee_size = Constants.consensus_committee_size vi.ctxt in
    let*? slot1 = Round.to_slot round1 ~committee_size in
    let* ctxt, (delegate1_pk, delegate1) =
      Stake_distribution.slot_owner vi.ctxt level slot1
    in
    let*? slot2 = Round.to_slot round2 ~committee_size in
    let* ctxt, (_delegate2_pk, delegate2) =
      Stake_distribution.slot_owner ctxt level slot2
    in
    let*? () =
      error_unless
        Signature.Public_key_hash.(delegate1 = delegate2)
        (Inconsistent_denunciation {kind = Block; delegate1; delegate2})
    in
    let delegate_pk, delegate = (delegate1_pk, delegate1) in
    let*? () =
      match
        Double_evidence.find
          (delegate, level)
          vs.anonymous_state.double_baking_evidences_seen
      with
      | None -> ok ()
      | Some oph' ->
          error
            (Conflicting_denunciation
               {kind = Block; delegate; level; hash = oph'})
    in
    let* already_slashed =
      Delegate.already_slashed_for_double_baking ctxt delegate level
    in
    let*? () =
      error_unless
        (not already_slashed)
        (Already_denounced {kind = Block; delegate; level})
    in
    let*? () = Block_header.check_signature bh1 vi.chain_id delegate_pk in
    let*? () = Block_header.check_signature bh2 vi.chain_id delegate_pk in
    let double_baking_evidences_seen =
      Double_evidence.add
        (delegate, level)
        oph
        vs.anonymous_state.double_baking_evidences_seen
    in
    return
      {
        vs with
        anonymous_state = {vs.anonymous_state with double_baking_evidences_seen};
      }

  let validate_seed_nonce_revelation vi vs
      (Seed_nonce_revelation {level = commitment_raw_level; nonce}) =
    let open Lwt_result_syntax in
    let commitment_level = Level.from_raw vi.ctxt commitment_raw_level in
    let*? () =
      error_unless
        (not
           (Raw_level.Set.mem
              commitment_raw_level
              vs.anonymous_state.seed_nonce_levels_seen))
        Conflicting_nonce_revelation
    in
    let* () = Nonce.check_unrevealed vi.ctxt commitment_level nonce in
    let seed_nonce_levels_seen =
      Raw_level.Set.add
        commitment_raw_level
        vs.anonymous_state.seed_nonce_levels_seen
    in
    let new_vs =
      {
        vs with
        anonymous_state = {vs.anonymous_state with seed_nonce_levels_seen};
      }
    in
    return new_vs

  let validate_vdf_revelation _vi vs (Vdf_revelation {solution = _}) = return vs
end

module Manager = struct
  open Validate_errors.Manager

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
      (* Note that it is important to always retrieve the public
         key. This includes the case where the key ends up not being
         used because the signature check is skipped in
         {!validate_manager_operation} called with
         [~should_check_signature:false]. Indeed, the mempool may use
         this argument when it has already checked the signature of
         the operation in the past; but if there has been a branch
         reorganization since then, the key might not be revealed in
         the new branch anymore, in which case
         {!Contract.get_manager_key} will return an error. *)
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

  let assert_not_zero_messages messages =
    match messages with
    | [] -> error Sc_rollup_errors.Sc_rollup_add_zero_messages
    | _ -> ok ()

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
      | Sc_rollup_originate _ | Sc_rollup_cement _ | Sc_rollup_publish _
      | Sc_rollup_refute _ | Sc_rollup_timeout _
      | Sc_rollup_execute_outbox_message _ ->
          let* () = assert_sc_rollup_feature_enabled vi in
          return remaining_gas
      | Sc_rollup_add_messages {messages; _} ->
          let* () = assert_sc_rollup_feature_enabled vi in
          let* () = assert_not_zero_messages messages in
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

  let validate_manager_operation vi vs ~should_check_signature source oph
      (type kind) (operation : kind Kind.manager operation) =
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
    let*? () =
      if should_check_signature then
        Operation.check_signature source_pk vi.chain_id operation
      else ok ()
    in
    let managers_seen =
      Signature.Public_key_hash.Map.add
        source
        oph
        vs.manager_state.managers_seen
    in
    let remaining_block_gas =
      maybe_update_remaining_block_gas vi vs batch_state
    in
    return {vs with manager_state = {managers_seen; remaining_block_gas}}
end

let validate_operation (vi : validate_operation_info)
    (vs : validate_operation_state) ?(should_check_signature = true) oph
    (type kind) (operation : kind operation) =
  let open Lwt_result_syntax in
  let* vs =
    match operation.protocol_data.contents with
    | Single (Activate_account _ as contents) ->
        Anonymous.validate_activate_account vi vs oph contents
    | Single (Double_preendorsement_evidence _ as contents) ->
        Anonymous.validate_double_preendorsement_evidence vi vs oph contents
    | Single (Double_endorsement_evidence _ as contents) ->
        Anonymous.validate_double_endorsement_evidence vi vs oph contents
    | Single (Double_baking_evidence _ as contents) ->
        Anonymous.validate_double_baking_evidence vi vs oph contents
    | Single (Seed_nonce_revelation _ as contents) ->
        Anonymous.validate_seed_nonce_revelation vi vs contents
    | Single (Vdf_revelation _ as contents) ->
        Anonymous.validate_vdf_revelation vi vs contents
    | Single (Manager_operation {source; _}) ->
        Manager.validate_manager_operation
          vi
          vs
          ~should_check_signature
          source
          oph
          operation
    | Cons (Manager_operation {source; _}, _) ->
        Manager.validate_manager_operation
          vi
          vs
          ~should_check_signature
          source
          oph
          operation
    | Single (Preendorsement _)
    | Single (Endorsement _)
    | Single (Dal_slot_availability _)
    | Single (Proposals _)
    | Single (Ballot _)
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
end
