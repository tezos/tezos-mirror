(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Helpers to check expected errors. *)

open Protocol
open Alpha_context
open Validate_errors.Manager

let check_error_constructor_name ~loc ~expected errs =
  Assert.expect_error ~loc errs (function
    | [err] ->
        let err_constructor_name =
          Stdlib.Obj.Extension_constructor.(of_val err |> name)
        in
        let expected_constructor_name =
          Stdlib.Obj.Extension_constructor.(of_val expected |> name)
        in
        Compare.String.(err_constructor_name = expected_constructor_name)
    | _ -> false)

(** Identifies the [Inconsistent_sources] error. *)
let check_inconsistent_sources ~first_source ~source = function
  | [Inconsistent_sources {expected_source; source = s}] ->
      Signature.Public_key_hash.(
        expected_source = Account.pkh_of_contract_exn first_source
        && s = Account.pkh_of_contract_exn source)
  | _ -> false

(** To be used as the [expect_failure] argument of
    {!Incremental.add_operation} when expecting the
    [Inconsistent_sources] error. *)
let expect_inconsistent_sources ~loc ~first_source ~source errs =
  Assert.expect_error
    ~loc
    errs
    (check_inconsistent_sources ~first_source ~source)

(** Identifies the [Inconsistent_counters] error. *)
let check_inconsistent_counters ~source ~previous_counter ~counter = function
  | [Inconsistent_counters {source = s; previous_counter = pc; counter = c}] ->
      Signature.Public_key_hash.(s = Account.pkh_of_contract_exn source)
      && Manager_counter.(c = counter && pc = previous_counter)
  | _ -> false

(** To be used as the [expect_failure] argument of
    {!Incremental.add_operation} when expecting the
    [Inconsistent_counters] error. *)
let expect_inconsistent_counters ~loc ~source ~previous_counter ~counter errs =
  Assert.expect_error
    ~loc
    errs
    (check_inconsistent_counters ~source ~previous_counter ~counter)

(** Same as {!expect_inconsistent_counters} but with [int] arguments
    for counters. *)
let expect_inconsistent_counters_int ~loc ~source ~previous_counter ~counter
    errs =
  let counter = Manager_counter.Internal_for_tests.of_int counter in
  let previous_counter =
    Manager_counter.Internal_for_tests.of_int previous_counter
  in
  expect_inconsistent_counters ~loc ~source ~previous_counter ~counter errs

(** To be used as the [expect_failure] argument of
    {!Incremental.add_operation} when expecting the
    [Incorrect_reveal_position] error. *)
let expect_incorrect_reveal_position ~loc errs =
  Assert.expect_error ~loc errs (function
    | [Incorrect_reveal_position] -> true
    | _ -> false)

let expect_forbidden_delegate ~loc ~delegate errs =
  Assert.expect_error ~loc errs (function
    | [Validate_errors.Consensus.Forbidden_delegate d] ->
        Signature.Public_key_hash.(d = Account.pkh_of_contract_exn delegate)
    | _ -> false)

let expect_outdated_denunciation ~loc ?kind ?level ?last_cycle errs =
  Assert.expect_error ~loc errs (function
    | [
        Validate_errors.Anonymous.Outdated_denunciation
          {kind = k; level = l; last_cycle = c};
      ] -> (
        (match kind with
        | Some kind ->
            Protocol.Alpha_context.Misbehaviour.compare_kind kind k = 0
        | None -> true)
        && (match level with
           | Some level -> Protocol.Alpha_context.Raw_level.equal level l
           | None -> true)
        &&
        match last_cycle with
        | Some last_cycle -> Cycle.equal last_cycle c
        | None -> true)
    | _ -> false)

let expect_outdated_denunciation_state ~loc ~state errs =
  let ds = state.State.double_signings in
  let ds = match ds with [a] -> a | _ -> assert false in
  let level =
    Protocol.Alpha_context.Raw_level.Internal_for_tests.from_repr
      ds.misbehaviour.level
  in
  let last_cycle =
    Cycle.add
      (Block.current_cycle_of_level
         ~blocks_per_cycle:state.State.constants.blocks_per_cycle
         ~current_level:(Protocol.Raw_level_repr.to_int32 ds.misbehaviour.level))
      Protocol.Constants_repr.denunciation_period
  in
  let (kind : Protocol.Alpha_context.Misbehaviour.kind) =
    (* This conversion would not be needed if
       Misbehaviour_repr.kind were moved to a
       separate file that doesn't have under/over
       Alpha_context versions. *)
    match ds.misbehaviour.kind with
    | Double_baking -> Double_baking
    | Double_attesting -> Double_attesting
    | Double_preattesting -> Double_preattesting
  in
  expect_outdated_denunciation ~loc ~kind ~level ~last_cycle errs

let expect_no_slots_found_for ~loc ~pkh err =
  Assert.error ~loc (Error err) (function
    | Block.No_slots_found_for p -> Signature.Public_key_hash.(p = pkh)
    | _ -> false)

let expect_empty_transaction ~loc ~contract errs =
  Assert.expect_error ~loc errs (function
    | [Apply.Empty_transaction c] -> Contract.(contract = c)
    | _ -> false)

let expect_balance_too_low ~loc errs =
  Assert.expect_error ~loc errs (function
    | [Contract_storage.Balance_too_low _; Tez_repr.Subtraction_underflow _] ->
        true
    | _ -> false)

(** [expect_failwith str err] checks whether [err] is a failwith error containing
    a string that matches the regular expression [str].

    For example, in case of a failure, [Assert.equal a b] prints a string saying
    that [a] is not equal to [b], replacing [a] and [b] with their actual values.
    In this case, [Str.regexp ".*\n.*is not equal to.*"] is constructed to check
    for such kind of errors. *)
let expect_failwith ~loc ?str err =
  Assert.error ~loc (Error err) (function
    | Exn (Failure s) -> (
        match str with Some str -> Str.string_match str s 0 | None -> true)
    | _ -> false)

let expect_empty_implicit_delegated_contract ~loc ~contract errs =
  Assert.expect_error ~loc errs (function
    | [Contract_storage.Empty_implicit_delegated_contract c] ->
        Signature.Public_key_hash.(c = Account.pkh_of_contract_exn contract)
    | _ -> false)

let expect_incorrect_bls_proof ~loc ~kind_pk ~pk errs =
  Assert.expect_error ~loc errs (function
    | [Incorrect_bls_proof {kind = err_kind; public_key = err_pk; proof = _}] ->
        err_kind = kind_pk && Signature.Public_key.(err_pk = pk)
    | _ -> false)

let expect_unused_bls_proof ~loc ~kind_pk errs =
  Assert.expect_error ~loc errs (function
    | [Unused_bls_proof {kind = err_kind; source = _; public_key = _}] ->
        err_kind = kind_pk
    | _ -> false)

let expect_missing_bls_proof ~loc ~kind_pk ~pk ~source_pkh errs =
  Assert.expect_error ~loc errs (function
    | [
        Missing_bls_proof
          {kind = err_kind; source = err_source; public_key = err_pk};
      ] ->
        err_kind = kind_pk
        && Signature.Public_key.(err_pk = pk)
        && Signature.Public_key_hash.(err_source = source_pkh)
    | _ -> false)

let invalid_signature = function
  | Operation_repr.Invalid_signature -> true
  | _ -> false

let conflicting_consensus_operation ?kind = function
  | Validate_errors.Consensus.Conflicting_consensus_operation {kind = kind'; _}
    ->
      Option.fold ~none:true ~some:(fun kind -> kind = kind') kind
  | _ -> false

let aggregate_disabled = function
  | Validate_errors.Consensus.Aggregate_disabled -> true
  | _ -> false

let aggregate_in_mempool = function
  | Validate_errors.Consensus.Aggregate_in_mempool -> true
  | _ -> false

let non_bls_key_in_aggregate = function
  | Validate_errors.Consensus.Non_bls_key_in_aggregate -> true
  | _ -> false

let unaggregated_eligible_attestation ?kind = function
  | Validate_errors.Consensus.Unaggregated_eligible_operation {kind = kind'; _}
    ->
      Option.fold ~none:true ~some:(fun kind -> kind = kind') kind
  | _ -> false

let empty_aggregation_committee = function
  | Validate_errors.Consensus.Empty_aggregation_committee -> true
  | _ -> false

let wrong_slot_used_for_preattestation = function
  | Validate_errors.Consensus.Wrong_slot_used_for_consensus_operation
      {kind = Preattestation} ->
      true
  | _ -> false

let wrong_slot_used_for_attestation = function
  | Validate_errors.Consensus.Wrong_slot_used_for_consensus_operation
      {kind = Attestation} ->
      true
  | _ -> false

let missing_companion_key_for_bls_dal = function
  | Validate_errors.Consensus.Missing_companion_key_for_bls_dal _ -> true
  | _ -> false

let expect_clst_empty_transfer ~loc errs =
  Assert.expect_error ~loc errs (function
    (* CLST is interacted with as a Michelson contract, as such the trace is
       always part of the interpreter error trace. *)
    | Script_interpreter_errors.Runtime_contract_error _
      :: Script_native.CLST_contract.Empty_transfer :: _ ->
        true
    | _ -> false)

let expect_clst_non_empty_transfer ~loc errs =
  Assert.expect_error ~loc errs (function
    (* CLST is interacted with as a Michelson contract, as such the trace is
       always part of the interpreter error trace. *)
    | Script_interpreter_errors.Runtime_contract_error _
      :: Script_native.CLST_contract.Non_empty_transfer _ :: _ ->
        true
    | _ -> false)

let expect_clst_non_implicit_depositer ~loc errs =
  Assert.expect_error ~loc errs (function
    (* CLST is interacted with as a Michelson contract, as such the trace is
       always part of the interpreter error trace. *)
    | Script_interpreter_errors.Runtime_contract_error _
      :: Script_native.CLST_contract.Non_implicit_contract _ :: _ ->
        true
    | _ -> false)

let expect_clst_balance_too_low ~loc errs =
  Assert.expect_error ~loc errs (function
    (* CLST is interacted with as a Michelson contract, as such the trace is
       always part of the interpreter error trace. *)
    | Script_interpreter_errors.Runtime_contract_error _
      :: Script_native.CLST_contract.Balance_too_low _ :: _ ->
        true
    | _ -> false)

let expect_clst_only_owner_can_change_operator ~loc errs =
  Assert.expect_error ~loc errs (function
    (* CLST is interacted with as a Michelson contract, as such the trace is
       always part of the interpreter error trace. *)
    | Script_interpreter_errors.Runtime_contract_error _
      :: Script_native.CLST_contract.Only_owner_can_change_operator _ :: _ ->
        true
    | _ -> false)

let expect_tz5_account_disabled ~loc errs =
  Assert.expect_error ~loc errs (function
    | [Validate_errors.Manager.Tz5_account_disabled] -> true
    | _ -> false)
