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
      | [Contract_storage.Balance_too_low _; Tez_repr.Subtraction_underflow _]
        ->
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
