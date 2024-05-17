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

(** Identifies the [Inconsistent_sources] error. *)
let check_inconsistent_sources ~first_source ~source = function
  | [Inconsistent_sources {fee_payer; source = s}] ->
      Signature.Public_key_hash.(
        fee_payer = Account.pkh_of_contract_exn first_source
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
    [Guest_operation_wrong_source] error. *)
let expect_guest_operation_wrong_source ~loc ~guest ~unexpected_source errs =
  Assert.expect_error ~loc errs (function
      | [Guest_operation_wrong_source {guest = g; source}] ->
          Signature.Public_key_hash.(
            g = Account.pkh_of_contract_exn guest
            && source = Account.pkh_of_contract_exn unexpected_source)
      | _ -> false)
