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
let check_inconsistent_sources ~first_source:_ ~source:_ = function
  | [Inconsistent_sources] -> true
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
let check_inconsistent_counters ~source:_ ~previous_counter:_ ~counter:_ =
  function
  | [Inconsistent_counters] -> true
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
