(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(* This module is deprecated, use [Qcheck_tezt].
   This module contains functions to wrap QCheck tests into Alcotest.

   It permits to run QCheck tests with Tezt through Alcotest.
   Since there is an ongoing effort to remove Alcotest, it is deprecated. *)

(** Wrap QCheck tests into Alcotest. *)
val qcheck_wrap :
  ?verbose:bool ->
  ?long:bool ->
  ?rand:Random.State.t ->
  QCheck2.Test.t list ->
  unit Alcotest.test_case list

(** Wrap QCheck tests into Alcotest_lwt. *)
val qcheck_wrap_lwt :
  ?verbose:bool ->
  ?long:bool ->
  ?rand:Random.State.t ->
  QCheck2.Test.t list ->
  (string * [`Quick | `Slow] * (unit -> unit Lwt.t)) list
