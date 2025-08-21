(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

(** Run the [install] CLI command. *)
val run :
  verbose:bool ->
  dry_run:bool ->
  jobs:int ->
  keep_temp:bool ->
  (string * Version.t) list ->
  (unit, [> `failed] error) result
