(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

(** Run the [build] CLI command. *)
val run :
  verbose:bool ->
  dry_run:bool ->
  jobs:int ->
  string list ->
  (unit, [> `failed] error) result
