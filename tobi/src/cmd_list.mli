(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

(** Run the [list] CLI command. *)
val run :
  verbose:bool ->
  installed:bool ->
  Version.t ->
  (unit, [> `failed] error) result
