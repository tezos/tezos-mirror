(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

(** Run the [install] CLI command. *)
val run :
  verbose:bool -> (string * Version.t) list -> (unit, [> `failed] error) result
