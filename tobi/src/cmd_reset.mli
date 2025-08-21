(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

(** Run the [reset] CLI command. *)
val run : verbose:bool -> dry_run:bool -> (unit, [> `failed] error) result
