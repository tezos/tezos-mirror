(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Type representing supported EVM versions *)
type t = Shanghai | Cancun

(** [string_of_evm_version v] converts an EVM version to its string representation *)
val to_string : t -> string
