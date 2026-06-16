(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Helpers for tx_kernel tests *)

(** [replace_variables string] sanitizes non-deterministic values in tx_kernel
    debug output (raw rollup address bytes, Ed25519 signatures, ticket IDs,
    operation byte arrays), then applies standard smart rollup sanitizations. *)
val replace_variables : string -> string

(** Hooks that handle hashes specific to tx_kernel debug output. *)
val hooks : Tezt.Process.hooks
