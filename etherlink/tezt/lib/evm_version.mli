(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Type representing supported EVM versions *)
type t = Shanghai | Cancun | Prague | Osaka

(** [string_of_evm_version v] converts an EVM version to its string representation *)
val to_string : t -> string

(** [to_solidity_evm_version v] converts an EVM version to its solidity string representation.

    NB: Sometimes the last evm version is available in the kernel but the solidity compiler
    didn't made the release for it, so we need this function to hack the evm version for a given
    version. *)
val to_solidity_evm_version : t -> string

(** [is_pre_cancun v] returns [true] if [v] < [Cancun] and [false] otherwise *)
val is_pre_cancun : t -> bool

(** [max a b] returns the maximum of [a] and [b] *)
val max : t -> t -> t
