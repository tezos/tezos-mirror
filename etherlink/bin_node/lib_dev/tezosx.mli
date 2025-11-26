(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Additional runtimes which can be enabled alongside the default Ethereum
    runtime in Tezos X. *)
type runtime = Tezos

val runtime_encoding : runtime Data_encoding.t

val runtime_of_string_opt : string -> runtime option

val string_of_runtime : runtime -> string

val pp_runtime : Format.formatter -> runtime -> unit

(** The list of known runtimes which can be enabled in Tezos X. *)
val known_runtimes : runtime list

(** [feature_flag runtime] is the path of the feature flag for [runtime].
    Creating a file at this path in the durable storage of the kernel will
    enable the feature. *)
val feature_flag : runtime -> string
