(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** [is_help_cmd args] returns [true] iff [--help] is amongst [args]. *)
val is_help_cmd : string list -> bool

(** [is_version_cmd args] returns [true] iff [--version] is amongst [args]. *)
val is_version_cmd : string list -> bool

(** Simple wrapper to handle the arguments of the agnostic baker. *)
type args = {node_endpoint : string; base_dir : string option}

(** [parse_args args] is a raw utility that aims to parse the given
    arguments from the command line and to return, respectively, the
    [endpoint] and [base_dir]. *)
val parse_args : string array -> args
