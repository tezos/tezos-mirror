(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** [only_exe args] returns [true] iff [args] only contains one argument, which is the executable. *)
val only_exe : string list -> bool

(** [is_help_cmd args] returns [true] iff [--help] is amongst [args]. *)
val is_help_cmd : string list -> bool

(** [is_version_cmd args] returns [true] iff [--version] is amongst [args]. *)
val is_version_cmd : string list -> bool

(** [is_man_cmd args] returns [true] iff [man] is amongst [args]. *)
val is_man_cmd : string list -> bool

(** [get_endpoint args] returns the value associated to the [--endpoint] argument
    amongst [args], and in its absence using the default node RPC port. *)
val get_endpoint : string list -> string

(** [get_base_dir args] returns the value associated to the [--base-dir] argument
    amongst [args]. *)
val get_base_dir : string list -> string option
