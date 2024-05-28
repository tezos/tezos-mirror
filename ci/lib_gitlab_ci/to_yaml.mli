(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** The YAML representation of a GitLab CI configuration. *)
val to_yaml : Types.config -> Yaml.value

(** Writes the YAML representation of a GitLab CI configuration to a file.

    If set, [?header] is prepended to the file. *)
val to_file : ?header:string -> filename:string -> Types.config -> unit
