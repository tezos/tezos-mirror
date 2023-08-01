(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(**
   Handling of YAML configuration file used by the kernel installer.

   A configuration has the form:
   ```
   instructions:
     - set:
         value: <hexadecimal value>
         to: <string>
     ...
   ```
*)

type error +=
  | Installer_config_yaml_error of string
  | Installer_config_invalid
  | Installer_config_invalid_instruction of int

(* Instructions of the installer configuration. *)
type instr = Set of {value : string; to_ : string}

(* A configuration is a list of instructions. *)
type t = instr list

val pp : Format.formatter -> t -> unit

val instr_encoding : instr Data_encoding.t

val encoding : t Data_encoding.t

(** [parse_yaml content] parses [content] as a YAML representing the
    configuration of the kernel installer, and returns the corresponding set of
    instructions. *)
val parse_yaml : string -> t tzresult

(** [generate_yaml instrs] generates the YAML representation of [instrs]. *)
val generate_yaml : t -> Yaml.yaml tzresult

(** [emit_yaml instrs] generates the YAML representation of [instrs] in textual
    format. *)
val emit_yaml : t -> string tzresult
