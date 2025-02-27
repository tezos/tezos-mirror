(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Opam file format parser. *)

open Misc

(** Version terms for dependency conditions.

    [Version_var] denotes the variable [version], typically used as [{= version}].

    [Version_value] denotes a version constant, such as ["1.0"]. *)
type version_term = Version_var | Version_value of string

(** Comparison operators.

    - [EQ]: [=]
    - [NEQ]: [!=]
    - [GEQ]: [>=]
    - [GT]: [>]
    - [LEQ]: [<=]
    - [LT]: [<] *)
type comparison_operator = EQ | NEQ | GEQ | GT | LEQ | LT

(** Dependency conditions.

    This is the part that is between braces next to dependencies,
    such as [{>= "1.0" & with-test}]. *)
type dependency_condition =
  | True
  | With_test
  | Comparison of comparison_operator * version_term
  | And of dependency_condition * dependency_condition
  | Or of dependency_condition * dependency_condition

(** Dependency definitions. *)
type dependency = {name : string; condition : dependency_condition}

(** Items in build instructions.

    [Const] denotes a constant expression, while [Var] denotes a variable
    that opam will interpret (such as [jobs] or [name]). *)
type command_item = Const of string | Var of string

(** Conditions of build instructions.

    This is the part that is between braces next to build instructions,
    such as [{with-test}]. *)
type build_condition = True | With_test

(** A single command of build instructions. *)
type build_instruction = {
  command : command_item;
  arguments : command_item list;
  condition : build_condition;
}

(** Opam package definitions. *)
type t = {
  depends : dependency list;
  depopts : dependency list;
  conflicts : dependency list;
  build : build_instruction list;
}

(** Pretty-print an opam package definition. *)
val pp : t -> PP.t

(** Parse a [.opam] file. *)
val parse_file : string -> (t, [> `failed]) r
