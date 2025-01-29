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

module Install : sig
  (** Parse Opam [.install] files and give meaning to them. *)

  (** Target directories.

      See https://opam.ocaml.org/doc/Manual.html#lt-pkgname-gt-install
      for the meaning of each directory. *)
  type directory =
    | Lib
    | Lib_root
    | Libexec
    | Libexec_root
    | Bin
    | Sbin
    | Toplevel
    | Share
    | Share_root
    | Etc
    | Doc
    | Stublibs
    | Man
    | Misc

  (** Semantics of a file in a [.install] file.

      [directory] is the target directory where to install the file.

      [source_path] is the path where the original file can be found.

      [target_path_relative_to_prefix] is the path, relative to [_opam],
      where to install the file. It is deduced from [directory], the package name,
      and the extension of [source_path].

      [permissions] is the permissions that the files should have once installed.
      It depends on [directory]. *)
  type file = {
    directory : directory;
    source_path : string;
    target_path_relative_to_prefix : string;
    permissions : int;
  }

  (** Parse an Opam [.install] file. *)
  val parse_file :
    package_name:string -> filename:string -> (file list, [> `failed]) r
end
