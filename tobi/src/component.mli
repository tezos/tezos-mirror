(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Component definitions. *)

open Misc

(** Component dependencies.

    Internal dependencies are dependencies on other components from the same repository.
    External dependencies are regular opam packages. *)
type dependency =
  | Internal of {name : string; version : Version.t; with_test : bool}
  | External of Opam.dependency

(** Component definitions.

    [paths] are read from Tobi's configuration.
    [dependencies] and [build] instructions are read from the opam file. *)
type t = {
  name : string;
  version : Version.t;
  paths : string * string list;
  dependencies : dependency list;
  build : Opam.build_instruction list;
}

(** Load the definition of a component for a given version. *)
val load : string -> Version.t -> (t, [> `failed]) r
