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

(** Get the cache directory for a component.

    The result is of the form [_tobi/cache/NAME/COMMIT_HASH].

    Fails on [Dev] versions because there is no commit hash for them. *)
val relative_cache_dir : t -> (string, [> `failed] error) result

(** Test whether a component is available in the cache.

    This checks that the [.install] file exists in [relative_cache_dir].
    This thus assumes that the current directory is the root of the repository,
    and that the cache is not brocken. *)
val available_in_cache : t -> (bool, [> `failed] error) result

(** Find whether a component is installed.

    If it is, return the commit hash corresponding to the installed version. *)
val get_installed_commit_hash :
  string -> (string option, [> `failed] error) result
