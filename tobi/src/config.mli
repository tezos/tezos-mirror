(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Tobi configuration (tobi/config).

    Tobi configuration files define components and the set of files and directories
    (relative to the root of the repository) that are needed to build them.

    Tobi configuration files use the following syntax.
    - Empty lines are ignored.
    - Lines starting with # are ignored.
    - Lines are of the form [key: value1, ..., valueN].
    - [key] is equivalent to [key:], i.e. an empty value.
    - Whitespace around keys and values is ignored.
    - Empty values are ignored, i.e. [key: a, , b] is equivalent to [key: a, b].

    If a key is defined multiple times, the sets of values are concatenated.
    Duplicate values for a single key are ignored.
    The order of values and keys does not matter.

    Keys can be:
    - [__pervasive]: values are paths that are needed to build all components;
    - anything else: the key is a component name, and the values are paths that
      are needed to build this component.

    Each component must need at least one non-pervasive path.

    For instance:
    [{
      # This is a comment.
      __pervasive: some/path, some/other/path
      __pervasive: yet/another/path
      hello: hello/src
      base-libs: base/lib1, base/lib2, base/lib3
    }]
    This configuration file states that:
    - there is a component named [hello], and to build it one needs
      [some/path], [some/other/path], [yet/another/path],
      and [hello/src];
    - there is a component named [base-libs], and to build it one needs
      [some/path], [some/other/path], [yet/another/path],
      [base/lib1], [base/lib2] and [base/lib3]. *)

open Misc

(** Configuration of a component.

    Components have:
    - a [name], which is also the name of the opam package for this component;
    - a list of [paths] that are needed to build the component
      (they can be files or directories, in which case the whole directory is
      fetched to build the component, including subdirectories);
    - [opam]: the path of the [.opam] file for this component, deduced from the [name]. *)
type component = {name : string; paths : string * string list; opam : string}

(** Tobi configuration. *)
type t

(** Get the list of pervasive paths.

    A pervasive path is a path that belong in all components.
    For instance, [dune-project] is typically needed to build all components,
    and can thus be defined as a pervasive path. *)
val pervasive_paths : t -> string list

(** Get the list of components. *)
val components : t -> component list

(** Find a component with a given name. *)
val find_component_by_name : string -> t -> (component, [> `not_found]) r

(** Load the configuration for a given version.

    This fetches file ["tobi/config"] for the given version and parses it.
    In particular this can fetch versions from older commits. *)
val load : Version.t -> (t, [> `failed]) r
