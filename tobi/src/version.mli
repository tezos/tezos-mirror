(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Component versions. *)

(** Component versions.

    - [Dev]: working directory version, with possibly some uncommitted changes.
    - [Old reference]: a Git reference (commit hash, branch name, tag). *)
type t = Dev | Old of string

(** Convert a string into a version.

    Parses ["dev"] as [Dev] and the rest as [Old]. *)
val parse : string -> t

(** Convert a version into a string.

    [parse (show v)] is always equal to [v]. *)
val show : t -> string

(** Test equality between two versions. *)
val equal : t -> t -> bool

(** Current committed version, i.e. [Old "HEAD"]. *)
val head : t
