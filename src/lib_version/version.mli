(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Version information.

    This module provides the current version number. *)

(** Additional version information.

    [Dev] means "Development Version".
    All non-release branches should use this.

    [Beta] refers to a version that is not believed to be stable
    but contains features for the next release.
    For each release, the first beta version has number 1.

    [Beta_dev] refers to a beta version "in development".

    [RC] means "Release Candidate".
    For each release, the first release candidate has number 1.

    [RC_dev] means "Release Candidate in development".
    This is a release branch where the release candidate tag is
    not associated to the HEAD of the branch.

    [Release] means "no additional information".
    This is an actual released version.
    No additional info is printed.

    Documentation on the semantics of version numbers is available
    at [docs/releases/releases.rst]. *)
type additional_info = Tezos_version_parser.additional_info =
  | Dev
  | Beta of int
  | Beta_dev of int
  | RC of int
  | RC_dev of int
  | Release

(** Convert additional version information to a string.

    The result is a string of the form ["+dev"], ["~rcX"], ["~rcX+dev"] or [""]. *)
val string_of_additional_info : additional_info -> string

type suite = Tezos_version_parser.suite = Octez | Etherlink

(** Version information.

    Major versions include significant new features and are usually
    released in new branches which start from master.

    Minor versions include mostly bug fixes and are usually released in
    branches which start from the previous release.
    When the major version is incremented, the minor version is reset to 0. *)
type t = Tezos_version_parser.t = {
  suite : suite;
  major : int;
  minor : int;
  additional_info : additional_info;
}

(** Convert a version to a string.

    Examples:
    - [to_string { suite = Octez; major = 7; minor = 0; additional_info = Release } = "7.0"]
    - [to_string { suite = Octez; major = 7; minor = 0; additional_info = Dev } = "7.0+dev"]
    - [to_string { suite = Octez; major = 7; minor = 0; additional_info = RC 1 } = "7.0~rc1"]
    - [to_string { suite = Octez; major = 7; minor = 0; additional_info = RC_dev 1 } = "7.0~rc1+dev"] *)
val to_string : t -> string

(** Version printer.

    [pp f x] prints [to_string x] in [f] *)
val pp : Format.formatter -> t -> unit

(* Parse an Octez version.

   Returns None if the version cannot be parsed. *)
val parse_version : string -> t option
