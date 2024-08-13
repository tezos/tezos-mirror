(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2024 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Node version information.

 This module provides several information regarding the node's version:
 - version: current version number
 - network_version: current version of the network
 - commit_info (optional): hash and date of the head commit
*)

type t = {
  version : Version.t;
  network_version : Network_version.t;
  commit_info : commit_info option;
}

and commit_info = {commit_hash : string; commit_date : string}

(** the namespace used for the node metrics *)
val namespace : string

val version_encoding : Version.t Data_encoding.t

val commit_info_encoding : commit_info Data_encoding.t

val commit_info_pp : Format.formatter -> commit_info -> unit

val commit_info_pp_short : Format.formatter -> commit_info -> unit

val commit_info_equivalent : commit_info -> commit_info -> bool

val encoding : t Data_encoding.t

(** [partially_compare v1 c1 v2 c2] is similar to compare like function but
    returns [None] when versions are not comparable.
    If [v1], [c1] and [v2], [c2] are comparable and 1 is older than 2, [Some x]
    with x<0 is returned.

    [v1], [c1] and [v2], [c2] are comparable if:
    - they have the same product and
      - they are [Beta], [RC] or [Release] or
      - they are [Dev], [Beta_dev] or [RC_dev] and
        - have commit info and
        - version and commit info are equal.
    If [v1] and [v2] are equal, but [c1] and [c2] are not then they are not
    comparable.

    [c1] and [c2] are used only for equality. If the hash of [c1] or [c2] is a
    prefix or equal to the other one, they are considered as equal.

    To determine which version is more recent or old:
    - [major] has the priority on [minor]
    - [minor] has priority on [additional_info]
    - [Beta] is older than [RC]
    - [RC] is older than [Release]
*)
val partially_compare :
  Version.t ->
  commit_info option ->
  Version.t ->
  commit_info option ->
  int option
