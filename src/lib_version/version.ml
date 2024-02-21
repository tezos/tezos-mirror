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

type additional_info = Tezos_version_parser.additional_info =
  | Dev
  | Beta of int
  | Beta_dev of int
  | RC of int
  | RC_dev of int
  | Release

type suite = Tezos_version_parser.suite = Octez | Etherlink

type t = Tezos_version_parser.t = {
  suite : suite;
  major : int;
  minor : int;
  additional_info : additional_info;
}

let parse_version s = Tezos_version_parser.version_tag (Lexing.from_string s)

let string_of_additional_info = function
  | Dev -> "+dev"
  | Beta n -> Format.asprintf "~beta%d" n
  | Beta_dev n -> Format.asprintf "~beta%d+dev" n
  | RC n -> Format.asprintf "~rc%d" n
  | RC_dev n -> Format.asprintf "~rc%d+dev" n
  | Release -> ""

let pp f {suite; major; minor; additional_info} =
  Format.fprintf
    f
    "%s%i.%i%s"
    (match suite with Octez -> "" | Etherlink -> "etherlink-")
    major
    minor
    (string_of_additional_info additional_info)

let to_string x = Format.asprintf "%a" pp x
