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

type additional_info = Tezos_version_parser.additional_info =
  | Dev
  | Beta of int
  | Beta_dev of int
  | RC of int
  | RC_dev of int
  | Release

type product = Tezos_version_parser.product =
  | Octez
  | Octez_evm_node
  | Octez_smart_rollup_node

type t = Tezos_version_parser.t = {
  product : product;
  major : int;
  minor : int;
  build : int;
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

let string_of_product = function
  | Octez -> "Octez"
  | Octez_evm_node -> "octez-evm-node"
  | Octez_smart_rollup_node -> "octez-smart-rollup-node"

let pp f {product; major; minor; build; additional_info} =
  Format.fprintf
    f
    "%s %i.%i%s (build: %i)"
    (string_of_product product)
    major
    minor
    (string_of_additional_info additional_info)
    build

let pp_simple f {product = _; major; minor; build = _; additional_info} =
  Format.fprintf
    f
    "%i.%i%s"
    major
    minor
    (string_of_additional_info additional_info)

let pp_arg f {product; major; minor; build = _; additional_info} =
  Format.fprintf
    f
    "%s-%i.%i%s"
    (String.lowercase_ascii (string_of_product product))
    major
    minor
    (string_of_additional_info additional_info)

let to_string x = Format.asprintf "%a" pp x

let to_json {product; major; minor; build; additional_info} commit_hash =
  Format.sprintf
    "{ \"product\": \"%s\", \"major\": \"%i\", \"minor\": \"%i\", \"build\": \
     \"%i\", \"info\": \"%s\", \"hash\": \"%s\" }"
    (string_of_product product)
    major
    minor
    build
    (string_of_additional_info additional_info)
    commit_hash
