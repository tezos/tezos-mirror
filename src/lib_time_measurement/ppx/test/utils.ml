(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module contains utility functions used to filter filename of input files. *)

(** [has_suffix s suffix] evaluates to true if [suffix] is a suffix of [s]. *)
let has_suffix s suffix =
  let length = String.length s in
  let suffix_length = String.length suffix in
  try
    let actual = String.sub s (length - suffix_length) suffix_length in
    actual = suffix
  with Invalid_argument _ -> false

let%test _ = has_suffix "A string with a suffix" "suffix"

let%test _ = not @@ has_suffix "A simple string" "suffix"

let%test _ = not @@ has_suffix "short string" "A very, very long string"

(** [input_prefix filename] evaluates to the [prefix] of the
    given string [s] if [s] is of the form: ["<prefix><suffix>"]
    given a specific [suffix].
    Else, it will evaluate to [None]. [prefix] can't be empty. *)
let input_prefix s suffix =
  if has_suffix s suffix then
    let length = String.length s in
    let suffix_lenght = String.length suffix in
    let prefix = String.sub s 0 (length - suffix_lenght) in
    if prefix = "" then None else Some prefix
  else None

let%test _ = input_prefix "test_input.ml" "input.ml" = Some "test_"

let%test _ = input_prefix "test_output.ml" "input.ml" = None

let%test _ = input_prefix "a" "input.ml" = None

let%test _ = input_prefix "input.ml" "input.ml" = None

(** [filter xs] filters out the elements of the given list of
    strings [xs] that do not match ["<prefix><suffix>"]
    given a specific [suffix].
    It then evaluates to the list of the [prefix]es of remaining
    elements. Those prefixes will be sorted in lexicographical order. *)
let input_prefixes filenames suffix =
  List.fold_left
    (fun acc filename ->
      match input_prefix filename suffix with
      | Some prefix -> prefix :: acc
      | None -> acc)
    []
    filenames
  |> List.sort String.compare

let%test _ = input_prefixes [] "input.ml" = []

let%test _ =
  input_prefixes
    [
      "cat_input.ml";
      "mushroom";
      "elephant_input.ml";
      "dog_input.ml";
      "dolphin_output.ml";
    ]
    "input.ml"
  = ["cat_"; "dog_"; "elephant_"]

let check_output_existence files input_suffix output_suffix prefix =
  if not @@ List.exists (fun file -> file = prefix ^ output_suffix) files then
    failwith
    @@ Format.sprintf
         "File %s%s detected but %s%s not found. Each input file should gets \
          its own output file."
         prefix
         input_suffix
         prefix
         output_suffix
