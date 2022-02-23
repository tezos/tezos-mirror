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

(** This module is used to generate a dune configuration on standard output.
    Resulting stanzas will be created for each input file of the folder,
    e.g. files with the following suffix: "input.ml". *)

let input_suffix = "input.ml"

let output_suffix = "output"

let actual_suffix = "actual"

let output_stanzas prefix =
  let input = prefix ^ input_suffix in
  let output = prefix ^ output_suffix in
  let actual = prefix ^ actual_suffix in

  Format.printf
    {|
; Prepreocesses %s
(rule
 (targets %s)
 (deps (:pp pp.exe) (:input %s))
 (action
  (with-stderr-to
    %%{targets}
   (bash "./%%{pp} -no-color --impl %%{input} || true"))))

; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff %s %s)))
|}
    input
    actual
    input
    output
    actual

let () =
  let files = Sys.readdir "." |> Array.to_list in
  let prefixes = Utils.input_prefixes files input_suffix in
  List.iter
    (Utils.check_output_existence files input_suffix output_suffix)
    prefixes ;
  List.iter output_stanzas prefixes
