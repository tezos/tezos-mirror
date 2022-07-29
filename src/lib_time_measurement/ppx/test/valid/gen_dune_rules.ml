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

let output_suffix = "output.ml"

let actual_suffix = "actual.ml"

let output_stanzas prefix =
  let input = prefix ^ input_suffix in
  let output = prefix ^ output_suffix in
  if not (Sys.file_exists output) then
    failwith
    @@ Format.sprintf
         "File %s detected but %s not found. Each input file should gets its \
          own output file."
         input
         output ;
  let actual = prefix ^ actual_suffix in
  let input_module = String.sub input 0 @@ (String.length input - 3) in
  let output_module = String.sub output 0 @@ (String.length output - 3) in
  Format.printf
    {|
; Prepreocesses %s
(rule
 (targets %s)
 (deps (:pp pp.exe) (:input %s))
 (action
   (run ./%%{pp} --impl %%{input} -o %%{targets})))
|}
    input
    actual
    input ;
  Format.printf
    {|
; Compares preprocessed output with expected output
(rule
 (alias runtest)
 (package tezos-time-measurement)
 (action (diff %s %s)))
|}
    output
    actual ;
  Format.printf
    {|
; Ensures that %s compiles
(library
 (name %s)
 (modules %s)
 (preprocess (pps tezos-time-measurement.ppx))
 (libraries lwt)
 (flags (:standard -open Lwt)))
|}
    input
    input_module
    input_module ;
  Format.printf
    {|
; Ensures that %s compiles
(library
 (name %s)
 (modules %s)
 (libraries lwt tezos-time-measurement)
 (flags (:standard -open Lwt)))
|}
    output
    output_module
    output_module

let () =
  Utils.test_files "."
  |> List.filter_map (fun f -> Filename.chop_suffix_opt f ~suffix:input_suffix)
  |> List.iter output_stanzas
