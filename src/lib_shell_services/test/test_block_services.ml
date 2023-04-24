(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Dailambda, Inc. <contact@dailambda.jp>                 *)
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

(** Testing
    -------
    Component:    Block_services
    Invocation:   dune exec src/lib_shell_services/test/main.exe \
                  -- --file test_block_services.ml
    Subject:      Unit tests for [Block_services]
*)

module Proof = Tezos_context_sigs.Context.Proof_types

let make_directory n f =
  let rec aux acc n =
    if n <= 0 then acc
    else aux (String.Map.add (string_of_int n) (f n) acc) (n - 1)
  in
  Proof.Dir (aux String.Map.empty n)

(** Check that JSON-encoding for a large directory never stack-overflows.
    This test fails for json-data-encoding.0.9.1 and older. *)
let test_json_encoding_of_large_directory () =
  let dir = make_directory 1_000_000 (fun _ -> Proof.Cut) in
  let _ =
    Data_encoding.Json.construct Block_services.raw_context_encoding dir
  in
  ()

let tests : (string * _) list =
  [("json-encoding of large directory", test_json_encoding_of_large_directory)]

let tests = List.map (fun (s, f) -> Alcotest.test_case s `Quick f) tests

let () =
  Alcotest.run ~__FILE__ "tezos-lib-shell-services" [("block-services", tests)]
