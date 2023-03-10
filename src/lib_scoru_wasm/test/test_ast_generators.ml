(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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
    Component:    Tree_encoding
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_ast_generators.ml
    Subject:      Encoding tests for the tezos-scoru-wasm library
*)

open Tztest

let qcheck ?print gen f =
  let open Lwt_result_syntax in
  let test =
    QCheck2.Test.make ?print gen (fun x -> Result.is_ok @@ Lwt_main.run (f x))
  in
  let res = QCheck_base_runner.run_tests [test] in
  if res = 0 then return_unit else failwith "QCheck tests failed"

let print = Format.asprintf "%a" Ast_printer.pp_module

(** A simple test that checks that generating and printing a module doesn't
    throw an exception. *)
let test_gen_print_module () =
  let open Lwt_result_syntax in
  qcheck ~print (Ast_generators.module_gen ()) (fun module_ ->
      let _ = print module_ in
      return_unit)

let tests = [tztest "Module" `Quick test_gen_print_module]

let () =
  Alcotest_lwt.run ~__FILE__ "test lib scoru wasm" [("AST Generators", tests)]
  |> Lwt_main.run
