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
    Component:    Lib_scoru_wasm
    Invocation:   dune runtest src/lib_scoru_wasm/
    Subject:      Tests for the tezos-scoru-wasm library
*)

let () =
  Alcotest_lwt.run
    "test lib scoru wasm"
    [
      ("Input", Test_input.tests);
      ("Output", Test_output.tests);
      ("Set/get", Test_get_set.tests);
      ("Durable storage", Test_durable_storage.tests);
      ("AST Generators", Test_ast_generators.tests);
      ("WASM Encodings", Test_wasm_encoding.tests);
      ("WASM PVM Encodings", Test_wasm_pvm_encodings.tests);
      ("Parser Encodings", Test_parser_encoding.tests);
      ("WASM PVM", Test_wasm_pvm.tests);
      ("Module Initialisation", Test_init.tests);
      ("Max nb of ticks", Test_fixed_nb_ticks.tests);
      ("Hash correspondence", Test_hash_consistency.tests);
      ("Reveal", Test_reveal.tests);
    ]
  |> Lwt_main.run
