(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Plonk_test

let () =
  Helpers.with_seed (fun () ->
      Helpers.with_output_to_file (fun () ->
          Alcotest.run
            "Plompiler"
            [
              ("Utils", Test_utils.tests);
              ("Core", Test_core.tests);
              ("Blake", Test_blake.tests);
              ("Poseidon", Test_poseidon.tests);
              ("Anemoi", Test_anemoi.tests);
              ("Enum", Test_enum.tests);
              ("Schnorr", Test_schnorr.tests);
              ("Merkle", Test_merkle.tests);
              ("Merkle N-arity: Plonk integration", Test_merkle_narity.tests);
              ("Edwards", Test_edwards.tests);
              ("Weierstrass", Test_weierstrass.tests);
              ("Serialization", Test_serialization.tests);
              ("Lookups", Test_lookup.tests);
              ("InputCom", Test_input_com.tests);
              ("Range-checks", Test_range_checks.tests);
              ("Linear algebra", Test_linear_algebra.tests);
              ("Bench", Benchmark.bench);
              ("Bench Poseidon", Bench_poseidon.bench);
              ("Optimizer", Test_optimizer.tests);
              ("Encoding", Test_encoding.tests);
            ]))
