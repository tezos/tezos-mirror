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
          Alcotest.run ~verbose:false "PlonK"
            [
              ("Utils", Test_utils.tests);
              ("Evaluations", Test_evaluations.tests);
              ("Plonk_Pack", Test_pack.tests);
              ("Polynomial Commitment", Test_polynomial_commitment.tests);
              ("Polynomial_protocol", Test_polynomial_protocol.tests);
              ("Permutations", Test_permutations.tests);
              ("Plookup", Test_plookup.tests);
              ("Range_Checks", Test_range_checks.tests);
              ("Main_Protocol", Test_main_protocol.tests);
              ("Circuit", Test_circuit.tests);
            ]))
