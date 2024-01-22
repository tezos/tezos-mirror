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

(** Testing
    -------
    Component:  Lib_bls12_381_polynomial
    Invocation: dune exec src/lib_bls12_381_polynomial/test/main.exe
    Subject:    Test for bls12_381 polynomial
*)

let () =
  (* Seed for deterministic pseudo-randomness:
     If the environment variable RANDOM_SEED is set, then its value is used as
     as seed. Otherwise, a random seed is used.
     WARNING: using [Random.self_init] elsewhere in the tests breaks thedeterminism.
  *)
  let seed =
    match Sys.getenv_opt "RANDOM_SEED" with
    | None ->
        Random.self_init () ;
        Random.int 1073741823
    | Some v -> (
        try int_of_string v
        with _ ->
          failwith
            (Format.sprintf
               "Invalid random seed '%s'. Maybe you need to run '$ unset \
                RANDOM_SEED' in your terminal?"
               v))
  in
  Format.printf "lib_bls12_381_polynomial random seed: %d%!" seed ;
  Random.init seed ;
  Alcotest.run
    ~__FILE__
    "PlonK"
    [
      ("Domains", Test_domains.tests);
      ("Coefficients", Test_coefficients.tests);
      ("Polynomial_c", Test_polynomial.tests);
      ("Evaluations_c", Test_evaluations.tests);
      ("Srs", Test_srs.tests);
      ("PBT", Test_pbt.tests);
    ]
