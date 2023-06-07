(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2021 Danny Willems <be.danny.willems@gmail.com>             *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    lib_mec
    Invocation:   dune exec src/lib_mec/test/main.exe \
                  -- --file test_linear_trick.ml
    Subject:      Test lib mec
*)

module Fp = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

let test_poseidon128_batch4_optim () =
  let width = 3 in
  let batch_size = 4 in
  let r_f = 8 in
  let r_p = 56 in
  let arc = Array.map Fp.of_string Ark_poseidon128.v in
  (* We format the MDS in a matrix. *)
  let mds = Array.map (fun a -> Array.map Fp.of_string a) Mds_poseidon128.v in
  let _, constants, _, _unbatched_arc =
    Mec.Permutation.HadesLinearOptimisation.compute_updated_constants
      (module Fp)
      r_p
      r_f
      width
      batch_size
      arc
      mds
  in
  (* 30 comes from the fact that we create 3 tmp variables, needing 3, 4 and 5
     vars (as they are recursively computed) and a state of 3 vars needing the 3
     inputs and 3 tmp vars: 3 + 4 + 5 + 3*6 = 30.
     6 comes from the round constants needed to compute the former 6 tmp and output vars.
  *)
  let nb_constants_expected = (6 + 30) * r_p / batch_size in
  let constants_expected =
    Array.map Fp.of_string Poseidon128_linear_trick_expected_output.v
  in
  assert (Array.length constants = nb_constants_expected) ;
  assert (Array.for_all2 Fp.eq constants constants_expected)

let () =
  Alcotest.run
    ~__FILE__
    "Linear trick"
    [
      ( "Regression test",
        [Alcotest.test_case "Poseidon128" `Quick test_poseidon128_batch4_optim]
      );
    ]
