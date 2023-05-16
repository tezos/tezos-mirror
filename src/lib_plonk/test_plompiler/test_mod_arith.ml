(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Plompiler
module CS = Plonk.Circuit
module Helpers = Plonk_test.Helpers.Make (Plonk.Main_protocol)

open Plonk_test.Helpers.Utils (LibCircuit)

module AddMod (L : LIB) = struct
  module ModArith = ArithMod25519 (L)
  open L

  let random_bits len =
    List.init len (fun _ -> if Random.bool () then "1" else "0")
    |> String.concat ""

  let random_mod_int ~modulus () =
    Z.rem (Z.of_bits @@ random_bits (128 + Z.log2 modulus)) modulus

  let add_circuit ~expected xs () =
    let* z_exp = ModArith.input_mod_int ~kind:`Public expected in
    let* xs = L.mapM ModArith.input_mod_int xs in
    let* z = L.foldM ModArith.add (List.hd xs) (List.tl xs) in
    (* Assert equal works here because the default prover uses the
       canonical representation of mod_int. Note that this is a test of
       correctness. For soundness, we would need to deploy a dedicated
       equal assertion for mod_int that ensures that the mod_ints we
       compare are both in canonical form. *)
    assert_equal z z_exp

  let ( ! ) = Z.of_int

  let tests_mod_add =
    let m = ModArith.modulus in
    let r = random_mod_int ~modulus:m () in
    let r' = random_mod_int ~modulus:m () in
    List.map
      (fun (xs, expected) ->
        test ~valid:true ~name:"AddMod.test_mod_add" (add_circuit ~expected xs))
      [
        ([!10; !(-7)], !3);
        ([!15; !5], !20);
        ([!0; !0], !0);
        ([m; m], !0);
        ([!0; r], r);
        ([r; !0], r);
        ([r; r'], Z.(r + r'));
        ([r; Z.neg r], !0);
        ([m; r], r);
        ([Z.(m - r + one); r], !1);
        ([!1; !1; !1; !1; !1], !5);
        ([m; m; m; m], m);
        (Z.[m + !1; m + !2; m + !3; m + !4], !10);
      ]

  let negative_tests_mod_add =
    let modulus = ModArith.modulus in
    List.map
      (fun (xs, expected) ->
        test
          ~valid:false
          ~name:"AddMod.test_mod_add (negative)"
          (add_circuit ~expected xs))
      [([!0; modulus], !1); ([!(-1); !2], !0)]

  let tests = tests_mod_add @ negative_tests_mod_add
end

open Plonk_test.Helpers

let tests =
  [
    Alcotest.test_case "ModAdd" `Quick (to_test (module AddMod : Test));
    Alcotest.test_case
      "ModAdd plonk"
      `Slow
      (to_test ~plonk:(module Plonk.Main_protocol) (module AddMod : Test));
  ]
