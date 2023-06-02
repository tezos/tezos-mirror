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

module ModArith (L : LIB) = struct
  module ModArith = ArithMod25519 (L)
  open L

  let random_bits len =
    List.init len (fun _ -> if Random.bool () then "1" else "0")
    |> String.concat ""

  let random_mod_int ~modulus () =
    Z.rem (Z.of_bits @@ random_bits (128 + Z.log2 modulus)) modulus

  let ( ! ) = Z.of_int

  let ( %! ) = Z.rem

  let name_suffix valid = if valid then "" else " (negative)"

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

  let tests_mod_add =
    let m = ModArith.modulus in
    let r = random_mod_int ~modulus:m () in
    let r' = random_mod_int ~modulus:m () in
    List.map
      (fun (xs, expected, valid) ->
        let name = "ModArith.test_mod_add" ^ name_suffix valid in
        test ~valid ~name (add_circuit ~expected xs))
      [
        ([!10; !(-7)], !3, true);
        ([!15; !5], !20, true);
        ([!0; !0], !0, true);
        ([m; m], !0, true);
        ([!0; r], r, true);
        ([r; !0], r, true);
        ([r; r'], Z.(r + r'), true);
        ([r; Z.neg r], !0, true);
        ([m; r], r, true);
        ([Z.(m - r + one); r], !1, true);
        ([!1; !1; !1; !1; !1], !5, true);
        ([m; m; m; m], m, true);
        (Z.[m + !1; m + !2; m + !3; m + !4], !10, true);
        ([!0; m], !1, false);
        ([!(-1); !2], !0, false);
      ]

  let sub_circuit ~expected x y () =
    let* z_exp = ModArith.input_mod_int ~kind:`Public expected in
    let* x = ModArith.input_mod_int x in
    let* y = ModArith.input_mod_int y in
    let* z = ModArith.sub x y in
    assert_equal z z_exp

  let tests_mod_sub =
    let m = ModArith.modulus in
    let r = random_mod_int ~modulus:m () in
    List.map
      (fun (x, y, expected, valid) ->
        let name = "ModArith.test_mod_sub" ^ name_suffix valid in
        test ~valid ~name (sub_circuit ~expected x y))
      [
        (r, !0, r, true);
        (r, r, !0, true);
        (!123, !100, !23, true);
        (!1, !(-2), !3, true);
        (r, Z.neg r, Z.(r + r), true);
        (r, m, r, true);
        (m, m, !0, true);
        (!1, !2, Z.(m - !1), true);
        (r, !1, r, false);
        (r, r, !1, false);
      ]

  let neg_circuit ~expected x () =
    let* z_exp = ModArith.input_mod_int ~kind:`Public expected in
    let* x = ModArith.input_mod_int x in
    let* z = ModArith.neg x in
    assert_equal z z_exp

  let tests_mod_neg =
    let m = ModArith.modulus in
    let r = random_mod_int ~modulus:m () in
    List.map
      (fun (x, expected, valid) ->
        let name = "ModArith.test_mod_neg" ^ name_suffix valid in
        test ~valid ~name (neg_circuit ~expected x))
      [
        (r, Z.neg r, true);
        (!0, !0, true);
        (m, !0, true);
        (m, m, true);
        (!1, !(-1), true);
        (Z.(m - !3), !3, true);
        (!1, Z.(m - !1), true);
        (!1, !1, false);
        (Z.(r + !1), Z.neg r, false);
      ]

  let const_circuit ~expected x () =
    let* z_exp = ModArith.input_mod_int ~kind:`Public expected in
    let* z = ModArith.constant x in
    assert_equal z z_exp

  let tests_mod_constant =
    let m = ModArith.modulus in
    List.map
      (fun (x, expected, valid) ->
        let name = "ModArith.test_mod_constant" ^ name_suffix valid in
        test ~valid ~name (const_circuit ~expected x))
      [
        (!0, !0, true);
        (!1, !1, true);
        (m, !0, true);
        (Z.(m + !1), !1, true);
        (Z.((!2 * m) + !2), !2, true);
        (Z.(!3 * (m + !2)), !6, true);
        (!(-1), Z.(m - !1), true);
        (!0, !1, false);
        (m, !1, false);
        (!(-2), Z.(m - !1), false);
      ]

  let mul_circuit ~expected xs () =
    let* z_exp = ModArith.input_mod_int ~kind:`Public expected in
    let* xs = L.mapM ModArith.input_mod_int xs in
    let* z = L.foldM ModArith.mul (List.hd xs) (List.tl xs) in
    assert_equal z z_exp

  let tests_mod_mul =
    let m = ModArith.modulus in
    let r = random_mod_int ~modulus:m () in
    let r' = random_mod_int ~modulus:m () in
    List.map
      (fun (xs, expected, valid) ->
        let name = "ModArith.test_mod_mul" ^ name_suffix valid in
        test ~valid ~name (mul_circuit ~expected xs))
      [
        ([!3; !5], !15, true);
        ([r; r'], Z.(r * r' %! m), true);
        ([!0; !1], !0, true);
        ([!1; !0], !0, true);
        ([m; !1], !0, true);
        ([!1; m], !0, true);
        ([m; m], !0, true);
        ([!1; !1], !1, true);
        ([!1; r], r, true);
        ([!(-1); r], Z.neg r, true);
        ([!1; !2; !3], !6, true);
        ([!1; !2; !3; !4], !24, true);
        ([!1; !2; !3; !4; !5], !120, true);
        ([Z.(m - r); Z.neg r], Z.(r * r), true);
        ([!1; !1; !1; !1; !1], !1, true);
        ([m; !2], !1, false);
        ([!0; !1], !1, false);
        ([!1; !0], !1, false);
        ([!(-1); !2], !2, false);
      ]

  let div_circuit ~expected x y () =
    let* z_exp = ModArith.input_mod_int ~kind:`Public expected in
    let* x = ModArith.input_mod_int x in
    let* y = ModArith.input_mod_int y in
    let* z = ModArith.div x y in
    assert_equal z z_exp

  let tests_mod_div =
    let m = ModArith.modulus in
    let r = random_mod_int ~modulus:m () in
    List.map
      (fun (x, y, expected, valid) ->
        let name = "ModArith.test_mod_div" ^ name_suffix valid in
        test ~valid ~name (div_circuit ~expected x y))
      [
        (r, !1, r, true);
        (r, r, !1, true);
        (Z.(r * r), r, r, true);
        (!10, !(-2), !(-5), true);
        (!91, !13, !7, true);
        (m, r, !0, true);
        (m, !1, m, true);
        (r, !(-1), Z.neg r, true);
        (r, !2, r, false);
        (!0, !1, !1, false);
        (r, !0, !0, false);
      ]

  let inv_circuit ~expected x () =
    let* z_exp = ModArith.input_mod_int ~kind:`Public expected in
    let* x = ModArith.input_mod_int x in
    let* z = ModArith.inv x in
    assert_equal z z_exp

  let tests_mod_inv =
    let m = ModArith.modulus in
    let r = random_mod_int ~modulus:m () in
    let inverse r =
      let d, u, _v = Z.gcdext r m in
      assert (Z.(d = one)) ;
      u
    in
    List.map
      (fun (x, expected, valid) ->
        let name = "ModArith.test_mod_inv" ^ name_suffix valid in
        test ~valid ~name (inv_circuit ~expected x))
      [
        (!1, !1, true);
        (!(-1), !(-1), true);
        (Z.(m - !1), !(-1), true);
        (r, inverse r, true);
        (!1, !(-1), false);
        (!1, !2, false);
        (!0, !0, false);
      ]

  let tests =
    tests_mod_add @ tests_mod_sub @ tests_mod_neg @ tests_mod_constant
    @ tests_mod_mul @ tests_mod_div @ tests_mod_inv
end

open Plonk_test.Helpers

let tests =
  [
    Alcotest.test_case "ModArith" `Quick (to_test (module ModArith : Test));
    Alcotest.test_case
      "ModArith plonk"
      `Slow
      (to_test ~plonk:(module Plonk.Main_protocol) (module ModArith : Test));
  ]
