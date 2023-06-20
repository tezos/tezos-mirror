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
                  -- --file test_curve25519_conversions_between_forms.ml
    Subject:      Test lib mec
*)

open Mec.Curve.Curve25519
module AffineEdwards = Mec.Curve.Curve25519.AffineEdwards

let test_generation () =
  let pt_ed1 = AffineEdwards.random () in
  let pt_mt1 = from_affine_edwards_to_affine_montgomery pt_ed1 in
  assert (Option.(is_some pt_mt1)) ;
  let pt_mt2 = AffineMontgomery.random () in
  let pt_ed2 = from_affine_montgomery_to_affine_edwards pt_mt2 in
  assert (Option.(is_some pt_ed2))

let test_addition () =
  (* Edwards -> Montgomery *)
  let pt_ed1 = AffineEdwards.random () in
  let pt_ed2 = AffineEdwards.random () in
  let pt_ed1_add = AffineEdwards.add pt_ed1 pt_ed2 in
  let pt_mt1 = from_affine_edwards_to_affine_montgomery pt_ed1 |> Option.get in
  let pt_mt2 = from_affine_edwards_to_affine_montgomery pt_ed2 |> Option.get in
  let pt_mt1_add = AffineMontgomery.add pt_mt1 pt_mt2 in
  let pt_ed2_add =
    from_affine_montgomery_to_affine_edwards pt_mt1_add |> Option.get
  in
  let pt_mt2_add =
    from_affine_edwards_to_affine_montgomery pt_ed1_add |> Option.get
  in
  assert (AffineMontgomery.eq pt_mt1_add pt_mt2_add) ;
  assert (AffineEdwards.eq pt_ed1_add pt_ed2_add) ;
  (* Montgomery -> Edwards *)
  let pt_mta = AffineMontgomery.random () in
  let pt_mtb = AffineMontgomery.random () in
  let pt_mta_add = AffineMontgomery.add pt_mta pt_mtb in
  let pt_eda = from_affine_montgomery_to_affine_edwards pt_mta |> Option.get in
  let pt_edb = from_affine_montgomery_to_affine_edwards pt_mtb |> Option.get in
  let pt_eda_add = AffineEdwards.add pt_eda pt_edb in
  let pt_mtb_add =
    from_affine_edwards_to_affine_montgomery pt_eda_add |> Option.get
  in
  let pt_edb_add =
    from_affine_montgomery_to_affine_edwards pt_mta_add |> Option.get
  in
  assert (AffineEdwards.eq pt_eda_add pt_edb_add) ;
  assert (AffineMontgomery.eq pt_mta_add pt_mtb_add)

let test_multiplication () =
  (* Edwards -> Montgomery *)
  let pt_ed1 = AffineEdwards.random () in
  let alpha = AffineEdwards.Scalar.random () in
  let pt_ed1_mul = AffineEdwards.mul pt_ed1 alpha in
  let pt_mt1 = from_affine_edwards_to_affine_montgomery pt_ed1 |> Option.get in
  let alpha = AffineMontgomery.Scalar.of_z (AffineEdwards.Scalar.to_z alpha) in
  let pt_mt1_mul = AffineMontgomery.mul pt_mt1 alpha in
  let pt_ed2_mul =
    from_affine_montgomery_to_affine_edwards pt_mt1_mul |> Option.get
  in
  let pt_mt2_mul =
    from_affine_edwards_to_affine_montgomery pt_ed1_mul |> Option.get
  in
  assert (AffineMontgomery.eq pt_mt1_mul pt_mt2_mul) ;
  assert (AffineEdwards.eq pt_ed1_mul pt_ed2_mul) ;
  (* Montgomery -> Edwards *)
  let pt_mta = AffineMontgomery.random () in
  let beta = AffineMontgomery.Scalar.random () in
  let pt_mta_mul = AffineMontgomery.mul pt_mta beta in
  let pt_eda = from_affine_montgomery_to_affine_edwards pt_mta |> Option.get in
  let beta = AffineEdwards.Scalar.of_z (AffineMontgomery.Scalar.to_z beta) in
  let pt_eda_mul = AffineEdwards.mul pt_eda beta in
  let pt_mtb_mul =
    from_affine_edwards_to_affine_montgomery pt_eda_mul |> Option.get
  in
  let pt_edb_mul =
    from_affine_montgomery_to_affine_edwards pt_mta_mul |> Option.get
  in
  assert (AffineEdwards.eq pt_eda_mul pt_edb_mul) ;
  assert (AffineMontgomery.eq pt_mta_mul pt_mtb_mul)

let get_tests () =
  let open Alcotest in
  ( "Edwards <-> Montgomery",
    [
      test_case
        "Points are on both curves"
        `Quick
        (Mec.Curve.Utils.PBT.repeat 100 test_generation);
      test_case "Addition" `Quick (Mec.Curve.Utils.PBT.repeat 100 test_addition);
      test_case
        "Scalar multiplication"
        `Quick
        (Mec.Curve.Utils.PBT.repeat 100 test_multiplication);
    ] )

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Curve 25519 conversions between the different forms"
    [get_tests ()]
