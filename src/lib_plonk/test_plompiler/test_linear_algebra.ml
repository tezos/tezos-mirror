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

module S = Plompiler.Csir.Scalar
module VS = Plompiler.Linear_algebra.Make_VectorSpace (S)

let random r c = Array.init r (fun _ -> Array.init c (fun _ -> S.random ()))

let sizes = List.init 10 (fun i -> i + 1)

let mk_tests f () = List.iter f sizes

let test_distributive n =
  let a = random (n + 1) n in
  let b = random n (n + 2) in
  let c = random n (n + 2) in
  let m1 = VS.(mul a (add b c)) in
  let m2 = VS.(add (mul a b) (mul a c)) in
  assert (VS.equal m1 m2)

let test_identity n =
  let id_n = VS.identity n in
  let inv = VS.inverse id_n in
  assert (VS.equal id_n inv) ;

  let m = random n n in
  assert (VS.(equal m (mul id_n m))) ;
  assert (VS.(equal m (mul m id_n)))

let test_transpose n =
  let m = random (n + 1) n in
  assert (VS.(equal m @@ transpose (transpose m)))

let test_inverse n =
  let open VS in
  let id_n = identity n in
  let m = random n n in
  let inv = inverse m in
  assert (equal id_n @@ mul m inv) ;
  assert (equal id_n @@ mul inv m) ;

  try
    let _ = inverse @@ zeros n n in
    assert false
  with Invalid_argument _ -> ()

let tests =
  [
    Alcotest.test_case "Distributive" `Quick (mk_tests test_distributive);
    Alcotest.test_case "Identity" `Quick (mk_tests test_identity);
    Alcotest.test_case "Transpose" `Quick (mk_tests test_transpose);
    Alcotest.test_case "Inverse" `Quick (mk_tests test_inverse);
  ]
