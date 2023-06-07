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

module P = Octez_bls12_381_polynomial.Polynomial

let polynomial_gen =
  QCheck.Gen.(sized (fun n -> return @@ P.generate_biased_random_polynomial n))

let polynomial =
  QCheck.make polynomial_gen (* ~print:print_tree ~shrink:shrink_tree *)

let count = 100

let commutative name op =
  QCheck.Test.make
    ~count
    ~name:(name ^ "_commutative")
    QCheck.(pair polynomial polynomial)
    (fun (p1, p2) -> P.(equal (op p1 p2) (op p2 p1)))

let mul_distribute_add =
  QCheck.Test.make
    ~count
    ~name:"mul_distibute_add"
    QCheck.(triple polynomial polynomial polynomial)
    (fun (p1, p2, p3) -> P.(equal ((p1 + p2) * p3) ((p1 * p3) + (p2 * p3))))

let identity name op el =
  QCheck.Test.make ~count ~name:(name ^ "_identity") polynomial (fun p ->
      P.(equal p (op p el)))

let absorbing name op el =
  QCheck.Test.make ~count ~name:(name ^ "_absorbing") polynomial (fun p ->
      P.(equal el (op p el)))

let tests =
  List.map
    (QCheck_alcotest.to_alcotest ~long:false)
    [
      commutative "add" P.add;
      commutative "mul" P.mul;
      mul_distribute_add;
      identity "add" P.add P.zero;
      identity "sub" P.sub P.zero;
      identity "mul" P.mul P.one;
      absorbing "add" P.mul P.zero;
    ]
