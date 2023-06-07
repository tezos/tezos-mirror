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
                  -- --file test_neptunus.ml
    Subject:      Test lib mec
*)

open Mec.Hash

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

module Poseidon = Neptunus.Make (Scalar)

let test_perm_is_consistent () =
  let x = Array.make Neptunus.Constant.width (Scalar.of_string "17") in
  let y = Array.make Neptunus.Constant.width (Scalar.of_string "17") in
  let z = Array.make Neptunus.Constant.width (Scalar.of_string "19") in

  let state_x = Poseidon.Strategy.init x in
  let state_y = Poseidon.Strategy.init y in
  let state_z = Poseidon.Strategy.init z in

  Poseidon.Strategy.apply_perm state_x ;
  Poseidon.Strategy.apply_perm state_y ;
  Poseidon.Strategy.apply_perm state_z ;

  let res_x = Poseidon.Strategy.get state_x in
  let res_y = Poseidon.Strategy.get state_y in
  let res_z = Poseidon.Strategy.get state_z in
  assert (Array.for_all2 Scalar.eq res_x res_y) ;
  assert (not @@ Array.for_all2 Scalar.eq res_x res_z)

let () =
  Alcotest.run
    ~__FILE__
    "Neptunus"
    [
      ( "Properties",
        [Alcotest.test_case "Perm is consistent" `Quick test_perm_is_consistent]
      );
    ]
