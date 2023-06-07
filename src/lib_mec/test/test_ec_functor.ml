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
                  -- --file test_ec_functor.ml
    Subject:      Test lib mec
*)

let test_b_cannot_be_null_for_weierstrass_form () =
  let module Fq = Ff.MakeFp (struct
    let prime_order = Z.of_int 3
  end) in
  try
    let module E =
      Mec.Curve.Utils.Functor.MakeProjectiveWeierstrass (Fq) (Fq)
        (struct
          let a = Fq.random ()

          let b = Fq.zero

          let cofactor = Z.one

          let bytes_generator = Bytes.make (Fq.size_in_bytes * 3) '\000'
        end)
    in
    assert false
  with _ -> assert true

let () =
  Alcotest.run
    ~__FILE__
    "Curve functors"
    [
      ( "Check initialisation strengthen conditions",
        [
          Alcotest.test_case
            "b cannot be null"
            `Quick
            test_b_cannot_be_null_for_weierstrass_form;
        ] );
    ]
