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

open Plompiler
open Plonk_test
module CS = Plonk.Circuit
open Helpers

module P = struct
  (* https://github.com/dusk-network/jubjub/blob/052ac22bc69403171ad1e32c3332b7510891419a/src/lib.rs#L121 *)

  module Schnorr = Plompiler.Schnorr (Plompiler.Anemoi128)
  module P = Schnorr.P
  module A = Schnorr.Curve

  let test_vanilla_schnorr () =
    let sk = A.Scalar.random () in
    let msg = S.random () in
    let rand = A.Scalar.random () in
    let pk = P.neuterize sk in
    let signature = P.sign sk msg rand in
    assert (P.verify ~msg ~pk ~signature ()) ;
    let msg = S.random () in
    assert (not @@ P.verify ~msg ~pk ~signature ())

  let test = test_vanilla_schnorr
end

module Schnorr (L : LIB) = struct
  open L

  open Utils (L)

  module Schnorr = Plompiler.Schnorr (Plompiler.Anemoi128)
  module Sc = Schnorr.V (L)
  module A = Schnorr.Curve

  let test_circuit_verify g pk msg signature () =
    let* g = input ~kind:`Public g in
    let* pk = input ~kind:`Public @@ Sc.pk_encoding.input pk in
    let* msg = input msg in
    let* signature = input @@ Sc.signature_encoding.input signature in
    let signature = Sc.signature_encoding.decode signature in
    with_bool_check (Sc.verify ~g ~msg ~pk ~signature ())

  let sk = A.Scalar.random ()

  let pk = Schnorr.P.neuterize sk

  let msg = S.random ()

  let rand = A.Scalar.random ()

  let signature = Schnorr.P.sign sk msg rand

  let nb_bits = Z.numbits A.Base.order

  let wrong_u_bytes =
    Plompiler.Utils.bool_list_of_z ~nb_bits
    @@ A.Scalar.to_z @@ A.Scalar.random ()

  let tests =
    let g = Sc.pk_encoding.input A.one in
    let msg = Input.scalar msg in
    [
      test
        ~valid:true
        ~name:"Schnorr.test_circuit_verify"
        (test_circuit_verify g pk msg signature);
      test
        ~valid:false
        ~name:"Schnorr.test_circuit_verify"
        (test_circuit_verify
           g
           pk
           msg
           {signature with sig_u_bytes = wrong_u_bytes});
    ]
end

let tests =
  [
    Alcotest.test_case "P" `Quick P.test;
    Alcotest.test_case "Schnorr" `Quick (to_test (module Schnorr : Test));
    Alcotest.test_case
      "Schnorr plonk"
      `Slow
      (to_test ~plonk:(module Plonk.Main_protocol) (module Schnorr : Test));
  ]
