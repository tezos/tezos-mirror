(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Crypto
    Invocation:   dune exec src/lib_hacl/test/main.exe
    Subject:      Property-tests over the interface Hacl.SIGNATURE checking
                  the equivalence between [pk_of_bytes_without_validation]
                  and [pk_of_bytes] on valid public keys.
                  This is currently only relevant for Hacl.P256 since it is
                  the only scheme in which these 2 functions are different.
*)
open Qcheck2_helpers

open QCheck2

module Pk_Properties (Desc : sig
  val name : string
end)
(X : Hacl.SIGNATURE) =
struct
  (** Checks that [pk_of_bytes_without_validation] and [pk_of_bytes] have
      the same output on valid public keys and always return a Some. *)
  let test_prop_sign_check (sk_bytes : string) =
    match X.sk_of_bytes @@ Bytes.of_string sk_bytes with
    | Some sk ->
        let pk = X.neuterize sk in
        let pk_bytes = X.to_bytes pk in
        let pk1 = X.pk_of_bytes_without_validation pk_bytes in
        let pk2 = X.pk_of_bytes pk_bytes in
        pk1 = pk2 && Option.is_some pk1
    | None ->
        Test.fail_report
          "X.sk_of_bytes @@ Bytes.of_string sk_bytes can't return a None."

  let test_prop_sign_check =
    Test.make
      ~name:(Desc.name ^ "_pk_of_bytes")
      ~print:Print.string
      Gen.(string_size @@ pure X.sk_size)
      test_prop_sign_check

  let tests = [test_prop_sign_check]
end

module P256_Props =
  Pk_Properties
    (struct
      let name = "P256"
    end)
    (Hacl.P256)

let () =
  Alcotest.run
    ~__FILE__
    "tezos-crypto-signature-pk"
    [("P256_Pros", qcheck_wrap P256_Props.tests)]
