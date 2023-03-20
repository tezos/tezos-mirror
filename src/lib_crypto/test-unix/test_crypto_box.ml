(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Invocation:   dune exec src/lib_crypto/test-unix/main.exe
    Subject:      Roundtrips for functions built on the HACL* NaCl API.
*)

let _sk, pk, _pkh = Crypto_box.random_keypair ()

(** The test defines a proof-of-work target, generates a proof-of-work
    for that target, and then verifies it the proof of work is accepted
    by [Crypto_box.check_proof_of_work].
*)
let test_check_pow () =
  let open Lwt.Syntax in
  let target = Crypto_box.make_pow_target 2. in
  let+ pow = Crypto_box.generate_proof_of_work pk target in
  Alcotest.(check bool)
    "check_pow"
    (Crypto_box.check_proof_of_work pk pow target)
    true

let tests_lwt = [("crypto_box", [("Check PoW", `Slow, test_check_pow)])]

let () = Lwt_main.run @@ Alcotest_lwt.run "tezos-crypto-lwt" tests_lwt
