(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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
    Component:    Contract_repr
    Invocation:   dune exec src/proto_016_PtMumbai/lib_protocol/test/unit/main.exe
    Dependencies: contract_hash.ml
    Subject:      To test the modules (including the top-level)
                  in contract_repr.ml as individual units, particularly
                  failure cases. Superficial goal: increase coverage percentage.
*)
open Protocol

open Tztest

(*

  TODO: Remove dependence on contract_hash.ml and mock it

 *)

module Test_contract_repr = struct
  (** Assert if [is_implicit] correctly returns the implicit contract *)
  open Contract_repr

  let dummy_operation_hash =
    Operation_hash.of_bytes_exn
      (Bytes.of_string "test-operation-hash-of-length-32")

  let dummy_origination_nonce = Origination_nonce.initial dummy_operation_hash

  let dummy_contract_hash =
    (* WARNING: Uses Contract_repr itself, which is yet to be tested. This happened because Contract_hash wasn't mocked *)
    let data =
      Data_encoding.Binary.to_bytes_exn
        Origination_nonce.encoding
        dummy_origination_nonce
    in
    Contract_hash.hash_bytes [data]

  let dummy_implicit_contract =
    Implicit Tezos_crypto.Signature.Public_key_hash.zero

  let dummy_originated_contract = originated_contract @@ dummy_origination_nonce

  let test_to_b58check_implicit () =
    Assert.equal
      ~loc:__LOC__
      String.equal
      "%s should have been equal to %"
      Format.pp_print_string
      (to_b58check dummy_implicit_contract)
      Tezos_crypto.Signature.Public_key_hash.(to_b58check zero)

  let test_to_b58check_originated () =
    Assert.equal
      ~loc:__LOC__
      String.equal
      "%s should have been equal to %"
      Format.pp_print_string
      (to_b58check dummy_originated_contract)
      Contract_hash.(to_b58check @@ dummy_contract_hash)

  let create_dummy_contracts n =
    let since = dummy_origination_nonce in
    let rec incr_n_times nonce = function
      | 0 -> nonce
      | n -> incr_n_times (Origination_nonce.incr nonce) (n - 1)
    in
    let until = incr_n_times since n in
    let contracts = originated_contracts ~since ~until in
    contracts

  let test_originated_contracts_basic () =
    let n = 5 in
    let contracts = create_dummy_contracts n in
    Assert.equal_int ~loc:__LOC__ (List.length contracts) n
end

let tests =
  [
    tztest
      "Contract_repr.to_b58check: must correctly stringify, b58check encoded, \
       an implicit contract"
      `Quick
      Test_contract_repr.test_to_b58check_implicit;
    tztest
      "Contract_repr.originated_contract: must correctly create an originated \
       contract"
      `Quick
      Test_contract_repr.test_originated_contracts_basic;
    tztest
      "Contract_repr.to_b58check: must correctly stringify, b58check encoded, \
       an originated contract"
      `Quick
      Test_contract_repr.test_to_b58check_originated;
  ]
