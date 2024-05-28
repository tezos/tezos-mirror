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

(* Testing
   -------
   Component: Client - hash data command
   Invocation: dune exec tezt/tests/main.exe -- --file hash_data.ml

               Note that to reset the regression outputs, one can use
               the [--reset-regressions] option. When doing so, it is
               recommended to clear the output directory [tezt/_regressions/hash_data]
               first to remove unused files in case some paths change.
   Subject: Tests of the client's `hash data ... of type` command.

            Regression capture the output of octez-client calls and compare it
            with the output from the previous run. The test passes only if the
            outputs match exactly. It is important that return values
            of `hash data` remain constant over time.
*)

(* These hooks must be attached to every process that should be captured for
   regression testing. Not plugged for negative tests, since octez-client
   shows its manpage, which will change overtime. *)
let hooks = Tezos_regression.hooks

(** Test.
    Call `octez-client hash data ... of type ...` with data on which
    it must return 0. In addition, regression is activated.
    to check that returned values remain constant over time. *)
let test_good_hash_data =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"hash data ... of type ... (good)"
    ~tags:["hash"; "data"; "mockup"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let data_n_type =
    let min_int64 = Int64.to_string Int64.min_int in
    let max_int64 = Int64.to_string Int64.max_int in
    [
      (* ints *)
      (min_int64 ^ max_int64, "int");
      (min_int64, "int");
      (Int.to_string Int.min_int, "int");
      ("-1", "int");
      ("1", "int");
      ("2", "int");
      (Int.to_string Int.max_int, "int");
      (max_int64, "int");
      (max_int64 ^ max_int64, "int");
      (* Boolean *)
      ("False", "bool");
      ("True", "bool");
      (* list(t)*)
      ("{}", "list(bool)");
      ("{}", "list(int)");
      ("{}", "list(unit)");
      ("{True; False}", "list(bool)");
      ("{True; True}", "list(bool)");
      ("{-1; 0}", "list(int)");
      (* misc *)
      ("\"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq\"", "address");
      ("\"KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi%entrypoint\"", "address");
      ("\"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx\"", "address");
      ("\"tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc\"", "address");
      ("\"tz3WEJYwJ6pPwVbSL8FrSoAXRmFHHZTuEnMA\"", "address");
      ("0", "bls12_381_fr");
      ("1", "bls12_381_fr");
      ("0x01", "bls12_381_fr");
      ("0x0001", "bls12_381_fr");
      ( "0x0572cbea904d67468808c8eb50a9450c9721db309128012543902d0ac358a62ae28f75bb8f1c7c42c39a8c5529bf0f4e166a9d8cabc673a322fda673779d8e3822ba3ecb8670e461f73bb9021d5fd76a4c56d9d4cd16bd1bba86881979749d28",
        "bls12_381_g1" );
      ( "0x0a4edef9c1ed7f729f520e47730a124fd70662a904ba1074728114d1031e1572c6c886f6b57ec72a6178288c47c335771638533957d540a9d2370f17cc7ed5863bc0b995b8825e0ee1ea1e1e4d00dbae81f14b0bf3611b78c952aacab827a0530f6d4552fa65dd2638b361543f887136a43253d9c66c411697003f7a13c308f5422e1aa0a59c8967acdefd8b6e36ccf30468fb440d82b0630aeb8dca2b5256789a66da69bf91009cbfe6bd221e47aa8ae88dece9764bf3bd999d95d71e4c9899",
        "bls12_381_g2" );
      ("0x", "bytes");
      ("0xABCDEF42", "bytes");
      ("0x7a06a770", "chain_id");
      ("\"NetXynUjJNZm7wi\"", "chain_id");
      ("\"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav\"", "key");
      ("\"edpkuJqtDcA2m2muMxViSM47MPsGQzmyjnNTawUPqR8vZTAMcx61ES\"", "key");
      ("\"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx\"", "key_hash");
      ("\"tz1XPTDmvT3vVE5Uunngmixm7gj7zmdbPq6k\"", "key_hash");
      ("{ }", "lambda unit unit");
      ("{ PUSH nat 1; ADD }", "lambda nat nat");
      ("{}", "list unit");
      ("{ 0 ; 10 }", "list nat");
      ("{ Some 10 ; None }", "list (option int)");
      ("{}", "map nat unit");
      ("{ Elt 0 0xCB ; Elt 1 0xAB }", "map nat bytes");
      ("0", "mutez");
      ("1", "mutez");
      ("99999", "mutez");
      ("0", "nat");
      ("1", "nat");
      ("99999", "nat");
      ("None", "option unit");
      ("Some \"foo\"", "option string");
      ("(Left True)", "or bool string");
      ("(Right \"foo\")", "or bool string");
      ("(Pair 0 True)", "pair int bool");
      ("(Pair 0 (Pair True 0x))", "pair int bool bytes");
      ("(Pair 0 True 0x)", "pair int bool bytes");
      ("{0; True; 0x}", "pair int bool bytes");
      ("{}", "set bool");
      ("{ 0 ; 3 ; 4 }", "set nat");
      ("\"ABC\\n123\"", "string");
      ( "\"edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7\"",
        "signature" );
      ( "\"spsig1PPUFZucuAQybs5wsqsNQ68QNgFaBnVKMFaoZZfi1BtNnuCAWnmL9wVy5HfHkR6AeodjVGxpBVVSYcJKyMURn6K1yknYLm\"",
        "signature" );
      ("\"foo\"", "string");
      ("\"2019-09-26T10:59:51Z\"", "timestamp");
      ("1571659294", "timestamp");
      ("Unit", "unit");
    ]
  in
  let hash_data (data, typ) =
    let* _ = Client.hash_data ~hooks ~data ~typ client in
    Lwt.return_unit
  in
  Lwt_list.iter_s hash_data data_n_type

(** Test.
    Call `octez-client hash data ... of type ...` with data on which it
    must fail (non-zero exit code). *)
let test_bad_hash_data =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"hash data ... of type ... (bad)"
    ~tags:["hash"; "data"; "mockup"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let data_n_type =
    [
      ("True", "int");
      ("{}", "big_map nat bytes");
      ("{ Elt 0 0xCB ; Elt 1 0xAB }", "big_map nat bytes");
      ("{}", "sapling_state 8");
      ("Pair \"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq\" 0x01 42", "ticket bytes");
      ("Pair \"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq\" Unit 42", "ticket unit");
    ]
  in
  let hash_data (data, typ) =
    let* () = Client.spawn_hash_data ~data ~typ client |> Process.check_error in
    Lwt.return_unit
  in
  Lwt_list.iter_s hash_data data_n_type

(** Test.
    Call `octez-client hash data ... of type ...` with data on which it
    fails in a somewhat unstructued manner, and prints in manpage.

    We therefore do not do regression on this test, because
    the manpage changes slowly over time. *)
let test_ugly_hash_data =
  Protocol.register_test
    ~__FILE__
    ~title:"hash data ... of type ... (ugly)"
    ~tags:["hash"; "data"; "mockup"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let data_n_type =
    [("[]", "list(int"); ("{}", "list('a)"); ("\"ABC\n123\"", "string")]
  in
  let hash_data (data, typ) =
    let* _ = Client.spawn_hash_data ~data ~typ client |> Process.check_error in
    Lwt.return_unit
  in
  Lwt_list.iter_s hash_data data_n_type

let register ~protocols =
  test_good_hash_data protocols ;
  test_bad_hash_data protocols ;
  test_ugly_hash_data protocols
