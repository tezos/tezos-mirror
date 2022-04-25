(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(* Testing
   -------
   Component:    Client keys
   Invocation:   dune exec tezt/tests/main.exe -- --file client_keys.ml
   Subject:      Checks client wallet commands
*)

let check_shown_account ~__LOC__ (expected : Account.aggregate_key)
    (shown : Account.aggregate_key) =
  if expected.aggregate_public_key_hash <> shown.aggregate_public_key_hash then
    Test.fail
      ~__LOC__
      "Expecting %s, got %s as public key hash from the client "
      expected.aggregate_public_key_hash
      shown.aggregate_public_key_hash
  else if expected.aggregate_public_key <> shown.aggregate_public_key then
    Test.fail
      ~__LOC__
      "Expecting %s, got %s as public key from the client "
      expected.aggregate_public_key
      shown.aggregate_public_key
  else if expected.aggregate_secret_key <> shown.aggregate_secret_key then
    let (Unencrypted sk) = shown.aggregate_secret_key in
    let (Unencrypted expected_sk) = shown.aggregate_secret_key in
    Test.fail
      ~__LOC__
      "Expecting %s, got %s as secret key from the client "
      expected_sk
      sk
  else return ()

let test_bls_import_secret_key () =
  Test.register
    ~__FILE__
    ~tags:["client"; "keys"]
    ~title:"Import BLS secret key"
    (fun () ->
      let* client = Client.init () in
      let* () = Client.bls_import_secret_key Constant.tz4_account client in
      let* shown_account =
        Client.bls_show_address
          ~alias:Constant.tz4_account.Account.aggregate_alias
          client
      in
      check_shown_account ~__LOC__ Constant.tz4_account shown_account)

let test_bls_show_address () =
  Test.register
    ~__FILE__
    ~tags:["client"; "keys"]
    ~title:"Shows the address of a registered BLS account"
    (fun () ->
      let* client = Client.init () in
      let* () = Client.bls_import_secret_key Constant.tz4_account client in
      let* shown_account =
        Client.bls_show_address
          ~alias:Constant.tz4_account.Account.aggregate_alias
          client
      in
      check_shown_account ~__LOC__ Constant.tz4_account shown_account)

let test_bls_gen_keys () =
  Test.register
    ~__FILE__
    ~tags:["client"; "keys"]
    ~title:"Generates new tz4 keys"
    (fun () ->
      let* client = Client.init () in
      let alias = "test_key" in
      let* () = Client.bls_gen_keys ~alias client in
      let* _account = Client.bls_show_address ~alias client in
      return ())

let test_bls_list_keys () =
  Test.register
    ~__FILE__
    ~tags:["client"; "keys"]
    ~title:"Lists known BLS aliases in the client"
    (fun () ->
      let* client = Client.init () in
      let Account.{aggregate_alias; aggregate_public_key_hash; _} =
        Constant.tz4_account
      in
      let* () = Client.bls_import_secret_key Constant.tz4_account client in
      let* maybe_keys = Client.bls_list_keys client in
      let expected_keys = [(aggregate_alias, aggregate_public_key_hash)] in
      if List.equal ( = ) expected_keys maybe_keys then return ()
      else
        let pp ppf l =
          Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
            (fun ppf (a, k) -> Format.fprintf ppf "%s: %s" a k)
            ppf
            l
        in
        Test.fail
          ~__LOC__
          "Expecting\n@[%a@]\ngot\n@[%a@]\nas keys from the client "
          pp
          expected_keys
          pp
          maybe_keys)

let register_protocol_independent () =
  test_bls_import_secret_key () ;
  test_bls_show_address () ;
  test_bls_gen_keys () ;
  test_bls_list_keys ()
