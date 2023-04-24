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
    Component:    Mockup wallet library
    Invocation:   dune exec src/lib_client_base_unix/test/main.exe \
                  -- --file test_mockup_wallet.ml
    Subject:      Unit tests of the Mockup wallet library
*)

open Tezos_mockup_commands.Mockup_wallet
open Tezos_error_monad.Error_monad
open Tezos_stdlib_unix
open Tezos_client_base
open Tezos_client_base_unix

let default_bootstrap_accounts_names =
  List.map (fun i -> "bootstrap" ^ string_of_int i) (1 -- 5)

let testable_public_key_hash =
  Alcotest.testable
    Tezos_crypto.Signature.Public_key_hash.pp
    Tezos_crypto.Signature.Public_key_hash.equal

let testable_public_key =
  Alcotest.testable
    Tezos_crypto.Signature.Public_key.pp
    Tezos_crypto.Signature.Public_key.equal

let testable_string_list_ignoring_order : string list Alcotest.testable =
  let (module StringListTestable) = Alcotest.(list string) in
  let module M = struct
    include StringListTestable

    let equal v1 v2 =
      String.Set.equal (String.Set.of_list v1) (String.Set.of_list v2)
  end in
  (module M)

(** Validate SK and PK consistency *)
let validate_key (_, pk_hash, pk_sig_opt, sk_uri_opt) =
  let open Lwt_result_syntax in
  match (pk_sig_opt, sk_uri_opt) with
  | Some pk_sig, Some sk_uri -> (
      let* pk_hash_from_sk, pk_sig_from_sk_opt =
        let* pk = Client_keys.neuterize sk_uri in
        Client_keys.public_key_hash pk
      in
      match pk_sig_from_sk_opt with
      | None -> return @@ Alcotest.fail "Is this a valid scenario?"
      | Some pk_sig_from_sk ->
          return
            (Alcotest.check
               testable_public_key_hash
               "PK hash is consistent with SK"
               pk_hash
               pk_hash_from_sk ;
             Alcotest.check
               testable_public_key
               "PK is consistent with SK"
               pk_sig
               pk_sig_from_sk))
  | _, _ -> failwith "Key has no public signature or secret key"

(** Check that names in [key_list] match the ones in [accounts_names],
    ignoring order *)
let validate_accounts_names key_list accounts_names =
  Alcotest.check
    testable_string_list_ignoring_order
    "Accounts names must match"
    (List.map (fun (name, _, _, _) -> name) key_list)
    accounts_names

(** When no bootstrap accounts file is provided, then the wallet is
    populated with the default bootstrap accounts *)
let test_no_bootstrap_accounts_file_populates_defaults =
  let open Lwt_result_syntax in
  Tztest.tztest
    "When no bootstrap accounts file is provided, then the wallet is populated \
     with the default bootstrap accounts"
    `Quick
    (fun () ->
      Lwt_utils_unix.with_tempdir "test_mockup_wallet" (fun base_dir ->
          let io_wallet =
            new Client_context_unix.unix_io_wallet
              ~base_dir
              ~password_filename:None
          in
          let () =
            Client_keys.register_signer
              (module Tezos_signer_backends.Unencrypted)
          in
          let* () = populate io_wallet None in
          let* key_list = Client_keys.list_keys io_wallet in
          validate_accounts_names key_list default_bootstrap_accounts_names ;
          List.iter_es validate_key key_list))

(** When a valid bootstrap accounts file is provided, then the wallet is
    populated with its content *)
let test_with_valid_bootstrap_accounts_file_populates =
  Tztest.tztest
    "When a valid bootstrap accounts file is provided, then the wallet is \
     populated with its content"
    `Quick
    (fun () ->
      Lwt_utils_unix.with_tempdir "test_mockup_wallet" (fun base_dir ->
          let open Lwt_result_syntax in
          let io_wallet =
            new Client_context_unix.unix_io_wallet
              ~base_dir
              ~password_filename:None
          in
          let account_name_1 = "account 1" in
          let account_name_2 = "account 2" in
          let bootstrap_accounts =
            `A
              [
                `O
                  [
                    ("name", `String account_name_1);
                    ( "sk_uri",
                      `String
                        "unencrypted:edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh"
                    );
                    ("amount", `String "doesn't matter");
                  ];
                `O
                  [
                    ("name", `String account_name_2);
                    ( "sk_uri",
                      `String
                        "unencrypted:edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo"
                    );
                    ("amount", `String "whatever");
                  ];
              ]
          in
          let bootstrap_accounts_file_path =
            Filename.concat base_dir "my_bootstrap_accounts_json"
          in
          let () =
            Client_keys.register_signer
              (module Tezos_signer_backends.Unencrypted)
          in
          let* () =
            Lwt_utils_unix.Json.write_file
              bootstrap_accounts_file_path
              bootstrap_accounts
          in
          let* () = populate io_wallet (Some bootstrap_accounts_file_path) in
          let* key_list = Client_keys.list_keys io_wallet in
          validate_accounts_names key_list [account_name_1; account_name_2] ;
          List.iter_es validate_key key_list))

let () =
  Alcotest_lwt.run
    ~__FILE__
    "tezos-mockup-commands"
    [
      ( "mockup_wallet",
        [
          test_no_bootstrap_accounts_file_populates_defaults;
          test_with_valid_bootstrap_accounts_file_populates;
        ] );
    ]
  |> Lwt_main.run
