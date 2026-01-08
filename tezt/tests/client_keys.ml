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

module BLS_normal_wallet = struct
  let check_shown_account ~__LOC__ (expected : Account.key)
      (shown : Account.key) =
    if expected.public_key_hash <> shown.public_key_hash then
      Test.fail
        ~__LOC__
        "Expecting %s, got %s as public key hash from the client "
        expected.public_key_hash
        shown.public_key_hash
    else if expected.public_key <> shown.public_key then
      Test.fail
        ~__LOC__
        "Expecting %s, got %s as public key from the client "
        expected.public_key
        shown.public_key
    else if expected.secret_key <> shown.secret_key then
      let sk = Account.uri_of_secret_key shown.secret_key in
      let expected_sk = Account.uri_of_secret_key shown.secret_key in
      Test.fail
        ~__LOC__
        "Expecting %s, got %s as secret key from the client "
        expected_sk
        sk
    else return ()

  let test_bls_import_secret_key () =
    Test.register
      ~__FILE__
      ~tags:["bls"; "client"; "keys"]
      ~title:"Import BLS secret key"
      ~uses_node:false
      (fun () ->
        let* client = Client.init () in
        let* () =
          let Account.{alias; secret_key; _} = Constant.tz4_account in
          Client.import_secret_key client ~alias secret_key
        in
        let* shown_account =
          Client.show_address ~alias:Constant.tz4_account.Account.alias client
        in
        check_shown_account ~__LOC__ Constant.tz4_account shown_account)

  let test_bls_show_address () =
    Test.register
      ~__FILE__
      ~tags:["bls"; "client"; "keys"]
      ~title:"Shows the address of a registered BLS account"
      ~uses_node:false
      (fun () ->
        let* client = Client.init () in
        let* () =
          let Account.{alias; secret_key; _} = Constant.tz4_account in
          Client.import_secret_key client ~alias secret_key
        in
        let* shown_account =
          Client.show_address ~alias:Constant.tz4_account.Account.alias client
        in
        check_shown_account ~__LOC__ Constant.tz4_account shown_account)

  let test_bls_gen_keys () =
    Test.register
      ~__FILE__
      ~tags:["bls"; "client"; "keys"]
      ~title:"Generates new tz4 keys"
      ~uses_node:false
      (fun () ->
        let* client = Client.init () in
        let* alias = Client.gen_keys ~sig_alg:"bls" client in
        let* _account = Client.show_address ~alias client in
        return ())

  let register_protocol_independent () =
    test_bls_import_secret_key () ;
    test_bls_show_address () ;
    test_bls_gen_keys ()
end

module Wallet = struct
  let test_duplicate_alias () =
    Test.register
      ~__FILE__
      ~tags:["client"; "keys"; "duplicate"]
      ~title:"Add a duplicate address"
      ~uses_node:false
    @@ fun () ->
    let* client = Client.init () in
    let* (_ : string) = Client.gen_keys client ~alias:"foo" in
    let* account_foo = Client.show_address client ~alias:"foo" in
    let msg =
      rex
        "this public key hash is already aliased as foo, use --force to insert \
         duplicate"
    in
    let* () =
      Client.spawn_add_address client ~alias:"baz" ~src:account_foo.alias
      |> Process.check_error ~msg
    in
    let* () =
      Client.spawn_add_address
        client
        ~alias:"baz"
        ~src:account_foo.public_key_hash
      |> Process.check_error ~msg
    in
    let* _alias2 =
      Client.add_address client ~force:true ~alias:"baz" ~src:"foo"
    in
    (* Check that we can read the secret key of [foo],
       and that the original [foo] is equal to [baz]
       (modulo the alias) *)
    let* account_foo2 = Client.show_address ~alias:"foo" client in
    let* account_baz = Client.show_address ~alias:"baz" client in
    Check.(
      (account_foo = account_foo2)
        Account.key_typ
        ~__LOC__
        ~error_msg:"Expected %R, got %L") ;
    Check.(
      (account_foo = {account_baz with alias = "foo"})
        Account.key_typ
        ~__LOC__
        ~error_msg:"Expected %R, got %L") ;
    return ()

  let test_remember_contract =
    Protocol.register_test
      ~__FILE__
      ~tags:["client"; "contract"; "remember"]
      ~title:"Test remember contract"
      ~uses_node:false
    @@ fun protocol ->
    let* client = Client.init_mockup ~protocol () in
    [
      ("test", "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc");
      ("test-2", "KT1TZCh8fmUbuDqFxetPWC2fsQanAHzLx4W9");
    ]
    |> Lwt_list.iter_s @@ fun (alias, address) ->
       let* () = Client.remember_contract client ~alias ~address in
       let* () = Client.remember_contract client ~force:true ~alias ~address in
       let* () =
         Client.spawn_remember_contract client ~alias ~address
         |> Process.check_error
              ~msg:(rex ("The contract alias " ^ alias ^ " already exists"))
       in
       unit

  (* Checks that the keys are correctly imported from a mnemonic. *)
  let test_import_key_mnemonic () =
    Test.register
      ~__FILE__
      ~title:"Wallet: Import key mnemonic"
      ~tags:["client"; "keys"; "import"; "mnemonic"]
      ~uses_node:false
    @@ fun () ->
    let mnemonic =
      [
        "seek";
        "paddle";
        "siege";
        "sting";
        "siege";
        "sick";
        "kidney";
        "detect";
        "coral";
        "because";
        "comfort";
        "long";
        "enforce";
        "napkin";
        "enter";
      ]
    in
    let* client = Client.init () in
    Log.info "Test a simple import." ;
    let* () =
      Client.import_keys_from_mnemonic
        ~force:true
        client
        ~alias:"zebra"
        ~mnemonic:
          [
            "release";
            "easy";
            "pulp";
            "drop";
            "select";
            "attack";
            "false";
            "math";
            "cook";
            "angry";
            "spin";
            "ostrich";
            "round";
            "dress";
            "acoustic";
          ]
    in
    let* addresses = Client.list_known_addresses client in
    Check.(
      (List.assoc_opt "zebra" addresses
      = Some "tz1aGUKE72eN21iWztoDEeH4FeKaxWb7SAUb")
        (option string)
        ~__LOC__
        ~error_msg:"Expected %R, got %L") ;
    Log.info "Test that importing fails if the alias is already present." ;
    let alias = "super_original" in
    let* () =
      Client.import_keys_from_mnemonic ~force:true client ~alias ~mnemonic
    in
    let* () =
      let process, output_channel =
        Client.spawn_import_keys_from_mnemonic client ~alias
      in
      let stdin = String.concat " " mnemonic ^ "\n\n" in
      let* () = Lwt_io.write_line output_channel stdin in
      let* () = Lwt_io.close output_channel in
      Process.check_error
        process
        ~msg:(rex "The secret_key alias super_original already exists.")
    in
    Log.info "Test an import where the user specifies a passphrase." ;
    let passphrase = "very_secure_passphrase" in
    let* () =
      Client.import_keys_from_mnemonic
        ~force:true
        client
        ~alias:"key"
        ~mnemonic
        ~passphrase
    in
    let* addresses = Client.list_known_addresses client in
    Check.(
      (List.assoc_opt "key" addresses
      = Some "tz1QSF4TSVzaosgbaxnFJpRbs7798Skeb8Re")
        (option string)
        ~__LOC__
        ~error_msg:"Expected %R, got %L") ;
    Log.info "Test an import where the user wants to encrypt the key." ;
    let* () =
      let alias = "cryptkey" in
      let encryption_password = "imgonnaencryptthiskeysohard" in
      let* () =
        Client.import_keys_from_mnemonic
          ~force:true
          ~encryption_password
          client
          ~alias
          ~mnemonic
          ~passphrase
      in
      let* addresses = Client.list_known_addresses client in
      Check.(
        (List.assoc_opt alias addresses
        = Some "tz1QSF4TSVzaosgbaxnFJpRbs7798Skeb8Re")
          (option string)
          ~__LOC__
          ~error_msg:"Expected %R, got %L") ;
      let* account = Client.show_address ~alias client in
      match account.secret_key with
      | Encrypted _ -> unit
      | Unencrypted _ | Remote _ -> Test.fail "Expected an encrypted secret key"
    in
    Log.info "Test that the command fails if the user gives a bad mnemonic." ;
    let* () =
      let process, output_channel =
        Client.spawn_import_keys_from_mnemonic client ~alias:"alias"
      in
      let stdin = "hello\n\n" in
      let* () = Lwt_io.write_line output_channel stdin in
      let* () = Lwt_io.close output_channel in
      Process.check_error
        process
        ~msg:(rex "\"hello\" is not a valid BIP39 mnemonic.")
    in
    unit

  let register_protocol_independent () =
    test_duplicate_alias () ;
    test_import_key_mnemonic ()

  let register protocols = test_remember_contract protocols
end

let register_protocol_independent () =
  BLS_normal_wallet.register_protocol_independent () ;
  Wallet.register_protocol_independent ()

let register ~protocols = Wallet.register protocols
