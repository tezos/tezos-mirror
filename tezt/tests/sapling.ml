(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(* Testing
   -------
   Component:    Sapling
   Invocation:   dune exec tezt/tests/main.exe -- --file sapling.ml
   Subject:      Tests for the Sapling client and protocol
*)

open Tezt_tezos

let sapling_contract_path ?(script = "sapling_contract.tz") protocol =
  sf
    "./src/%s/lib_protocol/test/integration/michelson/contracts/%s"
    (Protocol.directory protocol)
    script

module Helpers = struct
  let init protocol contract =
    let* node = Node.init [Node.Synchronisation_threshold 0; Connections 0] in
    let* client = Client.init ~endpoint:(Node node) () in
    let* () = Client.activate_protocol ~protocol client in
    Log.info "Activated protocol." ;
    let* contract_address =
      let prg = sapling_contract_path protocol in
      Client.originate_contract
        ~wait:"none"
        ~init:"{}"
        ~alias:contract
        ~amount:Tez.zero
        ~burn_cap:(Tez.of_int 10)
        ~src:Constant.bootstrap1.alias
        ~prg
        client
    in
    let* () = Client.bake_for_and_wait client in
    Log.info "  - Originated %s." contract ;
    return (client, contract_address)

  let gen_user (client, contract) name =
    let* (_ : string list) =
      Client.sapling_gen_key ~name ~unencrypted:true client
    in
    let* () =
      Client.sapling_use_key ~sapling_key:name ~contract ~memo_size:8 client
    in
    let* address, _index = Client.sapling_gen_address ~name client in
    return address

  let check_balance ~__LOC__ (client, contract) name amount =
    let* balance =
      Client.sapling_get_balance ~sapling_key:name ~contract client
    in
    Check.(
      (balance = amount)
        Tez.typ
        ~error_msg:"Expected sapling balance in contract %R, got %L"
        ~__LOC__) ;
    unit

  let shield ?(expect_failure = false) (client, contract) src dst amount_tez =
    if expect_failure then
      let* () =
        Client.spawn_sapling_shield
          ~qty:amount_tez
          ~src_tz:Account.(src.alias)
          ~dst_sap:dst
          ~sapling_contract:contract
          ~burn_cap:(Tez.of_int 2)
          client
        |> Process.check_error
      in
      return Tez.(zero, zero)
    else
      let* balance_diff =
        Client.sapling_shield
          ~qty:amount_tez
          ~src_tz:Account.(src.alias)
          ~dst_sap:dst
          ~sapling_contract:contract
          ~burn_cap:(Tez.of_int 2)
          client
      in
      let* () = Client.bake_for_and_wait client in
      Log.info "shield" ;
      return balance_diff

  let transfer ?(expect_failure = false) (client, contract) src dst amount_tez =
    let temp_sapling_transaction_file = Temp.file "sapling_transaction" in
    if expect_failure then
      Client.spawn_sapling_forge_transaction
        ~qty:amount_tez
        ~src_sap:src
        ~dst_sap:dst
        ~sapling_contract:contract
        ~file:temp_sapling_transaction_file
        client
      |> Process.check_error
    else
      let* () =
        Client.sapling_forge_transaction
          ~qty:amount_tez
          ~src_sap:src
          ~dst_sap:dst
          ~sapling_contract:contract
          ~file:temp_sapling_transaction_file
          client
      in
      let* () =
        Client.sapling_submit
          ~burn_cap:Tez.one
          ~file:temp_sapling_transaction_file
          ~alias_tz:Constant.bootstrap1.alias
          ~sapling_contract:contract
          client
      in
      let* () = Client.bake_for_and_wait client in
      Log.info "transfer" ;
      unit

  let unshield ?(expect_failure = false) (client, contract) src dst amount_tez =
    if expect_failure then
      let* () =
        Client.spawn_sapling_unshield
          ~burn_cap:Tez.one
          ~qty:amount_tez
          ~src_sap:src
          ~dst_tz:Account.(dst.alias)
          ~sapling_contract:contract
          client
        |> Process.check_error
      in
      return Tez.(zero, zero)
    else
      let* balance_diff =
        Client.sapling_unshield
          ~burn_cap:Tez.one
          ~qty:amount_tez
          ~src_sap:src
          ~dst_tz:Account.(dst.alias)
          ~sapling_contract:contract
          client
      in
      let* () = Client.bake_for_and_wait client in
      Log.info "unshield" ;
      return balance_diff

  let balance_tz1 (client, _contract) pkh =
    let* balance_tez =
      RPC.Client.call client
      @@ RPC.get_chain_block_context_contract_balance ~id:pkh ()
    in
    return balance_tez
end

module Insufficient_funds = struct
  let shield =
    Protocol.register_test
      ~__FILE__
      ~title:"insufficient_funds.shield"
      ~tags:["sapling"]
    @@ fun protocol ->
    let open Helpers in
    let* c = init protocol (sapling_contract_path protocol) in
    let alice_tz1 = Constant.bootstrap2 in
    let* alice_address = gen_user c "alice" in
    let* _ =
      shield
        ~expect_failure:true
        c
        alice_tz1
        alice_address
        (Tez.of_int 1_000_000_000)
    in
    unit

  let transfer =
    Protocol.register_test
      ~__FILE__
      ~title:"insufficient_funds.transfer"
      ~tags:["sapling"]
    @@ fun protocol ->
    let open Helpers in
    let* c = init protocol (sapling_contract_path protocol) in
    let alice_tz1 = Constant.bootstrap2 in
    let* alice_address = gen_user c "alice" in
    let* bob_address = gen_user c "bob" in
    let* _ = shield c alice_tz1 alice_address (Tez.of_int 10) in
    let* () =
      transfer ~expect_failure:true c "alice" bob_address (Tez.of_int 11)
    in
    unit

  let unshield =
    Protocol.register_test
      ~__FILE__
      ~title:"insufficient_funds.unshield"
      ~tags:["sapling"]
    @@ fun protocol ->
    let open Helpers in
    let* c = init protocol (sapling_contract_path protocol) in
    let alice_tz1 = Constant.bootstrap2 in
    let* alice_address = gen_user c "alice" in
    let* bob_address = gen_user c "bob" in
    let* _ = shield c alice_tz1 alice_address (Tez.of_int 10) in
    let* () = transfer c "alice" bob_address (Tez.of_int 10) in
    let* _ = unshield ~expect_failure:true c "bob" alice_tz1 (Tez.of_int 11) in
    unit
end

module Wallet = struct
  let mnemonic =
    [
      "morning";
      "dinosaur";
      "estate";
      "youth";
      "sausage";
      "feature";
      "apology";
      "bullet";
      "square";
      "type";
      "zoo";
      "coyote";
      "extra";
      "fabric";
      "grain";
      "phone";
      "pipe";
      "despair";
      "razor";
      "ranch";
      "blouse";
      "debris";
      "urge";
      "evidence";
    ]

  let non_originated_contract_address = "KT1MXuZJJFg4EVpLQeLeuHvznTRiNefh3yCs"

  let non_originated_contract_name = "fake-contract"

  let key_name = "test_key_name"

  let unencrypted = true

  let test_import_key =
    Protocol.register_test
      ~__FILE__
      ~title:"import key in sapling wallet"
      ~tags:["sapling"; "wallet"]
    @@ fun protocol ->
    let client_with_contract () =
      (* A client with a pre-registered contract to link the wallet with. *)
      let* client = Client.init_mockup ~protocol () in
      let* () =
        Client.remember_contract
          ~alias:non_originated_contract_name
          ~address:non_originated_contract_address
          client
      in
      return client
    in
    let* () =
      (* Import key without forcing and without pre-existing alias *)
      Log.info "Import key, no force" ;
      let* client = client_with_contract () in
      Client.sapling_import_key
        ~new_:key_name
        ~mnemonic
        ~unencrypted
        ~force:false
        client
    in
    let* () =
      (* Import key with forcing and without pre-existing alias *)
      Log.info "Import key, force and non-previously saved" ;
      let* client = client_with_contract () in
      Client.sapling_import_key
        ~new_:key_name
        ~mnemonic
        ~unencrypted
        ~force:true
        client
    in
    let* () =
      (* Import key with forcing and with pre-existing alias *)
      Log.info "Import key force and previously saved" ;
      let* client = client_with_contract () in
      let* () =
        Client.sapling_import_key
          ~new_:key_name
          ~mnemonic
          ~unencrypted
          ~force:false
          client
      in
      Client.sapling_import_key
        ~new_:key_name
        ~mnemonic
        ~unencrypted
        ~force:true
        client
    in
    unit

  let test_address_generation =
    Protocol.register_test
      ~__FILE__
      ~title:"address generation in sapling wallet"
      ~tags:["sapling"; "wallet"; "generation"]
    @@ fun protocol ->
    let client_with_key () =
      (* A client with a pre-registered key in the sapling wallet. *)
      let* client = Client.init_mockup ~protocol () in
      let* () =
        Client.sapling_import_key ~new_:key_name ~mnemonic ~unencrypted client
      in
      return client
    in
    let* () =
      let* client = client_with_key () in
      let expected_address =
        "zet13XtyU5Bkasoj1b19sy4DJc7U13XydbxLHqLUdf8Y5tarGbHgFLgDrT6J6FYJoHGL3"
      in
      let expected_index = 0 in
      let* address, index = Client.sapling_gen_address ~name:key_name client in
      Check.(
        (index = expected_index)
          int
          ~__LOC__
          ~error_msg:"Expected index %R, got %L") ;
      Check.(
        (address = expected_address)
          string
          ~__LOC__
          ~error_msg:"Expected address %R, got %L") ;
      unit
    in
    let* () =
      [
        ( 0,
          "zet13XtyU5Bkasoj1b19sy4DJc7U13XydbxLHqLUdf8Y5tarGbHgFLgDrT6J6FYJoHGL3",
          0 );
        ( 1,
          "zet13mN26QV67FgPzMSzrigXjKZNMMtCubhi9L3kUePnFYdXqEjc8pmjw1h2wC6NLZf5F",
          1 );
        ( 2,
          "zet12hzbYRRKbWwKPY61FkjLg7CRWTcjooqeH7VH18fA6Vxnwy7WyTrAEqBXmEdHp9woU",
          4 );
        ( 3,
          "zet12hzbYRRKbWwKPY61FkjLg7CRWTcjooqeH7VH18fA6Vxnwy7WyTrAEqBXmEdHp9woU",
          4 );
        ( 4,
          "zet12hzbYRRKbWwKPY61FkjLg7CRWTcjooqeH7VH18fA6Vxnwy7WyTrAEqBXmEdHp9woU",
          4 );
        ( 100,
          "zet14LmgdAzVrTtQKpeuD3f2y7wFSjXTMEexNNuiEWhGimm25enxkqmwmbdFsC4y6YmYx",
          100 );
        ( 143534,
          "zet14EWNxZYoHJASHFpcCYSfTfQokWSMzdJeV5SfaGEPDtiYiDCX5jz8QkMF5jZaK5F4k",
          143536 );
        ( 42,
          "zet143WVQUmNodhSe4ytHL6gvtdXYhRp7bywDWASUFYUCMGAS71juXT6AyWY89fjg3eZn",
          42 );
        ( 90870987456348,
          "zet13CiUqFsVEr2LdMnyyUQNL3Nh74sa4LdU6V3oD3YfcizbwuFtftPRYvRrB2zsVaEw1",
          90870987456348 );
      ]
      |> Lwt_list.iter_s
         @@ fun (requested_index, expected_address, expected_index) ->
         let* client = client_with_key () in
         let* address, index =
           Client.sapling_gen_address
             ~name:key_name
             ~address_index:requested_index
             client
         in
         Check.(
           (index = expected_index)
             int
             ~__LOC__
             ~error_msg:"Expected index %R, got %L") ;
         Check.(
           (address = expected_address)
             string
             ~__LOC__
             ~error_msg:"Expected address %R, got %L") ;
         unit
    in
    unit
end

(* Tests involving sapling key management and shielded transactions
   using the shielded tez example contract. *)
let test_sapling_shielded_tez =
  Protocol.register_test
    ~__FILE__
    ~title:"test sapling with shielded tez"
    ~tags:["sapling"; "shield"]
  @@ fun protocol ->
  let burn_cap = Tez.of_int 3 in
  let contract_path = sapling_contract_path protocol in
  let contract_name = "sapling" in
  Log.info "Originate sapling contract" ;
  let* client = Client.init_mockup ~protocol () in
  let sender = Constant.bootstrap1 in
  let* contract =
    Client.originate_contract
      ~wait:"none"
      ~init:"{}"
      ~alias:contract_name
      ~amount:Tez.zero
      ~burn_cap
      ~src:sender.alias
      ~prg:contract_path
      client
  in
  let c = (client, contract) in
  Log.info "Generate bob" ;
  let key_name = "bob" in
  let* bob_mnemonic =
    Client.sapling_gen_key ~name:key_name ~unencrypted:true client
  in
  let* () =
    Client.sapling_use_key ~sapling_key:key_name ~contract ~memo_size:8 client
  in
  Log.info "List keys bob" ;
  let* keys = Client.sapling_list_keys client in
  Check.(
    (["bob"] = keys)
      (list string)
      ~__LOC__
      ~error_msg:"Expected list of keys to be %L, found %R") ;

  (* NB: another key (ali) is generated in the test, but the
     mnemonic is not saved. We add this test to verify the list keys
     command orders alphabetically *)
  Log.info "List keys with ali and bob" ;
  let key_name = "ali" in
  let* (_ : string list) =
    Client.sapling_gen_key ~name:key_name ~unencrypted:true client
  in
  let* () =
    Client.sapling_use_key ~sapling_key:key_name ~contract ~memo_size:8 client
  in
  let* keys = Client.sapling_list_keys client in
  Check.(
    (["ali"; "bob"] = keys)
      (list string)
      ~__LOC__
      ~error_msg:"Expected list of keys to be %L, found %R") ;
  Log.info "Generate bob_address_0" ;
  let* bob_address_0, last_address_index =
    Client.sapling_gen_address ~name:"bob" client
  in
  Log.info "Generate bob_address_1" ;
  let* bob_address_1, bob_index_1 =
    Client.sapling_gen_address ~name:"bob" client
  in
  Check.(
    (bob_index_1 > last_address_index)
      int
      ~__LOC__
      ~error_msg:"Expected %L to be greater than %R") ;
  Log.info "Check bob balance" ;
  let* () = Helpers.check_balance ~__LOC__ c "bob" Tez.zero in
  Log.info "Shield bob_address_0" ;
  let tx_amount = Tez.of_int 100 in
  let* _ =
    Client.sapling_shield
      ~burn_cap
      ~qty:tx_amount
      ~src_tz:"bootstrap2"
      ~dst_sap:bob_address_0
      ~sapling_contract:contract
      client
  in
  let* () = Helpers.check_balance ~__LOC__ c "bob" tx_amount in
  Log.info "Check contract balance after shielding" ;
  let* balance = Client.get_balance_for ~account:contract client in
  Check.(
    (balance = tx_amount)
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected sapling contract's balance to be %R, was %L") ;
  Log.info "Regenerate bob from mnemonic" ;
  (* Overwrite the old 'bob' key with one restored from the mnemonic.  *)
  let key_name = "bob" in
  let* () =
    Client.sapling_import_key
      ~new_:key_name
      ~mnemonic:bob_mnemonic
      ~force:true
      ~unencrypted:true
      client
  in
  Log.info "Test derive alice" ;
  let* path =
    Client.sapling_derive_key
      ~name:"bob"
      ~new_:"alice"
      ~for_contract:contract
      ~child_index:0
      ~unencrypted:true
      client
  in
  Check.(
    (path = "0/0")
      string
      ~__LOC__
      ~error_msg:"Expected derived key for alice to have path %R, got %L") ;
  Log.info "Test derive yves" ;
  let* path =
    Client.sapling_derive_key
      ~name:"bob"
      ~new_:"yves"
      ~for_contract:contract
      ~child_index:1
      ~unencrypted:true
      client
  in
  Check.(
    (path = "0/1")
      string
      ~__LOC__
      ~error_msg:"Expected derived key for yves to have path %R, got %L") ;
  Log.info "Generate alice_address_0" ;
  let* alice_address_0, _ = Client.sapling_gen_address ~name:"alice" client in
  Log.info "Alice shields money insufficient funds" ;
  let bootstrap3 = Constant.bootstrap3.public_key_hash in
  let* alice_balance = Client.get_balance_for ~account:bootstrap3 client in
  let amount = Tez.(alice_balance + alice_balance) in
  let* () =
    let msg =
      rex
        (sf
           "Balance of contract %s too low \\(%s\\) to spend %s"
           bootstrap3
           (Tez.to_string alice_balance)
           (Tez.to_string amount))
    in
    Client.spawn_sapling_shield
      ~burn_cap
      ~qty:amount
      ~src_tz:bootstrap3
      ~dst_sap:alice_address_0
      ~sapling_contract:contract
      client
    |> Process.check_error ~msg
  in
  Log.info "Alice shields money" ;
  let* _balance_diff =
    Client.sapling_shield
      ~burn_cap
      ~qty:tx_amount
      ~src_tz:"bootstrap3"
      ~dst_sap:alice_address_0
      ~sapling_contract:contract
      client
  in
  let* () = Helpers.check_balance ~__LOC__ c "alice" tx_amount in
  Log.info "Forge alice to bob, insufficient funds" ;
  let* () =
    [("sapling_transaction_bin", false); ("sapling_transaction_json", true)]
    |> Lwt_list.iter_s (fun (transaction_file, use_json) ->
           let transaction_file = Temp.file transaction_file in
           let amount = Tez.of_int 2100000000 in
           let account = "alice" in
           Client.spawn_sapling_forge_transaction
             ~burn_cap
             ~qty:amount
             ~src_sap:account
             ~dst_sap:bob_address_1
             ~sapling_contract:contract
             ~file:transaction_file
             ~json:use_json
             client
           |> Process.check_error
                ~msg:
                  (rex
                     ("Balance too low \\(100\\) to spend "
                    ^ Tez.to_string amount)))
  in
  Log.info "Forge alice to bob_address_0" ;
  let sapling_transaction0_bin = Temp.file "sapling_transaction0.bin" in
  let* () =
    Client.sapling_forge_transaction
      ~burn_cap
      ~qty:tx_amount
      ~src_sap:"alice"
      ~dst_sap:bob_address_0
      ~sapling_contract:contract
      ~file:sapling_transaction0_bin
      client
  in
  Log.info "Forge alice to bob_address_1" ;
  let sapling_transaction1_bin = Temp.file "sapling_transaction1.bin" in
  let* () =
    Client.sapling_forge_transaction
      ~burn_cap
      ~qty:(Tez.of_int 50)
      ~src_sap:"alice"
      ~dst_sap:bob_address_0
      ~sapling_contract:contract
      ~file:sapling_transaction1_bin
      client
  in
  Log.info "Check sapling balances post forge (binary format)" ;
  let* () = Helpers.check_balance ~__LOC__ c "alice" tx_amount in
  let* () = Helpers.check_balance ~__LOC__ c "bob" tx_amount in
  Log.info "Submit alice to bob_address_1 (binary format)" ;
  let* () =
    Client.sapling_submit
      ~file:sapling_transaction1_bin
      ~alias_tz:"bootstrap2"
      ~sapling_contract:contract
      ~burn_cap
      client
  in
  Log.info "Check sapling balances after successful transaction in binary" ;
  let* () = Helpers.check_balance ~__LOC__ c "alice" (Tez.of_int 50) in
  let* () = Helpers.check_balance ~__LOC__ c "bob" (Tez.of_int 150) in
  Log.info "Forge alice to bob_address_1 (JSON format)" ;
  let sapling_transaction1_json = Temp.file "sapling_transaction1.json" in
  let* () =
    Client.sapling_forge_transaction
      ~burn_cap
      ~qty:(Tez.of_int 50)
      ~src_sap:"alice"
      ~dst_sap:bob_address_1
      ~sapling_contract:contract
      ~file:sapling_transaction1_json
      ~json:true
      client
  in
  (* Try to load the file as JSON. Must not fail. *)
  let (_ : JSON.t) = JSON.parse_file sapling_transaction1_json in
  Log.info "Check sapling balances after post forge in JSON" ;
  let* () = Helpers.check_balance ~__LOC__ c "alice" (Tez.of_int 50) in
  let* () = Helpers.check_balance ~__LOC__ c "bob" (Tez.of_int 150) in
  Log.info "Submit alice to bob_address_1 (JSON format)" ;
  let* () =
    Client.sapling_submit
      ~file:sapling_transaction1_json
      ~alias_tz:"bootstrap2"
      ~sapling_contract:contract
      ~burn_cap
      ~json:true
      client
  in
  Log.info "Check sapling balances after successful transaction in json" ;
  let* () = Helpers.check_balance ~__LOC__ c "alice" (Tez.of_int 0) in
  let* () = Helpers.check_balance ~__LOC__ c "bob" (Tez.of_int 200) in
  Log.info "Submit alice to bob0" ;
  let* () =
    Client.spawn_sapling_submit
      ~file:sapling_transaction0_bin
      ~alias_tz:"bootstrap2"
      ~sapling_contract:contract
      ~burn_cap
      client
    |> Process.check_error
  in
  Log.info "Unshields money, insufficient funds" ;
  let* () =
    [
      (2100000000, 200, "bob");
      (300, 200, "bob");
      (2100000000, 0, "alice");
      (100, 0, "alice");
    ]
    |> Lwt_list.iter_s @@ fun (requested_token, real_balance, key_name) ->
       let requested_token = Tez.of_int requested_token in
       let real_balance = Tez.of_int real_balance in
       Client.spawn_sapling_unshield
         ~qty:requested_token
         ~src_sap:key_name
         ~dst_tz:"bootstrap4"
         ~burn_cap
         ~sapling_contract:contract
         client
       |> Process.check_error
            ~msg:
              (rex
                 ("Balance too low \\(" ^ Tez.to_string real_balance
                ^ "\\) to spend "
                 ^ Tez.to_string requested_token))
  in
  Log.info "Bob unshields money" ;
  let* bootstrap4_balance_prev =
    Client.get_balance_for ~account:"bootstrap4" client
  in
  let amount = Tez.of_int 90 in
  let* _balance_diff, fees =
    Client.sapling_unshield
      ~qty:amount
      ~src_sap:"bob"
      ~dst_tz:"bootstrap4"
      ~sapling_contract:contract
      ~burn_cap
      client
  in
  let bobs_balance = Tez.(of_int 200 - amount) in
  let* () = Helpers.check_balance ~__LOC__ c "bob" bobs_balance in
  let* bootstrap4_balance =
    Client.get_balance_for ~account:"bootstrap4" client
  in
  Check.(
    (bootstrap4_balance = Tez.(bootstrap4_balance_prev - fees + amount))
      Tez.typ
      ~__LOC__
      ~error_msg:
        "Expected bootstrap4's balance to increase with transferred amount \
         minus fees (= %R), but found %L") ;
  (* Import key with forcing and with pre-existing alias *)
  Log.info "Check state with a new name" ;
  let bob2 = "bob2" in
  let* () =
    Client.sapling_import_key
      ~new_:bob2
      ~mnemonic:bob_mnemonic
      ~force:true
      ~unencrypted:true
      client
  in
  let* () = Helpers.check_balance ~__LOC__ c "bob2" bobs_balance in
  Log.info "Test shielded transfer using non-sapling transfer method" ;
  let transparent_signer = "bootstrap4" in
  let transaction_filename = Temp.file "sapling_transaction_2.bin" in
  let* () =
    Client.sapling_forge_transaction
      ~burn_cap
      ~qty:(Tez.of_int 10)
      ~src_sap:"bob"
      ~dst_sap:bob_address_1
      ~sapling_contract:contract
      ~file:transaction_filename
      client
  in
  let transaction =
    Base.read_file transaction_filename
    |> Base.replace_string (rex "\\s+") ~by:" "
  in
  let* () =
    Client.transfer
      ~burn_cap
      ~amount:Tez.zero
      ~giver:transparent_signer
      ~receiver:contract_name
      ~arg:("{" ^ transaction ^ " }")
      client
  in
  Log.info "Check sapling balances after calling smart contract" ;
  let* () = Helpers.check_balance ~__LOC__ c "alice" (Tez.of_int 0) in
  let* () = Helpers.check_balance ~__LOC__ c "bob" (Tez.of_int 110) in

  unit

let test_sapling_state_corruption =
  Protocol.register_test
    ~__FILE__
    ~title:"test sapling state corruption"
    ~tags:["sapling"; "shield"; "state_corruption"]
  @@ fun protocol ->
  let burn_cap = Tez.of_int 3 in
  let* client = Client.init_mockup ~protocol () in
  Log.info "Push sapling state with id is forbidden" ;
  let prg =
    sapling_contract_path ~script:"sapling_push_sapling_state.tz" protocol
  in
  let src = "bootstrap1" in
  let msg = rex "big_map or sapling_state type not expected here" in
  let* () =
    Client.spawn_originate_contract
      ~burn_cap
      ~alias:"push_sapling_state"
      ~prg
      ~amount:Tez.zero
      ~src
      ~init:"Unit"
      client
    |> Process.check_error ~msg
  in
  Log.info "Make sure sapling state with id 0 exists" ;
  let* _alias, _address =
    Client.originate_contract_at
      ~burn_cap
      ~amount:Tez.zero
      ~src
      ~init:"{}"
      client
      ["opcodes"; "sapling_empty_state"]
      protocol
  in
  Log.info "Originate with id is forbidden" ;
  let msg = rex "Unexpected forged value" in
  let _alias, process =
    Client.spawn_originate_contract_at
      ~burn_cap
      ~amount:Tez.zero
      ~src
      ~init:"0"
      ~alias:"sapling_empty_state2"
      client
      ["opcodes"; "sapling_empty_state"]
      protocol
  in
  process |> Process.check_error ~msg

module Memo_size = struct
  let burn_cap = Tez.of_int 3

  let test_sapling_memo_size =
    Protocol.register_test
      ~__FILE__
      ~title:"test sapling memo size"
      ~tags:["sapling"; "shield"; "memo_size"]
    @@ fun protocol ->
    let burn_cap = Tez.of_int 3 in
    let contract_name memo_size =
      "sapling_memo_size_" ^ string_of_int memo_size
    in
    let generate_contract memo_size =
      let contents =
        sf
          {|
           parameter unit;
           storage (sapling_state %d);
           code {
                 DROP;
                 SAPLING_EMPTY_STATE %d;
                 NIL operation;
                 PAIR;
                }
           |}
          memo_size
          memo_size
      in
      let contract_path = Temp.file "c.tz" in
      Base.write_file contract_path ~contents ;
      contract_path
    in
    let* client = Client.init_mockup ~protocol () in
    Log.info "Originate and update with valid size" ;
    let* () =
      [0; 1; 10; 42; 100; 65535]
      |> Lwt_list.iter_s @@ fun memo_size ->
         let contract_path = generate_contract memo_size in
         let sender = "bootstrap1" in
         let* _address =
           Client.originate_contract
             ~burn_cap
             ~alias:(contract_name memo_size)
             ~amount:Tez.zero
             ~prg:contract_path
             ~src:sender
             ~init:"{ }"
             client
         in
         Client.transfer
           ~amount:Tez.zero
           ~burn_cap
           ~giver:"bootstrap1"
           ~receiver:(contract_name memo_size)
           ~arg:"Unit"
           client
    in
    Log.info "Originate with invalid size" ;
    [-1; 65536; 65598909; 908923434]
    |> Lwt_list.iter_s @@ fun memo_size ->
       let contract_path = generate_contract memo_size in
       let sender = "bootstrap1" in
       Client.spawn_originate_contract
         ~burn_cap
         ~alias:(contract_name memo_size)
         ~amount:Tez.zero
         ~prg:contract_path
         ~src:sender
         client
       |> Process.check_error ~msg:(rex "expected a positive 16-bit integer")

  (* Deploy a sapling contract using a sapling state with a memo size N and *)
  (* create transactions with a memo size of M *)
  let test_sapling_with_different_memo_size =
    Protocol.register_test
      ~__FILE__
      ~title:"test sapling different memo size"
      ~tags:["sapling"; "shield"; "memo_size"]
    @@ fun protocol ->
    let* client = Client.init_mockup ~protocol () in
    Log.info "Shield with different memo size" ;
    let prg = sapling_contract_path protocol in
    let implicit_account = "bootstrap1" in
    let contract_name = "sapling_contract" in
    let* contract_address =
      Client.originate_contract
        ~alias:contract_name
        ~burn_cap
        ~amount:Tez.zero
        ~prg
        ~src:implicit_account
        ~init:"{ }"
        client
    in
    let* (_ : string list) =
      Client.sapling_gen_key ~name:"alice" ~unencrypted:true client
    in
    let* () =
      Client.sapling_use_key
        ~sapling_key:"alice"
        ~contract:contract_name
        ~memo_size:16
        client
    in
    let* address, _index = Client.sapling_gen_address ~name:"alice" client in
    Client.spawn_sapling_shield
      ~burn_cap
      ~qty:(Tez.of_int 100)
      ~src_tz:implicit_account
      ~dst_sap:address
      ~sapling_contract:contract_address
      client
    |> Process.check_error ~msg:(rex "Memo sizes of two sapling states")

  (* Deploy a sapling contract using a sapling state with a memo size N and *)
  (* create transactions with a memo size of N and diverse messages *)
  let test_sapling_with_right_memo_size =
    Protocol.register_test
      ~__FILE__
      ~title:"test sapling right memo size"
      ~tags:["sapling"; "shield"; "memo_size"]
    @@ fun protocol ->
    let* client = Client.init_mockup ~protocol () in
    let tx_amount = Tez.of_int 100 in
    let prg = sapling_contract_path protocol in
    Log.info "Shield with same memo size" ;
    let contract_name = "sapling_memo_size_same" in
    let implicit_account = "bootstrap1" in
    let* contract_address =
      Client.originate_contract
        ~alias:contract_name
        ~burn_cap
        ~amount:Tez.zero
        ~prg
        ~src:implicit_account
        ~init:"{ }"
        client
    in
    let* (_ : string list) =
      Client.sapling_gen_key ~name:"alice" ~unencrypted:true client
    in
    let* () =
      Client.sapling_use_key
        ~sapling_key:"alice"
        ~contract:contract_name
        ~memo_size:8
        client
    in
    let* address, _index = Client.sapling_gen_address ~name:"alice" client in

    (* Should pass since memo-sizes are equal and message is filled
       with 0's *)
    Log.info "Shielding with empty message" ;
    let* _balance_diff =
      Client.sapling_shield
        ~burn_cap
        ~qty:tx_amount
        ~src_tz:implicit_account
        ~dst_sap:address
        ~sapling_contract:contract_address
        client
    in
    Log.info "Shielding with a derived key" ;
    let* _path =
      Client.sapling_derive_key
        ~name:"alice"
        ~new_:"bob"
        ~for_contract:contract_name
        ~child_index:10
        ~unencrypted:true
        client
    in
    let* address_derived, _index =
      Client.sapling_gen_address ~name:"bob" client
    in
    let* _balance_diff =
      Client.sapling_shield
        ~burn_cap
        ~qty:tx_amount
        ~src_tz:implicit_account
        ~dst_sap:address_derived
        ~sapling_contract:contract_address
        client
    in
    Log.info "Now with a too short message" ;
    let* _balance_diff =
      Client.sapling_shield
        ~burn_cap
        ~qty:tx_amount
        ~src_tz:implicit_account
        ~dst_sap:address
        ~sapling_contract:contract_address
        ~message:"aB"
        client
    in
    Log.info "Now with a right length message" ;
    let* _balance_diff =
      Client.sapling_shield
        ~burn_cap
        ~qty:tx_amount
        ~src_tz:implicit_account
        ~dst_sap:address
        ~sapling_contract:contract_address
        ~message:"aBbf19F00a"
        client
    in
    Log.info "Now with a too long message" ;
    let* _balance_diff =
      Client.sapling_shield
        ~burn_cap
        ~qty:tx_amount
        ~src_tz:implicit_account
        ~dst_sap:address
        ~sapling_contract:contract_address
        ~message:"aBbf19F00aaBbf19F00aC"
        client
    in
    unit
end

(* Roundtrip of shield, transfer and unshield. At each step we check the
   balances of the Tezos account that submits the operations, the two Sapling
   accounts and the smart contract. *)
let successful_roundtrip =
  Protocol.register_test
    ~__FILE__
    ~title:"successful_roundtrip"
    ~tags:["sapling"]
  @@ fun protocol ->
  let open Helpers in
  let* c = init protocol (sapling_contract_path protocol) in
  let alice_tz1 = Constant.bootstrap2 in
  let* alice_address = gen_user c "alice" in
  let* bob_address = gen_user c "bob" in
  let* () = check_balance ~__LOC__ c "alice" Tez.zero in
  let* () = check_balance ~__LOC__ c "bob" Tez.zero in
  let shield_amount = Tez.of_int 10 in
  let* balance_alice_tz1_before = balance_tz1 c alice_tz1.public_key_hash in
  let* amount, fees = shield c alice_tz1 alice_address shield_amount in
  let* balance_alice_tz1_after = balance_tz1 c alice_tz1.public_key_hash in
  Check.(
    (amount = shield_amount) Tez.typ ~__LOC__ ~error_msg:"Expected %R, got %L") ;
  Check.(
    (balance_alice_tz1_after
    = Tez.(balance_alice_tz1_before - shield_amount - fees))
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let* balance_contract = balance_tz1 c (snd c) in
  Check.(
    (balance_contract = shield_amount)
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let* () = check_balance ~__LOC__ c "alice" shield_amount in
  let* () = check_balance ~__LOC__ c "bob" Tez.zero in
  let* () = transfer c "alice" bob_address shield_amount in
  let* balance_contract = balance_tz1 c (snd c) in
  Check.(
    (balance_contract = shield_amount)
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let* () = check_balance ~__LOC__ c "alice" Tez.zero in
  let* () = check_balance ~__LOC__ c "bob" shield_amount in
  let* balance_alice_tz1_before = balance_tz1 c alice_tz1.public_key_hash in
  let* amount, fees = unshield c "bob" alice_tz1 shield_amount in
  let* balance_alice_tz1_after = balance_tz1 c alice_tz1.public_key_hash in
  Check.(
    (amount = Tez.(to_mutez shield_amount * -1 |> Tez.of_mutez_int))
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  Check.(
    (balance_alice_tz1_after
    = Tez.(balance_alice_tz1_before + shield_amount - fees))
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let* balance_contract = balance_tz1 c (snd c) in
  Check.(
    (balance_contract = Tez.zero)
      Tez.typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let* () = check_balance ~__LOC__ c "alice" Tez.zero in
  let* () = check_balance ~__LOC__ c "bob" Tez.zero in
  unit

let register ~protocols =
  Insufficient_funds.shield protocols ;
  Insufficient_funds.transfer protocols ;
  Insufficient_funds.unshield protocols ;
  successful_roundtrip protocols ;
  Wallet.test_import_key protocols ;
  Wallet.test_address_generation protocols ;
  test_sapling_shielded_tez protocols ;
  test_sapling_state_corruption protocols ;
  Memo_size.test_sapling_memo_size protocols ;
  Memo_size.test_sapling_with_different_memo_size protocols ;
  Memo_size.test_sapling_with_right_memo_size protocols
