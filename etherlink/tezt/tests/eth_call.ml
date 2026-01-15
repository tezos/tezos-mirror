(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Etherlink: EVM simulator
   Requirement:  make -f etherlink.mk build
                 npm install eth-cli
                 # Install cast or foundry (see: https://book.getfoundry.sh/getting-started/installation)
                 curl -L https://foundry.paradigm.xyz | bash
                 foundryup
                 make octez-evm-node
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file eth_call.ml
*)

open Setup
open Test_helpers

let call_evm_rpc_no_err ?private_ node req =
  let* response = Evm_node.call_evm_rpc ?private_ node req in
  let err = JSON.(response |-> "error") in
  match JSON.unannotate err with
  | `Null ->
      (* No error *)
      return @@ Evm_node.extract_result response
  | _ ->
      Test.fail
        "JSONRPC call to %s failed with %s"
        req.method_
        (JSON.encode err)

let call_evm_rpc_err ?private_ node req =
  let* response = Evm_node.call_evm_rpc ?private_ node req in
  let err = Evm_node.extract_error_message response in
  match JSON.unannotate err with
  | `Null ->
      (* No error *)
      Test.fail "JSONRPC call to %s did not fail" req.method_
  | _ -> return @@ JSON.as_string err

let eth_call_and_estimate_gas ?private_ node parameters =
  let* call_result =
    Evm_node.(
      call_evm_rpc_no_err
        ?private_
        node
        {method_ = "eth_call"; parameters = `A parameters})
  in
  let* _ =
    Evm_node.(
      call_evm_rpc_no_err
        ?private_
        node
        {method_ = "eth_estimateGas"; parameters = `A parameters})
  in
  return call_result

let eth_call_and_estimate_gas_err ?private_ node parameters =
  let* error_message =
    Evm_node.(
      call_evm_rpc_err
        ?private_
        node
        {method_ = "eth_call"; parameters = `A parameters})
  in
  let* _ =
    Evm_node.(
      call_evm_rpc_err
        ?private_
        node
        {method_ = "eth_estimateGas"; parameters = `A parameters})
  in
  return error_message

let register ?genesis_timestamp ?eth_bootstrap_accounts ?tez_bootstrap_accounts
    ?(kernels = Kernel.etherlink_all) ?preimages_dir ?maximum_allowed_ticks
    ?enable_fa_bridge ?rollup_history_mode ?additional_uses ~title ~tags body
    protocols =
  register_test_for_kernels
    ~__FILE__
    ~time_between_blocks:Nothing
    ?genesis_timestamp
    ?eth_bootstrap_accounts
    ?tez_bootstrap_accounts
    ~kernels
    ?preimages_dir
    ?maximum_allowed_ticks
    ?enable_fa_bridge
    ?additional_uses
    ?rollup_history_mode
    ~enable_dal:false
    ~enable_multichain:false
    ~title:(title ^ " in eth_call and eth_estimateGas")
    ~tags:(tags @ ["evm"; "state_override"; "eth_call"; "eth_estimategas"])
    body
    protocols

let test_call_state_override_balance =
  register ~tags:["balance_override"] ~title:"Can override balance"
  @@ fun {sequencer; evm_version; _} _protocol ->
  (*
      This test checks that the simulation allows balance override.
      To do so we deploy a contract which returns the balance of the message
      sender, and call it with a non-sensical address.
  *)
  let* constant = Solidity_contracts.state_override_tester evm_version in
  let* () = Eth_cli.add_abi ~label:constant.label ~abi:constant.abi () in
  (* Deploy the contract. *)
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:constant.abi
         ~bin:constant.bin)
      sequencer
  in
  let caller_address = "0x0123456789012345678901234567890123456789" in
  let* calldata = Cast.calldata "getBalance()" in
  let call =
    `O
      [
        ("from", `String caller_address);
        ("to", `String contract);
        ("data", `String calldata);
      ]
  in
  let* call_result = eth_call_and_estimate_gas sequencer [call] in
  Check.(
    (call_result |> JSON.as_string
   = "0x0000000000000000000000000000000000000000000000000000000000000000")
      string)
    ~error_msg:"Expected result %R but got %L " ;
  let override_balance =
    `O [(caller_address, `O [("balance", `String "0xffff")])]
  in
  let* call_result =
    eth_call_and_estimate_gas sequencer [call; override_balance]
    (* we omit the block paramater to test the encoding *)
  in
  Check.(
    (call_result |> JSON.as_string
   = "0x000000000000000000000000000000000000000000000000000000000000ffff")
      string)
    ~error_msg:"Expected result %R but got %L " ;

  unit

let test_call_state_override_code =
  register ~tags:["code_override"] ~title:"Can override code"
  @@ fun {sequencer; evm_version; _} _protocol ->
  (*
      This test checks that the simulation allows code override.
      To do so we deploy a contract without any function, and call it with an
      alternative code that does have a function.
  *)
  let* constant = Solidity_contracts.state_override_tester evm_version in
  let* constant_readable =
    Solidity_contracts.state_override_tester_readable evm_version
  in
  let* () = Eth_cli.add_abi ~label:constant.label ~abi:constant.abi () in
  (* Deploy the contract. *)
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:constant.abi
         ~bin:constant.bin)
      sequencer
  in
  let bytecode_accessor = read_file constant_readable.deployed_bin in
  let* calldata = Cast.calldata "getCount()" in
  let call = `O [("to", `String contract); ("data", `String calldata)] in

  (* Check that the contract normaly doesn't allow "getCount()" *)
  let* err_msg =
    eth_call_and_estimate_gas_err sequencer [call; `String "latest"]
  in
  Check.((err_msg = "execution reverted") string)
    ~error_msg:"Expected error %R but got %L " ;

  (* try again with an override *)
  let override_code =
    `O [(contract, `O [("code", `String ("0x" ^ bytecode_accessor))])]
  in
  let* call_result =
    eth_call_and_estimate_gas sequencer [call; `String "latest"; override_code]
  in
  Check.(
    (call_result |> JSON.as_string
   = "0x000000000000000000000000000000000000000000000000000000000000002a")
      string)
    ~error_msg:"Expected result %R but got %L " ;

  unit

let test_call_state_override_nonce =
  register ~tags:["nonce_override"] ~title:"Can override nonce"
  @@ fun {sequencer; evm_version; _} _protocol ->
  (*
      This test checks that the simulation allows nonce override.
      To do so we deploy a contract that creates a contract and returns the
      address of the new contract, and call it twice with different nonce.
      The addresses should be different.
      *)
  let* factory = Solidity_contracts.state_override_tester evm_version in
  let* () = Eth_cli.add_abi ~label:factory.label ~abi:factory.abi () in
  (* Deploy the contract. *)
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:factory.abi
         ~bin:factory.bin)
      sequencer
  in

  let* calldata = Cast.calldata "create()" in
  let caller_address = Eth_account.bootstrap_accounts.(0).address in
  let call =
    `O
      [
        ("from", `String caller_address);
        ("to", `String contract);
        ("data", `String calldata);
      ]
  in

  (* Call a first time to have an address *)
  let* call_result =
    eth_call_and_estimate_gas sequencer [call; `String "latest"]
  in
  let addr1 = call_result |> JSON.as_string in

  (* try again with an override *)
  let override_code = `O [(contract, `O [("nonce", `String "0x2a")])] in
  let* call_result =
    eth_call_and_estimate_gas sequencer [call; `String "latest"; override_code]
  in
  let addr2 = call_result |> JSON.as_string in

  (* the two address were calculated with different nonce so should be different
     (hopefully) *)
  Check.((addr1 <> addr2) string)
    ~error_msg:"Address should have been different but got %R and %L" ;

  unit

let test_call_state_override_state_diff =
  register ~tags:["state_diff"] ~title:"Can override part of account storage"
  @@ fun {sequencer; evm_version; _} _protocol ->
  (*
      This test checks that the simulation allows state diff override.
      To do so we deploy a contract with a value in storage, and call it with an
      alternative storage that changes that value.
  *)
  let* constant =
    Solidity_contracts.state_override_tester_readable evm_version
  in
  let* () = Eth_cli.add_abi ~label:constant.label ~abi:constant.abi () in
  (* Deploy the contract. *)
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:constant.abi
         ~bin:constant.bin)
      sequencer
  in

  (* helpers *)
  let call_method m =
    let* calldata = Cast.calldata m in
    return (`O [("to", `String contract); ("data", `String calldata)])
  in
  let make_call ?(error = false) ?(override = []) m =
    let* call = call_method m in
    if not error then eth_call_and_estimate_gas sequencer (call :: override)
    else
      let* msg = eth_call_and_estimate_gas_err sequencer (call :: override) in
      return (JSON.annotate ~origin:"err" (`String msg))
  in
  let check_value call_result expected =
    Check.((call_result |> JSON.as_string = expected) string)
      ~error_msg:"Expected result %R but got %L "
  in

  (* Check the starting contract storage *)
  let* call_result = make_call "getCount()" in
  check_value
    call_result
    "0x000000000000000000000000000000000000000000000000000000000000002a" ;
  let* call_result = make_call "const2()" in
  check_value
    call_result
    "0x00000000000000000000000000000000000000000000000000000000ffffffff" ;
  let* call_result = make_call "const3()" in
  check_value
    call_result
    "0x00000000000000000000000000000000000000000000000000000000ffffffff" ;

  (* try again with an override *)
  let state_diff =
    `O
      [
        ( "0x0000000000000000000000000000000000000000000000000000000000000000",
          `String
            "0x0000000000000000000000000000000000000000000000001111111122222222"
        );
      ]
  in
  let override = [`O [(contract, `O [("stateDiff", state_diff)])]] in
  let* call_result = make_call ~override "getCount()" in
  check_value
    call_result
    "0x0000000000000000000000000000000000000000000000000000000022222222" ;
  (* const2 is stored in same memory slot so should change *)
  let* call_result = make_call ~override "const2()" in
  check_value
    call_result
    "0x0000000000000000000000000000000000000000000000000000000011111111" ;
  (* const3 is stored in a distinct memory slot so should be unchanged *)
  let* call_result = make_call ~override "const3()" in
  check_value
    call_result
    "0x00000000000000000000000000000000000000000000000000000000ffffffff" ;

  (* try with an invalid override *)
  let invalid =
    `O
      [
        ( "0x00",
          `String
            "0x0000000000000000000000000000000000000000000000000000000000000000"
        );
      ]
  in
  let override = [`O [(contract, `O [("stateDiff", invalid)])]] in
  let* call_result = make_call ~error:true ~override "getCount()" in
  Check.(
    (call_result |> JSON.as_string = "Error:\n  00 is not a valid storage key\n")
      string)
    ~error_msg:"Expected error %R but got %L " ;

  unit

let test_call_state_override_state =
  register
    ~tags:["state_replace"]
    ~title:"Can override completely account storage"
  @@ fun {sequencer; evm_version; _} _protocol ->
  (*
      This test checks that the simulation allows state override.
      To do so we deploy a contract with a value in storage, and call it with an
      alternative storage that changes that value.
  *)
  let* constant =
    Solidity_contracts.state_override_tester_readable evm_version
  in
  let* () = Eth_cli.add_abi ~label:constant.label ~abi:constant.abi () in
  (* Deploy the contract. *)
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:constant.abi
         ~bin:constant.bin)
      sequencer
  in

  (* helpers *)
  let call_method m =
    let* calldata = Cast.calldata m in
    return (`O [("to", `String contract); ("data", `String calldata)])
  in
  let make_call ?(error = false) ?(override = []) m =
    let* call = call_method m in
    if not error then eth_call_and_estimate_gas sequencer (call :: override)
    else
      let* msg = eth_call_and_estimate_gas_err sequencer (call :: override) in
      return (JSON.annotate ~origin:"err" (`String msg))
  in
  let check_value call_result expected =
    Check.((call_result |> JSON.as_string = expected) string)
      ~error_msg:"Expected result %R but got %L "
  in

  (* Check the starting contract storage *)
  let* call_result = make_call "getCount()" in
  check_value
    call_result
    "0x000000000000000000000000000000000000000000000000000000000000002a" ;
  let* call_result = make_call "const2()" in
  check_value
    call_result
    "0x00000000000000000000000000000000000000000000000000000000ffffffff" ;
  let* call_result = make_call "sep()" in
  check_value
    call_result
    "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" ;
  let* call_result = make_call "const3()" in
  check_value
    call_result
    "0x00000000000000000000000000000000000000000000000000000000ffffffff" ;

  (* try again with an override *)
  let state =
    `O
      [
        ( "0x0000000000000000000000000000000000000000000000000000000000000000",
          `String
            "0x0000000000000000000000000000000000000000000000001111111122222222"
        );
      ]
  in
  let override = [`O [(contract, `O [("state", state)])]] in
  let* call_result = make_call ~override "getCount()" in
  check_value
    call_result
    "0x0000000000000000000000000000000000000000000000000000000022222222" ;
  (* const2 is stored in same memory slot so should change *)
  let* call_result = make_call ~override "const2()" in
  check_value
    call_result
    "0x0000000000000000000000000000000000000000000000000000000011111111" ;
  (* sep is stored in a distinct memory slot but we replaced everything *)
  let* call_result = make_call ~override "sep()" in
  check_value
    call_result
    "0x0000000000000000000000000000000000000000000000000000000000000000" ;
  (* const3 is stored in a distinct memory slot but we replaced everything *)
  let* call_result = make_call ~override "const3()" in
  check_value
    call_result
    "0x0000000000000000000000000000000000000000000000000000000000000000" ;

  (* try with an invalid override *)
  let invalid =
    `O
      [
        ( "0x00",
          `String
            "0x0000000000000000000000000000000000000000000000000000000000000000"
        );
      ]
  in
  let override = [`O [(contract, `O [("state", invalid)])]] in
  let* call_result = make_call ~error:true ~override "getCount()" in
  Check.(
    (call_result |> JSON.as_string = "Error:\n  00 is not a valid storage key\n")
      string)
    ~error_msg:"Expected error %R but got %L " ;
  unit

let test_call_state_override_state_empty =
  register
    ~tags:["state_empty"]
    ~title:"Can override completely account storage by empty state"
  @@ fun {sequencer; evm_version; _} _protocol ->
  (*
      This test checks that the simulation allows state override.
      To do so we deploy a contract with a value in storage, and call it with an
      alternative storage that changes that value.
  *)
  let* constant =
    Solidity_contracts.state_override_tester_readable evm_version
  in
  let* () = Eth_cli.add_abi ~label:constant.label ~abi:constant.abi () in
  (* Deploy the contract. *)
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:constant.abi
         ~bin:constant.bin)
      sequencer
  in

  (* helpers *)
  let call_method m =
    let* calldata = Cast.calldata m in
    return (`O [("to", `String contract); ("data", `String calldata)])
  in
  let make_call ?(override = []) m =
    let* call = call_method m in
    eth_call_and_estimate_gas sequencer (call :: override)
  in
  let check_value call_result expected =
    Check.((call_result |> JSON.as_string = expected) string)
      ~error_msg:"Expected result %R but got %L "
  in

  (* Check the starting contract storage *)
  let* call_result = make_call "getCount()" in
  check_value
    call_result
    "0x000000000000000000000000000000000000000000000000000000000000002a" ;
  let* call_result = make_call "const2()" in
  check_value
    call_result
    "0x00000000000000000000000000000000000000000000000000000000ffffffff" ;
  let* call_result = make_call "sep()" in
  check_value
    call_result
    "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" ;
  let* call_result = make_call "const3()" in
  check_value
    call_result
    "0x00000000000000000000000000000000000000000000000000000000ffffffff" ;

  (* try with an empty storage override *)
  let empty = `O [] in
  let override = [`O [(contract, `O [("state", empty)])]] in
  let* call_result = make_call ~override "getCount()" in
  check_value
    call_result
    "0x0000000000000000000000000000000000000000000000000000000000000000" ;
  let* call_result = make_call ~override "const2()" in
  check_value
    call_result
    "0x0000000000000000000000000000000000000000000000000000000000000000" ;
  let* call_result = make_call ~override "sep()" in
  check_value
    call_result
    "0x0000000000000000000000000000000000000000000000000000000000000000" ;
  let* call_result = make_call ~override "const3()" in
  check_value
    call_result
    "0x0000000000000000000000000000000000000000000000000000000000000000" ;

  unit

let protocols = Protocol.all

let () =
  test_call_state_override_code protocols ;
  test_call_state_override_nonce protocols ;
  test_call_state_override_state_diff protocols ;
  test_call_state_override_state protocols ;
  test_call_state_override_state_empty protocols ;
  test_call_state_override_balance protocols
