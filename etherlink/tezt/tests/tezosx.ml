(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------

   Requirement:  make -f etherlink.mk build
                 make octez-node octez-client octez-smart-rollup-node octez-evm-node
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file tezosx.ml
 *)

open Rpc.Syntax

let runtime_tags = List.map Tezosx_runtime.tag

(* /!\ Read this before adding a new test

   By default, to write a test, you should use [register_sandbox_test]. Only
   for tests actually requiring to consider L1/L2 communication (typically,
   related to the bridge, or when we need to distinguish between the latest and
   finalized block) is it useful to use [register_fullstack_test]. *)

module Setup = struct
  let register_sandbox_test ?uses_client ~title ~tags ~with_runtimes
      ?tez_bootstrap_accounts =
    Test_helpers.register_sandbox
      ~__FILE__
      ?uses_client
      ?tez_bootstrap_accounts
      ~kernel:Latest
      ~title
      ~tags:(["tezosx"] @ runtime_tags with_runtimes @ tags)
      ~with_runtimes

  let register_fullstack_test ~title ~tags ~with_runtimes
      ?tez_bootstrap_accounts =
    Setup.register_test
      ~__FILE__
      ~rpc_server:Evm_node.Resto
      ~title
      ~tags:(["tezosx"] @ runtime_tags with_runtimes @ tags)
      ~kernel:Latest
      ~with_runtimes
      ~enable_dal:false
      ~enable_multichain:false
      ?tez_bootstrap_accounts
end

let test_runtime_feature_flag ~runtime () =
  Setup.register_sandbox_test
    ~title:"Set Tezos runtime feature flag"
    ~tags:["feature_flag"]
    ~with_runtimes:[runtime]
  @@ fun sandbox ->
  let* rpc_result =
    Rpc.state_value sandbox (Tezosx_runtime.feature_flag runtime)
  in
  match rpc_result with
  | Ok (Some _) -> unit
  | Ok None ->
      Test.fail
        "Feature flag for the %s runtime was not set"
        (String.capitalize_ascii (Tezosx_runtime.to_string runtime))
  | Error err ->
      Test.fail
        "Could not read feature flag %s: %s"
        (Tezosx_runtime.feature_flag runtime)
        err.message

let tezos_client node =
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.{(Evm_node.rpc_endpoint_record node) with path = "/tezlink"})
  in
  Client.init ~endpoint ()

let check_account sandbox (account : Account.key) =
  let* client = tezos_client sandbox in
  let* _balance =
    Client.get_balance_for ~account:account.public_key_hash client
  in
  unit

let account_str_rpc sequencer account key =
  let path =
    sf "/tezlink/chains/main/blocks/head/context/contracts/%s/%s" account key
  in

  let* res =
    Curl.get_raw
      ~name:("curl#" ^ Evm_node.name sequencer)
      ~args:["-v"]
      (Evm_node.endpoint sequencer ^ path)
    |> Runnable.run
  in
  return @@ JSON.parse ~origin:"curl_protocols" res

let account_rpc sequencer account key =
  account_str_rpc sequencer account.Account.public_key_hash key

let test_bootstrap_kernel_config () =
  let tez_bootstrap_accounts = Evm_node.tez_default_bootstrap_accounts in
  Setup.register_sandbox_test
    ~tez_bootstrap_accounts
    ~title:"Set tezos bootstrap accounts"
    ~uses_client:true
    ~tags:["bootstrap"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  Lwt_list.iter_p (check_account sandbox) tez_bootstrap_accounts

let deposit ~(l1_contracts : Tezt_etherlink.Setup.l1_contracts)
    ~sc_rollup_address ~sc_rollup_node ~sequencer ~client ~depositor
    ~(receiver_account : Account.key) ~amount =
  let receiver =
    Result.get_ok
    @@ Tezos_protocol_alpha.Protocol.Contract_repr.of_b58check
         receiver_account.public_key_hash
  in
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Tezos_protocol_alpha.Protocol.Contract_repr.encoding
      receiver
  in
  let (`Hex receiver) = Hex.of_bytes bytes in
  let deposit_info =
    Delayed_inbox.{receiver = TezosAddr receiver; chain_id = None}
  in
  let* () =
    Delayed_inbox.send_deposit_to_delayed_inbox
      ~rlp:true
      ~amount
      ~bridge:l1_contracts.bridge
      ~depositor
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* () =
    Delayed_inbox.wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  unit

let test_reveal =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Reveal tezos native account"
    ~tags:["reveal"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@
  fun {client; sc_rollup_node; sc_rollup_address; sequencer; l1_contracts; _}
      _protocol
    ->
  (* Make a deposit to have an unrevealed account *)
  let amount = Tez.of_int 1000 in
  let depositor = Constant.bootstrap5 in
  let receiver_account = Constant.bootstrap2 in

  let* () =
    deposit
      ~l1_contracts
      ~sc_rollup_address
      ~sc_rollup_node
      ~sequencer
      ~client
      ~depositor
      ~receiver_account
      ~amount
  in
  let* reveal =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~counter:1
            ~source:receiver_account
            (reveal receiver_account ());
        ])
      client
  in
  let* _ =
    Delayed_inbox.send_tezos_operation_to_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~tezosx_format:true
      reveal
  in
  let* () =
    Delayed_inbox.wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* manager_key = account_rpc sequencer receiver_account "manager_key" in
  Check.(
    JSON.(manager_key |> as_string_opt = Some Constant.bootstrap2.public_key)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_transfer =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Transfer on tezos native account"
    ~tags:["transfer"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let amount = Tez.of_int 1000 in
  let depositor = Constant.bootstrap5 in
  let* receiver_account = Client.gen_and_show_keys client in

  let* transfer =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~counter:1
            ~source:depositor
            (transfer ~dest:receiver_account ~amount:(Tez.to_mutez amount) ());
        ])
      client
  in
  let* _ =
    Delayed_inbox.send_tezos_operation_to_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~tezosx_format:true
      transfer
  in
  let* () =
    Delayed_inbox.wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* client = tezos_client sequencer in
  let* balance =
    Client.get_balance_for ~account:receiver_account.public_key_hash client
  in
  Check.(
    (Tez.to_mutez balance = 1_000_000_000)
      int
      ~error_msg:"Expected %R mutez but got %L") ;
  unit

let test_deposit =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Deposit on tezos native account"
    ~tags:["deposit"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let amount = Tez.of_int 1000 in
  let depositor = Constant.bootstrap5 in
  let* receiver_account = Client.gen_and_show_keys client in

  let* () =
    deposit
      ~l1_contracts
      ~sc_rollup_address
      ~sc_rollup_node
      ~sequencer
      ~client
      ~depositor
      ~receiver_account
      ~amount
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* client = tezos_client sequencer in
  let* balance =
    Client.get_balance_for ~account:receiver_account.public_key_hash client
  in
  Check.(
    (Tez.to_mutez balance = 1_000_000_000)
      int
      ~error_msg:"Expected %R mutez but got %L") ;

  (* We expect the deposit to be in the latest blueprint, but the latest
       block should be empty from the POV of Etherlink *)
  let* head = Rpc.get_block_by_number ~block:"latest" sequencer in
  let block_infos =
    match head with
    | Ok block -> block
    | Error _ -> failwith "Should have find latest block"
  in
  match block_infos.transactions with
  | Empty -> unit
  | _ -> failwith "Latest block should be empty"

let test_get_tezos_ethereum_address_rpc ~runtime () =
  Setup.register_sandbox_test
    ~title:"Test the tez_getTezosEthereumAddress RPC"
    ~tags:["rpc"]
    ~with_runtimes:[runtime]
  @@ fun sandbox ->
  let tezos_address = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" in
  let expected = "0xccef676171871a48bbd6e2be75bbcc09d38830c5" in
  let* rpc_result =
    Rpc.Tezosx.tez_getTezosEthereumAddress tezos_address sandbox
  in
  match rpc_result with
  | Ok evm_address ->
      Check.(
        (evm_address = expected) string ~error_msg:"Expected %R but got %L") ;
      unit
  | Error err ->
      Test.fail
        "Could not get the EVM address corresponding to the Tezos address %s: \
         %s"
        tezos_address
        err.Rpc.message

let test_eth_rpc_with_alias ~runtime =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Test ETH RPC on Ethereum address alias from Tezos account"
    ~tags:["rpc"; "alias"]
    ~with_runtimes:[runtime]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@
  fun {
        client;
        observer;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        _;
      }
      _protocol
    ->
  (* Deposit to instantiate the alias on ethereum side *)
  let amount = Tez.of_int 1000 in
  let depositor = Constant.bootstrap5 in
  let* receiver_account = Client.gen_and_show_keys client in

  let receiver =
    Result.get_ok
    @@ Tezos_protocol_alpha.Protocol.Contract_repr.of_b58check
         receiver_account.public_key_hash
  in
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Tezos_protocol_alpha.Protocol.Contract_repr.encoding
      receiver
  in
  let (`Hex receiver) = Hex.of_bytes bytes in
  let deposit_info =
    Delayed_inbox.{receiver = TezosAddr receiver; chain_id = None}
  in
  let* () =
    Delayed_inbox.send_deposit_to_delayed_inbox
      ~rlp:true
      ~amount
      ~bridge:l1_contracts.bridge
      ~depositor
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* () =
    Delayed_inbox.wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let*@ evm_address =
    Rpc.Tezosx.tez_getTezosEthereumAddress
      receiver_account.public_key_hash
      observer
  in
  (* Balance should be 0 *)
  let*@ balance = Rpc.get_balance ~address:evm_address observer in
  Check.((balance = Wei.zero) Wei.typ ~error_msg:"Expected %R wei but got %L") ;
  (* Code should have data *)
  let*@ code = Rpc.get_code ~address:evm_address observer in
  Check.(
    (code
   = "0x6080604052348015600e575f5ffd5b5060a280601a5f395ff3fe608060405236603a576040517f47e794ec00000000000000000000000000000000000000000000000000000000815260040160405180910390fd5b6040517f47e794ec00000000000000000000000000000000000000000000000000000000815260040160405180910390fdfea264697066735822122020f56a12d5d4b6faadf04140b08a9a4af043d02dff2e9363f6d5f11382e5ad5264736f6c634300081e0033"
    )
      string
      ~error_msg:"Expected %R but got %L") ;
  (* Transaction count should be 0 *)
  let*@ transaction_count =
    Rpc.get_transaction_count ~address:evm_address observer
  in
  Check.((transaction_count = 0L) int64 ~error_msg:"Expected %R but got %L") ;
  (* Get storage at 0x0 *)
  let*@ storage = Rpc.get_storage_at ~address:evm_address ~pos:"0x0" observer in
  Check.(
    (storage
   = "0x0000000000000000000000000000000000000000000000000000000000000000")
      string
      ~error_msg:"Expected %R but got %L") ;
  unit

let () =
  test_bootstrap_kernel_config () ;
  test_deposit [Alpha] ;
  test_reveal [Alpha] ;
  test_transfer [Alpha] ;
  test_eth_rpc_with_alias ~runtime:Tezos [Alpha] ;
  test_runtime_feature_flag ~runtime:Tezos () ;
  test_get_tezos_ethereum_address_rpc ~runtime:Tezos ()
