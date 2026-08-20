(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024-2026 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups: Etherlink Sequencer
   Requirement:  make -f etherlink.mk build
                 npm install eth-cli solc@0.8.31
                 # Install cast or foundry (see: https://book.getfoundry.sh/getting-started/installation)
                 curl -L https://foundry.paradigm.xyz | bash
                 foundryup
                 make fa-bridge-watchtower
                 ./scripts/install_dal_trusted_setup.sh
                 # Install websocat (see: https://github.com/vi/websocat?tab=readme-ov-file#installation)
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file bridge_withdrawals.ml
   Subject:      FA and fast withdrawals, including EIP-7702 callers.
*)

open Rpc.Syntax
open Contract_path
open Delayed_inbox
open Test_helpers
open Setup
open Fa_bridge_helpers
module Protocol = Test_helpers.Protocol_no_direct_register

let fast_withdrawal ?(expect_failure = false) ~sender ~endpoint ~amount_wei
    ~produce_block ~receiver ~fast_withdrawal_contract_address () =
  let* () =
    Eth_cli.add_abi
      ~label:"fast_withdraw_base58"
      ~abi:(predep_xtz_bridge_abi_path ())
      ()
  in
  (* Call the withdrawal precompiled contract. *)
  let call_fast_withdrawal =
    Eth_cli.contract_send
      ~expect_failure
      ~source_private_key:sender.Eth_account.private_key
      ~endpoint
      ~abi_label:"fast_withdraw_base58"
      ~address:Solidity_contracts.Precompile.xtz_bridge
        (* NB: the third parameter is unused for now, could be used later for
           maximum fees to pay, whitelist of service providers etc. *)
      ~method_call:
        (sf
           {|fast_withdraw_base58("%s","%s","%s")|}
           receiver
           fast_withdrawal_contract_address
           "0x0000000000000000000000000000000000000000000000000000000000000001")
      ~value:amount_wei
      ~gas:16_000_000
  in
  wait_for_application ~produce_block call_fast_withdrawal

type fast_withdrawal_event =
  | Tez of {
      target : string;
      withdrawal_id : string;
      amount : Tez.t;
      timestamp : string;
      payload : string;
      l2_caller : string;
    }
  | FA of {
      sender : string; (* Holds the L2 caller *)
      ticket_owner : string;
      receiver : string;
      proxy : string;
      amount : Tez.t;
          (* The amount of an FA token is denominated in the underlying asset's unit.
             Since we're testing wrapped Tez, the unit in this case is Tez. *)
      withdrawal_id : string;
      timestamp : string;
      payload : string;
    }

let find_and_decode_fast_withdrawal_event ?(fa_tokens = false) receipt :
    fast_withdrawal_event Lwt.t =
  (* Define the fast withdrawal event log topic, which will be searched for in the EVM logs.
     This topic is a hashed identifier that corresponds to the fast withdrawal transaction event. *)
  let fast_withdrawal_event_signature, fast_withdrawal_event_topic =
    if fa_tokens then
      ( fast_withdrawal_fa_token_event_signature,
        fast_withdrawal_fa_token_event_topic )
    else (fast_withdrawal_tez_event_signature, fast_withdrawal_tez_event_topic)
  in

  (* Extract the event data from the EVM logs based on the provided topic.
     This searches the transaction logs for the specified topic and returns the corresponding data. *)
  (* For FA tokens we match the topic containing ticket hash at the start of the
     signature. Ticket hash is an indexed uint256 and will not appear in the
     encoded data. Indexed fields is the standard way to create a topic in
     solidity, previous signature and topics pair were incorrect. *)
  let queried_topic =
    if fa_tokens then revm_fast_withdrawal_fa_token_event_topic
    else fast_withdrawal_event_topic
  in
  let log =
    let tx_log =
      List.find_opt
        (fun tx ->
          List.exists (String.equal queried_topic) tx.Transaction.topics)
        receipt.Transaction.logs
      |> Option.get
    in
    tx_log.Transaction.data
  in

  let* res =
    Eth_cli.decode_method
      ~abi_label:fast_withdrawal_event_signature
      ~method_:(String.sub fast_withdrawal_event_topic 2 8 ^ log)
      ()
  in
  return
  @@
  match JSON.parse ~origin:"tx_log" res |> JSON.unannotate with
  | `A
      [
        `String target;
        `String withdrawal_id;
        `String amount;
        `String timestamp;
        `String payload;
        `String l2_caller;
      ]
    when not fa_tokens ->
      Tez
        {
          target;
          withdrawal_id;
          amount = Wei.of_string amount |> Wei.to_tez;
          timestamp;
          payload;
          l2_caller;
        }
  | `A
      [
        `String sender;
        `String ticket_owner;
        `String receiver;
        `String proxy;
        `String amount;
        `String withdrawal_id;
        `String timestamp;
        `String payload;
      ]
    when fa_tokens ->
      FA
        {
          sender;
          ticket_owner;
          receiver;
          proxy;
          amount = Tez.of_int @@ int_of_string amount;
          withdrawal_id;
          timestamp;
          payload;
        }
  | _ -> failwith "Failed to decode fast withdrawal's event"

let execute_payout ~service_provider_pkh ~exchanger
    ~fast_withdrawal_contract_address ~service_provider_proxy ?withdraw_receiver
    client receipt =
  let* fast_withdrawal_event =
    find_and_decode_fast_withdrawal_event ~fa_tokens:false receipt
  in
  let withdraw_amount, withdrawal_id, timestamp, payload, l2_caller =
    match fast_withdrawal_event with
    | Tez {amount; withdrawal_id; timestamp; payload; l2_caller; _} ->
        (amount, withdrawal_id, timestamp, payload, l2_caller)
    | FA {amount; withdrawal_id; timestamp; payload; sender; _} ->
        (amount, withdrawal_id, timestamp, payload, sender)
  in
  (* The fast withdrawal contract rejects a payout whose timestamp is still in
     the future ([TIMESTAMP_IN_FUTURE]). With [expiration_seconds = -1], baking
     until the L1 clock reaches the withdrawal timestamp also makes the
     withdrawal "expired", so the full amount is paid out and the payload is
     no longer unpacked. This matters for both tez and FA withdrawals. *)
  let withdrawal_timestamp = Int64.of_string timestamp in
  let rec wait_until_withdrawal_timestamp () =
    let* current_l1_timestamp = l1_timestamp client in
    if
      withdrawal_timestamp
      < Tezos_base.Time.Protocol.to_seconds current_l1_timestamp
    then unit
    else
      let* () = Client.bake_for_and_wait ~keys:[] client in
      wait_until_withdrawal_timestamp ()
  in
  let* () = wait_until_withdrawal_timestamp () in
  let withdraw_receiver =
    match withdraw_receiver with
    | Some withdraw_receiver -> withdraw_receiver
    | None -> Test.fail "withdraw_receiver is required for tez fast withdrawals"
  in
  Client.transfer
    ~fee:(Tez.of_int 1)
    ~fee_cap:(Tez.of_int 1)
    ~storage_limit:Int.max_int
    ~burn_cap:(Tez.of_int 100)
    ~amount:withdraw_amount
    ~giver:Constant.bootstrap3.public_key_hash
    ~receiver:service_provider_proxy
    ~entrypoint:"payout_proxy_tez"
    ~arg:
      (Printf.sprintf
         "(Pair %S %S %s %S %s %S %s %s)"
         fast_withdrawal_contract_address
         exchanger
         withdrawal_id
         withdraw_receiver
         timestamp
         service_provider_pkh
         payload
         l2_caller)
    client

let test_fast_withdrawal_l2_caller =
  let commitment_period = 5 and challenge_window = 5 in
  register_all
    ~__FILE__
    ~tags:["fast_withdrawal"; "deposit"]
    ~title:"Deposit and fast withdraw tez with forwarding contract"
    ~commitment_period
    ~challenge_window
    ~enable_fast_withdrawal:true
    ~time_between_blocks:Nothing
    ~kernels:[Kernel.Mainnet; Kernel.Latest]
  @@ fun {sequencer; evm_version; _} _protocol ->
  let fast_withdrawal_contract_address =
    "KT1TczPwz5KjAuuJKvkTmttS7bBioT5gjQ4Y"
  in
  let produce_block () = Rpc.produce_block sequencer in
  let endpoint = Evm_node.endpoint sequencer in
  let* withdrawal_forwarder_contract =
    Solidity_contracts.call_fast_withdrawal evm_version
  in
  let sender = Eth_account.bootstrap_accounts.(0) in
  (* deploy the contract *)
  let* () =
    Eth_cli.add_abi
      ~label:withdrawal_forwarder_contract.label
      ~abi:withdrawal_forwarder_contract.abi
      ()
  in
  let* withdrawal_forwarder_contract_address, _tx =
    Eth_cli.deploy
      ~source_private_key:sender.Eth_account.private_key
      ~endpoint
      ~abi:withdrawal_forwarder_contract.label
      ~bin:withdrawal_forwarder_contract.bin
    |> wait_for_application ~produce_block
  in

  let withdraw_amount = Tez.of_int 50 in

  (* Define the Tezos address that will receive the fast withdrawal on L1. *)
  let withdraw_receiver = "tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX" in

  (* Define the amount for fast withdrawal as 50 tez (half of the deposited amount). *)
  let withdraw_amount_wei = Wei.of_tez withdraw_amount in

  let* tx_hash =
    Eth_cli.contract_send
      ~source_private_key:sender.Eth_account.private_key
      ~endpoint
      ~abi_label:withdrawal_forwarder_contract.label
      ~address:withdrawal_forwarder_contract_address
      ~method_call:
        (sf
           "forwardWithdrawal(\"%s\", \"%s\")"
           withdraw_receiver
           fast_withdrawal_contract_address)
      ~value:withdraw_amount_wei
      ~gas:16_000_000
    |> wait_for_application ~produce_block
  in

  let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  let receipt = Option.get receipt in

  let* fast_withdrawal_event = find_and_decode_fast_withdrawal_event receipt in
  let l2_caller =
    match fast_withdrawal_event with
    | Tez {l2_caller; _} -> l2_caller
    | _ -> failwith "Tez case of the fast withdrawal event expected"
  in
  Check.((withdrawal_forwarder_contract_address = l2_caller) string)
    ~error_msg:"Expected %L as L2 caller instead of %R" ;
  unit

let test_deposit_and_fast_withdraw =
  let commitment_period = 5 and challenge_window = 5 in
  register_all
    ~__FILE__
    ~tags:["fast_withdrawal"; "deposit"]
    ~title:"Deposit and fast withdraw tez"
    ~commitment_period
    ~challenge_window
    ~enable_fast_withdrawal:true
    ~time_between_blocks:Nothing
      (* Start the L1 clock at wall-clock time (instead of the default one year
         in the past) so that it stays aligned with the sequencer's wall-clock
         block timestamps. Otherwise a withdrawal's timestamp (taken from the L2
         block) is always ~1 year ahead of L1 time, and the fast-withdrawal
         contract's [assert_withdrawal_not_in_future] / expiration checks can
         never be satisfied. *)
    ~genesis_timestamp:Client.Now
    ~kernels:[Kernel.Mainnet; Kernel.Latest]
  @@
  fun {sequencer; sc_rollup_address; client; l1_contracts; sc_rollup_node; _}
      _protocol
    ->
  let {exchanger; _} = l1_contracts in
  let fast_withdrawal_prg = fast_withdrawal_path () in
  let fast_withdrawal_init =
    sf "Pair {} (Pair %S %S -1) {}" exchanger sc_rollup_address
  in
  let service_provider_prg = service_provider_path () in
  let service_provider_init =
    "Pair \"KT1CeFqjJRJPNVvhvznQrWfHad2jCiDZ6Lyj\" \
     \"KT1CeFqjJRJPNVvhvznQrWfHad2jCiDZ6Lyj\" 0 \
     \"tz1etHLky7fuVumvBDi92ogXQZZPESiFimWR\" 0 \
     \"tz1etHLky7fuVumvBDi92ogXQZZPESiFimWR\" 0x00 0x00 0"
  in
  let* fast_withdrawal_contract_address =
    Client.originate_contract
      ~alias:"fast_withdrawal_contract_address"
      ~amount:Tez.zero
      ~src:Constant.bootstrap5.public_key_hash
      ~init:fast_withdrawal_init
      ~prg:fast_withdrawal_prg
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in

  let* service_provider_proxy =
    Client.originate_contract
      ~alias:"service_provider"
      ~amount:Tez.zero
      ~src:Constant.bootstrap3.public_key_hash
      ~init:service_provider_init
      ~prg:service_provider_prg
      ~burn_cap:(Tez.of_int 890)
      client
  in
  let admin = Constant.bootstrap5 in
  let produce_block () = Rpc.produce_block sequencer in

  let withdraw_amount = Tez.of_int 50 in
  (* Define the amount to deposit in tez (100 tez), and specify the Ethereum-based receiver for the rollup. *)
  let deposit_amount = Tez.of_int 100 in
  let service_provider_pkh = "tz1TGKSrZrBpND3PELJ43nVdyadoeiM1WMzb" in
  let* initial_service_provider_balance =
    Client.get_balance_for ~account:service_provider_pkh client
  in

  let receiver =
    Eth_account.
      {
        address = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
        private_key =
          "0xb7c548b5442f5b28236f0dcd619f65aaaafd952240908adcf9642d8e616587ee";
      }
  in

  (* Define the Tezos address that will receive the fast withdrawal on L1. *)
  let withdraw_receiver = "tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX" in

  (* Check the initial balance of the L1 withdraw receiver. It should be 0 before the fast withdrawal occurs. *)
  let* balance_withdraw_receiver =
    Client.get_balance_for ~account:withdraw_receiver client
  in
  Check.((balance_withdraw_receiver = Tez.of_int 0) Tez.typ)
    ~error_msg:"Expected %R as initial balance instead of %L" ;

  (* Execute the deposit of 100 tez to the rollup. The depositor is the admin account, and the receiver is the Ethereum address. *)
  let deposit_info =
    {receiver = EthereumAddr receiver.address; chain_id = None}
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~bridge:l1_contracts.bridge
      ~amount:deposit_amount
      ~sc_rollup_address
      ~sc_rollup_node
      ~depositor:admin
      ~deposit_info
      client
  in

  (* Wait for the sequencer to detect and include the deposit. *)
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in

  (* Check that the receiver's balance in the rollup matches the deposited amount. *)
  let check_balance ~receiver ~endpoint expected_balance =
    let* balance = Eth_cli.balance ~account:receiver ~endpoint () in
    let balance = Wei.truncate_to_mutez balance in
    Check.((balance = Tez.to_mutez expected_balance) int)
      ~error_msg:
        (sf "Expected balance of %s should be %%R, but got %%L" receiver) ;
    unit
  in
  let* () =
    check_balance
      ~receiver:receiver.address
      ~endpoint:(Evm_node.endpoint sequencer)
      deposit_amount
  in

  (* Define the amount for fast withdrawal as 50 tez (half of the deposited amount). *)
  let withdraw_amount_wei = Wei.of_tez withdraw_amount in

  let* withdrawal_level = Client.level client in
  let* tx_hash =
    fast_withdrawal
      ~produce_block
      ~amount_wei:withdraw_amount_wei
      ~sender:receiver
      ~receiver:withdraw_receiver
      ~fast_withdrawal_contract_address
      ~endpoint:(Evm_node.endpoint sequencer)
      ()
  in

  let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  let receipt = Option.get receipt in

  let* () =
    execute_payout
      ~exchanger
      ~service_provider_pkh
      ~fast_withdrawal_contract_address
      ~service_provider_proxy
      ~withdraw_receiver
      client
      receipt
  in

  let* _ =
    find_and_execute_withdrawal
      ~outbox_lookup_depth:100
      ~withdrawal_level
      ~commitment_period
      ~challenge_window
      ~sc_rollup_node
      ~sc_rollup_address
      ~client
      ()
  in

  let* balance = Client.get_balance_for ~account:withdraw_receiver client in
  let* final_withdraw_receiver_balance =
    Client.get_balance_for ~account:service_provider_pkh client
  in

  (* Check that the destination address of the withdrawal has a balance of
     50 tez after the fast withdrawal is complete. *)
  Check.((balance = withdraw_amount) Tez.typ)
    ~error_msg:"Expected %R amount instead of %L after withdrawal" ;

  (* Verify that the service provider's balance increased by 50 tez after the
     fast withdrawal payout. *)
  Check.(
    (Tez.(initial_service_provider_balance + withdraw_amount)
    = final_withdraw_receiver_balance)
      Tez.typ)
    ~error_msg:
      "Expected %R amount instead of %L after outbox message was executed" ;

  return ()

let test_fa_withdrawal_eoa_without_code =
  register_all
    ~__FILE__
    ~da_fee:(Wei.of_string "1_000_000_000_000")
    ~tags:["evm"; "sequencer"; "fa_withdrawal"; "eoa"]
    ~title:"FA withdrawal from an EOA without code succeeds"
    ~enable_fa_bridge:true
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~additional_uses:[Constant.octez_codec]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        kernel;
        _;
      }
      _protocol
    ->
  let amount = 42 in
  let depositor = Constant.bootstrap5 in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let receiver = sender.address in

  (* Deposit FA tokens to the EOA without proxy *)
  let* () =
    send_fa_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
      ~receiver
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in

  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in
  let* ticket_balance_before =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Right sequencer)
  in
  Check.((amount = ticket_balance_before) int)
    ~error_msg:"After deposit we expect %L ticket balance, got %R" ;

  let* ticketer = ticket_creator l1_contracts.ticket_router_tester in
  let* content = ticket_content 0 in
  let routing_info =
    String.concat
      ""
      [
        "00000000000000000000000000000000000000000000";
        ticketer |> Hex.of_bytes |> Hex.show;
      ]
  in

  let ticketer_hex = ticketer |> Hex.of_bytes |> Hex.show in
  let content_hex = content |> Hex.of_bytes |> Hex.show in

  (* The EOA withdraws half its tickets via withdraw *)
  let* _tx =
    call_fa_withdraw
      ~sender
      ~endpoint:(Evm_node.endpoint sequencer)
      ~evm_node:sequencer
      ~ticket_owner:receiver
      ~routing_info
      ~amount:(amount / 2)
      ~ticketer:ticketer_hex
      ~content:content_hex
      ()
  in

  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in

  let* ticket_balance_mid =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Right sequencer)
  in
  Check.((amount / 2 = ticket_balance_mid) int)
    ~error_msg:"After first withdrawal we expect %L ticket balance, got %R" ;

  (* The EOA withdraws the rest via fa_fast_withdraw *)
  let* _tx =
    call_fa_fast_withdraw
      ~sender
      ~sequencer
      ~ticket_owner:receiver
      ~receiver:Constant.bootstrap4.public_key_hash
      ~amount:(amount / 2)
      ~ticketer:ticketer_hex
      ~content:content_hex
      ~fast_withdrawal_contract_address:Constant.bootstrap3.public_key_hash
      ()
  in

  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in

  let* ticket_balance_after =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Right sequencer)
  in
  Check.((0 = ticket_balance_after) int)
    ~error_msg:"After both withdrawals we expect %L ticket balance, got %R" ;
  unit

let test_fa_withdrawal_eoa_unauthorized_caller_fails =
  register_all
    ~__FILE__
    ~da_fee:(Wei.of_string "1_000_000_000_000")
    ~tags:["evm"; "sequencer"; "fa_withdrawal"; "eoa"; "unauthorized"]
    ~title:"FA withdrawal from another EOA fails"
    ~enable_fa_bridge:true
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~additional_uses:[Constant.octez_codec]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        kernel;
        _;
      }
      _protocol
    ->
  let amount = 42 in
  let depositor = Constant.bootstrap5 in
  let owner = Eth_account.bootstrap_accounts.(0) in
  let attacker = Eth_account.bootstrap_accounts.(1) in

  (* Deposit FA tokens to the owner EOA *)
  let* () =
    send_fa_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
      ~receiver:owner.address
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in

  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in
  let* ticket_balance_before =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:owner.address
      (Either.Right sequencer)
  in
  Check.((amount = ticket_balance_before) int)
    ~error_msg:"After deposit we expect %L ticket balance, got %R" ;

  let* ticketer = ticket_creator l1_contracts.ticket_router_tester in
  let* content = ticket_content 0 in
  let routing_info =
    String.concat
      ""
      [
        "00000000000000000000000000000000000000000000";
        ticketer |> Hex.of_bytes |> Hex.show;
      ]
  in

  let ticketer_hex = ticketer |> Hex.of_bytes |> Hex.show in
  let content_hex = content |> Hex.of_bytes |> Hex.show in

  (* A different EOA tries to withdraw the owner's tickets via withdraw *)
  let* () =
    expect_failure
      "Unauthorized caller should not be able to withdraw another EOA's tickets"
    @@ fun () ->
    call_fa_withdraw
      ~sender:attacker
      ~endpoint:(Evm_node.endpoint sequencer)
      ~evm_node:sequencer
      ~ticket_owner:owner.address
      ~routing_info
      ~amount
      ~ticketer:ticketer_hex
      ~content:content_hex
      ()
  in

  (* A different EOA tries via fa_fast_withdraw *)
  let* () =
    expect_failure
      "Unauthorized caller should not be able to fast-withdraw another EOA's \
       tickets"
    @@ fun () ->
    call_fa_fast_withdraw
      ~sender:attacker
      ~sequencer
      ~ticket_owner:owner.address
      ~receiver:Constant.bootstrap4.public_key_hash
      ~amount
      ~ticketer:ticketer_hex
      ~content:content_hex
      ~fast_withdrawal_contract_address:Constant.bootstrap3.public_key_hash
      ()
  in

  (* Verify the owner's tickets are still there *)
  let* ticket_balance_after =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:owner.address
      (Either.Right sequencer)
  in
  Check.((amount = ticket_balance_after) int)
    ~error_msg:"After failed withdrawals we expect %L ticket balance, got %R" ;
  unit

let test_fa_withdrawal_eip7702_unauthorized_caller_fails =
  register_all
    ~__FILE__
    ~da_fee:(Wei.of_string "1_000_000_000_000")
    ~tags:
      ["evm"; "sequencer"; "fa_withdrawal"; "eoa"; "eip7702"; "unauthorized"]
    ~title:"FA withdrawal from an EIP-7702 EOA by another EOA fails"
    ~enable_fa_bridge:true
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~additional_uses:[Constant.octez_codec]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        evm_version;
        kernel;
        _;
      }
      _protocol
    ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let owner = Eth_account.bootstrap_accounts.(1) in
  let attacker = Eth_account.bootstrap_accounts.(2) in
  let endpoint = Evm_node.endpoint sequencer in

  (* Deploy an EIP-7702 delegation contract and set it on the owner *)
  let* eip7702 = Solidity_contracts.eip7702 evm_version in
  let* () = Eth_cli.add_abi ~label:eip7702.label ~abi:eip7702.abi () in
  let* eip7702_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint
         ~abi:eip7702.abi
         ~bin:eip7702.bin)
      sequencer
  in

  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let*@ whale_nonce =
    Rpc.get_transaction_count ~address:whale.address sequencer
  in
  let* signed_auth =
    Cast.wallet_sign_auth
      ~authorization:eip7702_contract
      ~private_key:owner.private_key
      ~endpoint
      ()
  in
  let auth_list_json =
    ( "authorizationList",
      `A
        [
          `O
            [
              ("chainId", `String "0x539");
              ("address", `String eip7702_contract);
              ("nonce", `String "0x0");
              ("yParity", `String "0x0");
              ("r", `String "0x0");
              ("s", `String "0x0");
            ];
        ] )
  in
  let*@ estimated_gas =
    Rpc.estimate_gas
      [
        ("from", `String whale.address);
        ("to", `String owner.address);
        auth_list_json;
      ]
      sequencer
  in
  let gas = Int64.to_int estimated_gas in
  let* raw_set_eoa =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce:(Int64.to_int whale_nonce)
      ~gas
      ~gas_price
      ~value:Wei.zero
      ~authorization:signed_auth
      ~address:owner.address
      ~arguments:[]
      ~legacy:false
      ()
  in
  let*@ _set_eoa_hash =
    Rpc.send_raw_transaction ~raw_tx:raw_set_eoa sequencer
  in
  let* _ = produce_block sequencer in

  (* Verify the owner EOA now has code (EIP-7702 delegation) *)
  let*@ code = Rpc.get_code ~address:owner.address sequencer in
  Check.((code <> "0x") string)
    ~error_msg:"Expected the owner EOA to have code after EIP-7702 delegation" ;

  (* Deposit FA tokens to the EIP-7702 owner *)
  let amount = 42 in
  let depositor = Constant.bootstrap5 in
  let* () =
    send_fa_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
      ~receiver:owner.address
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in

  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in
  let* ticket_balance_before =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:owner.address
      (Either.Right sequencer)
  in
  Check.((amount = ticket_balance_before) int)
    ~error_msg:"After deposit we expect %L ticket balance, got %R" ;

  let* ticketer = ticket_creator l1_contracts.ticket_router_tester in
  let* content = ticket_content 0 in
  let routing_info =
    String.concat
      ""
      [
        "00000000000000000000000000000000000000000000";
        ticketer |> Hex.of_bytes |> Hex.show;
      ]
  in

  let ticketer_hex = ticketer |> Hex.of_bytes |> Hex.show in
  let content_hex = content |> Hex.of_bytes |> Hex.show in

  (* A plain EOA tries to withdraw the EIP-7702 owner's tickets via withdraw.
     The owner has code (delegation designator), but isWallet recognizes it,
     so the guard requires msg.sender == ticketOwner. *)
  let* () =
    expect_failure
      "Plain EOA should not be able to withdraw an EIP-7702 EOA's tickets"
    @@ fun () ->
    call_fa_withdraw
      ~sender:attacker
      ~endpoint
      ~evm_node:sequencer
      ~ticket_owner:owner.address
      ~routing_info
      ~amount
      ~ticketer:ticketer_hex
      ~content:content_hex
      ()
  in

  (* The plain EOA attacker tries via fa_fast_withdraw *)
  let* () =
    expect_failure
      "Plain EOA should not be able to fast-withdraw an EIP-7702 EOA's tickets"
    @@ fun () ->
    call_fa_fast_withdraw
      ~sender:attacker
      ~sequencer
      ~ticket_owner:owner.address
      ~receiver:Constant.bootstrap4.public_key_hash
      ~amount
      ~ticketer:ticketer_hex
      ~content:content_hex
      ~fast_withdrawal_contract_address:Constant.bootstrap3.public_key_hash
      ()
  in

  (* Verify the owner's tickets are still there *)
  let* ticket_balance_after =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:owner.address
      (Either.Right sequencer)
  in
  Check.((amount = ticket_balance_after) int)
    ~error_msg:"After failed withdrawals we expect %L ticket balance, got %R" ;
  unit

let test_fa_withdrawal_eip7702_eoa =
  register_all
    ~__FILE__
    ~da_fee:(Wei.of_string "1_000_000_000_000")
    ~tags:["evm"; "sequencer"; "fa_withdrawal"; "eoa"; "eip7702"]
    ~title:"FA withdrawal from an EIP-7702 EOA succeeds"
    ~enable_fa_bridge:true
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~additional_uses:[Constant.octez_codec]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        evm_version;
        kernel;
        _;
      }
      _protocol
    ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let eoa = Eth_account.bootstrap_accounts.(1) in
  let endpoint = Evm_node.endpoint sequencer in

  (* Deploy an EIP-7702 delegation contract *)
  let* eip7702 = Solidity_contracts.eip7702 evm_version in
  let* () = Eth_cli.add_abi ~label:eip7702.label ~abi:eip7702.abi () in
  let* eip7702_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint
         ~abi:eip7702.abi
         ~bin:eip7702.bin)
      sequencer
  in

  (* Set up EIP-7702 delegation for the EOA *)
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let*@ whale_nonce =
    Rpc.get_transaction_count ~address:whale.address sequencer
  in
  let* signed_auth =
    Cast.wallet_sign_auth
      ~authorization:eip7702_contract
      ~private_key:eoa.private_key
      ~endpoint
      ()
  in
  let auth_list_json =
    ( "authorizationList",
      `A
        [
          `O
            [
              ("chainId", `String "0x539");
              ("address", `String eip7702_contract);
              ("nonce", `String "0x0");
              ("yParity", `String "0x0");
              ("r", `String "0x0");
              ("s", `String "0x0");
            ];
        ] )
  in
  let*@ estimated_gas =
    Rpc.estimate_gas
      [
        ("from", `String whale.address);
        ("to", `String eoa.address);
        auth_list_json;
      ]
      sequencer
  in
  let gas = Int64.to_int estimated_gas in
  let* raw_set_eoa =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce:(Int64.to_int whale_nonce)
      ~gas
      ~gas_price
      ~value:Wei.zero
      ~authorization:signed_auth
      ~address:eoa.address
      ~arguments:[]
      ~legacy:false
      ()
  in
  let*@ _set_eoa_hash =
    Rpc.send_raw_transaction ~raw_tx:raw_set_eoa sequencer
  in
  let* _ = produce_block sequencer in

  (* Verify the EOA now has code *)
  let*@ code = Rpc.get_code ~address:eoa.address sequencer in
  Check.((code <> "0x") string)
    ~error_msg:"Expected the EOA to have code after EIP-7702 delegation" ;

  (* Deposit FA tokens to the EOA without proxy *)
  let amount = 42 in
  let depositor = Constant.bootstrap5 in
  let* () =
    send_fa_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
      ~receiver:eoa.address
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in

  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in
  let* ticket_balance_before =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:eoa.address
      (Either.Right sequencer)
  in
  Check.((amount = ticket_balance_before) int)
    ~error_msg:"After deposit we expect %L ticket balance, got %R" ;

  let* ticketer = ticket_creator l1_contracts.ticket_router_tester in
  let* content = ticket_content 0 in
  let routing_info =
    String.concat
      ""
      [
        "00000000000000000000000000000000000000000000";
        ticketer |> Hex.of_bytes |> Hex.show;
      ]
  in

  let ticketer_hex = ticketer |> Hex.of_bytes |> Hex.show in
  let content_hex = content |> Hex.of_bytes |> Hex.show in

  (* The EIP-7702 EOA withdraws half its tickets via withdraw.
     Even though ticketOwner.code.length > 0, the guard allows it
     because msg.sender == ticketOwner. *)
  let* _tx =
    call_fa_withdraw
      ~sender:eoa
      ~endpoint
      ~evm_node:sequencer
      ~ticket_owner:eoa.address
      ~routing_info
      ~amount:(amount / 2)
      ~ticketer:ticketer_hex
      ~content:content_hex
      ()
  in

  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in

  let* ticket_balance_mid =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:eoa.address
      (Either.Right sequencer)
  in
  Check.((amount / 2 = ticket_balance_mid) int)
    ~error_msg:"After first withdrawal we expect %L ticket balance, got %R" ;

  (* The EIP-7702 EOA withdraws the rest via fa_fast_withdraw *)
  let* _tx =
    call_fa_fast_withdraw
      ~sender:eoa
      ~sequencer
      ~ticket_owner:eoa.address
      ~receiver:Constant.bootstrap4.public_key_hash
      ~amount:(amount / 2)
      ~ticketer:ticketer_hex
      ~content:content_hex
      ~fast_withdrawal_contract_address:Constant.bootstrap3.public_key_hash
      ()
  in

  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in

  let* ticket_balance_after =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:eoa.address
      (Either.Right sequencer)
  in
  Check.((0 = ticket_balance_after) int)
    ~error_msg:"After both withdrawals we expect %L ticket balance, got %R" ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_deposit_and_fast_withdraw protocols ;
  test_fast_withdrawal_l2_caller protocols ;
  test_fa_withdrawal_eoa_without_code [Alpha] ;
  test_fa_withdrawal_eoa_unauthorized_caller_fails [Alpha] ;
  test_fa_withdrawal_eip7702_unauthorized_caller_fails [Alpha] ;
  test_fa_withdrawal_eip7702_eoa [Alpha]
