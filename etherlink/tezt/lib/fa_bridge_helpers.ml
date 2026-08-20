(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024-2026 Functori <contact@functori.com>         *)
(* SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>               *)
(*                                                                           *)
(*****************************************************************************)

(** Helpers shared by the FA bridge tests: ticket identifiers, the withdrawal
    entrypoints of the FA bridge precompile, and the event signatures and
    topics emitted by the bridge. *)

open Rpc.Syntax
open Contract_path
open Test_helpers

let encode_data json codec =
  let* hex_string = Codec.encode ~name:codec json in
  let hex = `Hex (Durable_storage_path.no_0x hex_string) in
  return (Hex.to_bytes hex)

let ticket_content token_id =
  let* content_bytes =
    encode_data
      (Ezjsonm.from_string
         (sf
            "{\"prim\": \"Pair\", \"args\": [{\"int\": \"%d\"}, {\"prim\": \
             \"None\"}]}"
            token_id))
      "alpha.script.expr"
  in
  return content_bytes

let ticket_creator ticketer =
  let* ticketer_bytes =
    encode_data (Ezjsonm.string ticketer) "alpha.contract"
  in
  return ticketer_bytes

let ticket_hash ticketer token_id =
  let* ticketer_bytes = ticket_creator ticketer in
  let* content_bytes = ticket_content token_id in
  let payload = Bytes.concat Bytes.empty [ticketer_bytes; content_bytes] in
  let payload_digest = Tezos_crypto.Hacl.Hash.Keccak_256.digest payload in
  return (payload_digest |> Hex.of_bytes |> Hex.show)

let ticket_balance ~kernel ~ticket_hash ~account endpoint =
  let account =
    account |> Durable_storage_path.no_0x |> String.lowercase_ascii
  in
  let* ticket_balance =
    let key =
      Durable_storage_path.Ticket_table.balance kernel ~ticket_hash ~account
    in
    match endpoint with
    | Either.Left sc_rollup_node ->
        Sc_rollup_node.RPC.call
          sc_rollup_node
          ~rpc_hooks:Tezos_regression.rpc_hooks
        @@ Sc_rollup_rpc.get_global_block_durable_state_value
             ~pvm_kind:"wasm_2_0_0"
             ~operation:Sc_rollup_rpc.Value
             ~key
             ()
    | Either.Right evm_node ->
        let*@ v = Rpc.state_value evm_node key in
        return v
  in
  return
  @@ Option.fold
       ~none:0
       ~some:(fun ticket_balance ->
         `Hex ticket_balance |> Hex.to_string |> Z.of_bits |> Z.to_int)
       ticket_balance

let call_fa_withdraw ?timestamp ?expect_failure ~sender ~endpoint ~evm_node
    ~ticket_owner ~routing_info ~amount ~ticketer ~content () =
  let* () =
    Eth_cli.add_abi
      ~label:"fa_withdrawal"
      ~abi:(predep_fa_bridge_abi_path ())
      ()
  in
  send_transaction_to_sequencer
    ?timestamp
    (Eth_cli.contract_send
       ?expect_failure
       ~source_private_key:sender.Eth_account.private_key
       ~endpoint
       ~abi_label:"fa_withdrawal"
       ~address:Solidity_contracts.Precompile.fa_bridge
       ~method_call:
         (sf
            {|withdraw("%s", "0x%s", %d, "0x%s", "0x%s")|}
            ticket_owner
            routing_info
            amount
            ticketer
            content))
    evm_node

let call_fa_fast_withdraw ?expect_failure ?timestamp ~sender ~sequencer
    ~ticket_owner ~receiver ~amount ~ticketer ~content
    ~fast_withdrawal_contract_address () =
  let endpoint = Evm_node.endpoint sequencer in
  let produce_block () = produce_block sequencer in
  let* receiver = ticket_creator receiver in
  let routing_info =
    String.concat "" [receiver |> Hex.of_bytes |> Hex.show; ticketer]
  in
  let* () =
    Eth_cli.add_abi
      ~label:"fa_fast_withdraw"
      ~abi:(predep_fa_bridge_abi_path ())
      ()
  in
  let call_fast_withdraw () =
    send_transaction_to_sequencer
      ?timestamp
      (Eth_cli.contract_send
         ?expect_failure
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi_label:"fa_fast_withdraw"
         ~address:Solidity_contracts.Precompile.fa_bridge
         ~method_call:
           (sf
              {|fa_fast_withdraw("%s", "0x%s", %d, "0x%s", "0x%s", "%s", "%s")|}
              ticket_owner
              routing_info
              amount
              ticketer
              content
              fast_withdrawal_contract_address
              "0x0000000000000000000000000000000000000000000000000000000000000001"))
      sequencer
  in
  wait_for_application ~produce_block call_fast_withdraw

let fast_withdrawal_tez_event_signature =
  "FastWithdrawal(bytes22,uint256,uint256,uint256,bytes,address)"

let fast_withdrawal_fa_token_event_signature =
  "FastFaWithdrawal(address,address,bytes22,bytes22,uint256,uint256,uint256,bytes)"

let revm_fast_withdrawal_fa_token_event_signature =
  "FastFaWithdrawal(uint256,address,address,bytes22,bytes22,uint256,uint256,uint256,bytes)"

let revm_queued_xtz_deposit_event_signature =
  "QueuedDeposit(uint256,uint256,address,uint256,uint256)"

let revm_xtz_deposit_event_signature =
  "Deposit(uint256,address,uint256,uint256)"

let eip7702_fallback_event_signature = "EIP7702FallbackCall(address)"

let event_topic ~event_signature =
  Tezos_crypto.Hacl.Hash.Keccak_256.digest (Bytes.of_string event_signature)
  |> Hex.of_bytes |> Hex.show |> add_0x

let fast_withdrawal_tez_event_topic =
  event_topic ~event_signature:fast_withdrawal_tez_event_signature

let fast_withdrawal_fa_token_event_topic =
  event_topic ~event_signature:fast_withdrawal_fa_token_event_signature

let revm_fast_withdrawal_fa_token_event_topic =
  event_topic ~event_signature:revm_fast_withdrawal_fa_token_event_signature

let revm_queued_xtz_deposit_event_topic =
  event_topic ~event_signature:revm_queued_xtz_deposit_event_signature

let revm_xtz_deposit_event_topic =
  event_topic ~event_signature:revm_xtz_deposit_event_signature

let eip7702_fallback_event_topic =
  event_topic ~event_signature:eip7702_fallback_event_signature
