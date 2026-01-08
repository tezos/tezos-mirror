(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2025 Trilitech <contact@trili.tech>               *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

open Rpc.Syntax
open Test_helpers
open Evm_node_lib_dev_encoding.Rlp

type receiver = TezosAddr of string | EthereumAddr of string

let raw_addr = function TezosAddr addr | EthereumAddr addr -> addr

type deposit_info = {receiver : receiver; chain_id : int option}

let encode_option encode_some = function
  | None -> List []
  | Some v ->
      let value = encode_some v in
      List [Value value]

let encode_deposit_info deposit_info =
  let receiver =
    match deposit_info.receiver with
    | TezosAddr addr ->
        let tag = Bytes.make 1 '\001' in
        let receiver = Hex.to_bytes (`Hex (remove_0x addr)) in
        List [Value tag; Value receiver]
    | EthereumAddr addr ->
        let receiver = Hex.to_bytes (`Hex (remove_0x addr)) in
        Value receiver
  in
  let bytes =
    encode (List [receiver; encode_option encode_int deposit_info.chain_id])
  in
  let (`Hex str) = Hex.of_bytes bytes in
  add_0x ("01" ^ str)

type endpoint = Sc_rollup_node of Sc_rollup_node.t | Evm_node of Evm_node.t

let subkeys path = function
  | Sc_rollup_node sc_rollup_node ->
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:"wasm_2_0_0"
           ~operation:Sc_rollup_rpc.Subkeys
           ~key:path
           ()
  | Evm_node evm_node ->
      let*@! list = Rpc.state_subkeys evm_node path in
      return list

let content = subkeys Durable_storage_path.delayed_inbox

let data endpoint hash =
  let path = sf "%s/%s/data" Durable_storage_path.delayed_inbox hash in
  match endpoint with
  | Sc_rollup_node sc_rollup_node ->
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:"wasm_2_0_0"
           ~operation:Sc_rollup_rpc.Value
           ~key:path
           ()
  | Evm_node evm_node ->
      let*@ res = Rpc.state_value evm_node path in
      return res

let assert_mem endpoint hash =
  let* delayed_transactions_hashes = content endpoint in
  Check.(list_mem string hash delayed_transactions_hashes)
    ~error_msg:"hash %L should be present in the delayed inbox %R" ;
  unit

let assert_empty endpoint =
  let* delayed_transactions_hashes = content endpoint in
  Check.is_true
    (List.length delayed_transactions_hashes <= 1)
    ~error_msg:"Expected empty delayed inbox" ;
  unit

let size endpoint =
  let* delayed_transactions_hashes = content endpoint in
  let size = List.length delayed_transactions_hashes - 1 in
  if size < 0 then
    (* /meta is removed, if the delayed inbox was empty it would be (-1) here. *)
    return 0
  else return size

let wait_for_delayed_inbox_add_tx_and_injected ~sequencer ~sc_rollup_node
    ~client =
  let event_watcher =
    let added = Evm_node.wait_for_evm_event New_delayed_transaction sequencer in
    let injected = Evm_node.wait_for_block_producer_tx_injected sequencer in
    let* (_transaction_kind, added_hash), injected_hash =
      Lwt.both added injected
    in
    Check.((added_hash = injected_hash) string)
      ~error_msg:"Injected hash %R is not the expected one %L" ;
    Lwt.return_unit
  in
  wait_for_event
    event_watcher
    ~sequencer
    ~sc_rollup_node
    ~client
    ~error_msg:
      "Timed out while waiting for transaction to be added to the delayed \
       inbox and injected"

let send_deposit_to_delayed_inbox ?(rlp = false) ~amount ~bridge ~depositor
    ~deposit_info ~sc_rollup_node ~sc_rollup_address client =
  (* Implements optional types decoding as in the RLP library from the
   kernel's library. *)
  let infos =
    if rlp then encode_deposit_info deposit_info
    else raw_addr deposit_info.receiver
  in
  let* () =
    Client.transfer
      ~entrypoint:"deposit"
      ~arg:(sf "Pair %S %s" sc_rollup_address infos)
      ~amount
      ~giver:depositor.Account.public_key_hash
      ~receiver:bridge
      ~burn_cap:Tez.one
      client
  in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  unit
