(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type rpc =
  | Eth_getBalance
  | Eth_accounts
  | Eth_blockNumber
  | Eth_getBlockByHash
  | Eth_getBlockByNumber
  | Eth_getBlockTransactionCountByHash
  | Eth_getBlockTransactionCountByNumber
  | Eth_getUncleByBlockHashAndIndex
  | Eth_getUncleByBlockNumberAndIndex
  | Eth_getUncleCountByBlockHash
  | Eth_getUncleCountByBlockNumber
  | Eth_getTransactionByHash
  | Eth_getTransactionByBlockHashAndIndex
  | Eth_getTransactionByBlockNumberAndIndex
  | Eth_getTransactionCount
  | Eth_getTransactionReceipt
  | Eth_getCode
  | Eth_getStorageAt
  | Eth_gasPrice
  | Eth_chainId
  | Eth_getBlockReceipts
  | Eth_maxPriorityFeePerGas
  | Eth_coinbase
  | Eth_estimateGas
  | Tez_kernelVersion
  | Tez_kernelRootHash
  | Net_version
  | Txpool_content
  | Web3_clientVersion
  | Web3_sha3
(* TODO : Add last rpc https://gitlab.com/tezos/tezos/-/issues/7663 *)

type config = {
  account_address : string;
  block_num : string;
  block_hash : string;
  tx_hash : string;
  contract_address : string;
  raw_tx : string;
  rpc : rpc list;
}

let rpc_to_string = function
  | Eth_getBalance -> "eth_getBalance"
  | Eth_accounts -> "eth_accounts"
  | Eth_blockNumber -> "eth_blockNumber"
  | Eth_getBlockByHash -> "eth_getBlockByHash"
  | Eth_getBlockByNumber -> "eth_getBlockByNumber"
  | Eth_getBlockTransactionCountByHash -> "eth_getBlockTransactionCountByHash"
  | Eth_getBlockTransactionCountByNumber ->
      "eth_getBlockTransactionCountByNumber"
  | Eth_getUncleByBlockHashAndIndex -> "eth_getUncleByBlockHashAndIndex"
  | Eth_getUncleByBlockNumberAndIndex -> "eth_getUncleByBlockNumberAndIndex"
  | Eth_getUncleCountByBlockHash -> "eth_getUncleCountByBlockHash"
  | Eth_getUncleCountByBlockNumber -> "eth_getUncleCountByBlockNumber"
  | Eth_getTransactionByHash -> "eth_getTransactionByHash"
  | Eth_getTransactionByBlockHashAndIndex ->
      "eth_getTransactionByBlockHashAndIndex"
  | Eth_getTransactionByBlockNumberAndIndex ->
      "eth_getTransactionByBlockNumberAndIndex"
  | Eth_getTransactionCount -> "eth_getTransactionCount"
  | Eth_getTransactionReceipt -> "eth_getTransactionReceipt"
  | Eth_getCode -> "eth_getCode"
  | Eth_getStorageAt -> "eth_getStorageAt"
  | Eth_gasPrice -> "eth_gasPrice"
  | Eth_chainId -> "eth_chainId"
  | Eth_getBlockReceipts -> "eth_getBlockReceipts"
  | Eth_maxPriorityFeePerGas -> "eth_maxPriorityFeePerGas"
  | Eth_coinbase -> "eth_coinbase"
  | Eth_estimateGas -> "eth_estimateGas"
  | Tez_kernelVersion -> "tez_kernelVersion"
  | Tez_kernelRootHash -> "tez_kernelRootHash"
  | Net_version -> "net_version"
  | Txpool_content -> "txpool_content"
  | Web3_clientVersion -> "web3_clientVersion"
  | Web3_sha3 -> "web3_sha3"
(* TODO : Add last rpc https://gitlab.com/tezos/tezos/-/issues/7663 *)

let path = "src/bin_testnet_scenarios/locust/"

let run ?(output = "perf") ?(file = "locustfile.py") ?(spawn_rate = 1)
    ?(users = 1) ?(time = "5s") endpoint =
  let ptime = Client.Time.now () in
  let string_time = Ptime.to_rfc3339 ptime in
  let* () =
    Process.run
      "locust"
      [
        "--skip-log";
        "--autostart";
        "--autoquit";
        "1";
        "-t";
        time;
        "--users";
        Int.to_string users;
        "--spawn-rate";
        Int.to_string spawn_rate;
        "-H";
        endpoint;
        "-f";
        path // file;
        "--csv";
        path // string_time // output;
      ]
  in

  return (path // string_time // (output ^ "_stats.csv"))

let config_to_json c =
  let open Ezjsonm in
  to_string
  @@ dict
       [
         ("account_address", string c.account_address);
         ("block_num", string c.block_num);
         ("block_hash", string c.block_hash);
         ("tx_hash", string c.tx_hash);
         ("contract_address", string c.contract_address);
         ("raw_tx", string c.raw_tx);
         ("rpc_list", list (fun i -> `String (rpc_to_string i)) c.rpc);
       ]

let write_config_file config_locust =
  let path = path // "config.json" in
  let json = config_to_json config_locust in
  Lwt_io.with_file ~mode:Output path @@ fun chan ->
  Lwt_io.atomic
    (fun chan ->
      let* () = Lwt_io.write chan json in
      Lwt_io.flush chan)
    chan

let split_comma line = String.split_on_char ',' line

let read_csv file : string list list =
  let ic = open_in file in
  let rec read_lines acc =
    match input_line ic with
    | line -> read_lines (split_comma line :: acc)
    | exception End_of_file ->
        close_in ic ;
        acc
  in
  read_lines []

let read_csv file_path =
  let l = read_csv file_path in
  let fist = List.hd l in
  List.nth fist 9
