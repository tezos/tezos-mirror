(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** RPC server configuration with address and port *)
type rpc = {addr : string; port : int}

(** Secret key for signing transactions *)
type secret_key = Libsecp256k1.External.Key.secret Libsecp256k1.External.Key.t

(** Whitelist item configuration for filtering deposits and withdrawals.
    Can specify a proxy address, ticket hashes (disjunction), or both. *)
type whitelist_item = {
  proxy : Ethereum_types.address option;
  ticket_hashes : Ethereum_types.hash list option;
}

(** Configuration for the FA bridge watchtower *)
type t = {
  evm_node_endpoint : Uri.t;  (** Endpoint URL for the EVM node *)
  gas_limit : int64;  (** Gas limit for transactions *)
  max_fee_per_gas : int64;  (** Maximum fee per gas unit *)
  rpc : rpc option;  (** Optional RPC server configuration *)
  secret_key : secret_key option;  (** Optional secret key for signing *)
  whitelist : whitelist_item list option;
      (** Optional list of whitelisted addresses *)
  monitor_all_deposits : bool;  (** Whether to store all deposits in the DB.  *)
  block_timeout : float;  (** Timeout for receiving a new block. *)
  rpc_timeout : Websocket_client.timeout option;
}

let ctxt = Efunc_core.Eth.Crypto.context ()

let from_hex_string v =
  try
    match (String.sub v 0 2, String.sub v 2 (String.length v - 2)) with
    | "0x", value ->
        Libsecp256k1.External.Key.read_sk_exn
          ctxt
          Hex.(to_bigstring_exn (`Hex value))
    | _, _ -> raise (Invalid_argument "secret_key_from_hex_value")
  with _ ->
    let reason = Format.sprintf "%s is not a valid hexa-encoded secret key" v in
    raise (Invalid_argument reason)

let secret_key_encoding =
  let open Data_encoding in
  conv (fun _sk -> assert false) (fun s -> from_hex_string s) string

let default_data_dir =
  Filename.concat (Sys.getenv "HOME") ".fa-bridge-watchtower"

let default_config_file ~data_dir = Filename.concat data_dir "config.json"

let default_evm_node_endpoint = Uri.of_string "http://127.0.0.1:8545"

let default_gas_limit = 1_000_000L

let default_max_fee_per_gas = 100_000_000_000L

let default_rpc_config = None

let default_secret_key = None

let default_whitelist = None

let default =
  {
    evm_node_endpoint = default_evm_node_endpoint;
    gas_limit = default_gas_limit;
    max_fee_per_gas = default_max_fee_per_gas;
    rpc = default_rpc_config;
    secret_key = default_secret_key;
    whitelist = default_whitelist;
    monitor_all_deposits = false;
    block_timeout = 10. (* seconds *);
    rpc_timeout = Some {timeout = 10.; on_timeout = `Retry 10};
  }

let rpc_encoding =
  let open Data_encoding in
  conv
    (fun {addr; port} -> (addr, port))
    (fun (addr, port) -> {addr; port})
    (obj2
       (req "addr" ~description:"Address for the RPC server" string)
       (req "port" ~description:"Port for the RPC server" int31))

let whitelist_item_encoding =
  let open Data_encoding in
  conv
    (fun {proxy; ticket_hashes} -> (proxy, ticket_hashes))
    (fun (proxy, ticket_hashes) -> {proxy; ticket_hashes})
    (obj2
       (opt
          "proxy"
          ~description:"Optional proxy address"
          Ethereum_types.address_encoding)
       (opt
          "ticket_hashes"
          ~description:"Optional list of ticket hashes"
          (list Ethereum_types.hash_encoding)))

let rpc_timeout_encoding =
  let open Data_encoding in
  let on_timeout_encoding =
    union
      [
        case
          (Tag 0)
          ~title:"fail"
          ~description:"Stop the watchtower on an RPC timeout"
          (constant "fail")
          (function `Fail -> Some () | _ -> None)
          (fun () -> `Fail);
        case
          (Tag 1)
          ~title:"retry"
          ~description:"Retry (with limit) on an RPC timeout"
          (obj1 (req "retry" int31))
          (function `Retry n -> Some n | _ -> None)
          (fun n -> `Retry n);
        case
          (Tag 2)
          ~title:"retry_forever"
          ~description:"Retry forever on an RPC timeout"
          (constant "retry_forever")
          (function `Retry_forever -> Some () | _ -> None)
          (fun () -> `Retry_forever);
      ]
  in
  conv
    (fun {Websocket_client.timeout; on_timeout} -> (timeout, on_timeout))
    (fun (timeout, on_timeout) -> {timeout; on_timeout})
    (obj2
       (req
          "timeout"
          ~description:"Timeout in seconds for JSON RPC requests to EVM node"
          float)
       (req
          "on_timeout"
          ~description:"Action to take on a timeout in a JSON RPC request"
          on_timeout_encoding))

let encoding =
  let open Data_encoding in
  conv
    (fun {
           evm_node_endpoint;
           gas_limit;
           max_fee_per_gas;
           rpc;
           secret_key;
           whitelist;
           monitor_all_deposits;
           block_timeout;
           rpc_timeout;
         }
       ->
      ( evm_node_endpoint,
        gas_limit,
        max_fee_per_gas,
        rpc,
        secret_key,
        whitelist,
        monitor_all_deposits,
        block_timeout,
        rpc_timeout ))
    (fun ( evm_node_endpoint,
           gas_limit,
           max_fee_per_gas,
           rpc,
           secret_key,
           whitelist,
           monitor_all_deposits,
           block_timeout,
           rpc_timeout )
       ->
      {
        evm_node_endpoint;
        gas_limit;
        max_fee_per_gas;
        rpc;
        secret_key;
        whitelist;
        monitor_all_deposits;
        block_timeout;
        rpc_timeout;
      })
    (obj9
       (dft
          "evm_node_endpoint"
          ~description:"URL of the EVM node"
          Tezos_rpc.Encoding.uri_encoding
          default.evm_node_endpoint)
       (dft
          "gas_limit"
          ~description:"Gas limit for transactions"
          int64
          default.gas_limit)
       (dft
          "max_fee_per_gas"
          ~description:"Maximum fee per gas unit"
          int64
          default.max_fee_per_gas)
       (opt
          "rpc"
          ~description:
            "Optional RPC server configuration. RPC server is not started when \
             absent."
          rpc_encoding)
       (opt
          "secret_key"
          ~description:
            "Optional secret key for signing transactions, when absent it is \
             read from the CLI or the environment variable \
             FA_BRIDGE_WATCHTOWER_SK."
          secret_key_encoding)
       (opt
          "whitelist"
          ~description:"Optional list of whitelisted items"
          (list whitelist_item_encoding))
       (dft
          "monitor_all_deposits"
          ~description:"Monitor and store all deposits in the DB."
          bool
          false)
       (dft
          "block_timeout"
          ~description:"Timeout for receiving a new block in seconds."
          float
          default.block_timeout)
       (dft
          "rpc_timeout"
          ~description:"Make RPCs to the EVM node with a given timeout"
          (option rpc_timeout_encoding)
          default.rpc_timeout))

(** [load_file ~data_dir] attempts to load the configuration file from the
    specified data directory. Returns [None] if the file doesn't exist or [Some
    config] with the loaded configuration if successful. *)
let load_file ~data_dir =
  let open Lwt_result_syntax in
  let config_file = default_config_file ~data_dir in
  let*! exists = Lwt_unix.file_exists config_file in
  if not exists then return_none
  else
    let* json = Lwt_utils_unix.Json.read_file config_file in
    let config = Data_encoding.Json.destruct encoding json in
    return_some config

(** [patch_config ~secret_key ~evm_node_endpoint ~monitor_all_deposits config]
    updates the configuration with the provided secret key and EVM node endpoint
    if they are present.  Returns the updated configuration. *)
let patch_config ~secret_key ~evm_node_endpoint ~monitor_all_deposits config =
  let config =
    Option.fold secret_key ~none:config ~some:(fun secret_key ->
        {config with secret_key = Some secret_key})
  in
  let config =
    Option.fold evm_node_endpoint ~none:config ~some:(fun evm_node_endpoint ->
        {config with evm_node_endpoint})
  in
  let config =
    if monitor_all_deposits then {config with monitor_all_deposits} else config
  in
  config
