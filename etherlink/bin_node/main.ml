(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023-2024 Functori <contact@functori.com>                   *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

open Configuration

let config_env name = "EVM_NODE_" ^ name

module Event = struct
  let section = ["evm_node"]

  let event_starting =
    Internal_event.Simple.declare_1
      ~section
      ~name:"start"
      ~msg:"starting the EVM node ({mode})"
      ~level:Notice
      ("mode", Data_encoding.string)

  let sequencer_disabled_native_execution =
    Internal_event.Simple.declare_0
      ~section
      ~name:"sequencer_disabled_native_execution"
      ~msg:"native execution is disabled in sequencer mode"
      ~level:Warning
      ()

  let buggy_dream_websocket =
    Internal_event.Simple.declare_0
      ~section
      ~name:"dream_websocket"
      ~msg:
        "websocket support in Dream is known to be buggy, consider using Resto \
         as an RPC server or disabling websockets"
      ~level:Warning
      ()
end

module Params = struct
  let string = Tezos_clic.parameter (fun _ s -> Lwt.return_ok s)

  let hex_string =
    Tezos_clic.parameter (fun _ s ->
        match `Hex s |> Hex.to_string with
        | Some bytes -> Lwt.return_ok bytes
        | None -> failwith "Invalid hex string")

  let int = Tezos_clic.parameter (fun _ s -> Lwt.return_ok (int_of_string s))

  let int64 =
    Tezos_clic.parameter (fun _ s -> Lwt.return_ok (Int64.of_string s))

  let endpoint =
    Tezos_clic.parameter (fun _ uri -> Lwt.return_ok (Uri.of_string uri))

  let optional_endpoint =
    Tezos_clic.parameter (fun _ uri ->
        let open Lwt_result_syntax in
        if uri = "" then return_none else return_some (Uri.of_string uri))

  let event_level =
    Tezos_clic.parameter (fun _ value ->
        Lwt.return_ok (Internal_event.Level.of_string_exn value))

  let kernel_path =
    Tezos_clic.parameter (fun _ s ->
        let open Lwt_result_syntax in
        let*! file_exists = Lwt_unix.file_exists s in
        let* () =
          when_ (not file_exists) @@ fun () -> failwith "%s is not a file" s
        in
        return (Evm_node_lib_dev.Wasm_debugger.On_disk s))

  let native_execution_policy =
    Tezos_clic.parameter (fun _ policy ->
        match policy with
        | "always" -> Lwt.return_ok Always
        | "rpcs_only" -> Lwt.return_ok Rpcs_only
        | "never" -> Lwt.return_ok Never
        | invalid_policy ->
            failwith
              "'%s' is not a valid native execution policy"
              invalid_policy)

  let supported_network =
    Tezos_clic.parameter (fun _ -> function
      | "mainnet" -> Lwt.return_ok Mainnet
      | "testnet" -> Lwt.return_ok Testnet
      | invalid_network ->
          failwith "'%s' is not a supported network" invalid_network)

  let rollup_node_endpoint = endpoint

  let evm_node_endpoint = endpoint

  let evm_node_private_endpoint = endpoint

  let sequencer_key =
    Tezos_clic.param
      ~name:"sequencer-key"
      ~desc:"Key to sign the blueprints."
      string

  let string_list =
    Tezos_clic.parameter (fun _ s ->
        let list = String.split ',' s in
        Lwt.return_ok list)

  let profile =
    let open Lwt_result_syntax in
    Tezos_clic.parameter (fun _ s ->
        match s with
        | "minimal" -> return Configuration.Minimal
        | "flamegraph" -> return Configuration.Flamegraph
        | _ -> failwith "Available options are 'minimal' and 'flamegraph'.")

  let time_between_blocks =
    Tezos_clic.parameter (fun _ s ->
        let time_between_blocks =
          if s = "none" then Nothing
          else Time_between_blocks (Float.of_string s)
        in
        Lwt.return_ok time_between_blocks)

  let timestamp =
    let open Lwt_result_syntax in
    Tezos_clic.parameter (fun _ timestamp ->
        let timestamp = String.trim timestamp in
        match Time.Protocol.of_notation timestamp with
        | Some t -> return t
        | None -> (
            match
              Int64.of_string_opt timestamp
              |> Option.map Time.Protocol.of_seconds
            with
            | Some t -> return t
            | None ->
                failwith
                  "Timestamp must be either in RFC3399 format  (e.g., \
                   [\"1970-01-01T00:00:00Z\"]) or in number of seconds since \
                   the {!Time.Protocol.epoch}."))

  let eth_address =
    Tezos_clic.parameter (fun _ address ->
        let open Lwt_result_syntax in
        let*? (`Hex hex) = Evm_node_lib_dev.Misc.normalize_hex address in
        let* () =
          when_
            (String.length hex <> 40)
            (fun () -> failwith "%s is not a valid address" address)
        in
        return (Evm_node_lib_dev_encoding.Ethereum_types.Address (Hex hex)))

  let tez_account =
    Tezos_clic.parameter (fun _ public_key ->
        Lwt.return (Signature.V1.Public_key.of_b58check public_key))

  let l2_level =
    Tezos_clic.parameter (fun () s ->
        Lwt.return_ok
        @@ Evm_node_lib_dev_encoding.Ethereum_types.Qty (Z.of_string s))

  let snapshot_file_or_url next =
    Tezos_clic.param
      ~name:"snapshot"
      ~desc:"Snapshot archive file, URL or stdin (when given `-`)."
      string
      next

  let history_param =
    Tezos_clic.parameter @@ fun () s ->
    let open Lwt_result_syntax in
    match Configuration.history_mode_of_string_opt s with
    | Some mode -> return mode
    | None ->
        failwith
          "Invalid history mode. Must be either archive, rolling:n or full:n \
           where n is the number of days to retain history."

  let history next =
    Tezos_clic.param
      ~name:"history"
      ~desc:
        "History mode, either archive, rolling:n or full:n where n is the \
         number of days of history to retain."
      history_param
      next
end

let wallet_dir_arg =
  Tezos_clic.default_arg
    ~long:"wallet-dir"
    ~short:'d'
    ~placeholder:"path"
    ~env:Client_config.base_dir_env_name
    ~default:Client_config.default_base_dir
    ~pp_default:(fun fmt -> Format.pp_print_string fmt "$HOME/.tezos-client")
    ~doc:"The directory where the Tezos client stores all its wallet data."
    Params.string

let rpc_addr_arg =
  Tezos_clic.arg
    ~long:"rpc-addr"
    ~placeholder:"ADDR"
    ~doc:"The EVM node server rpc address."
    Params.string

let rpc_port_arg =
  Tezos_clic.arg
    ~long:"rpc-port"
    ~placeholder:"PORT"
    ~doc:"The EVM node server rpc port."
    Params.int

let rpc_batch_limit_arg =
  Tezos_clic.arg
    ~long:"rpc-batch-limit"
    ~placeholder:"PORT"
    ~doc:
      "A limit on the number of requests allowed in a single batch. Can either \
       be `unlimited` or a positive integer."
    (Tezos_clic.parameter (fun _ctxt ->
         let open Lwt_result_syntax in
         function
         | "unlimited" -> return Unlimited
         | r -> (
             match int_of_string_opt r with
             | Some max ->
                 if 0 <= max then return (Limit max)
                 else
                   failwith
                     "Invalid argument for --rpc-batch-limit: %d is negative"
                     max
             | None ->
                 failwith
                   "Invalid argument for --rpc-batch-limit: %s. Should be \
                    `unlimited` or an integer"
                   r)))

let private_rpc_port_arg =
  Tezos_clic.arg
    ~long:"private-rpc-port"
    ~placeholder:"PORT"
    ~doc:"The EVM node private server rpc port."
    Params.int

let block_number_arg ~doc =
  let open Evm_node_lib_dev_encoding in
  Tezos_clic.map_arg ~f:(fun _ctxt i ->
      Lwt_result_syntax.return
        (Option.map (fun i -> Ethereum_types.Qty Z.(of_int i)) i))
  @@ Tezos_clic.arg ~long:"block-number" ~placeholder:"N" ~doc Params.int

let maximum_blueprints_lag_arg =
  Tezos_clic.arg
    ~long:"maximum-blueprints-lag"
    ~placeholder:"LAG"
    ~doc:
      "The maximum advance (in blueprints) the Sequencer accepts to have \
       before trying to send its backlog again."
    Params.int

let maximum_blueprints_ahead_arg =
  Tezos_clic.arg
    ~long:"maximum-blueprints-ahead"
    ~placeholder:"AHEAD"
    ~doc:"The maximum advance (in blueprints) the Sequencer accepts."
    Params.int

let maximum_blueprints_catchup_arg =
  Tezos_clic.arg
    ~long:"maximum-blueprints-catch-up"
    ~placeholder:"CATCH_UP"
    ~doc:"The maximum number of blueprints the Sequencer resends at once."
    Params.int

let catchup_cooldown_arg =
  Tezos_clic.arg
    ~long:"catch-up-cooldown"
    ~placeholder:"COOLDOWN"
    ~doc:
      "The maximum number of Layer 1 blocks the Sequencer waits after \
       resending its blueprints before trying to catch-up again."
    Params.int

let cors_allowed_headers_arg =
  Tezos_clic.arg
    ~long:"cors-headers"
    ~placeholder:"ALLOWED_HEADERS"
    ~doc:"List of accepted cors headers."
    Params.string_list

let cors_allowed_origins_arg =
  Tezos_clic.arg
    ~long:"cors-origins"
    ~placeholder:"ALLOWED_ORIGINS"
    ~doc:"List of accepted cors origins."
    Params.string_list

let mainnet_compat_arg =
  Tezos_clic.switch
    ~long:"mainnet-compat"
    ~doc:
      "Generate a configuration compatible with the first Etherlink Mainnet \
       kernel."
    ()

let profile_arg =
  Tezos_clic.arg_or_switch
    ~long:"profile"
    ~placeholder:"minimal|flamegraph"
    ~doc:
      "Profile the execution of the WASM PVM; 'minimal' provides a file to \
       which it streamlines tick and gas consumption, 'flamegraph' creates a \
       flamegraph indexed on tick consumption"
    ~default:"flamegraph"
    Params.profile

let disable_da_fees_arg =
  Tezos_clic.switch
    ~long:"disable-da-fees"
    ~doc:"Disable DA fees for this replay."
    ()

let omit_delayed_tx_events_arg =
  Tezos_clic.switch
    ~long:"omit-delayed-tx-events"
    ~doc:
      "Don't populate the delayed transactions in the store of the EVM node. \
       This is necessary when creating the data dir for an observer EVM node \
       because delayed transactions are going to be fetched from the \
       sequencer."
    ()

let keep_everything_arg =
  Tezos_clic.switch
    ~short:'k'
    ~long:"keep-everything"
    ~doc:"Do not filter out files outside of the `/evm` directory."
    ()

let verbose_arg =
  Tezos_clic.switch
    ~short:'v'
    ~long:"verbose"
    ~doc:"Sets logging level to debug. Beware, it is highly verbose."
    ()

let kernel_verbosity_arg =
  Tezos_clic.arg
    ~long:"kernel-verbosity"
    ~doc:
      "Sets kernel's logging verbosity, either `fatal`, `error`, `info`, \
       `debug`."
    ~placeholder:"info"
    ( Tezos_clic.parameter @@ fun _ctxt value ->
      let open Lwt_result_syntax in
      let open Evm_node_lib_dev.Events in
      match value with
      | "debug" -> return Debug
      | "info" -> return Info
      | "error" -> return Error
      | "fatal" -> return Fatal
      | _ -> failwith "%s is an invalid verbosity level" value )

let config_filename ~data_dir = function
  | None -> Filename.concat data_dir "config.json"
  | Some config_file -> config_file

let data_dir_arg =
  let default = Filename.concat (Sys.getenv "HOME") ".octez-evm-node" in
  Tezos_clic.default_arg
    ~env:(config_env "DATA_DIR")
    ~long:"data-dir"
    ~placeholder:"data-dir"
    ~doc:"The path to the EVM node data directory."
    ~pp_default:(fun fmt -> Format.fprintf fmt "$HOME/.octez-evm-node")
    ~default
    Params.string

let config_path_arg =
  Tezos_clic.arg
    ~long:"config-file"
    ~env:(config_env "CONFIG_FILE")
    ~placeholder:"path"
    ~doc:
      "Path to a configuration file. Defaults to `config.json` inside the data \
       directory of the node."
    Params.string

let print_config_arg =
  Tezos_clic.switch
    ~doc:"Print the full configuration to the standard output."
    ~short:'p'
    ~long:"print"
    ()

let level_arg =
  Tezos_clic.default_arg
    ~doc:
      "Set list_events filter level to either `fatal`, `error`, `warning`, \
       `notice` `info`, `debug`."
    ~short:'l'
    ~long:"level"
    ~placeholder:"info"
    ~default:"none"
    Params.event_level

let json_arg =
  Tezos_clic.switch
    ~short:'j'
    ~long:"json"
    ~doc:"Enables the display of json schemas."
    ()

let rollup_address_arg =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  parameter (fun _ hash ->
      let hash_opt =
        Tezos_crypto.Hashed.Smart_rollup_address.of_b58check_opt hash
      in
      match hash_opt with
      | Some hash -> return hash
      | None ->
          failwith
            "Parameter '%s' is an invalid smart rollup address encoded in a \
             base58 string."
            hash)
  |> default_arg
       ~long:"rollup-address"
       ~doc:
         "The smart rollup address in Base58 encoding used to produce the \
          chunked messages."
       ~default:Tezos_crypto.Hashed.Smart_rollup_address.(to_b58check zero)
       ~placeholder:"sr1..."

let kernel_arg ?(long = "kernel") ?(placeholder = "evm_kernel.wasm") () =
  Tezos_clic.arg
    ~long
    ~placeholder
    ~doc:
      "Path to the EVM kernel used to launch the PVM, it will be loaded from \
       storage afterward."
    Params.kernel_path

let initial_kernel_arg =
  kernel_arg ~long:"initial-kernel" ~placeholder:"evm_installer.wasm" ()

let force_arg ~doc = Tezos_clic.switch ~long:"force" ~short:'f' ~doc ()

let preimages_arg =
  Tezos_clic.arg
    ~long:"preimages-dir"
    ~doc:"Path to the preimages directory."
    ~placeholder:"_evm_installer_preimages"
    Params.string

let preimages_endpoint_arg =
  Tezos_clic.arg
    ~long:"preimages-endpoint"
    ~placeholder:"url"
    ~doc:
      "The address of a service which provides pre-images for the rollup. \
       Missing pre-images will be downloaded remotely if they are not already \
       present on disk."
    Params.endpoint

let native_execution_policy_arg =
  Tezos_clic.arg
    ~long:"native-execution-policy"
    ~short:'n'
    ~placeholder:"policy"
    ~doc:
      "Policy regarding the use of native execution for supported kernels. Can \
       be `never`, `rpcs_only` or `always`."
    Params.native_execution_policy

let supported_network_arg ?why () =
  Tezos_clic.arg
    ~env:(config_env "NETWORK")
    ~long:"network"
    ~placeholder:"network"
    ~doc:
      Format.(
        asprintf
          "The network the EVM node will be connecting to. Can be `mainnet` or \
           `testnet`.%a"
          (pp_print_option (fun fmt why -> fprintf fmt " %s" why))
          why)
    Params.supported_network

let rollup_node_endpoint_arg =
  Tezos_clic.arg
    ~long:"rollup-node-endpoint"
    ~placeholder:"url"
    ~doc:"The address of a rollup node."
    Params.rollup_node_endpoint

let dont_track_rollup_node_arg =
  Tezos_clic.switch
    ~long:"dont-track-rollup-node"
    ~doc:
      "Disable tracking the head of the rollup node. Tracking the state of a \
       rollup node allows to confirm the blocks received from the upstream EVM \
       node."
    ()

let no_sync_arg =
  Tezos_clic.switch
    ~long:"no-sync"
    ~doc:"Disable tracking the head of the EVM node endpoint."
    ()

let init_from_snapshot_arg =
  Tezos_clic.arg_or_switch
    ~long:"init-from-snapshot"
    ~doc:
      "Automatically download and import a recent snapshot for supported \
       networks on fresh data directories. If no snapshot provider is given \
       e.g. `--init-from-snapshot` with no argument, then it uses the default \
       built-in provider `https://snapshotter-sandbox.nomadic-labs.eu`. %r is \
       replaced by the short form of the Smart Rollup address, %R by the \
       complete Smart Rollup address, %n by the network (given by the argument \
       --network), %h by the history mode used by the node, and %% by %. Also \
       accepts a path to an existing snapshot."
    ~default:
      "https://snapshotter-sandbox.nomadic-labs.eu/local/etherlink-%n/%h/etherlink-%n-%h-latest.gz"
    ~placeholder:"snapshot url"
  @@ Params.string

let evm_node_endpoint_arg =
  Tezos_clic.arg
    ~long:"evm-node-endpoint"
    ~placeholder:"url"
    ~doc:"The address of an EVM node to connect to."
    Params.evm_node_endpoint

let replicate_arg =
  Tezos_clic.arg_or_switch
    ~pp_default:(fun fmt ->
      Format.fprintf fmt "the official relay endpoint if --network is used")
    ~long:"replicate"
    ~placeholder:"url"
    ~default:""
    ~doc:
      "Replicate a chain in real time from the EVM node whose address is \
       provided."
    Params.optional_endpoint

let evm_node_private_endpoint_arg =
  Tezos_clic.arg
    ~long:"evm-node-private-endpoint"
    ~placeholder:"url"
    ~doc:"The address of an EVM node and its private endpoint."
    Params.evm_node_private_endpoint

let bundler_node_endpoint_arg =
  Tezos_clic.arg
    ~long:"bundler-node-endpoint"
    ~placeholder:"url"
    ~doc:
      "The address of a service which encrypts incoming transactions for the \
       rollup."
    Params.endpoint

let sequencer_sidecar_endpoint_arg =
  Tezos_clic.arg
    ~long:"sequencer-sidecar-endpoint"
    ~placeholder:"url"
    ~doc:"The address of The sequencer sidecar to connect to."
    Params.endpoint

let time_between_blocks_arg =
  Tezos_clic.arg
    ~long:"time-between-blocks"
    ~doc:
      "Interval (in seconds) at which the sequencer creates an empty block by \
       default. If set to `none`, blocks are produced on demand only (see \
       private method produceBlock)."
    ~placeholder:"10."
    Params.time_between_blocks

let max_number_of_chunks_arg =
  Tezos_clic.arg
    ~long:"max-number-of-chunks"
    ~doc:"Maximum number of chunks per blueprint."
    ~placeholder:"10."
    Params.int

let keep_alive_arg =
  Tezos_clic.switch
    ~doc:
      "Keep the EVM node process alive even if the connection is lost with the \
       rollup node."
    ~short:'K'
    ~long:"keep-alive"
    ()

let blueprint_mode_arg =
  Tezos_clic.switch
    ~long:"as-blueprint"
    ~doc:"Chunk the data into a blueprint usable in sequencer mode."
    ()

let timestamp_arg =
  Params.timestamp
  |> Tezos_clic.default_arg
       ~long:"timestamp"
       ~doc:""
       ~placeholder:"1970-01-01T00:00:00Z"
       ~default:"0"

let genesis_timestamp_arg =
  Params.timestamp
  |> Tezos_clic.arg
       ~long:"genesis-timestamp"
       ~doc:
         "Timestamp used for the genesis block, uses machine's clock if not \
          provided."
       ~placeholder:"1970-01-01T00:00:00Z"

let blueprint_number_arg =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  parameter (fun _ number ->
      try String.trim number |> Z.of_string |> return
      with _ -> failwith "Blueprint number must be an integer")
  |> default_arg
       ~long:"number"
       ~doc:"Level of the blueprint."
       ~placeholder:"0"
       ~default:"0"

let parent_hash_arg =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  parameter (fun _ hash -> return hash)
  |> default_arg
       ~long:"parent-hash"
       ~doc:"Blueprint's parent hash."
       ~placeholder:
         "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
       ~default:
         "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

let tx_pool_timeout_limit_arg =
  Tezos_clic.arg
    ~long:"tx-pool-timeout-limit"
    ~placeholder:"3_600"
    ~doc:"Transaction timeout limit inside the transaction pool (in seconds)."
    Params.int64

let tx_pool_addr_limit_arg =
  Tezos_clic.arg
    ~long:"tx-pool-addr-limit"
    ~placeholder:"4_000"
    ~doc:"Maximum allowed addresses inside the transaction pool."
    Params.int64

let tx_pool_tx_per_addr_limit_arg =
  Tezos_clic.arg
    ~long:"tx-pool-tx-per-addr-limit"
    ~placeholder:"16"
    ~doc:
      "Maximum allowed transactions per user address inside the transaction \
       pool."
    Params.int64

let sequencer_key_arg =
  Tezos_clic.arg
    ~long:"sequencer-key"
    ~doc:"Key to sign the blueprints."
    ~placeholder:"edsk..."
    Params.string

let log_filter_max_nb_blocks_arg =
  Tezos_clic.arg
    ~long:"max-number-blocks"
    ~doc:"Maximum number of blocks kept in the log."
    ~placeholder:"100"
    Params.int

let log_filter_max_nb_logs_arg =
  Tezos_clic.arg
    ~long:"max-number-logs"
    ~doc:"Maximum number of logs kept."
    ~placeholder:"1000"
    Params.int

let log_filter_chunk_size_arg =
  Tezos_clic.arg
    ~long:"chunk-size"
    ~doc:
      "Blocks to be filtered are split in chunks, which will be filtered in \
       sequence. Within each chunk, the block filtering is done concurrently."
    ~placeholder:"10"
    Params.int

let read_only_arg =
  Tezos_clic.switch
    ~doc:"If the flag is set, the node refuses transactions."
    ~long:"read-only"
    ()

let finalized_view_arg =
  Tezos_clic.switch
    ~doc:
      "If the flag is set, the node will use the latest final state of the \
       rollup, not its current HEAD, for any read-only operation."
    ~long:"finalized-view"
    ()

let ignore_block_param_arg =
  Tezos_clic.switch
    ~doc:
      "If the flag is set, the node in proxy mode ignores the block parameter \
       submitted by the client and defaults to the latest block for \
       unsupported blocks."
    ~long:"ignore-block-param"
    ()

let restricted_rpcs_arg =
  Tezos_clic.arg
    ~doc:
      "Disable methods that matches the given Perl-like regular expression. \
       Cannot be used with --whitelisted-rpcs or --blacklisted-rpcs."
    ~long:"restricted-rpcs"
    ~placeholder:"regexp"
    (Tezos_clic.parameter (fun _ctxt raw ->
         Lwt_result_syntax.return
           (Configuration.make_pattern_restricted_rpcs raw)))

let whitelisted_rpcs_arg =
  Tezos_clic.arg
    ~doc:
      "Disable the RPC methods which are not part of the provided list. Cannot \
       be used with --restricted-rpcs or --blacklisted-rpcs."
    ~long:"whitelisted-rpcs"
    ~placeholder:"whitelist"
    (Tezos_clic.parameter (fun _ctxt raw ->
         Lwt_result_syntax.return
           (Configuration.Whitelist (String.split ',' raw))))

let blacklisted_rpcs_arg =
  Tezos_clic.arg
    ~doc:
      "Disable the RPC methods which are part of the provided list. Cannot be \
       used with --restricted-rpcs or --whitelisted-rpcs."
    ~long:"blacklisted-rpcs"
    ~placeholder:"blacklist"
    (Tezos_clic.parameter (fun _ctxt raw ->
         Lwt_result_syntax.return
           (Configuration.Blacklist (String.split ',' raw))))

let pick_restricted_rpcs r w b =
  match (r, w, b) with
  | None, None, None -> Lwt_result_syntax.return_none
  | Some r, None, None | None, Some r, None | None, None, Some r ->
      Lwt_result_syntax.return_some r
  | _ ->
      failwith
        "Can only use one CLI argument among --restricted-rpcs, \
         --blacklisted-rpcs and --whitelisted-rpcs"

let dal_slots_arg =
  Tezos_clic.arg
    ~long:"dal-slots"
    ~doc:
      "The DAL slots indices on which the sequencer is allowed to send \
       blueprints."
    ~placeholder:"slot indices"
    (Tezos_clic.parameter (fun _ctxt s ->
         s |> String.split ',' |> List.map int_of_string
         |> Lwt_result_syntax.return))

let num_download_retries =
  Tezos_clic.arg
    ~doc:
      "Number of times a revealed preimage can be redownloaded in case the it \
       doesn't pass the sanity check. It can be useful if the download is \
       corrupted for some reason."
    ~long:"retry"
    ~placeholder:"1"
    Params.int

let history_arg =
  Tezos_clic.arg
    ~long:"history"
    ~doc:"History mode for the EVM node. ':n' means n days of history."
    ~placeholder:"archive | rolling:n | full:n"
    Params.history_param

let common_config_args =
  Tezos_clic.args20
    data_dir_arg
    config_path_arg
    rpc_addr_arg
    rpc_port_arg
    rpc_batch_limit_arg
    cors_allowed_origins_arg
    cors_allowed_headers_arg
    log_filter_max_nb_blocks_arg
    log_filter_max_nb_logs_arg
    log_filter_chunk_size_arg
    keep_alive_arg
    rollup_node_endpoint_arg
    tx_pool_timeout_limit_arg
    tx_pool_addr_limit_arg
    tx_pool_tx_per_addr_limit_arg
    verbose_arg
    restricted_rpcs_arg
    blacklisted_rpcs_arg
    whitelisted_rpcs_arg
    finalized_view_arg

let compress_on_the_fly_arg : (bool, unit) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"compress-on-the-fly"
    ~doc:
      "Produce a compressed snapshot on the fly. The rollup node will use less \
       disk space to produce the snapshot but will lock the rollup node (if \
       running) for a longer time. Without this option, producing a snaphsot \
       requires the available disk space to be around the size of the data \
       dir."
    ()

let uncompressed : (bool, unit) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"uncompressed"
    ~doc:"Produce an uncompressed snapshot."
    ()

let snapshot_file_arg =
  Tezos_clic.arg
    ~long:"snapshot-file"
    ~short:'s'
    ~placeholder:"path"
    ~doc:
      "Path to the snapshot file to create, with string interpolation. %r is \
       replaced by the short form of the Smart Rollup address, %R by the \
       complete Smart Rollup address, %l by the current head of the node, %h \
       by the history mode used by the node, and %% by %. Default is located \
       in the current directory, and the filename is based on the snapshot \
       information."
    Params.string

module Groups = struct
  let run = Tezos_clic.{name = "run"; title = "Run commands"}

  let config = Tezos_clic.{name = "config"; title = "Configuration commands"}

  let snapshot = Tezos_clic.{name = "snapshot"; title = "Snapshots commands"}

  let storage = Tezos_clic.{name = "storage"; title = "Storage commands"}

  let kernel = Tezos_clic.{name = "kernel"; title = "Kernel commands"}

  let debug = Tezos_clic.{name = "debug"; title = "Debug commands"}
end

let websocket_checks config =
  match config.experimental_features with
  | {enable_websocket = true; rpc_server = Dream; _} ->
      Internal_event.Simple.emit Event.buggy_dream_websocket () |> Lwt_result.ok
  | _ -> Lwt_result_syntax.return_unit

let make_event_config ~verbosity ?daily_logs_path () =
  let open Tezos_event_logging.Internal_event in
  let open Tezos_base_unix.Internal_event_unix in
  let open Tezos_base.Internal_event_config in
  let log_cfg =
    Tezos_base_unix.Logs_simple_config.create_cfg ~advertise_levels:true ()
  in
  let config = make_with_defaults ~log_cfg ~verbosity () in
  match daily_logs_path with
  | Some daily_logs_path ->
      (* Show only above Info rpc_server events, they are not
         relevant as we do not have a REST-API server. If not
         set, the daily logs are polluted with these
         uninformative logs. *)
      let daily_logs_section_prefixes =
        [
          ("rpc_server", Some Notice);
          ("rpc_server", Some Warning);
          ("rpc_server", Some Error);
          ("rpc_server", Some Fatal);
        ]
      in
      let daily_log_file = "daily.log" in
      let uri =
        make_config_uri
          ~create_dirs:true
          ~daily_logs:7
          ~level:Info
          ~format:"pp-rfc5424"
          ~chmod:0o640
          ~section_prefixes:daily_logs_section_prefixes
          ~advertise_levels:true
          (`Path Filename.Infix.(daily_logs_path // daily_log_file))
      in
      add_uri_to_config uri config
  | None -> config

let init_logs ~daily_logs ?rpc_mode_port ~data_dir configuration =
  let open Tezos_base_unix.Internal_event_unix in
  let daily_logs_path =
    if daily_logs then
      match rpc_mode_port with
      | Some port ->
          Some
            Filename.Infix.(
              data_dir // ("daily_logs_rpc_" ^ string_of_int port))
      | None -> Some Filename.Infix.(data_dir // "daily_logs")
    else None
  in
  let config =
    make_event_config ~verbosity:configuration.verbose ?daily_logs_path ()
  in
  init ~config ()

let start_proxy ~data_dir ~config_file ~keep_alive ?rpc_addr ?rpc_port
    ?rpc_batch_limit ?cors_origins ?cors_headers ?log_filter_max_nb_blocks
    ?log_filter_max_nb_logs ?log_filter_chunk_size ?rollup_node_endpoint
    ?evm_node_endpoint ?tx_pool_timeout_limit ?tx_pool_addr_limit
    ?tx_pool_tx_per_addr_limit ?restricted_rpcs ~verbose ~read_only
    ~finalized_view ~ignore_block_param () =
  let open Lwt_result_syntax in
  let* config =
    Cli.create_or_read_config
      ~data_dir
      ~keep_alive
      ?rpc_addr
      ?rpc_port
      ?rpc_batch_limit
      ?cors_origins
      ?cors_headers
      ?log_filter_max_nb_blocks
      ?log_filter_max_nb_logs
      ?log_filter_chunk_size
      ?rollup_node_endpoint
      ?evm_node_endpoint
      ?tx_pool_timeout_limit
      ?tx_pool_addr_limit
      ?tx_pool_tx_per_addr_limit
      ?restricted_rpcs
      ~finalized_view
      ~proxy_ignore_block_param:ignore_block_param
      ~verbose
      (config_filename ~data_dir config_file)
  in
  (* We patch [config] to take into account the proxy-specific argument
     [--read-only]. *)
  let config =
    {
      config with
      experimental_features =
        {
          config.experimental_features with
          enable_send_raw_transaction =
            (if read_only then false
             else config.experimental_features.enable_send_raw_transaction);
        };
    }
  in
  let*! () = init_logs ~daily_logs:true ~data_dir config in
  let*! () = Internal_event.Simple.emit Event.event_starting "proxy" in
  let* () = Evm_node_lib_dev.Proxy.main config in
  let wait, _resolve = Lwt.wait () in
  let* () = wait in
  return_unit

let register_wallet ?password_filename ~wallet_dir () =
  let wallet_ctxt =
    new Client_context_unix.unix_io_wallet
      ~base_dir:wallet_dir
      ~password_filename
  in
  let () =
    Client_main_run.register_default_signer
      (wallet_ctxt :> Client_context.io_wallet)
  in
  wallet_ctxt

let sequencer_disable_native_execution configuration =
  let open Lwt_syntax in
  match configuration.kernel_execution.native_execution_policy with
  | Always | Rpcs_only ->
      let+ () =
        Internal_event.Simple.emit Event.sequencer_disabled_native_execution ()
      in
      {
        configuration with
        kernel_execution =
          {configuration.kernel_execution with native_execution_policy = Never};
      }
  | Never -> return configuration

let kernel_from_args network kernel =
  let open Evm_node_lib_dev.Wasm_debugger in
  Option.either
    kernel
    (Option.bind network (function
        | Mainnet -> Some (In_memory Evm_node_supported_installers.mainnet)
        | Testnet -> None))

let start_sequencer ?password_filename ~wallet_dir ~data_dir ?rpc_addr ?rpc_port
    ?rpc_batch_limit ?cors_origins ?cors_headers ?tx_pool_timeout_limit
    ?tx_pool_addr_limit ?tx_pool_tx_per_addr_limit ~keep_alive
    ?rollup_node_endpoint ~verbose ?preimages ?preimages_endpoint
    ?native_execution_policy ?time_between_blocks ?max_number_of_chunks
    ?private_rpc_port ?sequencer_str ?max_blueprints_lag ?max_blueprints_ahead
    ?max_blueprints_catchup ?catchup_cooldown ?log_filter_max_nb_blocks
    ?log_filter_max_nb_logs ?log_filter_chunk_size ?genesis_timestamp
    ?restricted_rpcs ?kernel ?dal_slots ?sandbox_config ~finalized_view
    config_file =
  let open Lwt_result_syntax in
  let wallet_ctxt = register_wallet ?password_filename ~wallet_dir () in
  let* sequencer_key =
    match sandbox_config with
    | Some Evm_node_lib_dev.Sequencer.{secret_key = sk; _} ->
        let*? sk = Tezos_signer_backends.Unencrypted.make_sk sk in
        return_some sk
    | None ->
        Option.map_es
          (Client_keys.Secret_key.parse_source_string wallet_ctxt)
          sequencer_str
  in
  let* configuration =
    Cli.create_or_read_config
      ~data_dir
      ?rpc_addr
      ?rpc_port
      ?rpc_batch_limit
      ?cors_origins
      ?cors_headers
      ?tx_pool_timeout_limit
      ?tx_pool_addr_limit
      ?tx_pool_tx_per_addr_limit
      ~keep_alive
      ?rollup_node_endpoint
      ~verbose
      ?preimages
      ?preimages_endpoint
      ?native_execution_policy
      ?time_between_blocks
      ?max_number_of_chunks
      ?private_rpc_port
      ?sequencer_key
      ?max_blueprints_lag
      ?max_blueprints_ahead
      ?max_blueprints_catchup
      ?catchup_cooldown
      ?log_filter_max_nb_blocks
      ?log_filter_max_nb_logs
      ?log_filter_chunk_size
      ?restricted_rpcs
      ?dal_slots
      ~finalized_view
      config_file
  in
  let*! () = init_logs ~daily_logs:true ~data_dir configuration in
  let*! configuration =
    match sandbox_config with
    | None ->
        (* We are running in sequencer mode (not in sandbox mode), we need to disable native execution *)
        sequencer_disable_native_execution configuration
    | _ -> Lwt.return configuration
  in
  let* () = websocket_checks configuration in
  let*! () = Internal_event.Simple.emit Event.event_starting "sequencer" in

  Evm_node_lib_dev.Sequencer.main
    ~data_dir
    ?genesis_timestamp
    ~cctxt:(wallet_ctxt :> Client_context.wallet)
    ~configuration
    ?kernel
    ?sandbox_config
    ()

let rpc_run_args =
  Tezos_clic.args5
    evm_node_endpoint_arg
    evm_node_private_endpoint_arg
    preimages_arg
    preimages_endpoint_arg
    native_execution_policy_arg

let rpc_command =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  command
    ~group:Groups.run
    ~desc:"Start the EVM node in rpc mode."
    (merge_options common_config_args rpc_run_args)
    (prefixes ["experimental"; "run"; "rpc"] stop)
    (fun ( ( data_dir,
             config_file,
             rpc_addr,
             rpc_port,
             rpc_batch_limit,
             cors_origins,
             cors_headers,
             log_filter_max_nb_blocks,
             log_filter_max_nb_logs,
             log_filter_chunk_size,
             keep_alive,
             rollup_node_endpoint,
             tx_pool_timeout_limit,
             tx_pool_addr_limit,
             tx_pool_tx_per_addr_limit,
             verbose,
             restricted_rpcs,
             blacklisted_rpcs,
             whitelisted_rpcs,
             finalized_view ),
           ( evm_node_endpoint,
             evm_node_private_endpoint,
             preimages,
             preimages_endpoint,
             native_execution_policy ) )
         () ->
      let* restricted_rpcs =
        pick_restricted_rpcs restricted_rpcs whitelisted_rpcs blacklisted_rpcs
      in
      let* rpc_port =
        match rpc_port with
        | Some rpc_port -> return rpc_port
        | None -> failwith "--rpc-port argument required"
      in
      let* () =
        when_ Option.(is_some rollup_node_endpoint) @@ fun () ->
        failwith "unexpected --rollup-node-endpoint argument"
      in
      let config_file = config_filename ~data_dir config_file in
      let* read_write_config =
        (* We read the configuration used for the read-write node, without
           altering it ([keep_alive] and [verbose] are
           mandatory field of the function, but we don’t actually need those
           fields so it’s fine).

           This config will be used to infer what is the endpoint to use to
           connect to the read-write node. *)
        Cli.create_or_read_config
          ~data_dir
          ~keep_alive
          ~verbose
          ~finalized_view
          config_file
      in
      let default_addr rpc =
        let evm_node_addr =
          match rpc.addr with
          | "0.0.0.0" (* IPv4 catch-all bind address *)
          | "[::]" (* IPv6 catch-all bind address *) ->
              "localhost"
          | addr -> addr
        in
        Uri.make ~scheme:"http" ~host:evm_node_addr ~port:rpc.port ()
      in
      (* We reuse `--evm-node-endpoint` to allow to overwrite the
               endpoint used to connect to the read-write node. *)
      let evm_node_endpoint =
        Option.value
          ~default:(default_addr read_write_config.public_rpc)
          evm_node_endpoint
      in
      let evm_node_private_endpoint =
        match evm_node_private_endpoint with
        | Some _ as v -> v
        | None -> Option.map default_addr read_write_config.private_rpc
      in
      let config =
        Cli.patch_configuration_from_args
          ~keep_alive
          ?cors_origins
          ?cors_headers
          ?rpc_batch_limit
          ~verbose
          ?preimages
          ?preimages_endpoint
          ?native_execution_policy
          ?tx_pool_timeout_limit
          ?tx_pool_addr_limit
          ?tx_pool_tx_per_addr_limit
          ?log_filter_chunk_size
          ?log_filter_max_nb_logs
          ?log_filter_max_nb_blocks
          ?restricted_rpcs
          ~rpc_port
          ?rpc_addr
          read_write_config
          ~finalized_view
      in
      let*! () =
        init_logs ~daily_logs:true ~rpc_mode_port:rpc_port ~data_dir config
      in
      let* () = websocket_checks config in
      let*! () = Internal_event.Simple.emit Event.event_starting "rpc" in
      Evm_node_lib_dev.Rpc.main
        ~data_dir
        ~evm_node_endpoint
        ?evm_node_private_endpoint
        ~config
        ())

let start_observer ~data_dir ~keep_alive ?rpc_addr ?rpc_port ?rpc_batch_limit
    ?private_rpc_port ?cors_origins ?cors_headers ~verbose ?preimages
    ?preimages_endpoint ?native_execution_policy ?rollup_node_endpoint
    ~dont_track_rollup_node ?evm_node_endpoint
    ?threshold_encryption_bundler_endpoint ?tx_pool_timeout_limit
    ?tx_pool_addr_limit ?tx_pool_tx_per_addr_limit ?log_filter_chunk_size
    ?log_filter_max_nb_logs ?log_filter_max_nb_blocks ?restricted_rpcs ?kernel
    ~no_sync ~init_from_snapshot ?history_mode ~finalized_view ?network
    config_file =
  let open Lwt_result_syntax in
  let* config =
    Cli.create_or_read_config
      ~data_dir
      ~keep_alive
      ?rpc_addr
      ?rpc_port
      ?private_rpc_port
      ?rpc_batch_limit
      ?cors_origins
      ?cors_headers
      ?rollup_node_endpoint
      ?dont_track_rollup_node:
        (* If `dont_track_rollup_node` is false, it means the argument was
           omitted from the command-line. As a consequence, we default to
           the config value by passing [None]. *)
        (if dont_track_rollup_node then Some true else None)
      ~verbose
      ?preimages
      ?preimages_endpoint
      ?native_execution_policy
      ?evm_node_endpoint
      ?threshold_encryption_bundler_endpoint
      ?tx_pool_timeout_limit
      ?tx_pool_addr_limit
      ?tx_pool_tx_per_addr_limit
      ?log_filter_chunk_size
      ?log_filter_max_nb_logs
      ?log_filter_max_nb_blocks
      ?restricted_rpcs
      ?dal_slots:None
      ?history_mode
      ~finalized_view
      ?network
      config_file
  in
  let*! () = init_logs ~daily_logs:true ~data_dir config in
  let* () = websocket_checks config in
  let*! () = Internal_event.Simple.emit Event.event_starting "observer" in
  Evm_node_lib_dev.Observer.main
    ?network
    ~no_sync
    ~init_from_snapshot
    ?kernel_path:kernel
    ~data_dir
    ~config
    ()

let make_dev_messages ~kind ~smart_rollup_address data =
  let open Lwt_result_syntax in
  let open Evm_node_lib_dev in
  let open Evm_node_lib_dev_encoding in
  let transactions =
    List.map
      (fun s -> Ethereum_types.hex_of_string s |> Ethereum_types.hex_to_bytes)
      data
  in
  let* messages =
    match kind with
    | `Blueprint (cctxt, sk_uri, timestamp, number, parent_hash) ->
        let* blueprint_chunks =
          Sequencer_blueprint.prepare
            ~cctxt
            ~sequencer_key:sk_uri
            ~timestamp
            ~number:(Ethereum_types.quantity_of_z number)
            ~parent_hash:(Ethereum_types.block_hash_of_string parent_hash)
            ~transactions
            ~delayed_transactions:[]
        in
        let blueprint_payload =
          Sequencer_blueprint.create_inbox_payload
            ~smart_rollup_address
            ~chunks:blueprint_chunks
        in
        return @@ List.map (fun (`External s) -> s) blueprint_payload
    | `Transaction ->
        let*? chunks =
          List.map_e
            (fun tx ->
              Transaction_format.make_encoded_messages ~smart_rollup_address tx)
            transactions
        in
        return (chunks |> List.map snd |> List.flatten)
  in
  return (List.map (fun m -> m |> Hex.of_string |> Hex.show) messages)

let from_data_or_file data_for_file =
  let open Lwt_result_syntax in
  Client_aliases.parse_alternatives
    [
      ( "file",
        fun filename ->
          Lwt.catch
            (fun () ->
              let*! data = Lwt_utils_unix.read_file filename in
              return @@ String.split_on_char ' ' (String.trim data))
            (fun exn ->
              failwith "cannot read file (%s)" (Printexc.to_string exn)) );
      ("data", fun data -> return [data]);
    ]
    data_for_file

let chunker_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group:Groups.kernel
    ~desc:
      "Chunk hexadecimal data according to the message representation of the \
       EVM rollup."
    (args8
       rollup_address_arg
       blueprint_mode_arg
       timestamp_arg
       blueprint_number_arg
       parent_hash_arg
       sequencer_key_arg
       wallet_dir_arg
       (Client_config.password_filename_arg ()))
    (prefixes ["chunk"; "data"]
    @@ seq_of_param
    @@ param
         ~name:"data or file"
         ~desc:
           "Data to prepare and chunk with the EVM rollup format. If the data \
            is prefixed with `file:`, the content is read from the given \
            filename and can contain a list of data separated by a whitespace."
         (Tezos_clic.parameter (fun _ -> from_data_or_file)))
    (fun ( rollup_address,
           as_blueprint,
           blueprint_timestamp,
           blueprint_number,
           blueprint_parent_hash,
           sequencer_str,
           wallet_dir,
           password_filename )
         data
         () ->
      let* kind =
        if as_blueprint then
          let*! sequencer_str =
            match sequencer_str with
            | Some k -> Lwt.return k
            | None -> Lwt.fail_with "missing sequencer key"
          in
          let wallet_ctxt = register_wallet ?password_filename ~wallet_dir () in
          let+ sequencer_key =
            Client_keys.Secret_key.parse_source_string wallet_ctxt sequencer_str
          in
          `Blueprint
            ( wallet_ctxt,
              sequencer_key,
              blueprint_timestamp,
              blueprint_number,
              blueprint_parent_hash )
        else return `Transaction
      in
      let data = List.flatten data in
      let print_chunks smart_rollup_address data =
        let* messages = make_dev_messages ~kind ~smart_rollup_address data in
        Format.printf "Chunked transactions :\n%!" ;
        List.iter (Format.printf "%s\n%!") messages ;
        return_unit
      in
      let rollup_address =
        Tezos_crypto.Hashed.Smart_rollup_address.to_string rollup_address
      in
      print_chunks rollup_address data)

let make_upgrade_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group:Groups.kernel
    ~desc:"Create bytes payload for the upgrade entrypoint."
    no_options
    (prefixes ["make"; "upgrade"; "payload"; "with"; "root"; "hash"]
    @@ param
         ~name:"preimage_hash"
         ~desc:"Root hash of the kernel to upgrade to."
         Params.string
    @@ prefixes ["at"; "activation"; "timestamp"]
    @@ param
         ~name:"activation_timestamp"
         ~desc:
           "After activation timestamp, the kernel will upgrade to this value."
         Params.timestamp
    @@ stop)
    (fun () root_hash timestamp () ->
      let payload =
        Evm_node_lib_dev_encoding.Evm_events.Upgrade.(
          to_bytes @@ {hash = Hash (Hex root_hash); timestamp})
      in
      Printf.printf "%s%!" Hex.(of_bytes payload |> show) ;
      return_unit)

let make_sequencer_upgrade_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group:Groups.kernel
    ~desc:"Create bytes payload for the sequencer upgrade entrypoint."
    (args1 wallet_dir_arg)
    (prefixes ["make"; "sequencer"; "upgrade"; "payload"]
    @@ prefixes ["with"; "pool"; "address"]
    @@ Tezos_clic.param
         ~name:"pool_address"
         ~desc:"Pool address of the sequencer"
         Params.eth_address
    @@ prefixes ["at"; "activation"; "timestamp"]
    @@ param
         ~name:"activation_timestamp"
         ~desc:
           "After activation timestamp, the kernel will upgrade to this value."
         Params.timestamp
    @@ prefix "for" @@ Params.sequencer_key @@ stop)
    (fun wallet_dir pool_address activation_timestamp sequencer_str () ->
      let wallet_ctxt = register_wallet ~wallet_dir () in
      let* _pk_uri, sequencer_sk_opt =
        Client_keys.Public_key.parse_source_string wallet_ctxt sequencer_str
      in
      let*? sequencer =
        Option.to_result
          ~none:[error_of_fmt "invalid format or unknown public key."]
          sequencer_sk_opt
      in
      let* payload =
        let open Evm_node_lib_dev_encoding.Evm_events in
        let sequencer_upgrade : Sequencer_upgrade.t =
          {sequencer; pool_address; timestamp = activation_timestamp}
        in
        return @@ Sequencer_upgrade.to_bytes sequencer_upgrade
      in
      Printf.printf "%s%!" Hex.(of_bytes payload |> show) ;
      return_unit)

let init_from_rollup_node_command =
  let open Tezos_clic in
  let rollup_node_data_dir_param =
    Tezos_clic.param
      ~name:"rollup-node-data-dir"
      ~desc:(Format.sprintf "The path to the rollup node data directory.")
      Params.string
  in
  command
    ~group:Groups.run
    ~desc:
      "Initialises the EVM node data-dir using the data-dir of a rollup node."
    (args3 data_dir_arg config_path_arg omit_delayed_tx_events_arg)
    (prefixes ["init"; "from"; "rollup"; "node"]
    @@ rollup_node_data_dir_param @@ stop)
    (fun (data_dir, config_file, omit_delayed_tx_events) rollup_node_data_dir () ->
      let open Lwt_result_syntax in
      let config_file = config_filename ~data_dir config_file in
      let* configuration = Cli.create_or_read_config ~data_dir config_file in
      let*! () = init_logs ~daily_logs:false ~data_dir configuration in
      Evm_node_lib_dev.Evm_context.init_from_rollup_node
        ~configuration
        ~omit_delayed_tx_events
        ~data_dir
        ~rollup_node_data_dir
        ~tx_container:(module Evm_node_lib_dev.Tx_queue.Tx_container)
        ())

let dump_to_rlp_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group:Groups.debug
    ~desc:"Transforms the JSON list of instructions to a RLP list."
    (args1 keep_everything_arg)
    (prefixes ["transform"; "dump"]
    @@ param ~name:"dump.json" ~desc:"Description." Params.string
    @@ prefixes ["to"; "rlp"]
    @@ param ~name:"dump.rlp" ~desc:"Description." Params.string
    @@ stop)
    (fun keep_everything dump_json dump_rlp () ->
      let* dump_json = Lwt_utils_unix.Json.read_file dump_json in
      let config =
        Data_encoding.Json.destruct
          Octez_smart_rollup.Installer_config.encoding
          dump_json
      in

      let bytes =
        let aux =
          let open Evm_node_lib_dev_encoding.Rlp in
          if keep_everything then
            fun acc Octez_smart_rollup.Installer_config.(Set {value; to_}) ->
            List [Value (String.to_bytes to_); Value (String.to_bytes value)]
            :: acc
          else fun acc Octez_smart_rollup.Installer_config.(Set {value; to_}) ->
            if String.starts_with ~prefix:"/evm" to_ then
              List [Value (String.to_bytes to_); Value (String.to_bytes value)]
              :: acc
            else acc
        in
        let open Evm_node_lib_dev_encoding.Rlp in
        List.fold_left aux [] config |> fun l -> encode (List l)
      in

      let write_bytes_to_file filename bytes =
        let oc = open_out filename in
        output_bytes oc bytes ;
        close_out oc
      in

      write_bytes_to_file dump_rlp bytes ;

      return_unit)

let reset_command =
  let open Tezos_clic in
  command
    ~group:Groups.debug
    ~desc:"Reset evm node data-dir to a specific block level."
    (args2
       data_dir_arg
       (force_arg
          ~doc:
            "Force suppression of data to reset state of sequencer to a \
             specified l2 level."))
    (prefixes ["reset"; "at"]
    @@ Tezos_clic.param
         ~name:"level"
         ~desc:"Level to reset to state to."
         (Tezos_clic.parameter (fun () s ->
              Lwt.return_ok
              @@ Evm_node_lib_dev_encoding.Ethereum_types.Qty (Z.of_string s)))
    @@ stop)
    (fun (data_dir, force) l2_level () ->
      if force then Evm_node_lib_dev.Evm_context.reset ~data_dir ~l2_level
      else
        failwith
          "You must provide the `--force` switch in order to use this command \
           and not accidentally delete data.")

let replay_args =
  Tezos_clic.args9
    data_dir_arg
    config_path_arg
    preimages_arg
    preimages_endpoint_arg
    native_execution_policy_arg
    (kernel_arg ())
    kernel_verbosity_arg
    profile_arg
    disable_da_fees_arg

let replay_many_command =
  let open Tezos_clic in
  command
    ~group:Groups.debug
    ~desc:"Replay a specific block level."
    replay_args
    (prefixes ["replay"; "blueprint"]
    @@ Tezos_clic.param ~name:"level" ~desc:"Level to replay." Params.l2_level
    @@ stop)
    (fun ( data_dir,
           config_file,
           preimages,
           preimages_endpoint,
           native_execution_policy,
           kernel,
           kernel_verbosity,
           profile,
           disable_da_fees )
         l2_level
         () ->
      let open Lwt_result_syntax in
      let config_file = config_filename ~data_dir config_file in
      let* configuration =
        Cli.create_or_read_config
          ~data_dir
          ?preimages
          ?preimages_endpoint
          ?native_execution_policy
          config_file
      in
      let*! () = init_logs ~daily_logs:false ~data_dir configuration in
      Evm_node_lib_dev.Replay.main
        ~disable_da_fees
        ?kernel
        ?kernel_verbosity
        ~data_dir
        ~number:l2_level
        ?profile
        configuration)

let replay_command =
  let open Tezos_clic in
  command
    ~group:Groups.debug
    ~desc:"Replay a range of block levels."
    replay_args
    (prefixes ["replay"; "blueprints"; "from"]
    @@ Tezos_clic.param
         ~name:"level"
         ~desc:"First block to replay."
         Params.l2_level
    @@ prefix "to"
    @@ Tezos_clic.param
         ~name:"level"
         ~desc:"Last block to replay."
         Params.l2_level
    @@ stop)
    (fun ( data_dir,
           config_file,
           preimages,
           preimages_endpoint,
           native_execution_policy,
           kernel,
           kernel_verbosity,
           profile,
           disable_da_fees )
         l2_level
         upto
         () ->
      let open Lwt_result_syntax in
      let config_file = config_filename ~data_dir config_file in
      let* configuration =
        Cli.create_or_read_config
          ~data_dir
          ?preimages
          ?preimages_endpoint
          ?native_execution_policy
          config_file
      in
      let*! () = init_logs ~daily_logs:false ~data_dir configuration in
      Evm_node_lib_dev.Replay.main
        ~disable_da_fees
        ?kernel
        ?kernel_verbosity
        ~data_dir
        ~number:l2_level
        ?profile
        ~upto
        configuration)

let patch_kernel_command =
  let open Tezos_clic in
  command
    ~group:Groups.kernel
    ~desc:
      "Patch the kernel used by the EVM node from its current HEAD. This is an \
       unsafe command, which can lead to the EVM node diverging from the \
       Etherlink main branch if the new kernel is not compatible with the one \
       deployed on the network."
    (args4
       data_dir_arg
       config_path_arg
       (block_number_arg
          ~doc:
            "If provided, the state resulting in the application of the \
             requested block will be used instead of the latest state. In that \
             case, the patch will only impact replays of the successor block \
             only.")
       (force_arg ~doc:"Force patching the kernel."))
    (prefixes ["patch"; "kernel"; "with"]
    @@ Tezos_clic.string ~name:"kernel_path" ~desc:"Path to the kernel."
    @@ stop)
    (fun (data_dir, config_file, block_number, force) kernel_path () ->
      let open Lwt_result_syntax in
      let open Evm_node_lib_dev in
      let config_file = config_filename ~data_dir config_file in
      let* configuration = Cli.create_or_read_config ~data_dir config_file in
      let*! () = init_logs ~daily_logs:false ~data_dir configuration in
      (* We remove the [observer] configuration. This [patch] should not need
         to interact with an upstream EVM node. *)
      let configuration = {configuration with observer = None} in
      if force then
        let* _status =
          Evm_context.start
            ~configuration
            ~data_dir
            ~store_perm:`Read_write
            ~tx_container:(module Evm_node_lib_dev.Tx_queue.Tx_container)
            ()
        in
        Evm_context.patch_kernel ?block_number (On_disk kernel_path)
      else
        failwith
          "You must add --force to your command-line to execute this command. \
           As a reminder, patching the kernel is an advanced and unsafe \
           procedure.")

let describe_config_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group:Groups.config
    ~desc:
      {|Prints the JSON schema of the configuration file to the standard output.|}
    no_options
    (prefixes ["describe"; "config"] @@ stop)
    (fun () () ->
      Configuration.describe () ;
      return_unit)

let init_config_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group:Groups.config
    ~desc:
      "Create an initial config with default value.\n\
       If the <rollup-node-endpoint> is set then adds the configuration for \
       the proxy mode. If the  <sequencer-key> is set,then adds the \
       configuration for the sequencer and threshold encryption sequencer \
       modes. If the <evm-node-endpoint> is set then adds the configuration \
       for the observer mode."
    (merge_options
       common_config_args
       (args20
          (* sequencer and observer config*)
          preimages_arg
          preimages_endpoint_arg
          native_execution_policy_arg
          time_between_blocks_arg
          max_number_of_chunks_arg
          private_rpc_port_arg
          sequencer_key_arg
          maximum_blueprints_lag_arg
          maximum_blueprints_ahead_arg
          maximum_blueprints_catchup_arg
          catchup_cooldown_arg
          evm_node_endpoint_arg
          bundler_node_endpoint_arg
          sequencer_sidecar_endpoint_arg
          history_arg
          (* others option *)
          dont_track_rollup_node_arg
          wallet_dir_arg
          (Tezos_clic.switch
             ~long:"force"
             ~short:'f'
             ~doc:"Overwrites the configuration file when it exists."
             ())
          dal_slots_arg
          (supported_network_arg
             ~why:
               "If set, some configuration options defaults to well-known \
                values for the selected network."
             ())))
    (prefixes ["init"; "config"] @@ stop)
    (fun ( ( data_dir,
             config_file,
             rpc_addr,
             rpc_port,
             rpc_batch_limit,
             cors_origins,
             cors_headers,
             log_filter_max_nb_blocks,
             log_filter_max_nb_logs,
             log_filter_chunk_size,
             keep_alive,
             rollup_node_endpoint,
             tx_pool_timeout_limit,
             tx_pool_addr_limit,
             tx_pool_tx_per_addr_limit,
             verbose,
             restricted_rpcs,
             blacklisted_rpcs,
             whitelisted_rpcs,
             finalized_view ),
           ( preimages,
             preimages_endpoint,
             native_execution_policy,
             time_between_blocks,
             max_number_of_chunks,
             private_rpc_port,
             sequencer_str,
             max_blueprints_lag,
             max_blueprints_ahead,
             max_blueprints_catchup,
             catchup_cooldown,
             evm_node_endpoint,
             threshold_encryption_bundler_endpoint,
             sequencer_sidecar_endpoint,
             history_mode,
             dont_track_rollup_node,
             wallet_dir,
             force,
             dal_slots,
             network ) )
         () ->
      let config_file = config_filename ~data_dir config_file in
      Evm_node_lib_dev.Data_dir.use ~data_dir @@ fun () ->
      let* restricted_rpcs =
        pick_restricted_rpcs restricted_rpcs whitelisted_rpcs blacklisted_rpcs
      in
      let* sequencer_key =
        Option.map_es
          (fun str ->
            let wallet_ctxt = register_wallet ~wallet_dir () in
            Client_keys.Secret_key.parse_source_string wallet_ctxt str)
          sequencer_str
      in
      let* config =
        Cli.create_or_read_config
          ~data_dir
          ?rpc_addr
          ?rpc_port
          ?rpc_batch_limit
          ?cors_origins
          ?cors_headers
          ?log_filter_max_nb_blocks
          ?log_filter_max_nb_logs
          ?log_filter_chunk_size
          ~keep_alive
          ?rollup_node_endpoint
          ?dont_track_rollup_node:
            (* If `dont_track_rollup_node` is false, it means the argument was
               omitted from the command-line. As a consequence, we default to
               the config value by passing [None]. *)
            (if dont_track_rollup_node then Some true else None)
          ?tx_pool_timeout_limit
          ?tx_pool_addr_limit
          ?tx_pool_tx_per_addr_limit
          ?preimages
          ?preimages_endpoint
          ?native_execution_policy
          ?time_between_blocks
          ?max_number_of_chunks
          ?private_rpc_port
          ?sequencer_key
          ?evm_node_endpoint
          ?threshold_encryption_bundler_endpoint
          ?sequencer_sidecar_endpoint
          ?max_blueprints_lag
          ?max_blueprints_ahead
          ?max_blueprints_catchup
          ?catchup_cooldown
          ?restricted_rpcs
          ~verbose
          ?dal_slots
          ~finalized_view
          ?network
          ?history_mode
          config_file
      in
      let*! () = init_logs ~daily_logs:false ~data_dir config in
      let* () = websocket_checks config in
      Configuration.save ~force ~data_dir config config_file)

let check_config_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group:Groups.config
    ~desc:
      "Read and validate configuration files. By default, look for config.json \
       in the data-dir. If --filename is used, check this config file instead."
    (args4
       data_dir_arg
       config_path_arg
       print_config_arg
       (supported_network_arg
          ~why:
            "If set, check as if `--network N` is set when running the node \
             (as it turns some required configuration fields into optional \
             ones)."
          ()))
    (prefixes ["check"; "config"] @@ stop)
    (fun (data_dir, config_file, print_config, network) () ->
      let config_file = config_filename ~data_dir config_file in
      let* config = load_file ?network ~data_dir config_file in
      let*! () = init_logs ~daily_logs:false ~data_dir config in
      let* () = websocket_checks config in
      if print_config then
        Format.printf "%a\n" (Configuration.pp_print_json ~data_dir) config
      else Format.printf "Configuration has been parsed successfully.\n" ;
      return_unit)

let config_key_arg ~name ~placeholder =
  let open Lwt_result_syntax in
  let long = String.mapi (fun _ c -> if c = '_' then '-' else c) name in
  let doc = Format.sprintf "Value for %s in the installer config." name in
  Tezos_clic.arg ~long ~doc ~placeholder
  @@ Tezos_clic.parameter (fun _ s -> return (name, s))

let config_key_flag ~name =
  let open Lwt_result_syntax in
  let long = String.mapi (fun _ c -> if c = '_' then '-' else c) name in
  let doc = Format.sprintf "Enable flag %s in the installer config." name in
  Tezos_clic.map_arg ~f:(fun _ enable ->
      if enable then return_some (name, "") else return_none)
  @@ Tezos_clic.switch ~long ~doc ()

let eth_bootstrap_account_arg =
  let long = "eth-bootstrap-account" in
  let doc =
    Format.sprintf "Add an etherlink bootstrap account in the installer config."
  in
  Tezos_clic.multiple_arg ~long ~doc ~placeholder:"0x..." Params.eth_address

let tez_bootstrap_account_arg =
  let long = "tez-bootstrap-account" in
  let doc =
    Format.sprintf "Add a tezlink bootstrap account in the installer config."
  in
  Tezos_clic.multiple_arg ~long ~doc ~placeholder:"edp..." Params.tez_account

let eth_bootstrap_balance_arg =
  Tezos_clic.default_arg
    ~long:"eth-bootstrap-balance"
    ~doc:"Balance (in Wei) of the etherlink bootstrap accounts"
    ~default:"9999000000000000000000"
    ~placeholder:"9999000000000000000000"
  @@ Tezos_clic.parameter (fun _ s -> Lwt_result.return @@ Z.of_string s)

let tez_bootstrap_balance_arg =
  Tezos_clic.default_arg
    ~long:"tez-bootstrap-balance"
    ~doc:"Balance (in tez) of the tezlink bootstrap accounts"
    ~default:"3800000"
    ~placeholder:"3800000"
  @@ Tezos_clic.parameter (fun _ s ->
         Lwt.return @@ Error_monad.catch
         @@ fun () -> Evm_node_lib_dev_tezlink.Tezos_types.Tez.of_string_exn s)

let set_account_code =
  let long = "set-code" in
  let doc = Format.sprintf "Add code to an account in the installer config." in
  Tezos_clic.multiple_arg ~long ~doc ~placeholder:"0x...,0x...."
  @@ Tezos_clic.parameter (fun _ address_code ->
         let open Lwt_result_syntax in
         match String.split ',' address_code with
         | [address; code] ->
             let*? (`Hex address) =
               Evm_node_lib_dev.Misc.normalize_hex address
             in
             return (String.trim address, code)
         | _ -> failwith "Parsing error for set-code")

let evm_version_arg =
  let long = "evm-version" in
  let doc = Format.sprintf "Value for evm_version in the installer config." in
  Tezos_clic.arg ~long ~doc ~placeholder:"cancun|shanghai"
  @@ Tezos_clic.parameter (fun _ evm_version ->
         let open Lwt_result_syntax in
         match evm_version with
         | "shanghai" -> return Evm_node_lib_dev.Kernel_config.Shanghai
         | "cancun" -> return Evm_node_lib_dev.Kernel_config.Cancun
         | _ -> failwith "Parsing error for evm-version")

let make_l2_kernel_config_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  let open Evm_node_lib_dev_encoding.L2_types in
  command
    ~group:Groups.kernel
    ~desc:
      "Produce a file containing the part of the kernel configuration \
       instructions related to a particular L2 chain."
    (args12
       (config_key_arg ~name:"minimum_base_fee_per_gas" ~placeholder:"111...")
       (config_key_arg ~name:"da_fee_per_byte" ~placeholder:"111...")
       (config_key_arg ~name:"sequencer_pool_address" ~placeholder:"0x...")
       (config_key_arg
          ~name:"maximum_gas_per_transaction"
          ~placeholder:"30000...")
       eth_bootstrap_balance_arg
       eth_bootstrap_account_arg
       tez_bootstrap_balance_arg
       tez_bootstrap_account_arg
       set_account_code
       (config_key_arg
          ~name:"world_state_path"
          ~placeholder:"/evm/world_state/<chain_id>")
       (Tezos_clic.arg
          ~long:"l2-chain-id"
          ~doc:"L2 chain id."
          ~placeholder:"1"
          (Tezos_clic.parameter (fun _ s -> return @@ Chain_id.of_string_exn s)))
       (Tezos_clic.default_arg
          ~long:"l2-chain-family"
          ~doc:"Configure the family (either EVM or Michelson) of the L2 chain."
          ~default:"EVM"
          ~placeholder:"EVM"
          (Tezos_clic.parameter (fun _ s ->
               return @@ Chain_family.of_string_exn s))))
    (prefixes ["make"; "l2"; "kernel"; "installer"; "config"]
    @@ param
         ~name:"kernel config file"
         ~desc:"File path where the config will be written to."
         Params.string
    @@ stop)
    (fun ( minimum_base_fee_per_gas,
           da_fee_per_byte,
           sequencer_pool_address,
           maximum_gas_per_transaction,
           eth_bootstrap_balance,
           eth_bootstrap_accounts,
           tez_bootstrap_balance,
           tez_bootstrap_accounts,
           set_account_code,
           world_state_path,
           l2_chain_id,
           l2_chain_family )
         output
         () ->
      let* l2_chain_id =
        match l2_chain_id with
        | None ->
            failwith
              "Chain_id is mandatory when trying to setup an l2 chain, use \
               --l2-chain-id to set it."
        | Some l2_chain_id -> return (Chain_id.to_string l2_chain_id)
      in
      Evm_node_lib_dev.Kernel_config.make_l2
        ~eth_bootstrap_balance
        ~tez_bootstrap_balance
        ?eth_bootstrap_accounts
        ?tez_bootstrap_accounts
        ?minimum_base_fee_per_gas
        ?da_fee_per_byte
        ?sequencer_pool_address
        ?maximum_gas_per_transaction
        ?set_account_code
        ?world_state_path
        ~l2_chain_id
        ~l2_chain_family
        ~output
        ())

let l2_chain_ids_arg =
  let open Evm_node_lib_dev_encoding in
  Tezos_clic.multiple_arg
    ~long:"l2-chain-id"
    ~doc:"Specify one of the chain ids in the kernel, can be used several times"
    ~placeholder:"1"
  @@ Tezos_clic.parameter (fun _ chain_id ->
         Lwt.return_ok @@ L2_types.Chain_id.of_string_exn chain_id)

let make_kernel_config_command =
  let open Tezos_clic in
  command
    ~group:Groups.kernel
    ~desc:"Create a configuration for the kernel installer."
    (merge_options
       (args23
          mainnet_compat_arg
          (config_key_arg ~name:"kernel_root_hash" ~placeholder:"root hash")
          (config_key_arg ~name:"chain_id" ~placeholder:"chain id")
          (config_key_arg ~name:"sequencer" ~placeholder:"edpk...")
          (config_key_arg ~name:"delayed_bridge" ~placeholder:"KT1...")
          (config_key_arg ~name:"ticketer" ~placeholder:"KT1...")
          (config_key_arg ~name:"admin" ~placeholder:"KT1..")
          (config_key_arg ~name:"sequencer_governance" ~placeholder:"KT1...")
          (config_key_arg ~name:"kernel_governance" ~placeholder:"KT1...")
          (config_key_arg
             ~name:"kernel_security_governance"
             ~placeholder:"KT1...")
          (config_key_arg
             ~name:"minimum_base_fee_per_gas"
             ~placeholder:"111...")
          (config_key_arg ~name:"da_fee_per_byte" ~placeholder:"111...")
          (config_key_arg ~name:"delayed_inbox_timeout" ~placeholder:"111...")
          (config_key_arg
             ~name:"delayed_inbox_min_levels"
             ~placeholder:"111...")
          (config_key_arg ~name:"sequencer_pool_address" ~placeholder:"0x...")
          (config_key_arg ~name:"maximum_allowed_ticks" ~placeholder:"11000...")
          (config_key_arg
             ~name:"maximum_gas_per_transaction"
             ~placeholder:"30000...")
          (config_key_arg
             ~name:"max_blueprint_lookahead_in_seconds"
             ~placeholder:"500")
          (config_key_flag ~name:"remove_whitelist")
          eth_bootstrap_balance_arg
          eth_bootstrap_account_arg
          (config_key_flag ~name:"enable_fa_bridge")
          (config_key_arg ~name:"dal_slots" ~placeholder:"0,1,4,6,..."))
       (args8
          (config_key_flag ~name:"enable_dal")
          (config_key_flag ~name:"enable_multichain")
          l2_chain_ids_arg
          (config_key_arg
             ~name:"max_delayed_inbox_blueprint_length"
             ~placeholder:"1000")
          (config_key_flag ~name:"enable_fast_withdrawal")
          (config_key_flag ~name:"enable_fast_fa_withdrawal")
          evm_version_arg
          set_account_code))
    (prefixes ["make"; "kernel"; "installer"; "config"]
    @@ param
         ~name:"kernel config file"
         ~desc:"File path where the config will be written to."
         Params.string
    @@ stop)
    (fun ( ( mainnet_compat,
             kernel_root_hash,
             chain_id,
             sequencer,
             delayed_bridge,
             ticketer,
             admin,
             sequencer_governance,
             kernel_governance,
             kernel_security_governance,
             minimum_base_fee_per_gas,
             da_fee_per_byte,
             delayed_inbox_timeout,
             delayed_inbox_min_levels,
             sequencer_pool_address,
             maximum_allowed_ticks,
             maximum_gas_per_transaction,
             max_blueprint_lookahead_in_seconds,
             remove_whitelist,
             eth_bootstrap_balance,
             eth_bootstrap_accounts,
             enable_fa_bridge,
             dal_slots ),
           ( enable_dal,
             enable_multichain,
             l2_chain_ids,
             max_delayed_inbox_blueprint_length,
             enable_fast_withdrawal,
             enable_fast_fa_withdrawal,
             evm_version,
             set_account_code ) )
         output
         () ->
      Evm_node_lib_dev.Kernel_config.make
        ~mainnet_compat
        ~eth_bootstrap_balance
        ?l2_chain_ids
        ?kernel_root_hash
        ?chain_id
        ?sequencer
        ?delayed_bridge
        ?ticketer
        ?admin
        ?sequencer_governance
        ?kernel_governance
        ?kernel_security_governance
        ?evm_version
        ?minimum_base_fee_per_gas
        ?da_fee_per_byte
        ?delayed_inbox_timeout
        ?delayed_inbox_min_levels
        ?sequencer_pool_address
        ?maximum_allowed_ticks
        ?maximum_gas_per_transaction
        ?max_blueprint_lookahead_in_seconds
        ?remove_whitelist
        ?eth_bootstrap_accounts
        ?enable_fa_bridge
        ?enable_dal
        ?dal_slots
        ?enable_multichain
        ?set_account_code
        ?max_delayed_inbox_blueprint_length
        ?enable_fast_withdrawal
        ?enable_fast_fa_withdrawal
        ~output
        ())

let proxy_command =
  let open Tezos_clic in
  command
    ~group:Groups.run
    ~desc:"Start the EVM node in proxy mode."
    (merge_options
       common_config_args
       (args3 read_only_arg ignore_block_param_arg evm_node_endpoint_arg))
    (prefixes ["run"; "proxy"] @@ stop)
    (fun ( ( data_dir,
             config_file,
             rpc_addr,
             rpc_port,
             rpc_batch_limit,
             cors_origins,
             cors_headers,
             log_filter_max_nb_blocks,
             log_filter_max_nb_logs,
             log_filter_chunk_size,
             keep_alive,
             rollup_node_endpoint,
             tx_pool_timeout_limit,
             tx_pool_addr_limit,
             tx_pool_tx_per_addr_limit,
             verbose,
             restricted_rpcs,
             blacklisted_rpcs,
             whitelisted_rpcs,
             finalized_view ),
           (read_only, ignore_block_param, evm_node_endpoint) )
         () ->
      let open Lwt_result_syntax in
      let* restricted_rpcs =
        pick_restricted_rpcs restricted_rpcs whitelisted_rpcs blacklisted_rpcs
      in
      start_proxy
        ~data_dir
        ~config_file
        ~keep_alive
        ?rpc_addr
        ?rpc_port
        ?rpc_batch_limit
        ?cors_origins
        ?cors_headers
        ?log_filter_max_nb_blocks
        ?log_filter_max_nb_logs
        ?log_filter_chunk_size
        ?rollup_node_endpoint
        ?evm_node_endpoint
        ?tx_pool_timeout_limit
        ?tx_pool_addr_limit
        ?tx_pool_tx_per_addr_limit
        ?restricted_rpcs
        ~verbose
        ~read_only
        ~finalized_view
        ~ignore_block_param
        ())

let sequencer_config_args =
  Tezos_clic.args15
    preimages_arg
    preimages_endpoint_arg
    time_between_blocks_arg
    max_number_of_chunks_arg
    private_rpc_port_arg
    sequencer_key_arg
    maximum_blueprints_lag_arg
    maximum_blueprints_ahead_arg
    maximum_blueprints_catchup_arg
    catchup_cooldown_arg
    genesis_timestamp_arg
    initial_kernel_arg
    wallet_dir_arg
    (Client_config.password_filename_arg ())
    dal_slots_arg

let fund_arg =
  let long = "fund" in
  let doc =
    "The address of an account to provide with funds in the sandbox (can be \
     repeated to fund multiple accounts)."
  in
  Tezos_clic.multiple_arg ~long ~doc ~placeholder:"0x..." Params.eth_address

let sandbox_config_args =
  Tezos_clic.args16
    preimages_arg
    preimages_endpoint_arg
    native_execution_policy_arg
    time_between_blocks_arg
    max_number_of_chunks_arg
    private_rpc_port_arg
    genesis_timestamp_arg
    (kernel_arg ())
    wallet_dir_arg
    (Client_config.password_filename_arg ())
    (supported_network_arg ())
    init_from_snapshot_arg
    fund_arg
    replicate_arg
    disable_da_fees_arg
    kernel_verbosity_arg

let sequencer_command =
  let open Tezos_clic in
  command
    ~group:Groups.run
    ~desc:"Start the EVM node in sequencer mode."
    (merge_options common_config_args sequencer_config_args)
    (prefixes ["run"; "sequencer"] stop)
    (fun ( ( data_dir,
             config_file,
             rpc_addr,
             rpc_port,
             rpc_batch_limit,
             cors_origins,
             cors_headers,
             log_filter_max_nb_blocks,
             log_filter_max_nb_logs,
             log_filter_chunk_size,
             keep_alive,
             rollup_node_endpoint,
             tx_pool_timeout_limit,
             tx_pool_addr_limit,
             tx_pool_tx_per_addr_limit,
             verbose,
             restricted_rpcs,
             blacklisted_rpcs,
             whitelisted_rpcs,
             finalized_view ),
           ( preimages,
             preimages_endpoint,
             time_between_blocks,
             max_number_of_chunks,
             private_rpc_port,
             sequencer_str,
             max_blueprints_lag,
             max_blueprints_ahead,
             max_blueprints_catchup,
             catchup_cooldown,
             genesis_timestamp,
             kernel,
             wallet_dir,
             password_filename,
             dal_slots ) )
         () ->
      let open Lwt_result_syntax in
      let* restricted_rpcs =
        pick_restricted_rpcs restricted_rpcs whitelisted_rpcs blacklisted_rpcs
      in
      let config_file = config_filename ~data_dir config_file in
      start_sequencer
        ?password_filename
        ~wallet_dir
        ~data_dir
        ?rpc_addr
        ?rpc_port
        ?rpc_batch_limit
        ?cors_origins
        ?cors_headers
        ?tx_pool_timeout_limit
        ?tx_pool_addr_limit
        ?tx_pool_tx_per_addr_limit
        ~keep_alive
        ?rollup_node_endpoint
        ~verbose
        ?preimages
        ?preimages_endpoint
        ?time_between_blocks
        ?max_number_of_chunks
        ?private_rpc_port
        ?sequencer_str
        ?max_blueprints_lag
        ?max_blueprints_ahead
        ?max_blueprints_catchup
        ?catchup_cooldown
        ?log_filter_max_nb_blocks
        ?log_filter_max_nb_logs
        ?log_filter_chunk_size
        ?genesis_timestamp
        ?restricted_rpcs
        ?kernel
        ?dal_slots
        ~finalized_view
        config_file)

let sandbox_command =
  let open Tezos_clic in
  command
    ~group:Groups.run
    ~desc:
      "Start the EVM node in sandbox mode. The sandbox mode is a \
       sequencer-like mode that produces blocks with a fake key and no rollup \
       node connection."
    (merge_options common_config_args sandbox_config_args)
    (prefixes ["run"; "sandbox"] stop)
    (fun ( ( data_dir,
             config_file,
             rpc_addr,
             rpc_port,
             rpc_batch_limit,
             cors_origins,
             cors_headers,
             log_filter_max_nb_blocks,
             log_filter_max_nb_logs,
             log_filter_chunk_size,
             keep_alive,
             rollup_node_endpoint,
             tx_pool_timeout_limit,
             tx_pool_addr_limit,
             tx_pool_tx_per_addr_limit,
             verbose,
             restricted_rpcs,
             blacklisted_rpcs,
             whitelisted_rpcs,
             finalized_view ),
           ( preimages,
             preimages_endpoint,
             native_execution_policy,
             time_between_blocks,
             max_number_of_chunks,
             private_rpc_port,
             genesis_timestamp,
             kernel,
             wallet_dir,
             password_filename,
             network,
             init_from_snapshot,
             funded_addresses,
             main_endpoint,
             disable_da_fees,
             kernel_verbosity ) )
         () ->
      let open Lwt_result_syntax in
      let* restricted_rpcs =
        pick_restricted_rpcs restricted_rpcs whitelisted_rpcs blacklisted_rpcs
      in
      let _pkh, pk, sk =
        Tezos_crypto.Signature.(generate_key ~algo:Ed25519) ()
      in
      let rollup_node_endpoint =
        Option.value ~default:Uri.empty rollup_node_endpoint
      in
      let kernel = kernel_from_args network kernel in
      let* parent_chain =
        match (main_endpoint, network) with
        | Some (Some endpoint), _ -> return_some endpoint
        | Some None, Some network ->
            return_some
              (Uri.of_string (Configuration.observer_evm_node_endpoint network))
        | Some None, None ->
            failwith
              "Cannot infer which EVM node to use to fetch blueprints. Use \
               --network or add an endpoint as the argument of --replicate."
        | None, _ -> return_none
      in
      let sandbox_config =
        Evm_node_lib_dev.Sequencer.
          {
            public_key = pk;
            secret_key = sk;
            init_from_snapshot;
            network;
            funded_addresses = Option.value ~default:[] funded_addresses;
            parent_chain;
            disable_da_fees;
            kernel_verbosity;
          }
      in
      let config_file = config_filename ~data_dir config_file in
      start_sequencer
        ?password_filename
        ~wallet_dir
        ~data_dir
        ?rpc_addr
        ?rpc_port
        ?rpc_batch_limit
        ?cors_origins
        ?cors_headers
        ?tx_pool_timeout_limit
        ?tx_pool_addr_limit
        ?tx_pool_tx_per_addr_limit
        ~keep_alive
        ~rollup_node_endpoint
        ~verbose
        ?preimages
        ?preimages_endpoint
        ?native_execution_policy
        ?time_between_blocks
        ?max_number_of_chunks
        ?private_rpc_port
        ~max_blueprints_lag:100_000_000
        ~max_blueprints_ahead:100_000_000
        ~max_blueprints_catchup:100_000_000
        ~catchup_cooldown:100_000_000
        ?log_filter_max_nb_blocks
        ?log_filter_max_nb_logs
        ?log_filter_chunk_size
        ?genesis_timestamp
        ?restricted_rpcs
        ?kernel
        ~sandbox_config
        ~finalized_view
        config_file)

let observer_run_args =
  Tezos_clic.args12
    private_rpc_port_arg
    evm_node_endpoint_arg
    bundler_node_endpoint_arg
    preimages_arg
    preimages_endpoint_arg
    native_execution_policy_arg
    initial_kernel_arg
    dont_track_rollup_node_arg
    no_sync_arg
    init_from_snapshot_arg
    history_arg
    (supported_network_arg
       ~why:
         "If set, additional sanity checks are performed on the node’s startup."
       ())

let observer_command =
  let open Tezos_clic in
  command
    ~group:Groups.run
    ~desc:"Start the EVM node in observer mode."
    (merge_options common_config_args observer_run_args)
    (prefixes ["run"; "observer"] stop)
    (fun ( ( data_dir,
             config_file,
             rpc_addr,
             rpc_port,
             rpc_batch_limit,
             cors_origins,
             cors_headers,
             log_filter_max_nb_blocks,
             log_filter_max_nb_logs,
             log_filter_chunk_size,
             keep_alive,
             rollup_node_endpoint,
             tx_pool_timeout_limit,
             tx_pool_addr_limit,
             tx_pool_tx_per_addr_limit,
             verbose,
             restricted_rpcs,
             blacklisted_rpcs,
             whitelisted_rpcs,
             finalized_view ),
           ( private_rpc_port,
             evm_node_endpoint,
             threshold_encryption_bundler_endpoint,
             preimages,
             preimages_endpoint,
             native_execution_policy,
             kernel,
             dont_track_rollup_node,
             no_sync,
             init_from_snapshot,
             history_mode,
             network ) )
         () ->
      let open Lwt_result_syntax in
      let config_file = config_filename ~data_dir config_file in
      let* restricted_rpcs =
        pick_restricted_rpcs restricted_rpcs whitelisted_rpcs blacklisted_rpcs
      in
      let kernel = kernel_from_args network kernel in
      start_observer
        ~data_dir
        ~keep_alive
        ?rpc_addr
        ?rpc_port
        ?rpc_batch_limit
        ?cors_origins
        ?cors_headers
        ?private_rpc_port
        ~verbose
        ?preimages
        ?preimages_endpoint
        ?native_execution_policy
        ?rollup_node_endpoint
        ~dont_track_rollup_node
        ?evm_node_endpoint
        ?threshold_encryption_bundler_endpoint
        ?tx_pool_timeout_limit
        ?tx_pool_addr_limit
        ?tx_pool_tx_per_addr_limit
        ?log_filter_chunk_size
        ?log_filter_max_nb_logs
        ?log_filter_max_nb_blocks
        ?restricted_rpcs
        ?kernel
        ~no_sync
        ~init_from_snapshot
        ?history_mode
        ~finalized_view
        ?network
        config_file)

let export_snapshot
    (data_dir, config_file, snapshot_file, compress_on_the_fly, uncompressed) =
  let open Lwt_result_syntax in
  let open Evm_node_lib_dev.Snapshots in
  let config_file = config_filename ~data_dir config_file in
  let* configuration =
    Configuration.Cli.create_or_read_config ~data_dir config_file
  in
  let*! () = init_logs ~daily_logs:false ~data_dir configuration in

  let compression =
    match (compress_on_the_fly, uncompressed) with
    | true, true ->
        Format.eprintf
          "Cannot have both --uncompressed and --compress-on-the-fly" ;
        exit 1
    | true, false -> On_the_fly
    | false, false -> After
    | false, true -> No
  in
  let* snapshot_file = export ?snapshot_file ~compression ~data_dir () in
  Format.printf "Snapshot exported to %s@." snapshot_file ;
  return_unit

let export_snapshot_command =
  let open Tezos_clic in
  command
    ~group:Groups.snapshot
    ~desc:"Export a snapshot of the EVM node."
    (args5
       data_dir_arg
       config_path_arg
       snapshot_file_arg
       compress_on_the_fly_arg
       uncompressed)
    (prefixes ["snapshot"; "export"] @@ stop)
    (fun params () -> export_snapshot params)

let import_snapshot_command =
  let open Tezos_clic in
  command
    ~group:Groups.snapshot
    ~desc:"Import a snapshot of the EVM node."
    (args2
       data_dir_arg
       (force_arg
          ~doc:
            "Allow importing snapshot in already populated data dir (previous \
             contents is removed first, even if the snapshot is corrupted), or \
             importing a legacy snapshot in an empty data dir."))
    (prefixes ["snapshot"; "import"] @@ Params.snapshot_file_or_url @@ stop)
    (fun (data_dir, force) snapshot_file () ->
      let open Lwt_result_syntax in
      let* _ =
        Evm_node_lib_dev.Snapshots.import_from
          ~force
          ~data_dir
          ~snapshot_file
          ()
      in
      return_unit)

let snapshot_info_command =
  let open Tezos_clic in
  command
    ~group:Groups.snapshot
    ~desc:"Display information about an EVM node snapshot file."
    no_options
    (prefixes ["snapshot"; "info"] @@ Params.snapshot_file_or_url @@ stop)
    (fun () snapshot_file () ->
      let open Lwt_result_syntax in
      let open Evm_node_lib_dev in
      let* header, compressed = Snapshots.info ~snapshot_file in
      let rollup_address, current_level, legacy_block_storage, history_info =
        match header with
        | V0_legacy {rollup_address; current_level} ->
            (rollup_address, current_level, true, None)
        | V1 {rollup_address; current_level; history_mode; first_level} ->
            ( rollup_address,
              current_level,
              false,
              Some (history_mode, first_level) )
      in
      let pp_rollup fmt () =
        match
          List.find_opt
            (fun network ->
              Octez_smart_rollup.Address.(
                rollup_address = Constants.rollup_address network))
            Constants.supported_networks
        with
        | Some net ->
            Format.fprintf
              fmt
              "Etherlink:       %s@,"
              (Constants.network_name net)
        | None -> ()
      in
      let pp_history fmt () =
        match history_info with
        | None -> ()
        | Some (history_mode, first_level) ->
            Format.fprintf
              fmt
              "@,History mode:    %s@,First level:     %a"
              (match history_mode with
              | Archive -> "Archive"
              | Rolling gc | Full gc ->
                  let hist_span =
                    Ptime.Span.of_int_s
                      (gc.split_frequency_in_seconds * gc.number_of_chunks)
                  in
                  Format.asprintf
                    "%a (with %a history)"
                    Configuration.pp_history_mode_info
                    history_mode
                    Ptime.Span.pp
                    hist_span)
              Evm_node_lib_dev_encoding.Ethereum_types.pp_quantity
              first_level
      in
      Format.printf
        "@[<v 0>Valid EVM node snapshot.@,\
         Format:          %scompressed@,\
         %aRollup address:  %a@,\
         Current level:   %a@,\
         Block storage:   %s%a@]@."
        (match compressed with `Compressed -> "" | `Uncompressed -> "un")
        pp_rollup
        ()
        Octez_smart_rollup.Address.pp
        rollup_address
        Evm_node_lib_dev_encoding.Ethereum_types.pp_quantity
        current_level
        (if legacy_block_storage then "Legacy" else "Sqlite3")
        pp_history
        () ;
      return_unit)

let switch_history_mode_command =
  let open Tezos_clic in
  command
    ~group:Groups.storage
    ~desc:"Switch history mode of the node."
    (args2 data_dir_arg config_path_arg)
    (prefixes ["switch"; "history"; "to"] @@ Params.history @@ stop)
    (fun (data_dir, config_file) history_mode () ->
      let open Lwt_result_syntax in
      let open Evm_node_lib_dev in
      let*! populated = Data_dir.populated ~data_dir in
      let*? () =
        if not populated then
          error_with
            "Data directory %S is empty or is not an EVM node data dir."
            data_dir
        else Ok ()
      in
      let config_file = config_filename ~data_dir config_file in
      let* store = Evm_store.init ~data_dir ~perm:`Read_write () in
      let* () =
        Evm_store.use store @@ fun conn ->
        let* config =
          Cli.create_or_read_config ~data_dir ~history_mode config_file
        in
        let*! () = init_logs ~daily_logs:false ~data_dir config in
        let* store_history_mode = Evm_store.Metadata.find_history_mode conn in

        let* history_mode =
          Evm_context.check_history_mode
            ~switch:true
            ~store_history_mode
            ~history_mode:config.history_mode
            ()
        in
        let* () = Evm_store.Metadata.store_history_mode conn history_mode in
        let*! config_exists = Lwt_unix.file_exists config_file in
        when_ config_exists @@ fun () ->
        (* Update configuration accordingly for nodes that already have one. *)
        Configuration.save ~force:true ~data_dir config config_file
      in
      let*! () = Evm_store.close store in
      return_unit)

let patch_state_command =
  let open Tezos_clic in
  let open Lwt_result_syntax in
  command
    ~group:Groups.debug
    ~desc:
      "Patches the state with an arbitrary value. This is an unsafe command, \
       it should be used for debugging only. Patched state is persisted and \
       you need to use the command `reset` to revert the changes."
    (args4
       data_dir_arg
       config_path_arg
       (block_number_arg
          ~doc:
            "If provided, the state resulting in the application of the \
             requested block will be used instead of the latest state.")
       (force_arg ~doc:"Force patching the state."))
    (prefixes ["patch"; "state"; "at"]
    @@ param ~name:"path" ~desc:"Durable storage path." Params.string
    @@ prefixes ["with"]
    @@ param ~name:"value" ~desc:"Patched value." Params.hex_string
    @@ stop)
    (fun (data_dir, config_file, block_number, force) key value () ->
      let open Evm_node_lib_dev in
      let config_file = config_filename ~data_dir config_file in
      let* configuration = Cli.create_or_read_config ~data_dir config_file in
      let*! () = init_logs ~daily_logs:false ~data_dir configuration in
      if force then
        (* We remove the [observer] configuration. This [patch] should not need
           to interact with an upstream EVM node. *)
        let configuration = {configuration with observer = None} in
        let* _status =
          Evm_context.start
            ~configuration
            ~data_dir
            ~store_perm:`Read_write
            ~tx_container:(module Evm_node_lib_dev.Tx_queue.Tx_container)
            ()
        in
        Evm_context.patch_state ?block_number ~key ~value ()
      else
        failwith
          "You must add --force to your command-line to execute this command. \
           As a reminder, patching the state is an advanced and unsafe \
           procedure.")

let preemptive_kernel_download_command =
  let open Tezos_clic in
  command
    ~group:Groups.kernel
    ~desc:"Preemptively download a kernel before running the EVM node."
    (args5
       data_dir_arg
       config_path_arg
       preimages_arg
       preimages_endpoint_arg
       num_download_retries)
    (prefixes ["download"; "kernel"]
    @@ param
         ~name:"kernel-id"
         ~desc:
           "Either a root hash of the kernel to download, or the name of a \
            supported kernel (\"bifrost\", \"calypso\", \"calypso2\" or \
            \"dionysus\")."
         (Tezos_clic.parameter (fun _ str ->
              let open Evm_node_lib_dev.Constants in
              let open Lwt_result_syntax in
              match kernel_from_string str with
              | Some kernel -> return (root_hash_from_kernel kernel)
              | None ->
                  trace
                    (error_of_fmt
                       "%s in neither a known kernel nor a valid root hash"
                       str)
                    (let open Lwt_result_syntax in
                     let*? hex = Evm_node_lib_dev.Misc.normalize_hex str in
                     return hex)))
    @@ stop)
    (fun ( data_dir,
           config_file,
           preimages,
           preimages_endpoint,
           num_download_retries )
         root_hash
         () ->
      let open Lwt_result_syntax in
      let config_file = config_filename ~data_dir config_file in
      let* configuration =
        Cli.create_or_read_config
          ~data_dir
          ?preimages
          ?preimages_endpoint
          config_file
      in
      let*! () = init_logs ~daily_logs:false ~data_dir configuration in
      let kernel_execution_config = configuration.kernel_execution in
      let*? preimages_endpoint =
        Option.either
          preimages_endpoint
          kernel_execution_config.preimages_endpoint
        |> Option.value_e ~error:[error_of_fmt "missing preimages endpoint."]
      in
      let preimages =
        Option.value preimages ~default:kernel_execution_config.preimages
      in
      let*! () = Lwt_utils_unix.create_dir preimages in
      Evm_node_lib_dev.Kernel_download.download
        ~root_hash
        ~preimages
        ~preimages_endpoint
        ?num_download_retries
        ())

let debug_print_store_schemas_command =
  let open Tezos_clic in
  command
    ~group:Groups.debug
    ~desc:"Print SQL statements describing the tables created in the store."
    no_options
    (prefixes ["debug"; "print"; "store"; "schemas"] @@ stop)
    (fun () () ->
      let open Lwt_result_syntax in
      let open Evm_node_lib_dev in
      Lwt_utils_unix.with_tempdir "store" @@ fun data_dir ->
      let* store = Evm_store.init ~data_dir ~perm:`Read_write () in
      let* schemas = Evm_store.(use store Schemas.get_all) in
      let output = String.concat ";\n\n" schemas in
      Format.printf "%s\n" output ;
      return_unit)

let list_metrics_command =
  let open Tezos_clic in
  command
    ~group:Groups.debug
    ~desc:"List the metrics exported by the EVM node."
    no_options
    (prefixes ["list"; "metrics"] @@ stop)
    (fun () () ->
      let open Lwt_result_syntax in
      let*! metrics = Evm_node_lib_dev.Metrics.listing () in
      Format.printf "%s\n" metrics ;
      return_unit)

let list_events_command =
  let open Tezos_clic in
  command
    ~group:Groups.debug
    ~desc:"List the events emitted by the EVM node."
    (args2 level_arg json_arg)
    (prefixes ["list"; "events"] stop)
    (fun (level, show_json) () ->
      let open Lwt_result_syntax in
      let prefix = Internal_event.Section.make_sanitized ["evm_node"] in
      let events =
        let filter Internal_event.Generic.(Definition (section, _, def)) =
          let module E = (val def) in
          match section with
          | Some section ->
              Internal_event.Section.is_prefix ~prefix section
              && Some E.level >= level
          | None -> false
        in
        Internal_event.All_definitions.get ~filter ()
        |> List.sort
             (fun
               (Internal_event.Generic.Definition (_, name1, _))
               (Internal_event.Generic.Definition (_, name2, _))
             -> String.compare name1 name2)
      in
      Format.printf
        "@[<v>%a@]@."
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
           (fun fmt
                Internal_event.Generic.(Definition (section, name, definition)) ->
             let module E = (val definition) in
             Format.fprintf
               fmt
               "@[<v 2>%s:@,description: %s@,level: %s@,%a@,%a@]"
               name
               E.doc
               (Internal_event.Level.to_string E.level)
               (Format.pp_print_option (fun fmt s ->
                    Format.fprintf fmt "section: %a" Internal_event.Section.pp s))
               section
               (fun fmt () ->
                 if show_json then
                   Format.fprintf
                     fmt
                     "@[<v 2>json format:@,@[<hov 2>%a@]@]@,"
                     Json_schema.pp
                     (Data_encoding.Json.schema E.encoding))
               ()))
        events ;
      return_unit)

(* List of commands not ready to be used by our end-users *)
let in_development_commands = []

(* List of program commands *)
let commands =
  [
    sandbox_command;
    proxy_command;
    sequencer_command;
    observer_command;
    rpc_command;
    init_config_command;
    snapshot_info_command;
    export_snapshot_command;
    import_snapshot_command;
    switch_history_mode_command;
    chunker_command;
    make_upgrade_command;
    make_sequencer_upgrade_command;
    init_from_rollup_node_command;
    reset_command;
    replay_command;
    replay_many_command;
    patch_kernel_command;
    check_config_command;
    describe_config_command;
    make_kernel_config_command;
    make_l2_kernel_config_command;
    preemptive_kernel_download_command;
    dump_to_rlp_command;
    patch_state_command;
    debug_print_store_schemas_command;
    list_metrics_command;
    list_events_command;
  ]

let global_options = Tezos_clic.no_options

let executable_name = Filename.basename Sys.executable_name

let argv () = Array.to_list Sys.argv |> List.tl |> Stdlib.Option.get

let dispatch args =
  let open Lwt_result_syntax in
  let commands =
    Tezos_clic.add_manual
      ~executable_name
      ~global_options
      (if Unix.isatty Unix.stdout then Tezos_clic.Ansi else Tezos_clic.Plain)
      Format.std_formatter
      commands
    @ in_development_commands
  in
  let* (), remaining_args =
    Tezos_clic.parse_global_options global_options () args
  in
  Tezos_clic.dispatch ~enable_argDefSwitch:true commands () remaining_args

let handle_error = function
  | Ok _ -> ()
  | Error [Tezos_clic.Version] ->
      let devmode =
        Tezos_version_value.Bin_version.octez_evm_node_version_string
      in
      Format.printf "%s\n" devmode ;
      exit 0
  | Error [Tezos_clic.Help command] ->
      Tezos_clic.usage
        Format.std_formatter
        ~executable_name
        ~global_options
        (match command with None -> [] | Some c -> [c]) ;
      Stdlib.exit 0
  | Error errs ->
      Tezos_clic.pp_cli_errors
        Format.err_formatter
        ~executable_name
        ~global_options
        ~default:Error_monad.pp
        errs ;
      Stdlib.exit 1

let () = Tezos_layer2_store.Snapshot_utils.add_download_command ()

let () =
  Random.self_init () ;
  ignore
    Tezos_clic.(
      setup_formatter
        ~isatty:(Unix.isatty Unix.stdout)
        Format.std_formatter
        Details) ;
  ignore
    Tezos_clic.(
      setup_formatter
        ~isatty:(Unix.isatty Unix.stderr)
        Format.err_formatter
        Details) ;
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  Tezos_base_unix.Event_loop.main_run (fun () ->
      Lwt_exit.wrap_and_exit (dispatch (argv ())))
  |> handle_error
