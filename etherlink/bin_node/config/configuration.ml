(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type supported_network = Mainnet | Testnet

let pp_supported_network fmt network =
  Format.pp_print_string
    fmt
    (match network with Mainnet -> "mainnet" | Testnet -> "testnet")

let strictly_positive_encoding = Data_encoding.ranged_int 1 ((1 lsl 30) - 1)

type log_filter_config = {
  max_nb_blocks : int;
  max_nb_logs : int;
  chunk_size : int;
}

type time_between_blocks = Nothing | Time_between_blocks of float

type blueprints_publisher_config = {
  max_blueprints_lag : int;
  max_blueprints_ahead : int;
  max_blueprints_catchup : int;
  catchup_cooldown : int;
  dal_slots : int list option;
}

type native_execution_policy = Always | Rpcs_only | Never

type kernel_execution_config = {
  preimages : string;
  preimages_endpoint : Uri.t option;
  native_execution_policy : native_execution_policy;
}

type garbage_collector_parameters = {
  split_frequency_in_seconds : int;
  number_of_chunks : int;
}

type history_mode =
  | Archive
  | Rolling of garbage_collector_parameters
  | Full of garbage_collector_parameters

let history_mode_partial_eq h1 h2 =
  match (h1, h2) with
  | Archive, Archive -> true
  | Rolling _, Rolling _ -> true
  | Full _, Full _ -> true
  | _ -> false

type rpc_server = Resto | Dream

type profile_mode = Minimal | Flamegraph

type monitor_websocket_heartbeat = {ping_interval : float; ping_timeout : float}

let chain_id network =
  L2_types.Chain_id
    (Z.of_int (match network with Mainnet -> 0xa729 | Testnet -> 0x1f47b))

let chain_id_encoding : L2_types.chain_id Data_encoding.t =
  let open L2_types in
  let open Data_encoding in
  conv (fun (Chain_id z) -> z) (fun z -> Chain_id z) z

type l2_chain = {
  chain_id : L2_types.chain_id;
  chain_family : L2_types.chain_family;
}

type tx_queue = {
  max_size : int;
  max_transaction_batch_length : int option;
  max_lifespan_s : int;
  tx_per_addr_limit : int64;
}

let default_tx_queue =
  {
    max_size = 1000;
    max_transaction_batch_length = None;
    max_lifespan_s = 4;
    tx_per_addr_limit = 16L;
  }

let tx_queue_encoding =
  let open Data_encoding in
  conv
    (fun {
           max_size;
           max_transaction_batch_length;
           max_lifespan_s;
           tx_per_addr_limit;
         } ->
      (max_size, max_transaction_batch_length, max_lifespan_s, tx_per_addr_limit))
    (fun ( max_size,
           max_transaction_batch_length,
           max_lifespan_s,
           tx_per_addr_limit ) ->
      {
        max_size;
        max_transaction_batch_length;
        max_lifespan_s;
        tx_per_addr_limit;
      })
    (obj4
       (dft "max_size" int31 default_tx_queue.max_size)
       (dft
          "max_transaction_batch_length"
          (option int31)
          default_tx_queue.max_transaction_batch_length)
       (dft "max_lifespan" int31 default_tx_queue.max_lifespan_s)
       (dft "tx_per_addr_limit" int64 default_tx_queue.tx_per_addr_limit))

let tx_queue_opt_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"tx queue configuration"
        Json_only
        (option tx_queue_encoding)
        (function Some tx_queue -> Some (Some tx_queue) | None -> Some None)
        Fun.id;
      case
        ~title:"tx queue enable"
        Json_only
        bool
        (function _ -> None)
        (function true -> Some default_tx_queue | _ -> None);
    ]

type websocket_rate_limit = {
  max_frames : int;
  max_messages : int option;
  interval : int;
  strategy : [`Wait | `Error | `Close];
}

let default_websocket_rate_limit_strategy = `Close

type experimental_features = {
  drop_duplicate_on_injection : bool;
  blueprints_publisher_order_enabled : bool;
  enable_send_raw_transaction : bool;
  overwrite_simulation_tick_limit : bool;
  rpc_server : rpc_server;
  enable_websocket : bool;
  max_websocket_message_length : int;
  monitor_websocket_heartbeat : monitor_websocket_heartbeat option;
  websocket_rate_limit : websocket_rate_limit option;
  spawn_rpc : int option;
  l2_chains : l2_chain list option;
  enable_tx_queue : tx_queue option;
  periodic_snapshot_path : string option;
}

type sequencer = {
  time_between_blocks : time_between_blocks;
  max_number_of_chunks : int;
  sequencer : Client_keys.sk_uri;
  blueprints_publisher_config : blueprints_publisher_config;
}

(* Variant is needed to avoid type-checking errors. *)
type threshold_encryption_sequencer =
  | Threshold_encryption_sequencer of {
      time_between_blocks : time_between_blocks;
      max_number_of_chunks : int;
      sequencer : Client_keys.sk_uri;
      blueprints_publisher_config : blueprints_publisher_config;
      sidecar_endpoint : Uri.t;
    }

type observer = {
  evm_node_endpoint : Uri.t;
  threshold_encryption_bundler_endpoint : Uri.t option;
  rollup_node_tracking : bool;
}

type proxy = {
  finalized_view : bool option;
  evm_node_endpoint : Uri.t option;
  ignore_block_param : bool;
}

type fee_history_max_count = Unlimited | Limit of int

type fee_history = {max_count : fee_history_max_count; max_past : int option}

(* The regular expression is compiled at the launch of the node, and encoded
   into its raw form. *)
type restricted_rpcs =
  | Unrestricted
  | Pattern of {raw : string; regex : Re.re}
  | Blacklist of string list
  | Whitelist of string list

type limit = Unlimited | Limit of int

type rpc = {
  port : int;
  addr : string;
  cors_origins : string list;
  cors_headers : string list;
  max_active_connections :
    Tezos_rpc_http_server.RPC_server.Max_active_rpc_connections.t;
  batch_limit : limit;
  restricted_rpcs : restricted_rpcs;
}

type t = {
  public_rpc : rpc;
  private_rpc : rpc option;
  log_filter : log_filter_config;
  kernel_execution : kernel_execution_config;
  sequencer : sequencer option;
  threshold_encryption_sequencer : threshold_encryption_sequencer option;
  observer : observer option;
  proxy : proxy;
  tx_pool_timeout_limit : int64;
  tx_pool_addr_limit : int64;
  tx_pool_tx_per_addr_limit : int64;
  keep_alive : bool;
  rollup_node_endpoint : Uri.t;
  verbose : Internal_event.level;
  experimental_features : experimental_features;
  fee_history : fee_history;
  finalized_view : bool;
  history_mode : history_mode option;
}

let is_tx_queue_enabled {experimental_features = {enable_tx_queue; _}; _} =
  Option.is_some enable_tx_queue

let retrieve_chain_family ~l2_chains =
  match l2_chains with
  | Some [l2_chain] -> l2_chain.chain_family
  | None -> L2_types.EVM
  | _ -> assert false

let default_filter_config ?max_nb_blocks ?max_nb_logs ?chunk_size () =
  {
    max_nb_blocks = Option.value ~default:100 max_nb_blocks;
    max_nb_logs = Option.value ~default:1000 max_nb_logs;
    chunk_size = Option.value ~default:10 chunk_size;
  }

let default_enable_send_raw_transaction = true

let default_history_mode = Archive

let gc_param_from_retention_period ~days =
  {split_frequency_in_seconds = 86_400; number_of_chunks = days}

let history_mode_of_string_opt str =
  let open Option_syntax in
  match String.split_on_char ':' str with
  | ["archive"] -> return Archive
  | ["rolling"; days] ->
      let* days = int_of_string_opt days in
      if days > 0 then return (Rolling (gc_param_from_retention_period ~days))
      else None
  | ["full"; days] ->
      let* days = int_of_string_opt days in
      if days > 0 then return (Full (gc_param_from_retention_period ~days))
      else None
  | _ -> None

let string_of_history_mode_debug = function
  | Archive -> "archive"
  | Rolling gc -> Format.sprintf "rolling:%d" gc.number_of_chunks
  | Full gc -> Format.sprintf "full:%d" gc.number_of_chunks

let string_of_history_mode_info = function
  | Archive -> "Archive"
  | Rolling _ -> Format.sprintf "Rolling"
  | Full _ -> Format.sprintf "Full"

let pp_history_mode_debug fmt h =
  Format.pp_print_string fmt @@ string_of_history_mode_debug h

let pp_history_mode_info fmt h =
  Format.pp_print_string fmt @@ string_of_history_mode_info h

(* This should be enough for messages we expect to receive in the ethereum
   JSONRPC protocol. *)
let default_max_socket_message_length = 4096 * 1024

let default_monitor_websocket_heartbeat =
  Some {ping_interval = 5.; ping_timeout = 30.}

let default_l2_chains = None

let default_experimental_features =
  {
    enable_send_raw_transaction = default_enable_send_raw_transaction;
    drop_duplicate_on_injection = false;
    blueprints_publisher_order_enabled = false;
    overwrite_simulation_tick_limit = false;
    rpc_server = Resto;
    enable_websocket = false;
    max_websocket_message_length = default_max_socket_message_length;
    monitor_websocket_heartbeat = default_monitor_websocket_heartbeat;
    websocket_rate_limit = None;
    spawn_rpc = None;
    l2_chains = default_l2_chains;
    enable_tx_queue = Some default_tx_queue;
    periodic_snapshot_path = None;
  }

let default_rpc_addr = "127.0.0.1"

let default_rpc_port = 8545

let default_sequencer_sidecar_endpoint = Uri.of_string "127.0.0.1:5303"

let default_keep_alive = false

let default_finalized_view = false

let default_rollup_node_endpoint = Uri.of_string "http://localhost:8932"

let default_rollup_node_tracking = true

let default_cors_origins = []

let default_cors_headers = []

let default_rpc ?(rpc_port = default_rpc_port) ?(rpc_addr = default_rpc_addr)
    ?(cors_origins = default_cors_origins)
    ?(cors_headers = default_cors_headers) ?(batch_limit = Unlimited)
    ?(restricted_rpcs = Unrestricted)
    ?(max_active_connections =
      Tezos_rpc_http_server.RPC_server.Max_active_rpc_connections.Unlimited) ()
    =
  {
    port = rpc_port;
    addr = rpc_addr;
    cors_headers;
    cors_origins;
    batch_limit;
    restricted_rpcs;
    max_active_connections;
  }

let default_max_active_connections =
  Tezos_rpc_http_server.RPC_server.Max_active_rpc_connections.default

let default_preimages data_dir = Filename.Infix.(data_dir // "wasm_2_0_0")

let default_preimages_endpoint = function
  | Mainnet ->
      Uri.of_string "https://snapshots.tzinit.org/etherlink-mainnet/wasm_2_0_0"
  | Testnet ->
      Uri.of_string "https://snapshots.tzinit.org/etherlink-ghostnet/wasm_2_0_0"

let default_time_between_blocks = Time_between_blocks 5.

let hard_maximum_number_of_chunks =
  (* The kernel doesn't accept blueprints whose cumulated chunk size is higher
     than 512kb. *)
  let max_cumulated_chunks_size = 512 * 1024 in
  (* External message size *)
  let chunk_size = 4095 in
  max_cumulated_chunks_size / chunk_size

let default_max_number_of_chunks = hard_maximum_number_of_chunks

let default_tx_pool_timeout_limit = Int64.of_int 3600

let default_tx_pool_addr_limit = Int64.of_int 4000

let default_tx_pool_tx_per_addr_limit = Int64.of_int 16

let default_blueprints_publisher_config_without_dal =
  {
    max_blueprints_lag = 50;
    (* When the sequencer is more than max_blueprints_lag L2
       levels ahead of the rollup, a cath-up mechanism is
       triggered. It consists in resending some blueprints on the
       inbox. *)
    max_blueprints_ahead = 100;
    (* When the sequencer is more than max_blueprints_ahead L2 levels
       ahead of the rollup, it locks its tx_pool for some time. This
       has the effect of considerably slowing down the creation of L2
       blocks. *)
    max_blueprints_catchup = 50;
    (* This is the maximum number of blueprints that the sequencer
       resends at each L1 level during catch up. *)
    catchup_cooldown = 1;
    (* When the catch-up mechanism is triggered, it deactivates itself
       for [catchup_cooldown] L1 levels. *)
    dal_slots = None;
    (* If this is None, the DAL will not be used by the
       sequencer. Otherwise, this is the list of DAL slot indices on
       which the rollup node will try to publish DAL slots. It should
       be included in the dal_slots list of the kernel configuration
       otherwise the kernel will ignore any DAL slots published on
       forbidden indices. *)
  }

let default_blueprints_publisher_config_with_dal =
  {
    default_blueprints_publisher_config_without_dal with
    max_blueprints_lag = 400;
    (* This lag should be large enough to avoid triggerring the
       catch-up mechanism before the rollup had the opportunity to
       import the blueprints from the DAL. This typically takes about
       10 L1 levels (so 200 L2 levels assuming a 10s block time on the
       L1 and 500ms block time on the L2) when there is no congestion
       on the DAL. We double this typical value to support some DAL
       congestion. *)
    max_blueprints_ahead = 500;
    (* For this parameter to make sense, it should be
       significantly larger than max_blueprints_lag. *)
  }

let default_fee_history = {max_count = Limit 1024; max_past = None}

let make_pattern_restricted_rpcs raw =
  Pattern {raw; regex = Re.Perl.compile_pat raw}

let limit_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"unlimited"
        ~description:"No limit on the size of a JSON RPC API batch."
        Json_only
        (constant "unlimited")
        (function Unlimited -> Some () | _ -> None)
        (fun () -> Unlimited);
      case
        ~title:"limited"
        ~description:
          "Upper bound on the size of a JSON RPC API batch. For batches larger \
           than the limit, every request fails."
        Json_only
        int31
        (function Limit i -> Some i | _ -> None)
        (fun i -> Limit i);
    ]

let restricted_rpcs_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"unrestricted"
        ~description:"Allow all JSON RPC API methods supported by the server."
        Json_only
        (constant "unrestricted")
        (function Unrestricted -> Some () | _ -> None)
        (fun () -> Unrestricted);
      case
        ~title:"pattern"
        ~description:
          "Disallow the JSON RPC API methods whose name match this Perl-like \
           regexp."
        Json_only
        string
        (function Pattern {raw; _} -> Some raw | _ -> None)
        make_pattern_restricted_rpcs;
      case
        ~title:"whitelist"
        ~description:"The list of JSON RPC API methods allowed for this server."
        Json_only
        (obj1 (req "whitelist" (list string)))
        (function Whitelist l -> Some l | _ -> None)
        (fun l -> Whitelist l);
      case
        ~title:"blacklist"
        ~description:
          "The list of JSON RPC API methods disallowed for this server."
        Json_only
        (obj1 (req "blacklist" (list string)))
        (function Blacklist l -> Some l | _ -> None)
        (fun l -> Blacklist l);
    ]

let default_native_execution_policy = Rpcs_only

let kernel_execution_config_dft ~data_dir ?preimages ?preimages_endpoint
    ?native_execution_policy () =
  {
    preimages = Option.value ~default:(default_preimages data_dir) preimages;
    preimages_endpoint;
    native_execution_policy =
      Option.value
        ~default:default_native_execution_policy
        native_execution_policy;
  }

let sequencer_config_dft ?time_between_blocks ?max_number_of_chunks ~sequencer
    ?max_blueprints_lag ?max_blueprints_ahead ?max_blueprints_catchup
    ?catchup_cooldown ?dal_slots () =
  let default_blueprints_publisher_config =
    if Option.is_some dal_slots then
      default_blueprints_publisher_config_with_dal
    else default_blueprints_publisher_config_without_dal
  in
  let blueprints_publisher_config =
    {
      max_blueprints_lag =
        Option.value
          ~default:default_blueprints_publisher_config.max_blueprints_lag
          max_blueprints_lag;
      max_blueprints_ahead =
        Option.value
          ~default:default_blueprints_publisher_config.max_blueprints_ahead
          max_blueprints_ahead;
      max_blueprints_catchup =
        Option.value
          ~default:default_blueprints_publisher_config.max_blueprints_catchup
          max_blueprints_catchup;
      catchup_cooldown =
        Option.value
          ~default:default_blueprints_publisher_config.catchup_cooldown
          catchup_cooldown;
      dal_slots;
    }
  in
  {
    time_between_blocks =
      Option.value ~default:default_time_between_blocks time_between_blocks;
    max_number_of_chunks =
      Option.value ~default:default_max_number_of_chunks max_number_of_chunks;
    sequencer;
    blueprints_publisher_config;
  }

let threshold_encryption_sequencer_config_dft ?time_between_blocks
    ?max_number_of_chunks ~sequencer ?sidecar_endpoint ?max_blueprints_lag
    ?max_blueprints_ahead ?max_blueprints_catchup ?catchup_cooldown ?dal_slots
    () =
  let default_blueprints_publisher_config =
    if Option.is_some dal_slots then
      default_blueprints_publisher_config_with_dal
    else default_blueprints_publisher_config_without_dal
  in
  let blueprints_publisher_config =
    {
      max_blueprints_lag =
        Option.value
          ~default:default_blueprints_publisher_config.max_blueprints_lag
          max_blueprints_lag;
      max_blueprints_ahead =
        Option.value
          ~default:default_blueprints_publisher_config.max_blueprints_ahead
          max_blueprints_ahead;
      max_blueprints_catchup =
        Option.value
          ~default:default_blueprints_publisher_config.max_blueprints_catchup
          max_blueprints_catchup;
      catchup_cooldown =
        Option.value
          ~default:default_blueprints_publisher_config.catchup_cooldown
          catchup_cooldown;
      dal_slots;
    }
  in
  Threshold_encryption_sequencer
    {
      time_between_blocks =
        Option.value ~default:default_time_between_blocks time_between_blocks;
      max_number_of_chunks =
        Option.value ~default:default_max_number_of_chunks max_number_of_chunks;
      sequencer;
      blueprints_publisher_config;
      sidecar_endpoint =
        Option.value
          ~default:default_sequencer_sidecar_endpoint
          sidecar_endpoint;
    }

let observer_evm_node_endpoint = function
  | Mainnet -> "https://relay.mainnet.etherlink.com"
  | Testnet -> "https://relay.ghostnet.etherlink.com"

let observer_config_dft ~evm_node_endpoint
    ?threshold_encryption_bundler_endpoint ?rollup_node_tracking () =
  {
    evm_node_endpoint;
    threshold_encryption_bundler_endpoint;
    rollup_node_tracking =
      Option.value ~default:default_rollup_node_tracking rollup_node_tracking;
  }

let log_filter_config_encoding : log_filter_config Data_encoding.t =
  let open Data_encoding in
  let default_filter_config = default_filter_config () in
  conv
    (fun {max_nb_blocks; max_nb_logs; chunk_size} ->
      (max_nb_blocks, max_nb_logs, chunk_size))
    (fun (max_nb_blocks, max_nb_logs, chunk_size) ->
      {max_nb_blocks; max_nb_logs; chunk_size})
    (obj3
       (dft
          "max_nb_blocks"
          ~description:
            "The maximum number of blocks that can be visited when executing a \
             `eth_getLogs` request."
          strictly_positive_encoding
          default_filter_config.max_nb_blocks)
       (dft
          "max_nb_logs"
          ~description:
            "The maximum number of logs that can be collected when executing a \
             `eth_getLogs` request."
          strictly_positive_encoding
          default_filter_config.max_nb_logs)
       (dft
          "chunk_size"
          ~description:
            "Number of blocks to be filter concurrently when executing a \
             `eth_getLogs` request."
          strictly_positive_encoding
          default_filter_config.chunk_size))

let pp_time_between_blocks fmt = function
  | Nothing -> Format.pp_print_string fmt "nothing"
  | Time_between_blocks duration -> Format.fprintf fmt "%fs" duration

let time_between_blocks_encoding : time_between_blocks Data_encoding.t =
  let open Data_encoding in
  def "time_between_blocks"
  @@ conv
       (function Nothing -> None | Time_between_blocks f -> Some f)
       (function None -> Nothing | Some f -> Time_between_blocks f)
       (option float)

let blueprints_publisher_config_encoding =
  let open Data_encoding in
  let default_blueprints_publisher_config =
    default_blueprints_publisher_config_without_dal
  in
  conv
    (fun {
           max_blueprints_lag;
           max_blueprints_ahead;
           max_blueprints_catchup;
           catchup_cooldown;
           dal_slots;
         } ->
      ( max_blueprints_lag,
        max_blueprints_ahead,
        max_blueprints_catchup,
        catchup_cooldown,
        dal_slots ))
    (fun ( max_blueprints_lag,
           max_blueprints_ahead,
           max_blueprints_catchup,
           catchup_cooldown,
           dal_slots ) ->
      {
        max_blueprints_lag;
        max_blueprints_ahead;
        max_blueprints_catchup;
        catchup_cooldown;
        dal_slots;
      })
    (obj5
       (dft
          ~description:
            "The number of EVM blocks after which the sequencer considers that \
             something went wrong with the injection of a previous blueprint. \
             Once reached, the sequencer tries to send them a second time."
          "max_blueprints_lag"
          strictly_positive_encoding
          default_blueprints_publisher_config.max_blueprints_lag)
       (dft
          ~description:
            "The maximum number of EVM blocks that the sequencer accepts to \
             create speculatively. If the difference between its head and the \
             head of its companion rollup node reaches this number, the \
             sequencer will stop creating new blueprints until the rollup node \
             has caught up."
          "max_blueprints_ahead"
          strictly_positive_encoding
          default_blueprints_publisher_config.max_blueprints_ahead)
       (dft
          ~description:
            "The maximum number of blueprints the sequencer retries to send at \
             once whenever its companion rollup node is lagging behind."
          "max_blueprints_catchup"
          strictly_positive_encoding
          default_blueprints_publisher_config.max_blueprints_catchup)
       (dft
          ~description:
            "The number of Layer 1 blocks the sequencer awaits before sending \
             another batch of blueprints, as part of its catchup mechanism."
          "catchup_cooldown"
          strictly_positive_encoding
          default_blueprints_publisher_config.catchup_cooldown)
       (opt "dal_slots" (list int8)))

let time_between_blocks_field =
  Data_encoding.dft
    ~description:
      "The maximum number of seconds separating two consecutive blocks. If the \
       TX pool of the sequencer is empty after this duration, an empty \
       blueprint is produced."
    "time_between_blocks"
    time_between_blocks_encoding
    default_time_between_blocks

let max_number_of_chunks_field =
  Data_encoding.(
    dft
      ~description:
        "Maximum number of chunks a blueprint can be divided into. The \
         sequencer will not produce blueprints unable to fit in this limit."
      "max_number_of_chunks"
      (ranged_int 1 hard_maximum_number_of_chunks)
      default_max_number_of_chunks)

let sequencer_field =
  Data_encoding.(
    req
      ~description:"Secret key URI of the sequencer."
      "sequencer"
      (string' Plain))

let sequencer_encoding =
  let open Data_encoding in
  let default_blueprints_publisher_config =
    default_blueprints_publisher_config_without_dal
  in
  conv
    (fun {
           time_between_blocks;
           max_number_of_chunks;
           sequencer;
           blueprints_publisher_config;
         } ->
      ( time_between_blocks,
        max_number_of_chunks,
        Client_keys.string_of_sk_uri sequencer,
        blueprints_publisher_config ))
    (fun ( time_between_blocks,
           max_number_of_chunks,
           sequencer,
           blueprints_publisher_config ) ->
      {
        time_between_blocks;
        max_number_of_chunks;
        sequencer = Client_keys.sk_uri_of_string sequencer;
        blueprints_publisher_config;
      })
    (obj4
       time_between_blocks_field
       max_number_of_chunks_field
       sequencer_field
       (dft
          "blueprints_publisher_config"
          blueprints_publisher_config_encoding
          default_blueprints_publisher_config))

let threshold_encryption_sequencer_encoding =
  let open Data_encoding in
  let default_blueprints_publisher_config =
    default_blueprints_publisher_config_without_dal
  in
  conv
    (function
      | Threshold_encryption_sequencer
          {
            time_between_blocks;
            max_number_of_chunks;
            sequencer;
            blueprints_publisher_config;
            sidecar_endpoint;
          } ->
          ( time_between_blocks,
            max_number_of_chunks,
            Client_keys.string_of_sk_uri sequencer,
            blueprints_publisher_config,
            sidecar_endpoint ))
    (fun ( time_between_blocks,
           max_number_of_chunks,
           sequencer,
           blueprints_publisher_config,
           sidecar_endpoint ) ->
      Threshold_encryption_sequencer
        {
          time_between_blocks;
          max_number_of_chunks;
          sequencer = Client_keys.sk_uri_of_string sequencer;
          blueprints_publisher_config;
          sidecar_endpoint;
        })
    (obj5
       time_between_blocks_field
       (dft "max_number_of_chunks" int31 default_max_number_of_chunks)
       sequencer_field
       (dft
          "blueprints_publisher_config"
          blueprints_publisher_config_encoding
          default_blueprints_publisher_config)
       (dft
          "sidecar_endpoint"
          Tezos_rpc.Encoding.uri_encoding
          default_sequencer_sidecar_endpoint))

let observer_encoding ?network () =
  let open Data_encoding in
  let evm_node_endpoint_field ~description name encoding =
    match network with
    | Some network ->
        dft ~description name encoding (observer_evm_node_endpoint network)
    | None -> req ~description name encoding
  in
  conv
    (fun {
           evm_node_endpoint;
           threshold_encryption_bundler_endpoint;
           rollup_node_tracking;
         } ->
      ( Uri.to_string evm_node_endpoint,
        threshold_encryption_bundler_endpoint,
        rollup_node_tracking ))
    (fun ( evm_node_endpoint,
           threshold_encryption_bundler_endpoint,
           rollup_node_tracking ) ->
      {
        evm_node_endpoint = Uri.of_string evm_node_endpoint;
        threshold_encryption_bundler_endpoint;
        rollup_node_tracking;
      })
    (obj3
       (evm_node_endpoint_field
          ~description:
            "Upstream EVM node endpoint used to fetch speculative blueprints \
             and forward incoming transactions."
          "evm_node_endpoint"
          (string' Plain))
       (opt
          "threshold_encryption_bundler_endpoint"
          Tezos_rpc.Encoding.uri_encoding)
       (dft
          ~description:
            "Enable or disable monitoring a companion rollup node to verify \
             the correctness of the speculative history coming from the \
             upstream EVM node."
          "rollup_node_tracking"
          bool
          default_rollup_node_tracking))

let rpc_server_encoding =
  let open Data_encoding in
  string_enum [("resto", Resto); ("dream", Dream)]

let history_mode_schema =
  Data_encoding.(
    Json.schema @@ string_enum [("archive", ()); ("rolling:n", ())])

let history_mode_encoding =
  let open Data_encoding in
  def
    "history_mode"
    ~description:
      "Compact notation for the history mode. Can either be `archive` and \
       `rolling:N` with `N` being the number of days to use as the retention \
       period"
  @@ conv_with_guard
       ~schema:history_mode_schema
       string_of_history_mode_debug
       (fun str ->
         match history_mode_of_string_opt str with
         | None -> Error (Format.sprintf "%s is not a valid history mode" str)
         | Some m -> Ok m)
       string

let monitor_websocket_heartbeat_encoding =
  let open Data_encoding in
  conv
    (fun {ping_interval; ping_timeout} -> (ping_interval, ping_timeout))
    (fun (ping_interval, ping_timeout) -> {ping_interval; ping_timeout})
  @@ obj2
       (req
          ~description:
            "Interval, in seconds, at which a ping will be sent to the client \
             to monitor the websocket connection."
          "ping_interval"
          float)
       (req
          ~description:
            "Timeout in seconds after which the connection will be considered \
             dead and closed."
          "ping_timeout"
          float)

let opt_monitor_websocket_heartbeat_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"disabled"
        ~description:"Disable websocket connection monitoring"
        (constant "disabled")
        (function None -> Some () | _ -> None)
        (fun () -> None);
      case
        (Tag 1)
        ~title:"enabled"
        monitor_websocket_heartbeat_encoding
        Fun.id
        Option.some;
    ]

let l2_chain_encoding : l2_chain Data_encoding.t =
  let open L2_types in
  let open Data_encoding in
  conv
    (fun {chain_id; chain_family} -> (chain_id, chain_family))
    (fun (chain_id, chain_family) -> {chain_id; chain_family})
  @@ obj2
       (req "chain_id" ~description:"The id of the l2 chain" chain_id_encoding)
       (req
          "chain_family"
          ~description:"The family of the l2 chain"
          Chain_family.encoding)

let websocket_rate_limit_strategy_encoding =
  Data_encoding.string_enum
    [("wait", `Wait); ("error", `Error); ("close", `Close)]

let websocket_rate_limit_encoding =
  let open Data_encoding in
  conv_with_guard
    (fun {max_frames; max_messages; interval; strategy} ->
      (Some max_frames, max_messages, interval, strategy))
    (fun (max_frames, max_messages, interval, strategy) ->
      let open Result_syntax in
      (* The "rate limit on frames" acts as a first protection against spam (the
         check is run earlier) but is violent because it closes the connection,
         whereas the limit on messages can be more fine tuned to allow other
         behaviors. It must be set when rate limiting is enabled. *)
      let+ max_frames =
        match (max_frames, max_messages) with
        | None, None -> fail "Specify max_frames and/or max_messages"
        | None, Some max_messages ->
            (* We've chosen to allow 10 x more frames than messages by default if
               the user forgets to provide a frame limit to be on the safe side,
               but it's recommended that users set both limits depending on their
               setup and application. *)
            return (10 * max_messages)
        | Some max_frames, None -> return max_frames
        | Some max_frames, Some max_messages ->
            if max_messages > max_frames then
              (* We will always get more frames than messages because of control
                 frames and message splitting. *)
              fail "max_messages cannot be greater than max_frames"
            else return max_frames
      in
      {max_frames; max_messages; interval; strategy})
  @@ obj4
       (opt
          "max_frames"
          ~description:
            "Max allowed websocket frames in the below interval (10x \
             max_messages when unspecified)."
          int31)
       (opt
          "max_messages"
          ~description:"Max allowed websocket messages in the below interval."
          int31)
       (req
          "interval"
          ~description:"Interval in seconds for the rate limit."
          int31)
       (dft
          "strategy"
          ~description:
            "Strategy to adopt when a client sends messages which exceed the \
             defined rate limit."
          websocket_rate_limit_strategy_encoding
          default_websocket_rate_limit_strategy)

let experimental_features_encoding =
  let open Data_encoding in
  conv
    (fun {
           drop_duplicate_on_injection;
           blueprints_publisher_order_enabled;
           enable_send_raw_transaction;
           overwrite_simulation_tick_limit;
           rpc_server;
           enable_websocket;
           max_websocket_message_length;
           monitor_websocket_heartbeat;
           websocket_rate_limit;
           spawn_rpc;
           l2_chains : l2_chain list option;
           enable_tx_queue;
           periodic_snapshot_path;
         } ->
      ( ( drop_duplicate_on_injection,
          blueprints_publisher_order_enabled,
          enable_send_raw_transaction,
          None,
          overwrite_simulation_tick_limit,
          None ),
        ( rpc_server,
          enable_websocket,
          max_websocket_message_length,
          monitor_websocket_heartbeat,
          websocket_rate_limit,
          spawn_rpc,
          l2_chains,
          enable_tx_queue,
          periodic_snapshot_path ) ))
    (fun ( ( drop_duplicate_on_injection,
             blueprints_publisher_order_enabled,
             enable_send_raw_transaction,
             _node_transaction_validation,
             overwrite_simulation_tick_limit,
             _next_wasm_runtime ),
           ( rpc_server,
             enable_websocket,
             max_websocket_message_length,
             monitor_websocket_heartbeat,
             websocket_rate_limit,
             spawn_rpc,
             l2_chains,
             enable_tx_queue,
             periodic_snapshot_path ) ) ->
      {
        drop_duplicate_on_injection;
        blueprints_publisher_order_enabled;
        enable_send_raw_transaction;
        overwrite_simulation_tick_limit;
        rpc_server;
        enable_websocket;
        max_websocket_message_length;
        monitor_websocket_heartbeat;
        websocket_rate_limit;
        spawn_rpc;
        l2_chains;
        enable_tx_queue;
        periodic_snapshot_path;
      })
    (merge_objs
       (obj6
          (dft
             ~description:
               "Request the rollup node to filter messages it has already \
                forwarded to the Layer 1 network. Require an unreleased \
                version of the Smart Rollup node."
             "drop_duplicate_on_injection"
             bool
             default_experimental_features.drop_duplicate_on_injection)
          (dft
             ~description:
               "Request the rollup node to prioritize messages by level when \
                publishing blueprints in the layer 1."
             "blueprints_publisher_order_enabled"
             bool
             default_experimental_features.blueprints_publisher_order_enabled)
          (dft
             ~description:
               "Enable or disable the `eth_sendRawTransaction` method. \
                DEPRECATED:  You should use \"rpc.restricted_rpcs\" instead."
             "enable_send_raw_transaction"
             bool
             default_experimental_features.enable_send_raw_transaction)
          (opt
             ~description:
               "DEPRECATED: You should remove this option from your \
                configuration file."
             "node_transaction_validation"
             bool)
          (dft
             "overwrite_simulation_tick_limit"
             ~description:
               "When enabled, the eth_call method is not subject to the tick \
                limit. This can be useful to execute calls that will not be \
                injected in transactions (similarly to what the Uniswap V3 \
                frontend does to prepare swaps). However, it can lead to \
                confusing UX for users, where eth_estimateGas fails when \
                eth_call succeeded."
             bool
             default_experimental_features.overwrite_simulation_tick_limit)
          (opt
             "next_wasm_runtime"
             ~description:
               "Enable or disable the experimental WASM runtime that is \
                expected to replace the Smart Rollup’s Fast Exec runtime. \
                DEPRECATED: You should remove this option from your \
                configuration file."
             bool))
       (obj9
          (dft
             "rpc_server"
             ~description:
               "Choose the RPC server implementation, \'dream\' or \'resto\', \
                the latter being the default one."
             rpc_server_encoding
             default_experimental_features.rpc_server)
          (dft
             "enable_websocket"
             ~description:"Enable or disable the experimental websocket server"
             bool
             default_experimental_features.enable_websocket)
          (dft
             "max_websocket_message_length"
             ~description:
               "Maximum message size accepted by the websocket server (only \
                for Resto backend)"
             int31
             default_max_socket_message_length)
          (dft
             "monitor_websocket_heartbeat"
             ~description:"Parameters to monitor websocket connections"
             opt_monitor_websocket_heartbeat_encoding
             default_monitor_websocket_heartbeat)
          (opt
             "websocket_rate_limit"
             ~description:"Rate limit for websocket server"
             websocket_rate_limit_encoding)
          (dft
             "spawn_rpc"
             ~description:"Spawn a RPC node listening on the given port"
             (option @@ obj1 (req "protected_port" (ranged_int 1 65535)))
             default_experimental_features.spawn_rpc)
          (dft
             "l2_chains"
             ~description:
               "Configuration of l2_chains for multisequencing.\n\
               \                 If not set, the node will adopt a single \
                chain behaviour."
             (option (list l2_chain_encoding))
             default_l2_chains)
          (dft
             "enable_tx_queue"
             ~description:"Replace the observer tx pool by a tx queue"
             tx_queue_opt_encoding
             default_experimental_features.enable_tx_queue)
          (dft
             "periodic_snapshot_path"
             ~description:"Path to the periodic snapshot file"
             (option string)
             default_experimental_features.periodic_snapshot_path)))

let proxy_encoding =
  let open Data_encoding in
  conv
    (fun {finalized_view; evm_node_endpoint; ignore_block_param} ->
      ( finalized_view,
        Option.map Uri.to_string evm_node_endpoint,
        ignore_block_param ))
    (fun (finalized_view, evm_node_endpoint, ignore_block_param) ->
      {
        finalized_view;
        evm_node_endpoint = Option.map Uri.of_string evm_node_endpoint;
        ignore_block_param;
      })
  @@ obj3
       (opt
          ~description:
            "When enabled, the node only expose blocks that are finalized, \
             i.e., the `latest` block parameter becomes a synonym for \
             `finalized`. DEPRECATED: use the top level `finalized_view` \
             option instead."
          "finalized_view"
          bool)
       (opt "evm_node_endpoint" string)
       (dft "ignore_block_param" bool false)

let default_proxy ?evm_node_endpoint ?(ignore_block_param = false) () =
  {finalized_view = None; evm_node_endpoint; ignore_block_param}

let fee_history_encoding =
  let open Data_encoding in
  let max_count_encoding : fee_history_max_count Data_encoding.t =
    union
      [
        case
          ~title:"unlimited"
          ~description:"Allow any number for block_count parameter request."
          Json_only
          (constant "unlimited")
          (function
            | (Unlimited : fee_history_max_count) -> Some () | _ -> None)
          (fun () -> Unlimited);
        case
          ~title:"limit"
          ~description:"Limit the number of block allowed to be queried."
          Json_only
          strictly_positive_encoding
          (function
            | (Limit limit : fee_history_max_count) -> Some limit | _ -> None)
          (fun limit -> Limit limit);
      ]
  in
  conv
    (fun {max_count; max_past} -> (max_count, max_past))
    (fun (max_count, max_past) -> {max_count; max_past})
    (obj2
       (dft
          ~description:
            "The maximum number of blocks whose fee history can be retrieved \
             at once"
          "max_count"
          max_count_encoding
          default_fee_history.max_count)
       (opt
          ~description:
            "The maximum number of blocks in the past where the fee history is \
             available"
          "max_past"
          strictly_positive_encoding))

let native_execution_policy_encoding =
  Data_encoding.(
    def
      "native_execution_policy"
      ~title:"native_execution_policy"
      ~description:
        "`never` means the native execution will never be used by the node. \
         `rpcs_only` will restrict the usage of the native execution to RPC \
         calls, leaving blueprint application to the WASM runtime. `always` \
         will always use native execution for supported kernels."
    @@ string_enum
         [("never", Never); ("rpcs_only", Rpcs_only); ("always", Always)])

let kernel_execution_encoding ?network data_dir =
  Data_encoding.(
    let preimages_endpoint_field ~description name encoding =
      match network with
      | Some network ->
          dft
            ~description
            name
            (option encoding)
            (Some (default_preimages_endpoint network))
      | None -> opt ~description name encoding
    in
    conv
      (fun {preimages; preimages_endpoint; native_execution_policy} ->
        (preimages, preimages_endpoint, native_execution_policy))
      (fun (preimages, preimages_endpoint, native_execution_policy) ->
        {preimages; preimages_endpoint; native_execution_policy})
      (obj3
         (dft
            ~description:
              "Path to a directory containing the preimages the kernel can \
               reveal."
            "preimages"
            string
            (default_preimages data_dir))
         (preimages_endpoint_field
            ~description:
              "Endpoint for downloading the preimages that cannot be found in \
               the preimages directory. These preimages are downloaded by the \
               node, stored in the preimages directory and fed to the kernel."
            "preimages_endpoint"
            Tezos_rpc.Encoding.uri_encoding)
         (dft
            ~description:
              "Policy regarding when to use the native execution for supported \
               kernels. Native execution provides better performance, but \
               increases the complexity of the software stack of the node by \
               adding an additional layer between what is executed by the EVM \
               node and the Smart Rollup nodes. Can be `never`, `rpcs_only` or \
               `always`. Default to `never`."
            "native_execution_policy"
            native_execution_policy_encoding
            default_native_execution_policy)))

let rpc_encoding =
  Data_encoding.(
    conv
      (fun {
             port;
             addr;
             cors_origins;
             cors_headers;
             batch_limit;
             restricted_rpcs;
             max_active_connections;
           } ->
        ( port,
          addr,
          cors_origins,
          cors_headers,
          batch_limit,
          restricted_rpcs,
          max_active_connections ))
      (fun ( port,
             addr,
             cors_origins,
             cors_headers,
             batch_limit,
             restricted_rpcs,
             max_active_connections ) ->
        {
          port;
          addr;
          cors_origins;
          cors_headers;
          batch_limit;
          restricted_rpcs;
          max_active_connections;
        })
      (obj7
         (dft
            ~description:"The port used to bind the socket of the RPC server."
            "port"
            (ranged_int 1 65535)
            default_rpc_port)
         (dft
            ~description:
              "The address used to bind the socket of the RPC server."
            "addr"
            (string' Plain)
            default_rpc_addr)
         (dft
            ~description:
              "Cross-Origin Resource Sharing (CORS) origin values. See the \
               CORS specification."
            "cors_origins"
            (list (string' Plain))
            default_cors_origins)
         (dft
            ~description:
              "Cross-Origin Resource Sharing (CORS) header values. See the \
               CORS specification."
            "cors_headers"
            (list (string' Plain))
            default_cors_headers)
         (dft "batch_limit" limit_encoding Unlimited)
         (dft "restricted_rpcs" restricted_rpcs_encoding Unrestricted)
         (dft
            "max_active_connections"
            Tezos_rpc_http_server.RPC_server.Max_active_rpc_connections.encoding
            default_max_active_connections)))

let encoding ?network data_dir : t Data_encoding.t =
  let open Data_encoding in
  let observer_field name encoding =
    match network with
    | Some network ->
        dft
          name
          (option encoding)
          (Some
             (observer_config_dft
                ~evm_node_endpoint:
                  (Uri.of_string (observer_evm_node_endpoint network))
                ()))
    | None -> opt name encoding
  in
  conv
    (fun {
           public_rpc;
           private_rpc;
           log_filter;
           sequencer;
           threshold_encryption_sequencer;
           observer;
           proxy;
           tx_pool_timeout_limit;
           tx_pool_addr_limit;
           tx_pool_tx_per_addr_limit;
           keep_alive;
           rollup_node_endpoint;
           verbose;
           experimental_features;
           fee_history;
           kernel_execution;
           finalized_view;
           history_mode;
         } ->
      ( (log_filter, sequencer, threshold_encryption_sequencer, observer),
        ( ( tx_pool_timeout_limit,
            tx_pool_addr_limit,
            tx_pool_tx_per_addr_limit,
            keep_alive,
            rollup_node_endpoint,
            verbose,
            experimental_features,
            proxy,
            fee_history ),
          ( kernel_execution,
            public_rpc,
            private_rpc,
            finalized_view,
            history_mode ) ) ))
    (fun ( (log_filter, sequencer, threshold_encryption_sequencer, observer),
           ( ( tx_pool_timeout_limit,
               tx_pool_addr_limit,
               tx_pool_tx_per_addr_limit,
               keep_alive,
               rollup_node_endpoint,
               verbose,
               experimental_features,
               proxy,
               fee_history ),
             ( kernel_execution,
               public_rpc,
               private_rpc,
               finalized_view,
               history_mode ) ) ) ->
      {
        public_rpc;
        private_rpc;
        log_filter;
        sequencer;
        threshold_encryption_sequencer;
        observer;
        proxy;
        tx_pool_timeout_limit;
        tx_pool_addr_limit;
        tx_pool_tx_per_addr_limit;
        keep_alive;
        rollup_node_endpoint;
        verbose;
        experimental_features;
        fee_history;
        kernel_execution;
        finalized_view;
        history_mode;
      })
    (merge_objs
       (obj4
          (dft
             "log_filter"
             log_filter_config_encoding
             (default_filter_config ()))
          (opt "sequencer" sequencer_encoding)
          (opt
             "threshold_encryption_sequencer"
             threshold_encryption_sequencer_encoding)
          (observer_field "observer" (observer_encoding ?network ())))
       (merge_objs
          (obj9
             (dft
                "tx_pool_timeout_limit"
                ~description:
                  "Transaction timeout limit inside the transaction pool"
                int64
                default_tx_pool_timeout_limit)
             (dft
                "tx_pool_addr_limit"
                ~description:
                  "Maximum allowed addresses inside the transaction pool."
                int64
                default_tx_pool_addr_limit)
             (dft
                "tx_pool_tx_per_addr_limit"
                ~description:
                  "Maximum allowed transactions per user address inside the \
                   transaction pool."
                int64
                default_tx_pool_tx_per_addr_limit)
             (dft
                "keep_alive"
                ~description:
                  "Enable or disable if the EVM node retries HTTP requests on \
                   failure."
                bool
                default_keep_alive)
             (dft
                "rollup_node_endpoint"
                ~description:
                  "An endpoint to a companion rollup node. It is mainly used \
                   to keep track of the state of the smart rollup powering the \
                   Layer 2 chain. In sequencer mode, the blueprint created by \
                   the node are forwarded to the rollup node to be injected in \
                   Layer 1 blocks."
                Tezos_rpc.Encoding.uri_encoding
                default_rollup_node_endpoint)
             (dft "verbose" Internal_event.Level.encoding Internal_event.Notice)
             (dft
                "experimental_features"
                experimental_features_encoding
                default_experimental_features)
             (dft "proxy" proxy_encoding (default_proxy ()))
             (dft "fee_history" fee_history_encoding default_fee_history))
          (obj5
             (dft
                "kernel_execution"
                (kernel_execution_encoding ?network data_dir)
                (kernel_execution_config_dft
                   ?preimages_endpoint:
                     (Option.map default_preimages_endpoint network)
                   ~data_dir
                   ()))
             (dft "public_rpc" rpc_encoding (default_rpc ()))
             (opt "private_rpc" rpc_encoding)
             (dft
                ~description:
                  "When enabled, the node only expose blocks that are \
                   finalized, i.e., the `latest` block parameter becomes a \
                   synonym for `finalized`."
                "finalized_view"
                bool
                false)
             (opt
                "history"
                ~description:"History mode of the EVM node"
                history_mode_encoding))))

let pp_print_json ~data_dir fmt config =
  let json =
    Data_encoding.Json.construct
      ~include_default_fields:`Always
      (encoding data_dir)
      config
  in
  Data_encoding.Json.pp fmt json

let save ~force ~data_dir config config_file =
  let open Lwt_result_syntax in
  let json = Data_encoding.Json.construct (encoding data_dir) config in
  let*! exists = Lwt_unix.file_exists config_file in
  if exists && not force then
    failwith
      "Configuration file %S already exists. Use --force to overwrite."
      config_file
  else
    let*! () = Lwt_utils_unix.create_dir (Filename.dirname config_file) in
    Lwt_utils_unix.Json.write_file config_file json

module Json_syntax = struct
  let ( |-> ) = Ezjsonm.find_opt

  let ( |->! ) = Ezjsonm.find

  let ( |?> ) x f = Option.map f x
end

let warn =
  Format.kasprintf @@ fun s ->
  let reset = Pretty_printing.add_ansi_marking Format.err_formatter in
  Format.eprintf "@{<fg_yellow>[Warning] %s@}@." s ;
  reset ()

let warn_deprecated json path =
  let open Json_syntax in
  match json |-> path with
  | None -> ()
  | Some _ -> warn "Deprecated configuration field %s" (String.concat "." path)

let warn_deprecated json = List.iter (warn_deprecated json)

(* Syntactic checks related to deprecations *)
let precheck json =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let open Json_syntax in
      (* Conflicts between [.proxy.finalized_view] and [.finalized_view] *)
      let proxy_conf =
        json |-> ["proxy"; "finalized_view"] |?> Ezjsonm.get_bool
      in
      let toplevel_conf = json |-> ["finalized_view"] |?> Ezjsonm.get_bool in
      let* () =
        match (proxy_conf, toplevel_conf) with
        | Some b, Some b' ->
            when_ (b <> b') @@ fun () ->
            failwith
              "`proxy.finalized_view` and `finalized_view` are inconsistent."
        | _ -> return_unit
      in
      let next_wasm_runtime_conf =
        json
        |-> ["experimental_features"; "next_wasm_runtime"]
        |?> Ezjsonm.get_bool
      in
      let* () =
        match next_wasm_runtime_conf with
        | Some false ->
            failwith
              "`experimental_features.next_wasm_runtime` cannot be set to \
               `false` anymore."
        | _ -> return_unit
      in
      let node_transaction_validation_conf =
        json
        |-> ["experimental_features"; "node_transaction_validation"]
        |?> Ezjsonm.get_bool
      in
      let* () =
        match node_transaction_validation_conf with
        | Some false ->
            failwith
              "`experimental_features.node_transaction_validation` cannot be \
               set to `false` anymore."
        | _ -> return_unit
      in
      warn_deprecated
        json
        [
          ["experimental_features"; "next_wasm_runtime"];
          ["experimental_features"; "node_transaction_validation"];
          ["experimental_features"; "history_mode"];
          ["experimental_features"; "garbage_collector_parameters"];
        ] ;

      return_unit)
    (fun _exn -> failwith "Syntax error in the configuration file")

(* Syntactic config patching to migrate stabilized experimental features. *)
let migrate_experimental_feature ?new_path ?(transform = Fun.id) path json =
  let open Json_syntax in
  let new_path = Option.value new_path ~default:path in
  let exp_path = "experimental_features" :: path in
  let value = json |-> new_path in
  let experimental_value = json |-> exp_path in
  match (value, experimental_value) with
  | None, Some v ->
      (* Experimental feature present, but stabilized feature not set. *)
      let json = Ezjsonm.update json exp_path None in
      Ezjsonm.update json new_path (Some (transform v))
  | _, _ -> (* Don't override otherwise *) json

let migrate_stabilized_experimental_features json =
  let open Json_syntax in
  json
  |> migrate_experimental_feature ["history_mode"] ~new_path:["history"]
  |> migrate_experimental_feature
       ["garbage_collector_parameters"]
       ~new_path:["history"]
       ~transform:(fun json ->
         let split_frequency_in_seconds =
           json |->! ["split_frequency_in_seconds"] |> Ezjsonm.get_int
         in
         if split_frequency_in_seconds <> 86400 then
           Stdlib.failwith
             "experimental_features.garbage_collector_parameters.split_frequency_in_seconds \
              must be 86400, use new format." ;
         let nb = json |->! ["number_of_chunks"] in
         `O [("mode", `String "rolling"); ("retention", nb)])

let load_file ?network ~data_dir path =
  let open Lwt_result_syntax in
  let* json = Lwt_utils_unix.Json.read_file path in
  let* () = precheck json in
  let json = migrate_stabilized_experimental_features json in
  let config = Data_encoding.Json.destruct (encoding ?network data_dir) json in
  return config

let load ?network ~data_dir config_file =
  load_file ?network ~data_dir config_file

let error_missing_config ~name = [error_of_fmt "missing %s config" name]

let sequencer_config_exn {sequencer; _} =
  Option.to_result ~none:(error_missing_config ~name:"sequencer") sequencer

let threshold_encryption_sequencer_config_exn
    {threshold_encryption_sequencer; _} =
  Option.to_result
    ~none:(error_missing_config ~name:"threshold_encryption_sequencer")
    threshold_encryption_sequencer

let observer_config_exn {observer; _} =
  Option.to_result ~none:(error_missing_config ~name:"observer") observer

let evm_node_endpoint_resolved network evm_node_endpoint =
  Option.either
    evm_node_endpoint
    (Option.map
       (fun network -> Uri.of_string (observer_evm_node_endpoint network))
       network)

module Cli = struct
  let default ~data_dir ?evm_node_endpoint ?network () =
    let observer =
      Option.map
        (fun evm_node_endpoint -> observer_config_dft ~evm_node_endpoint ())
        (evm_node_endpoint_resolved network evm_node_endpoint)
    in
    let kernel_execution =
      kernel_execution_config_dft
        ~data_dir
        ?preimages_endpoint:(Option.map default_preimages_endpoint network)
        ()
    in
    {
      public_rpc = default_rpc ();
      private_rpc = None;
      log_filter = default_filter_config ();
      kernel_execution;
      sequencer = None;
      threshold_encryption_sequencer = None;
      observer;
      proxy = default_proxy ();
      tx_pool_timeout_limit = default_tx_pool_timeout_limit;
      tx_pool_addr_limit = default_tx_pool_addr_limit;
      tx_pool_tx_per_addr_limit = default_tx_pool_tx_per_addr_limit;
      keep_alive = false;
      rollup_node_endpoint = default_rollup_node_endpoint;
      verbose = Internal_event.Notice;
      experimental_features = default_experimental_features;
      fee_history = default_fee_history;
      finalized_view = default_finalized_view;
      history_mode = None;
    }

  let patch_kernel_execution_config kernel_execution ?preimages
      ?preimages_endpoint ?native_execution_policy () =
    let preimages =
      Option.value preimages ~default:kernel_execution.preimages
    in
    let preimages_endpoint =
      Option.either preimages_endpoint kernel_execution.preimages_endpoint
    in
    let native_execution_policy =
      Option.value
        ~default:kernel_execution.native_execution_policy
        native_execution_policy
    in
    {preimages; preimages_endpoint; native_execution_policy}

  let patch_rpc ?rpc_addr ?rpc_port ?cors_origins ?cors_headers ?batch_limit
      ?restricted_rpcs ?max_active_connections rpc =
    {
      port = Option.value ~default:rpc.port rpc_port;
      addr = Option.value ~default:rpc.addr rpc_addr;
      cors_headers = Option.value ~default:rpc.cors_headers cors_headers;
      cors_origins = Option.value ~default:rpc.cors_origins cors_origins;
      batch_limit = Option.value ~default:rpc.batch_limit batch_limit;
      restricted_rpcs =
        Option.value ~default:rpc.restricted_rpcs restricted_rpcs;
      max_active_connections =
        Option.value ~default:rpc.max_active_connections max_active_connections;
    }

  let patch_configuration_from_args ?rpc_addr ?rpc_port ?rpc_batch_limit
      ?cors_origins ?cors_headers ?tx_pool_timeout_limit ?tx_pool_addr_limit
      ?tx_pool_tx_per_addr_limit ?keep_alive ?rollup_node_endpoint
      ?dont_track_rollup_node ?verbose ?preimages ?preimages_endpoint
      ?native_execution_policy ?time_between_blocks ?max_number_of_chunks
      ?private_rpc_port ?sequencer_key ?evm_node_endpoint
      ?threshold_encryption_bundler_endpoint ?log_filter_max_nb_blocks
      ?log_filter_max_nb_logs ?log_filter_chunk_size ?max_blueprints_lag
      ?max_blueprints_ahead ?max_blueprints_catchup ?catchup_cooldown
      ?sequencer_sidecar_endpoint ?restricted_rpcs ?finalized_view
      ?proxy_ignore_block_param ?history_mode ?dal_slots configuration =
    let public_rpc =
      patch_rpc
        ?rpc_addr
        ?rpc_port
        ?cors_headers
        ?cors_origins
        ?batch_limit:rpc_batch_limit
        ?restricted_rpcs
        configuration.public_rpc
    in
    let private_rpc =
      match configuration.private_rpc with
      | None ->
          Option.map (fun rpc_port -> default_rpc ~rpc_port ()) private_rpc_port
      | Some rpc -> Some (patch_rpc ?rpc_port:private_rpc_port rpc)
    in
    let keep_alive =
      Option.value keep_alive ~default:configuration.keep_alive
    in
    let finalized_view =
      Option.value finalized_view ~default:configuration.finalized_view
    in
    let verbose =
      match verbose with
      | Some true -> Internal_event.Debug
      | _ -> configuration.verbose
    in
    let sequencer =
      let sequencer_config = configuration.sequencer in
      match sequencer_config with
      | Some sequencer_config ->
          let blueprints_publisher_config =
            let blueprints_publisher_config =
              sequencer_config.blueprints_publisher_config
            in
            {
              max_blueprints_lag =
                Option.value
                  ~default:blueprints_publisher_config.max_blueprints_lag
                  max_blueprints_lag;
              max_blueprints_ahead =
                Option.value
                  ~default:blueprints_publisher_config.max_blueprints_ahead
                  max_blueprints_ahead;
              max_blueprints_catchup =
                Option.value
                  ~default:blueprints_publisher_config.max_blueprints_catchup
                  max_blueprints_catchup;
              catchup_cooldown =
                Option.value
                  ~default:blueprints_publisher_config.catchup_cooldown
                  catchup_cooldown;
              dal_slots =
                Option.either dal_slots blueprints_publisher_config.dal_slots;
            }
          in
          Some
            {
              time_between_blocks =
                Option.value
                  ~default:sequencer_config.time_between_blocks
                  time_between_blocks;
              max_number_of_chunks =
                Option.value
                  ~default:sequencer_config.max_number_of_chunks
                  max_number_of_chunks;
              sequencer =
                Option.value ~default:sequencer_config.sequencer sequencer_key;
              blueprints_publisher_config;
            }
      | None ->
          Option.map
            (fun sequencer ->
              sequencer_config_dft
                ?time_between_blocks
                ?max_number_of_chunks
                ?max_blueprints_lag
                ?max_blueprints_ahead
                ?max_blueprints_catchup
                ?catchup_cooldown
                ~sequencer
                ?dal_slots
                ())
            sequencer_key
    in
    let threshold_encryption_sequencer =
      let threshold_encryption_sequencer_config =
        configuration.threshold_encryption_sequencer
      in
      match threshold_encryption_sequencer_config with
      | Some
          (Threshold_encryption_sequencer threshold_encryption_sequencer_config)
        ->
          let blueprints_publisher_config =
            let blueprints_publisher_config =
              threshold_encryption_sequencer_config.blueprints_publisher_config
            in
            {
              max_blueprints_lag =
                Option.value
                  ~default:blueprints_publisher_config.max_blueprints_lag
                  max_blueprints_lag;
              max_blueprints_ahead =
                Option.value
                  ~default:blueprints_publisher_config.max_blueprints_ahead
                  max_blueprints_ahead;
              max_blueprints_catchup =
                Option.value
                  ~default:blueprints_publisher_config.max_blueprints_catchup
                  max_blueprints_catchup;
              catchup_cooldown =
                Option.value
                  ~default:blueprints_publisher_config.catchup_cooldown
                  catchup_cooldown;
              dal_slots =
                Option.either dal_slots blueprints_publisher_config.dal_slots;
            }
          in
          Some
            (Threshold_encryption_sequencer
               {
                 time_between_blocks =
                   Option.value
                     ~default:
                       threshold_encryption_sequencer_config.time_between_blocks
                     time_between_blocks;
                 max_number_of_chunks =
                   Option.value
                     ~default:
                       threshold_encryption_sequencer_config
                         .max_number_of_chunks
                     max_number_of_chunks;
                 sequencer =
                   Option.value
                     ~default:threshold_encryption_sequencer_config.sequencer
                     sequencer_key;
                 blueprints_publisher_config;
                 sidecar_endpoint =
                   Option.value
                     ~default:
                       threshold_encryption_sequencer_config.sidecar_endpoint
                     sequencer_sidecar_endpoint;
               })
      | None ->
          Option.map
            (fun sequencer ->
              threshold_encryption_sequencer_config_dft
                ?time_between_blocks
                ?max_number_of_chunks
                ?max_blueprints_lag
                ?max_blueprints_ahead
                ?max_blueprints_catchup
                ?catchup_cooldown
                ~sequencer
                ?sidecar_endpoint:sequencer_sidecar_endpoint
                ?dal_slots
                ())
            sequencer_key
    in
    let observer =
      match configuration.observer with
      | Some observer_config ->
          Some
            {
              evm_node_endpoint =
                Option.value
                  ~default:observer_config.evm_node_endpoint
                  evm_node_endpoint;
              threshold_encryption_bundler_endpoint =
                (match threshold_encryption_bundler_endpoint with
                | None -> observer_config.threshold_encryption_bundler_endpoint
                | endpoint -> endpoint);
              rollup_node_tracking =
                Option.(
                  value
                    ~default:observer_config.rollup_node_tracking
                    (map not dont_track_rollup_node));
            }
      | None ->
          Option.map
            (fun evm_node_endpoint ->
              observer_config_dft
                ~evm_node_endpoint
                ?threshold_encryption_bundler_endpoint
                ?rollup_node_tracking:(Option.map not dont_track_rollup_node)
                ())
            evm_node_endpoint
    in
    let proxy =
      {
        evm_node_endpoint =
          Option.either evm_node_endpoint configuration.proxy.evm_node_endpoint;
        ignore_block_param =
          Option.value
            ~default:configuration.proxy.ignore_block_param
            proxy_ignore_block_param;
        finalized_view =
          (if finalized_view then Some true
           else configuration.proxy.finalized_view);
      }
    in
    let log_filter =
      {
        max_nb_blocks =
          Option.value
            ~default:configuration.log_filter.max_nb_blocks
            log_filter_max_nb_blocks;
        max_nb_logs =
          Option.value
            ~default:configuration.log_filter.max_nb_logs
            log_filter_max_nb_logs;
        chunk_size =
          Option.value
            ~default:configuration.log_filter.chunk_size
            log_filter_chunk_size;
      }
    in
    let kernel_execution =
      patch_kernel_execution_config
        configuration.kernel_execution
        ?preimages
        ?preimages_endpoint
        ?native_execution_policy
        ()
    in
    let rollup_node_endpoint =
      Option.value
        ~default:configuration.rollup_node_endpoint
        rollup_node_endpoint
    in

    {
      public_rpc;
      private_rpc;
      log_filter;
      kernel_execution;
      sequencer;
      threshold_encryption_sequencer;
      observer;
      proxy;
      tx_pool_timeout_limit =
        Option.value
          ~default:configuration.tx_pool_timeout_limit
          tx_pool_timeout_limit;
      tx_pool_addr_limit =
        Option.value
          ~default:configuration.tx_pool_addr_limit
          tx_pool_addr_limit;
      tx_pool_tx_per_addr_limit =
        Option.value
          ~default:configuration.tx_pool_tx_per_addr_limit
          tx_pool_tx_per_addr_limit;
      keep_alive = configuration.keep_alive || keep_alive;
      rollup_node_endpoint;
      verbose;
      experimental_features = configuration.experimental_features;
      fee_history = configuration.fee_history;
      finalized_view = finalized_view || configuration.finalized_view;
      history_mode = Option.either history_mode configuration.history_mode;
    }

  let create ~data_dir ?rpc_addr ?rpc_port ?rpc_batch_limit ?cors_origins
      ?cors_headers ?tx_pool_timeout_limit ?tx_pool_addr_limit
      ?tx_pool_tx_per_addr_limit ?keep_alive ?rollup_node_endpoint
      ?dont_track_rollup_node ?verbose ?preimages ?preimages_endpoint
      ?native_execution_policy ?time_between_blocks ?max_number_of_chunks
      ?private_rpc_port ?sequencer_key ?evm_node_endpoint
      ?threshold_encryption_bundler_endpoint ?log_filter_max_nb_blocks
      ?log_filter_max_nb_logs ?log_filter_chunk_size ?max_blueprints_lag
      ?max_blueprints_ahead ?max_blueprints_catchup ?catchup_cooldown
      ?sequencer_sidecar_endpoint ?restricted_rpcs ?finalized_view
      ?proxy_ignore_block_param ?dal_slots ?network ?history_mode () =
    default ~data_dir ?network ?evm_node_endpoint ()
    |> patch_configuration_from_args
         ?rpc_addr
         ?rpc_port
         ?rpc_batch_limit
         ?cors_origins
         ?cors_headers
         ?tx_pool_timeout_limit
         ?tx_pool_addr_limit
         ?tx_pool_tx_per_addr_limit
         ?keep_alive
         ?rollup_node_endpoint
         ?dont_track_rollup_node
         ?verbose
         ?preimages
         ?preimages_endpoint
         ?native_execution_policy
         ?time_between_blocks
         ?max_number_of_chunks
         ?private_rpc_port
         ?sequencer_key
         ?evm_node_endpoint
         ?threshold_encryption_bundler_endpoint
         ?log_filter_max_nb_blocks
         ?log_filter_max_nb_logs
         ?log_filter_chunk_size
         ?max_blueprints_lag
         ?max_blueprints_ahead
         ?max_blueprints_catchup
         ?catchup_cooldown
         ?sequencer_sidecar_endpoint
         ?restricted_rpcs
         ?finalized_view
         ?proxy_ignore_block_param
         ?dal_slots
         ?history_mode

  let create_or_read_config ~data_dir ?rpc_addr ?rpc_port ?rpc_batch_limit
      ?cors_origins ?cors_headers ?tx_pool_timeout_limit ?tx_pool_addr_limit
      ?tx_pool_tx_per_addr_limit ?keep_alive ?rollup_node_endpoint
      ?dont_track_rollup_node ?verbose ?preimages ?preimages_endpoint
      ?native_execution_policy ?time_between_blocks ?max_number_of_chunks
      ?private_rpc_port ?sequencer_key ?evm_node_endpoint
      ?threshold_encryption_bundler_endpoint ?max_blueprints_lag
      ?max_blueprints_ahead ?max_blueprints_catchup ?catchup_cooldown
      ?log_filter_max_nb_blocks ?log_filter_max_nb_logs ?log_filter_chunk_size
      ?sequencer_sidecar_endpoint ?restricted_rpcs ?finalized_view
      ?proxy_ignore_block_param ?dal_slots ?network ?history_mode config_file =
    let open Lwt_result_syntax in
    let open Filename.Infix in
    (* Check if the data directory of the evm node is not the one of Octez
       node *)
    let* () =
      let*! identity_file_in_data_dir_exists =
        Lwt_unix.file_exists (data_dir // "identity.json")
      in
      if identity_file_in_data_dir_exists then
        failwith
          "Invalid data directory. This is a data directory for an Octez node, \
           please choose a different directory for the EVM node data."
      else return_unit
    in
    let*! exists_config = Lwt_unix.file_exists config_file in
    if exists_config then
      (* Read configuration from file and patch if user wanted to override
         some fields with values provided by arguments. *)
      let* configuration = load ?network ~data_dir config_file in
      let configuration =
        patch_configuration_from_args
          ?rpc_addr
          ?rpc_port
          ?rpc_batch_limit
          ?cors_origins
          ?cors_headers
          ?keep_alive
          ?sequencer_key
          ?evm_node_endpoint
          ?threshold_encryption_bundler_endpoint
          ?preimages
          ?preimages_endpoint
          ?native_execution_policy
          ?time_between_blocks
          ?max_number_of_chunks
          ?private_rpc_port
          ?max_blueprints_lag
          ?max_blueprints_ahead
          ?max_blueprints_catchup
          ?catchup_cooldown
          ?tx_pool_timeout_limit
          ?tx_pool_addr_limit
          ?tx_pool_tx_per_addr_limit
          ?rollup_node_endpoint
          ?dont_track_rollup_node
          ?verbose
          ?log_filter_max_nb_blocks
          ?log_filter_max_nb_logs
          ?log_filter_chunk_size
          ?sequencer_sidecar_endpoint
          ?restricted_rpcs
          ?finalized_view
          ?proxy_ignore_block_param
          ?history_mode
          ?dal_slots
          configuration
      in
      return configuration
    else
      let config =
        create
          ~data_dir
          ?rpc_addr
          ?rpc_port
          ?rpc_batch_limit
          ?cors_origins
          ?cors_headers
          ?keep_alive
          ?sequencer_key
          ?evm_node_endpoint
          ?threshold_encryption_bundler_endpoint
          ?preimages
          ?preimages_endpoint
          ?native_execution_policy
          ?time_between_blocks
          ?max_number_of_chunks
          ?private_rpc_port
          ?max_blueprints_lag
          ?max_blueprints_ahead
          ?max_blueprints_catchup
          ?catchup_cooldown
          ?tx_pool_timeout_limit
          ?tx_pool_addr_limit
          ?tx_pool_tx_per_addr_limit
          ?rollup_node_endpoint
          ?dont_track_rollup_node
          ?verbose
          ?log_filter_max_nb_blocks
          ?log_filter_max_nb_logs
          ?log_filter_chunk_size
          ?sequencer_sidecar_endpoint
          ?restricted_rpcs
          ?finalized_view
          ?proxy_ignore_block_param
          ?dal_slots
          ?network
          ?history_mode
          ()
      in
      return config
end

let describe () =
  Data_encoding.Json.schema (encoding "DATA_DIR_PATH")
  |> Format.printf "%a" Json_schema.pp
