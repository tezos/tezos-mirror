(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

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

type kernel_execution_config = {
  preimages : string;
  preimages_endpoint : Uri.t option;
}

type experimental_features = {
  drop_duplicate_on_injection : bool;
  enable_send_raw_transaction : bool;
  node_transaction_validation : bool;
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
  finalized_view : bool;
  evm_node_endpoint : Uri.t option;
  ignore_block_param : bool;
}

type fee_history = {max_count : int option; max_past : int option}

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
}

let default_filter_config ?max_nb_blocks ?max_nb_logs ?chunk_size () =
  {
    max_nb_blocks = Option.value ~default:100 max_nb_blocks;
    max_nb_logs = Option.value ~default:1000 max_nb_logs;
    chunk_size = Option.value ~default:10 chunk_size;
  }

let default_enable_send_raw_transaction = true

let default_experimental_features =
  {
    enable_send_raw_transaction = default_enable_send_raw_transaction;
    drop_duplicate_on_injection = false;
    node_transaction_validation = false;
  }

let default_data_dir = Filename.concat (Sys.getenv "HOME") ".octez-evm-node"

let config_filename ~data_dir = Filename.concat data_dir "config.json"

let default_rpc_addr = "127.0.0.1"

let default_rpc_port = 8545

let default_sequencer_sidecar_endpoint = Uri.of_string "127.0.0.1:5303"

let default_keep_alive = false

let default_rollup_node_endpoint = Uri.of_string "http://localhost:8937"

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

let default_blueprints_publisher_config =
  {
    max_blueprints_lag = 50;
    max_blueprints_ahead = 100;
    max_blueprints_catchup = 50;
    catchup_cooldown = 1;
    dal_slots = None;
  }

let default_fee_history = {max_count = None; max_past = None}

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

let kernel_execution_config_dft ~data_dir ?preimages ?preimages_endpoint () =
  {
    preimages = Option.value ~default:(default_preimages data_dir) preimages;
    preimages_endpoint;
  }

let sequencer_config_dft ?time_between_blocks ?max_number_of_chunks ~sequencer
    ?max_blueprints_lag ?max_blueprints_ahead ?max_blueprints_catchup
    ?catchup_cooldown ?dal_slots () =
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

let observer_encoding =
  let open Data_encoding in
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
       (req
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

let experimental_features_encoding =
  let open Data_encoding in
  conv
    (fun {
           drop_duplicate_on_injection;
           enable_send_raw_transaction;
           node_transaction_validation;
         } ->
      ( drop_duplicate_on_injection,
        enable_send_raw_transaction,
        node_transaction_validation ))
    (fun ( drop_duplicate_on_injection,
           enable_send_raw_transaction,
           node_transaction_validation ) ->
      {
        drop_duplicate_on_injection;
        enable_send_raw_transaction;
        node_transaction_validation;
      })
    (obj3
       (dft
          ~description:
            "Request the rollup node to filter messages it has already \
             forwarded to the Layer 1 network. Require an unreleased version \
             of the Smart Rollup node."
          "drop_duplicate_on_injection"
          bool
          false)
       (dft
          ~description:
            "Enable or disable the `eth_sendRawTransaction` method. \
             DEPRECATED:  You should use \"rpc.restricted_rpcs\" instead."
          "enable_send_raw_transaction"
          bool
          default_enable_send_raw_transaction)
       (dft "node_transaction_validation" bool false))

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
       (dft "finalized_view" bool false)
       (opt "evm_node_endpoint" string)
       (dft "ignore_block_param" bool false)

let default_proxy ?(finalized_view = false) ?evm_node_endpoint
    ?(ignore_block_param = false) () =
  {finalized_view; evm_node_endpoint; ignore_block_param}

let fee_history_encoding =
  let open Data_encoding in
  conv
    (fun {max_count; max_past} -> (max_count, max_past))
    (fun (max_count, max_past) -> {max_count; max_past})
    (obj2
       (opt
          ~description:
            "The maximum number of blocks whose fee history can be retrieved \
             at once"
          "max_count"
          strictly_positive_encoding)
       (opt
          ~description:
            "The maximum number of blocks in the past where the fee history is \
             available"
          "max_past"
          strictly_positive_encoding))

let kernel_execution_encoding data_dir =
  Data_encoding.(
    conv
      (fun {preimages; preimages_endpoint} -> (preimages, preimages_endpoint))
      (fun (preimages, preimages_endpoint) -> {preimages; preimages_endpoint})
      (obj2
         (dft
            ~description:
              "Path to a directory containing the preimages the kernel can \
               reveal."
            "preimages"
            string
            (default_preimages data_dir))
         (opt
            ~description:
              "Endpoint for downloading the preimages that cannot be found in \
               the preimages directory. These preimages are downloaded by the \
               node, stored in the preimages directory and fed to the kernel."
            "preimages_endpoint"
            Tezos_rpc.Encoding.uri_encoding)))

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

let encoding data_dir : t Data_encoding.t =
  let open Data_encoding in
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
          (kernel_execution, public_rpc, private_rpc) ) ))
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
             (kernel_execution, public_rpc, private_rpc) ) ) ->
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
          (opt "observer" observer_encoding))
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
          (obj3
             (dft
                "kernel_execution"
                (kernel_execution_encoding data_dir)
                (kernel_execution_config_dft ~data_dir ()))
             (dft "public_rpc" rpc_encoding (default_rpc ()))
             (opt "private_rpc" rpc_encoding))))

let pp_print_json ~data_dir fmt config =
  let json =
    Data_encoding.Json.construct
      ~include_default_fields:`Always
      (encoding data_dir)
      config
  in
  Data_encoding.Json.pp fmt json

let save ~force ~data_dir config =
  let open Lwt_result_syntax in
  let json = Data_encoding.Json.construct (encoding data_dir) config in
  let config_file = config_filename ~data_dir in
  let*! exists = Lwt_unix.file_exists config_file in
  if exists && not force then
    failwith
      "Configuration file %S already exists. Use --force to overwrite."
      config_file
  else
    let*! () = Lwt_utils_unix.create_dir data_dir in
    Lwt_utils_unix.Json.write_file config_file json

let load_file ~data_dir path =
  let open Lwt_result_syntax in
  let+ json = Lwt_utils_unix.Json.read_file path in
  let config = Data_encoding.Json.destruct (encoding data_dir) json in
  config

let load ~data_dir = load_file ~data_dir (config_filename ~data_dir)

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

module Cli = struct
  let create ~data_dir ?rpc_addr ?rpc_port ?rpc_batch_limit ?cors_origins
      ?cors_headers ?tx_pool_timeout_limit ?tx_pool_addr_limit
      ?tx_pool_tx_per_addr_limit ~keep_alive ?rollup_node_endpoint
      ?dont_track_rollup_node ~verbose ?preimages ?preimages_endpoint
      ?time_between_blocks ?max_number_of_chunks ?private_rpc_port
      ?sequencer_key ?evm_node_endpoint ?threshold_encryption_bundler_endpoint
      ?log_filter_max_nb_blocks ?log_filter_max_nb_logs ?log_filter_chunk_size
      ?max_blueprints_lag ?max_blueprints_ahead ?max_blueprints_catchup
      ?catchup_cooldown ?sequencer_sidecar_endpoint ?restricted_rpcs
      ?proxy_finalized_view ?proxy_ignore_block_param ?dal_slots () =
    let public_rpc =
      default_rpc
        ?rpc_port
        ?rpc_addr
        ?batch_limit:rpc_batch_limit
        ?cors_origins
        ?cors_headers
        ?restricted_rpcs
        ()
    in
    let private_rpc =
      Option.map (fun port -> default_rpc ~rpc_port:port ()) private_rpc_port
    in
    let sequencer =
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
      Option.map
        (fun evm_node_endpoint ->
          let rollup_node_tracking = Option.map not dont_track_rollup_node in
          observer_config_dft
            ~evm_node_endpoint
            ?threshold_encryption_bundler_endpoint
            ?rollup_node_tracking
            ())
        evm_node_endpoint
    in
    let proxy =
      default_proxy
        ?finalized_view:proxy_finalized_view
        ?evm_node_endpoint
        ?ignore_block_param:proxy_ignore_block_param
        ()
    in
    let log_filter =
      default_filter_config
        ?max_nb_blocks:log_filter_max_nb_blocks
        ?max_nb_logs:log_filter_max_nb_logs
        ?chunk_size:log_filter_chunk_size
        ()
    in
    let rollup_node_endpoint =
      Option.value ~default:default_rollup_node_endpoint rollup_node_endpoint
    in
    let kernel_execution =
      kernel_execution_config_dft ~data_dir ?preimages ?preimages_endpoint ()
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
          ~default:default_tx_pool_timeout_limit
          tx_pool_timeout_limit;
      tx_pool_addr_limit =
        Option.value ~default:default_tx_pool_addr_limit tx_pool_addr_limit;
      tx_pool_tx_per_addr_limit =
        Option.value
          ~default:default_tx_pool_tx_per_addr_limit
          tx_pool_tx_per_addr_limit;
      keep_alive;
      rollup_node_endpoint;
      verbose = (if verbose then Debug else Internal_event.Notice);
      experimental_features = default_experimental_features;
      fee_history = default_fee_history;
    }

  let patch_kernel_execution_config kernel_execution ?preimages
      ?preimages_endpoint () =
    let preimages =
      Option.value preimages ~default:kernel_execution.preimages
    in
    let preimages_endpoint =
      Option.either preimages_endpoint kernel_execution.preimages_endpoint
    in
    {preimages; preimages_endpoint}

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
      ?tx_pool_tx_per_addr_limit ~keep_alive ?rollup_node_endpoint
      ?dont_track_rollup_node ~verbose ?preimages ?preimages_endpoint
      ?time_between_blocks ?max_number_of_chunks ?private_rpc_port
      ?sequencer_key ?evm_node_endpoint ?threshold_encryption_bundler_endpoint
      ?log_filter_max_nb_blocks ?log_filter_max_nb_logs ?log_filter_chunk_size
      ?max_blueprints_lag ?max_blueprints_ahead ?max_blueprints_catchup
      ?catchup_cooldown ?sequencer_sidecar_endpoint ?restricted_rpcs
      ?proxy_finalized_view ?proxy_ignore_block_param ~dal_slots configuration =
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
      Option.map
        (patch_rpc ?rpc_port:private_rpc_port)
        configuration.private_rpc
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
      default_proxy
        ?finalized_view:proxy_finalized_view
        ?evm_node_endpoint
        ?ignore_block_param:proxy_ignore_block_param
        ()
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
      verbose = (if verbose then Debug else configuration.verbose);
      experimental_features = configuration.experimental_features;
      fee_history = configuration.fee_history;
    }

  let create_or_read_config ~data_dir ?rpc_addr ?rpc_port ?rpc_batch_limit
      ?cors_origins ?cors_headers ?tx_pool_timeout_limit ?tx_pool_addr_limit
      ?tx_pool_tx_per_addr_limit ~keep_alive ?rollup_node_endpoint
      ?dont_track_rollup_node ~verbose ?preimages ?preimages_endpoint
      ?time_between_blocks ?max_number_of_chunks ?private_rpc_port
      ?sequencer_key ?evm_node_endpoint ?threshold_encryption_bundler_endpoint
      ?max_blueprints_lag ?max_blueprints_ahead ?max_blueprints_catchup
      ?catchup_cooldown ?log_filter_max_nb_blocks ?log_filter_max_nb_logs
      ?log_filter_chunk_size ?sequencer_sidecar_endpoint ?restricted_rpcs
      ?proxy_finalized_view ?proxy_ignore_block_param ?dal_slots () =
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
    let config_file = config_filename ~data_dir in
    let*! exists_config = Lwt_unix.file_exists config_file in
    if exists_config then
      (* Read configuration from file and patch if user wanted to override
         some fields with values provided by arguments. *)
      let* configuration = load ~data_dir in
      let configuration =
        patch_configuration_from_args
          ?rpc_addr
          ?rpc_port
          ?rpc_batch_limit
          ?cors_origins
          ?cors_headers
          ~keep_alive
          ?sequencer_key
          ?evm_node_endpoint
          ?threshold_encryption_bundler_endpoint
          ?preimages
          ?preimages_endpoint
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
          ~verbose
          ?log_filter_max_nb_blocks
          ?log_filter_max_nb_logs
          ?log_filter_chunk_size
          ?sequencer_sidecar_endpoint
          ?restricted_rpcs
          ?proxy_finalized_view
          ?proxy_ignore_block_param
          ~dal_slots
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
          ~keep_alive
          ?sequencer_key
          ?evm_node_endpoint
          ?threshold_encryption_bundler_endpoint
          ?preimages
          ?preimages_endpoint
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
          ~verbose
          ?log_filter_max_nb_blocks
          ?log_filter_max_nb_logs
          ?log_filter_chunk_size
          ?sequencer_sidecar_endpoint
          ?restricted_rpcs
          ?proxy_finalized_view
          ?proxy_ignore_block_param
          ?dal_slots
          ()
      in
      return config
end

let describe () =
  Data_encoding.Json.schema (encoding "DATA_DIR_PATH")
  |> Format.printf "%a" Json_schema.pp
