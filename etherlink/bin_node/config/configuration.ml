(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

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

type experimental_features = {
  drop_duplicate_on_injection : bool;
  enable_send_raw_transaction : bool;
  node_transaction_validation : bool;
}

type sequencer = {
  preimages : string;
  preimages_endpoint : Uri.t option;
  time_between_blocks : time_between_blocks;
  max_number_of_chunks : int;
  private_rpc_port : int option;
  sequencer : Client_keys.sk_uri;
  blueprints_publisher_config : blueprints_publisher_config;
}

(* Variant is needed to avoid type-checking errors. *)
type threshold_encryption_sequencer =
  | Threshold_encryption_sequencer of {
      preimages : string;
      preimages_endpoint : Uri.t option;
      time_between_blocks : time_between_blocks;
      max_number_of_chunks : int;
      private_rpc_port : int option;
      sequencer : Client_keys.sk_uri;
      blueprints_publisher_config : blueprints_publisher_config;
      sidecar_endpoint : Uri.t;
    }

type observer = {
  evm_node_endpoint : Uri.t;
  threshold_encryption_bundler_endpoint : Uri.t option;
  preimages : string;
  preimages_endpoint : Uri.t option;
}

type proxy = {finalized_view : bool}

type fee_history = {max_count : int option; max_past : int option}

(* The regular expression is compiled at the launch of the node, and encoded
   into its raw form. *)
type restricted_rpcs = {raw : string; regex : Re.re}

type t = {
  rpc_addr : string;
  rpc_port : int;
  cors_origins : string list;
  cors_headers : string list;
  log_filter : log_filter_config;
  sequencer : sequencer option;
  threshold_encryption_sequencer : threshold_encryption_sequencer option;
  observer : observer option;
  proxy : proxy;
  max_active_connections :
    Tezos_rpc_http_server.RPC_server.Max_active_rpc_connections.t;
  tx_pool_timeout_limit : int64;
  tx_pool_addr_limit : int64;
  tx_pool_tx_per_addr_limit : int64;
  keep_alive : bool;
  rollup_node_endpoint : Uri.t;
  verbose : Internal_event.level;
  experimental_features : experimental_features;
  fee_history : fee_history;
  restricted_rpcs : restricted_rpcs option;
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

let default_cors_origins = []

let default_cors_headers = []

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

let make_restricted_rpcs raw = {raw; regex = Re.Perl.compile_pat raw}

let sequencer_config_dft ~data_dir ?preimages ?preimages_endpoint
    ?time_between_blocks ?max_number_of_chunks ?private_rpc_port ~sequencer
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
    preimages = Option.value ~default:(default_preimages data_dir) preimages;
    preimages_endpoint;
    time_between_blocks =
      Option.value ~default:default_time_between_blocks time_between_blocks;
    max_number_of_chunks =
      Option.value ~default:default_max_number_of_chunks max_number_of_chunks;
    private_rpc_port;
    sequencer;
    blueprints_publisher_config;
  }

let threshold_encryption_sequencer_config_dft ~data_dir ?preimages
    ?preimages_endpoint ?time_between_blocks ?max_number_of_chunks
    ?private_rpc_port ~sequencer ?sidecar_endpoint ?max_blueprints_lag
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
      preimages = Option.value ~default:(default_preimages data_dir) preimages;
      preimages_endpoint;
      time_between_blocks =
        Option.value ~default:default_time_between_blocks time_between_blocks;
      max_number_of_chunks =
        Option.value ~default:default_max_number_of_chunks max_number_of_chunks;
      private_rpc_port;
      sequencer;
      blueprints_publisher_config;
      sidecar_endpoint =
        Option.value
          ~default:default_sequencer_sidecar_endpoint
          sidecar_endpoint;
    }

let observer_config_dft ~data_dir ?preimages ?preimages_endpoint
    ~evm_node_endpoint ?threshold_encryption_bundler_endpoint () =
  {
    evm_node_endpoint;
    threshold_encryption_bundler_endpoint;
    preimages = Option.value ~default:(default_preimages data_dir) preimages;
    preimages_endpoint;
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
       (dft "max_nb_blocks" int31 default_filter_config.max_nb_blocks)
       (dft "max_nb_logs" int31 default_filter_config.max_nb_logs)
       (dft "chunk_size" int31 default_filter_config.chunk_size))

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
          "max_blueprints_lag"
          int31
          default_blueprints_publisher_config.max_blueprints_lag)
       (dft
          "max_blueprints_ahead"
          int31
          default_blueprints_publisher_config.max_blueprints_ahead)
       (dft
          "max_blueprints_catchup"
          int31
          default_blueprints_publisher_config.max_blueprints_catchup)
       (dft
          "catchup_cooldown"
          int31
          default_blueprints_publisher_config.catchup_cooldown)
       (opt "dal_slots" (list int8)))

let sequencer_encoding data_dir =
  let open Data_encoding in
  conv
    (fun {
           preimages;
           preimages_endpoint;
           time_between_blocks;
           max_number_of_chunks;
           private_rpc_port;
           sequencer;
           blueprints_publisher_config;
         } ->
      ( preimages,
        preimages_endpoint,
        time_between_blocks,
        max_number_of_chunks,
        private_rpc_port,
        Client_keys.string_of_sk_uri sequencer,
        blueprints_publisher_config ))
    (fun ( preimages,
           preimages_endpoint,
           time_between_blocks,
           max_number_of_chunks,
           private_rpc_port,
           sequencer,
           blueprints_publisher_config ) ->
      {
        preimages;
        preimages_endpoint;
        time_between_blocks;
        max_number_of_chunks;
        private_rpc_port;
        sequencer = Client_keys.sk_uri_of_string sequencer;
        blueprints_publisher_config;
      })
    (obj7
       (dft "preimages" string (default_preimages data_dir))
       (opt "preimages_endpoint" Tezos_rpc.Encoding.uri_encoding)
       (dft
          "time_between_blocks"
          time_between_blocks_encoding
          default_time_between_blocks)
       (dft "max_number_of_chunks" int31 default_max_number_of_chunks)
       (opt
          "private-rpc-port"
          ~description:"RPC port for private server"
          uint16)
       (req "sequencer" string)
       (dft
          "blueprints_publisher_config"
          blueprints_publisher_config_encoding
          default_blueprints_publisher_config))

let threshold_encryption_sequencer_encoding data_dir =
  let open Data_encoding in
  conv
    (function
      | Threshold_encryption_sequencer
          {
            preimages;
            preimages_endpoint;
            time_between_blocks;
            max_number_of_chunks;
            private_rpc_port;
            sequencer;
            blueprints_publisher_config;
            sidecar_endpoint;
          } ->
          ( preimages,
            preimages_endpoint,
            time_between_blocks,
            max_number_of_chunks,
            private_rpc_port,
            Client_keys.string_of_sk_uri sequencer,
            blueprints_publisher_config,
            sidecar_endpoint ))
    (fun ( preimages,
           preimages_endpoint,
           time_between_blocks,
           max_number_of_chunks,
           private_rpc_port,
           sequencer,
           blueprints_publisher_config,
           sidecar_endpoint ) ->
      Threshold_encryption_sequencer
        {
          preimages;
          preimages_endpoint;
          time_between_blocks;
          max_number_of_chunks;
          private_rpc_port;
          sequencer = Client_keys.sk_uri_of_string sequencer;
          blueprints_publisher_config;
          sidecar_endpoint;
        })
    (obj8
       (dft "preimages" string (default_preimages data_dir))
       (opt "preimages_endpoint" Tezos_rpc.Encoding.uri_encoding)
       (dft
          "time_between_blocks"
          time_between_blocks_encoding
          default_time_between_blocks)
       (dft "max_number_of_chunks" int31 default_max_number_of_chunks)
       (opt
          "private-rpc-port"
          ~description:"RPC port for private server"
          uint16)
       (req "sequencer" string)
       (dft
          "blueprints_publisher_config"
          blueprints_publisher_config_encoding
          default_blueprints_publisher_config)
       (dft
          "sidecar_endpoint"
          Tezos_rpc.Encoding.uri_encoding
          default_sequencer_sidecar_endpoint))

let observer_encoding data_dir =
  let open Data_encoding in
  conv
    (fun {
           preimages;
           preimages_endpoint;
           evm_node_endpoint;
           threshold_encryption_bundler_endpoint;
         } ->
      ( preimages,
        preimages_endpoint,
        Uri.to_string evm_node_endpoint,
        threshold_encryption_bundler_endpoint,
        None ))
    (fun ( preimages,
           preimages_endpoint,
           evm_node_endpoint,
           threshold_encryption_bundler_endpoint,
           _ ) ->
      {
        preimages;
        preimages_endpoint;
        evm_node_endpoint = Uri.of_string evm_node_endpoint;
        threshold_encryption_bundler_endpoint;
      })
    (obj5
       (dft "preimages" string (default_preimages data_dir))
       (opt "preimages_endpoint" Tezos_rpc.Encoding.uri_encoding)
       (req "evm_node_endpoint" string)
       (opt
          "threshold_encryption_bundler_endpoint"
          Tezos_rpc.Encoding.uri_encoding)
       (opt
          "time_between_blocks"
          ~description:"Deprecated field, value is ignored"
          Json.encoding))

let experimental_features_encoding =
  let open Data_encoding in
  conv
    (fun {
           drop_duplicate_on_injection;
           enable_send_raw_transaction;
           node_transaction_validation;
         } ->
      ( None,
        drop_duplicate_on_injection,
        enable_send_raw_transaction,
        node_transaction_validation ))
    (fun ( _,
           drop_duplicate_on_injection,
           enable_send_raw_transaction,
           node_transaction_validation ) ->
      {
        drop_duplicate_on_injection;
        enable_send_raw_transaction;
        node_transaction_validation;
      })
    (obj4
       (* `sqlite_journal_mode` field is kept for now for backward compatibility. *)
       (opt "sqlite_journal_mode" Json.encoding)
       (dft "drop_duplicate_on_injection" bool false)
       (dft
          "enable_send_raw_transaction"
          bool
          default_enable_send_raw_transaction)
       (dft "node_transaction_validation" bool false))

let proxy_encoding =
  let open Data_encoding in
  conv
    (fun {finalized_view} -> finalized_view)
    (fun finalized_view -> {finalized_view})
  @@ obj1 (dft "finalized_view" bool false)

let default_proxy ?(finalized_view = false) () = {finalized_view}

let fee_history_encoding =
  let open Data_encoding in
  conv
    (fun {max_count; max_past} -> (max_count, max_past))
    (fun (max_count, max_past) -> {max_count; max_past})
    (obj2 (opt "max_count" int31) (opt "max_past" int31))

let encoding data_dir : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           rpc_addr;
           rpc_port;
           cors_origins;
           cors_headers;
           log_filter;
           sequencer;
           threshold_encryption_sequencer;
           observer;
           proxy;
           max_active_connections;
           tx_pool_timeout_limit;
           tx_pool_addr_limit;
           tx_pool_tx_per_addr_limit;
           keep_alive;
           rollup_node_endpoint;
           verbose;
           experimental_features;
           fee_history;
           restricted_rpcs;
         } ->
      ( ( rpc_addr,
          rpc_port,
          None
          (* devmode is still part of the encoding for compatibiltiy reasons. *),
          cors_origins,
          cors_headers,
          log_filter,
          sequencer,
          threshold_encryption_sequencer,
          observer,
          max_active_connections ),
        ( tx_pool_timeout_limit,
          tx_pool_addr_limit,
          tx_pool_tx_per_addr_limit,
          keep_alive,
          Uri.to_string rollup_node_endpoint,
          verbose,
          experimental_features,
          proxy,
          fee_history,
          Option.map (fun {raw; _} -> raw) restricted_rpcs ) ))
    (fun ( ( rpc_addr,
             rpc_port,
             _devmode,
             cors_origins,
             cors_headers,
             log_filter,
             sequencer,
             threshold_encryption_sequencer,
             observer,
             max_active_connections ),
           ( tx_pool_timeout_limit,
             tx_pool_addr_limit,
             tx_pool_tx_per_addr_limit,
             keep_alive,
             rollup_node_endpoint,
             verbose,
             experimental_features,
             proxy,
             fee_history,
             restricted_rpcs ) ) ->
      {
        rpc_addr;
        rpc_port;
        cors_origins;
        cors_headers;
        log_filter;
        sequencer;
        threshold_encryption_sequencer;
        observer;
        proxy;
        max_active_connections;
        tx_pool_timeout_limit;
        tx_pool_addr_limit;
        tx_pool_tx_per_addr_limit;
        keep_alive;
        rollup_node_endpoint = Uri.of_string rollup_node_endpoint;
        verbose;
        experimental_features;
        fee_history;
        restricted_rpcs = Option.map make_restricted_rpcs restricted_rpcs;
      })
    (merge_objs
       (obj10
          (dft "rpc-addr" ~description:"RPC address" string default_rpc_addr)
          (dft "rpc-port" ~description:"RPC port" uint16 default_rpc_port)
          (opt ~description:"DEPRECATED" "devmode" bool)
          (dft "cors_origins" (list string) default_cors_origins)
          (dft "cors_headers" (list string) default_cors_headers)
          (dft
             "log_filter"
             log_filter_config_encoding
             (default_filter_config ()))
          (opt "sequencer" (sequencer_encoding data_dir))
          (opt
             "threshold_encryption_sequencer"
             (threshold_encryption_sequencer_encoding data_dir))
          (opt "observer" (observer_encoding data_dir))
          (dft
             "max_active_connections"
             Tezos_rpc_http_server.RPC_server.Max_active_rpc_connections
             .encoding
             default_max_active_connections))
       (obj10
          (dft
             "tx-pool-timeout-limit"
             ~description:
               "Transaction timeout limit inside the transaction pool"
             int64
             default_tx_pool_timeout_limit)
          (dft
             "tx-pool-addr-limit"
             ~description:
               "Maximum allowed addresses inside the transaction pool."
             int64
             default_tx_pool_addr_limit)
          (dft
             "tx-pool-tx-per-addr-limit"
             ~description:
               "Maximum allowed transactions per user address inside the \
                transaction pool."
             int64
             default_tx_pool_tx_per_addr_limit)
          (dft "keep_alive" bool default_keep_alive)
          (req "rollup_node_endpoint" string)
          (dft "verbose" Internal_event.Level.encoding Internal_event.Notice)
          (dft
             "experimental_features"
             experimental_features_encoding
             default_experimental_features)
          (dft "proxy" proxy_encoding (default_proxy ()))
          (dft "fee_history" fee_history_encoding default_fee_history)
          (opt "restricted_rpcs" string)))

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

let load ~data_dir =
  let open Lwt_result_syntax in
  let+ json = Lwt_utils_unix.Json.read_file (config_filename ~data_dir) in
  let config = Data_encoding.Json.destruct (encoding data_dir) json in
  config

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
  let create ~data_dir ?rpc_addr ?rpc_port ?cors_origins ?cors_headers
      ?tx_pool_timeout_limit ?tx_pool_addr_limit ?tx_pool_tx_per_addr_limit
      ~keep_alive ~rollup_node_endpoint ~verbose ?preimages ?preimages_endpoint
      ?time_between_blocks ?max_number_of_chunks ?private_rpc_port
      ?sequencer_key ?evm_node_endpoint ?threshold_encryption_bundler_endpoint
      ?log_filter_max_nb_blocks ?log_filter_max_nb_logs ?log_filter_chunk_size
      ?max_blueprints_lag ?max_blueprints_ahead ?max_blueprints_catchup
      ?catchup_cooldown ?sequencer_sidecar_endpoint ?restricted_rpcs
      ?proxy_finalized_view ?dal_slots () =
    let sequencer =
      Option.map
        (fun sequencer ->
          sequencer_config_dft
            ~data_dir
            ?preimages
            ?preimages_endpoint
            ?time_between_blocks
            ?max_number_of_chunks
            ?private_rpc_port
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
            ~data_dir
            ?preimages
            ?preimages_endpoint
            ?time_between_blocks
            ?max_number_of_chunks
            ?private_rpc_port
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
          observer_config_dft
            ~data_dir
            ?preimages
            ?preimages_endpoint
            ~evm_node_endpoint
            ?threshold_encryption_bundler_endpoint
            ())
        evm_node_endpoint
    in
    let proxy = default_proxy ?finalized_view:proxy_finalized_view () in
    let log_filter =
      default_filter_config
        ?max_nb_blocks:log_filter_max_nb_blocks
        ?max_nb_logs:log_filter_max_nb_logs
        ?chunk_size:log_filter_chunk_size
        ()
    in
    let restricted_rpcs = Option.map make_restricted_rpcs restricted_rpcs in
    {
      rpc_addr = Option.value ~default:default_rpc_addr rpc_addr;
      rpc_port = Option.value ~default:default_rpc_port rpc_port;
      cors_origins = Option.value ~default:default_cors_origins cors_origins;
      cors_headers = Option.value ~default:default_cors_headers cors_headers;
      log_filter;
      sequencer;
      threshold_encryption_sequencer;
      observer;
      proxy;
      max_active_connections = default_max_active_connections;
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
      restricted_rpcs;
    }

  let patch_configuration_from_args ~data_dir ?rpc_addr ?rpc_port ?cors_origins
      ?cors_headers ?tx_pool_timeout_limit ?tx_pool_addr_limit
      ?tx_pool_tx_per_addr_limit ~keep_alive ?rollup_node_endpoint ~verbose
      ?preimages ?preimages_endpoint ?time_between_blocks ?max_number_of_chunks
      ?private_rpc_port ?sequencer_key ?evm_node_endpoint
      ?threshold_encryption_bundler_endpoint ?log_filter_max_nb_blocks
      ?log_filter_max_nb_logs ?log_filter_chunk_size ?max_blueprints_lag
      ?max_blueprints_ahead ?max_blueprints_catchup ?catchup_cooldown
      ?sequencer_sidecar_endpoint ?restricted_rpcs ?proxy_finalized_view
      ~dal_slots configuration =
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
              preimages =
                Option.value ~default:sequencer_config.preimages preimages;
              preimages_endpoint =
                Option.either
                  preimages_endpoint
                  sequencer_config.preimages_endpoint;
              time_between_blocks =
                Option.value
                  ~default:sequencer_config.time_between_blocks
                  time_between_blocks;
              max_number_of_chunks =
                Option.value
                  ~default:sequencer_config.max_number_of_chunks
                  max_number_of_chunks;
              private_rpc_port =
                Option.either private_rpc_port sequencer_config.private_rpc_port;
              sequencer =
                Option.value ~default:sequencer_config.sequencer sequencer_key;
              blueprints_publisher_config;
            }
      | None ->
          Option.map
            (fun sequencer ->
              sequencer_config_dft
                ~data_dir
                ?preimages
                ?preimages_endpoint
                ?time_between_blocks
                ?max_number_of_chunks
                ?private_rpc_port
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
                 preimages =
                   Option.value
                     ~default:threshold_encryption_sequencer_config.preimages
                     preimages;
                 preimages_endpoint =
                   Option.either
                     preimages_endpoint
                     threshold_encryption_sequencer_config.preimages_endpoint;
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
                 private_rpc_port =
                   Option.either
                     private_rpc_port
                     threshold_encryption_sequencer_config.private_rpc_port;
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
                ~data_dir
                ?preimages
                ?preimages_endpoint
                ?time_between_blocks
                ?max_number_of_chunks
                ?private_rpc_port
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
              preimages =
                Option.value ~default:observer_config.preimages preimages;
              preimages_endpoint =
                Option.either
                  preimages_endpoint
                  observer_config.preimages_endpoint;
              evm_node_endpoint =
                Option.value
                  ~default:observer_config.evm_node_endpoint
                  evm_node_endpoint;
              threshold_encryption_bundler_endpoint =
                (match threshold_encryption_bundler_endpoint with
                | None -> observer_config.threshold_encryption_bundler_endpoint
                | endpoint -> endpoint);
            }
      | None ->
          Option.map
            (fun evm_node_endpoint ->
              observer_config_dft
                ~data_dir
                ?preimages
                ?preimages_endpoint
                ~evm_node_endpoint
                ?threshold_encryption_bundler_endpoint
                ())
            evm_node_endpoint
    in
    let proxy = default_proxy ?finalized_view:proxy_finalized_view () in
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
    let restricted_rpcs =
      Option.either
        (Option.map make_restricted_rpcs restricted_rpcs)
        configuration.restricted_rpcs
    in
    {
      rpc_addr = Option.value ~default:configuration.rpc_addr rpc_addr;
      rpc_port = Option.value ~default:configuration.rpc_port rpc_port;
      cors_origins =
        Option.value ~default:configuration.cors_origins cors_origins;
      cors_headers =
        Option.value ~default:configuration.cors_headers cors_headers;
      log_filter;
      sequencer;
      threshold_encryption_sequencer;
      observer;
      proxy;
      max_active_connections = configuration.max_active_connections;
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
      rollup_node_endpoint =
        Option.value
          ~default:configuration.rollup_node_endpoint
          rollup_node_endpoint;
      verbose = (if verbose then Debug else configuration.verbose);
      experimental_features = configuration.experimental_features;
      fee_history = configuration.fee_history;
      restricted_rpcs;
    }

  let create_or_read_config ~data_dir ?rpc_addr ?rpc_port ?cors_origins
      ?cors_headers ?tx_pool_timeout_limit ?tx_pool_addr_limit
      ?tx_pool_tx_per_addr_limit ~keep_alive ?rollup_node_endpoint ~verbose
      ?preimages ?preimages_endpoint ?time_between_blocks ?max_number_of_chunks
      ?private_rpc_port ?sequencer_key ?evm_node_endpoint
      ?threshold_encryption_bundler_endpoint ?max_blueprints_lag
      ?max_blueprints_ahead ?max_blueprints_catchup ?catchup_cooldown
      ?log_filter_max_nb_blocks ?log_filter_max_nb_logs ?log_filter_chunk_size
      ?sequencer_sidecar_endpoint ?restricted_rpcs ?proxy_finalized_view
      ?dal_slots () =
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
          ~data_dir
          ?rpc_addr
          ?rpc_port
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
          ~verbose
          ?log_filter_max_nb_blocks
          ?log_filter_max_nb_logs
          ?log_filter_chunk_size
          ?sequencer_sidecar_endpoint
          ?restricted_rpcs
          ?proxy_finalized_view
          ~dal_slots
          configuration
      in
      return configuration
    else
      let*? rollup_node_endpoint =
        Option.to_result
          ~none:(error_missing_config ~name:"rollup_node_endpoint")
          rollup_node_endpoint
      in
      let config =
        create
          ~data_dir
          ?rpc_addr
          ?rpc_port
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
          ~rollup_node_endpoint
          ~verbose
          ?log_filter_max_nb_blocks
          ?log_filter_max_nb_logs
          ?log_filter_chunk_size
          ?sequencer_sidecar_endpoint
          ?restricted_rpcs
          ?proxy_finalized_view
          ?dal_slots
          ()
      in
      return config
end
