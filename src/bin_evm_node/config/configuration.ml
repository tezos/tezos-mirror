(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type version = Prod | Dev

type log_filter_config = {
  max_nb_blocks : int;
  max_nb_logs : int;
  chunk_size : int;
}

type t = {
  rpc_addr : string;
  rpc_port : int;
  debug : bool;
  rollup_node_endpoint : Uri.t;
  version : version;
  cors_origins : string list;
  cors_headers : string list;
  verbose : bool;
  log_filter : log_filter_config;
}

let default_filter_config =
  {max_nb_blocks = 100; max_nb_logs = 1000; chunk_size = 10}

let default_data_dir = Filename.concat (Sys.getenv "HOME") ".octez-evm-node"

let config_filename ~data_dir = Filename.concat data_dir "config.json"

let default_rpc_addr = "127.0.0.1"

let default_rpc_port = 8545

let default =
  {
    rpc_addr = default_rpc_addr;
    rpc_port = default_rpc_port;
    debug = true;
    rollup_node_endpoint = Uri.empty;
    version = Prod;
    cors_origins = [];
    cors_headers = [];
    verbose = false;
    log_filter = default_filter_config;
  }

let log_filter_config_encoding : log_filter_config Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {max_nb_blocks; max_nb_logs; chunk_size} ->
      (max_nb_blocks, max_nb_logs, chunk_size))
    (fun (max_nb_blocks, max_nb_logs, chunk_size) ->
      {max_nb_blocks; max_nb_logs; chunk_size})
    (obj3
       (dft "max_nb_blocks" int31 default_filter_config.max_nb_blocks)
       (dft "max_nb_logs" int31 default_filter_config.max_nb_logs)
       (dft "chunk_size" int31 default_filter_config.chunk_size))

let version_encoding : version Data_encoding.t =
  let open Data_encoding in
  union
    [
      case
        ~title:"Prod"
        (Tag 0)
        unit
        (function Prod -> Some () | _ -> None)
        (fun () -> Prod);
      case
        ~title:"Dev"
        (Tag 1)
        unit
        (function Dev -> Some () | _ -> None)
        (fun () -> Dev);
    ]

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           rpc_addr;
           rpc_port;
           debug;
           rollup_node_endpoint;
           version;
           cors_origins;
           cors_headers;
           verbose;
           log_filter;
         } ->
      ( rpc_addr,
        rpc_port,
        debug,
        Uri.to_string rollup_node_endpoint,
        version,
        cors_origins,
        cors_headers,
        verbose,
        log_filter ))
    (fun ( rpc_addr,
           rpc_port,
           debug,
           rollup_node_endpoint,
           version,
           cors_origins,
           cors_headers,
           verbose,
           log_filter ) ->
      {
        rpc_addr;
        rpc_port;
        debug;
        rollup_node_endpoint = Uri.of_string rollup_node_endpoint;
        version;
        cors_origins;
        cors_headers;
        verbose;
        log_filter;
      })
    (obj9
       (dft "rpc-addr" ~description:"RPC address" string default_rpc_addr)
       (dft "rpc-port" ~description:"RPC port" uint16 default_rpc_port)
       (dft "debug" bool default.debug)
       (dft
          "rollup_node_endpoint"
          string
          (Uri.to_string default.rollup_node_endpoint))
       (dft "version" version_encoding default.version)
       (dft "cors_origins" (list string) default.cors_origins)
       (dft "cors_headers" (list string) default.cors_headers)
       (dft "verbose" bool default.verbose)
       (dft "log_filter" log_filter_config_encoding default_filter_config))

let save ~force ~data_dir config =
  let open Lwt_result_syntax in
  let json = Data_encoding.Json.construct encoding config in
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
  let config = Data_encoding.Json.destruct encoding json in
  config

module Cli = struct
  let create ?version ?rpc_addr ?rpc_port ?debug ?cors_origins ?cors_headers
      ?log_filter ~rollup_node_endpoint ~verbose () =
    {
      rpc_addr = Option.value ~default:default.rpc_addr rpc_addr;
      rpc_port = Option.value ~default:default.rpc_port rpc_port;
      debug = Option.value ~default:default.debug debug;
      rollup_node_endpoint;
      version = Option.value ~default:default.version version;
      cors_origins = Option.value ~default:default.cors_origins cors_origins;
      cors_headers = Option.value ~default:default.cors_headers cors_headers;
      verbose;
      log_filter = Option.value ~default:default_filter_config log_filter;
    }

  let patch_configuration_from_args ?version ?rpc_addr ?rpc_port ?debug
      ?cors_origins ?cors_headers ?log_filter ~rollup_node_endpoint ~verbose
      configuration =
    {
      rpc_addr = Option.value ~default:configuration.rpc_addr rpc_addr;
      rpc_port = Option.value ~default:configuration.rpc_port rpc_port;
      debug = Option.value ~default:configuration.debug debug;
      rollup_node_endpoint;
      version = Option.value ~default:configuration.version version;
      cors_origins =
        Option.value ~default:configuration.cors_origins cors_origins;
      cors_headers =
        Option.value ~default:configuration.cors_headers cors_headers;
      verbose;
      log_filter = Option.value ~default:default_filter_config log_filter;
    }

  let create_or_read_config ~data_dir ?version ?rpc_addr ?rpc_port ?debug
      ?cors_origins ?cors_headers ?log_filter ~rollup_node_endpoint ~verbose ()
      =
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
          ?version
          ?rpc_addr
          ?rpc_port
          ?debug
          ?cors_origins
          ?cors_headers
          ?log_filter
          ~rollup_node_endpoint
          ~verbose
          configuration
      in
      return configuration
    else
      let config =
        create
          ?version
          ?rpc_addr
          ?rpc_port
          ?debug
          ?cors_origins
          ?cors_headers
          ?log_filter
          ~rollup_node_endpoint
          ~verbose
          ()
      in
      return config
end
