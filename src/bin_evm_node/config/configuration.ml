(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type log_filter_config = {
  max_nb_blocks : int;
  max_nb_logs : int;
  chunk_size : int;
}

type proxy = {rollup_node_endpoint : Uri.t}

type sequencer = {
  rollup_node_endpoint : Uri.t;
  kernel : string;
  preimages : string;
}

type 'a t = {
  rpc_addr : string;
  rpc_port : int;
  debug : bool;
  devmode : bool;
  cors_origins : string list;
  cors_headers : string list;
  verbose : bool;
  log_filter : log_filter_config;
  mode : 'a;
}

let default_filter_config =
  {max_nb_blocks = 100; max_nb_logs = 1000; chunk_size = 10}

let default_data_dir = Filename.concat (Sys.getenv "HOME") ".octez-evm-node"

let config_filename ~data_dir = Filename.concat data_dir "config.json"

let default_rpc_addr = "127.0.0.1"

let default_rpc_port = 8545

let default default_mode =
  {
    rpc_addr = default_rpc_addr;
    rpc_port = default_rpc_port;
    debug = true;
    devmode = false;
    cors_origins = [];
    cors_headers = [];
    verbose = false;
    log_filter = default_filter_config;
    mode = default_mode;
  }

let default_proxy = {rollup_node_endpoint = Uri.empty}

let default_sequencer =
  {
    kernel = "sequencer.wasm";
    preimages = "_evm_installer_preimages";
    rollup_node_endpoint = Uri.empty;
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

let encoding_proxy =
  let open Data_encoding in
  conv
    (fun ({rollup_node_endpoint} : proxy) -> Uri.to_string rollup_node_endpoint)
    (fun rollup_node_endpoint ->
      {rollup_node_endpoint = Uri.of_string rollup_node_endpoint})
    (obj1
       (dft
          "rollup_node_endpoint"
          string
          (Uri.to_string default_proxy.rollup_node_endpoint)))

let encoding_sequencer =
  let open Data_encoding in
  conv
    (fun {kernel; preimages; rollup_node_endpoint} ->
      (kernel, preimages, Uri.to_string rollup_node_endpoint))
    (fun (kernel, preimages, rollup_node_endpoint) ->
      {
        kernel;
        preimages;
        rollup_node_endpoint = Uri.of_string rollup_node_endpoint;
      })
    (obj3
       (dft "kernel" string default_sequencer.kernel)
       (dft "preimages" string default_sequencer.preimages)
       (dft
          "rollup_node_endpoint"
          string
          (Uri.to_string default_proxy.rollup_node_endpoint)))

let encoding ~default_mode mode_encoding =
  let open Data_encoding in
  let default = default default_mode in
  conv
    (fun {
           rpc_addr;
           rpc_port;
           debug;
           devmode;
           cors_origins;
           cors_headers;
           verbose;
           log_filter;
           mode;
         } ->
      ( ( rpc_addr,
          rpc_port,
          debug,
          devmode,
          cors_origins,
          cors_headers,
          verbose,
          log_filter ),
        mode ))
    (fun ( ( rpc_addr,
             rpc_port,
             debug,
             devmode,
             cors_origins,
             cors_headers,
             verbose,
             log_filter ),
           mode ) ->
      {
        rpc_addr;
        rpc_port;
        debug;
        devmode;
        cors_origins;
        cors_headers;
        verbose;
        log_filter;
        mode;
      })
    (merge_objs
       (obj8
          (dft "rpc-addr" ~description:"RPC address" string default_rpc_addr)
          (dft "rpc-port" ~description:"RPC port" uint16 default_rpc_port)
          (dft "debug" bool default.debug)
          (dft "devmode" bool default.devmode)
          (dft "cors_origins" (list string) default.cors_origins)
          (dft "cors_headers" (list string) default.cors_headers)
          (dft "verbose" bool default.verbose)
          (dft "log_filter" log_filter_config_encoding default_filter_config))
       (obj1 (dft "mode" mode_encoding default_mode)))

let save ~force ~data_dir encoding config =
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

let save_proxy ~force ~data_dir config =
  let encoding = encoding ~default_mode:default_proxy encoding_proxy in
  save ~force ~data_dir encoding config

let save_sequencer ~force ~data_dir config =
  let encoding = encoding ~default_mode:default_sequencer encoding_sequencer in
  save ~force ~data_dir encoding config

let load ~data_dir encoding =
  let open Lwt_result_syntax in
  let+ json = Lwt_utils_unix.Json.read_file (config_filename ~data_dir) in
  let config = Data_encoding.Json.destruct encoding json in
  config

let load_proxy ~data_dir =
  let encoding = encoding ~default_mode:default_proxy encoding_proxy in
  load ~data_dir encoding

let load_sequencer ~data_dir =
  let encoding = encoding ~default_mode:default_sequencer encoding_sequencer in
  load ~data_dir encoding

module Cli = struct
  let create ~devmode ?rpc_addr ?rpc_port ?debug ?cors_origins ?cors_headers
      ?log_filter ~verbose ~mode () =
    let default = default mode in
    {
      rpc_addr = Option.value ~default:default.rpc_addr rpc_addr;
      rpc_port = Option.value ~default:default.rpc_port rpc_port;
      debug = Option.value ~default:default.debug debug;
      devmode;
      cors_origins = Option.value ~default:default.cors_origins cors_origins;
      cors_headers = Option.value ~default:default.cors_headers cors_headers;
      verbose;
      log_filter = Option.value ~default:default_filter_config log_filter;
      mode;
    }

  let create_proxy ~devmode ?rpc_addr ?rpc_port ?debug ?cors_origins
      ?cors_headers ?log_filter ~verbose ~rollup_node_endpoint =
    create
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?debug
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~verbose
      ~mode:{rollup_node_endpoint}

  let create_sequencer ~devmode ?rpc_addr ?rpc_port ?debug ?cors_origins
      ?cors_headers ?log_filter ~verbose ?rollup_node_endpoint ?kernel
      ?preimages =
    let mode =
      {
        rollup_node_endpoint =
          Option.value
            ~default:default_sequencer.rollup_node_endpoint
            rollup_node_endpoint;
        kernel = Option.value ~default:default_sequencer.kernel kernel;
        preimages = Option.value ~default:default_sequencer.preimages preimages;
      }
    in
    create
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?debug
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~verbose
      ~mode

  let patch_configuration_from_args ~devmode ?rpc_addr ?rpc_port ?debug
      ?cors_origins ?cors_headers ?log_filter ~verbose ~mode configuration =
    {
      rpc_addr = Option.value ~default:configuration.rpc_addr rpc_addr;
      rpc_port = Option.value ~default:configuration.rpc_port rpc_port;
      debug = Option.value ~default:configuration.debug debug;
      devmode;
      cors_origins =
        Option.value ~default:configuration.cors_origins cors_origins;
      cors_headers =
        Option.value ~default:configuration.cors_headers cors_headers;
      verbose;
      log_filter = Option.value ~default:default_filter_config log_filter;
      mode;
    }

  let patch_proxy_configuration_from_args ~devmode ?rpc_addr ?rpc_port ?debug
      ?cors_origins ?cors_headers ?log_filter ~verbose ?rollup_node_endpoint
      configuration =
    let mode =
      match rollup_node_endpoint with
      | Some rollup_node_endpoint -> {rollup_node_endpoint}
      | None -> configuration.mode
    in
    patch_configuration_from_args
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?debug
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~verbose
      ~mode
      configuration

  let patch_sequencer_configuration_from_args ~devmode ?rpc_addr ?rpc_port
      ?debug ?cors_origins ?cors_headers ?log_filter ~verbose
      ?rollup_node_endpoint ?kernel ?preimages configuration =
    let mode =
      {
        rollup_node_endpoint =
          Option.value
            ~default:configuration.mode.rollup_node_endpoint
            rollup_node_endpoint;
        kernel = Option.value ~default:configuration.mode.kernel kernel;
        preimages = Option.value ~default:configuration.mode.preimages preimages;
      }
    in
    patch_configuration_from_args
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?debug
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~verbose
      ~mode
      configuration

  let create_or_read_config ~data_dir ~devmode ?rpc_addr ?rpc_port ?debug
      ?cors_origins ?cors_headers ?log_filter ~verbose ~load
      ~patch_configuration_from_args ~create () =
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
          ~devmode
          ?rpc_addr
          ?rpc_port
          ?debug
          ?cors_origins
          ?cors_headers
          ?log_filter
          ~verbose
          configuration
      in
      return configuration
    else
      let config =
        create
          ~devmode
          ?rpc_addr
          ?rpc_port
          ?debug
          ?cors_origins
          ?cors_headers
          ?log_filter
          ~verbose
          ()
      in
      return config

  let create_or_read_proxy_config ~data_dir ~devmode ?rpc_addr ?rpc_port ?debug
      ?cors_origins ?cors_headers ?log_filter ~verbose ~rollup_node_endpoint ()
      =
    create_or_read_config
      ~data_dir
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?debug
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~verbose
      ~load:load_proxy
      ~patch_configuration_from_args:
        (patch_proxy_configuration_from_args ~rollup_node_endpoint)
      ~create:(create_proxy ~rollup_node_endpoint)
      ()

  let create_or_read_sequencer_config ~data_dir ~devmode ?rpc_addr ?rpc_port
      ?debug ?cors_origins ?cors_headers ?log_filter ~verbose
      ?rollup_node_endpoint ?kernel ?preimages () =
    create_or_read_config
      ~data_dir
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?debug
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~verbose
      ~load:load_sequencer
      ~patch_configuration_from_args:
        (patch_sequencer_configuration_from_args
           ?rollup_node_endpoint
           ?kernel
           ?preimages)
      ~create:(create_sequencer ?rollup_node_endpoint ?kernel ?preimages)
      ()
end
