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

type time_between_blocks = Nothing | Time_between_blocks of float

(** TODO https://gitlab.com/tezos/tezos/-/issues/6811
    We need to properly handle the secrets in the node. *)
type sequencer = {
  rollup_node_endpoint : Uri.t;
  preimages : string;
  preimages_endpoint : Uri.t option;
  time_between_blocks : time_between_blocks;
  private_rpc_port : int;
  sequencer : Signature.public_key_hash;
}

type observer = {evm_node_endpoint : Uri.t; preimages : string}

type 'a t = {
  rpc_addr : string;
  rpc_port : int;
  devmode : bool;
  cors_origins : string list;
  cors_headers : string list;
  log_filter : log_filter_config;
  mode : 'a;
  max_active_connections : int;
}

let default_filter_config =
  {max_nb_blocks = 100; max_nb_logs = 1000; chunk_size = 10}

let default_data_dir = Filename.concat (Sys.getenv "HOME") ".octez-evm-node"

let config_filename ~data_dir = Filename.concat data_dir "config.json"

let default_rpc_addr = "127.0.0.1"

let default_rpc_port = 8545

let default_devmode = false

let default_cors_origins = []

let default_cors_headers = []

let default_max_active_connections = 100

let default_proxy = {rollup_node_endpoint = Uri.empty}

let default_preimages = "_evm_installer_preimages"

let default_rollup_node_endpoint = Uri.empty

let default_evm_node_endpoint = Uri.empty

let default_time_between_blocks = Time_between_blocks 5.

let default_private_rpc_port = 8546

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

let encoding_time_between_blocks : time_between_blocks Data_encoding.t =
  let open Data_encoding in
  def "time_between_blocks"
  @@ conv
       (function Nothing -> None | Time_between_blocks f -> Some f)
       (function None -> Nothing | Some f -> Time_between_blocks f)
       (option float)

let encoding_sequencer =
  let open Data_encoding in
  conv
    (fun {
           preimages;
           preimages_endpoint;
           rollup_node_endpoint;
           time_between_blocks;
           private_rpc_port;
           sequencer;
         } ->
      ( preimages,
        preimages_endpoint,
        Uri.to_string rollup_node_endpoint,
        time_between_blocks,
        private_rpc_port,
        sequencer ))
    (fun ( preimages,
           preimages_endpoint,
           rollup_node_endpoint,
           time_between_blocks,
           private_rpc_port,
           sequencer ) ->
      {
        preimages;
        preimages_endpoint;
        rollup_node_endpoint = Uri.of_string rollup_node_endpoint;
        time_between_blocks;
        private_rpc_port;
        sequencer;
      })
    (obj6
       (dft "preimages" string default_preimages)
       (opt "preimages_endpoint" Tezos_rpc.Encoding.uri_encoding)
       (dft
          "rollup_node_endpoint"
          string
          (Uri.to_string default_rollup_node_endpoint))
       (dft
          "time_between_blocks"
          encoding_time_between_blocks
          default_time_between_blocks)
       (dft
          "private-rpc-port"
          ~description:"RPC port for private server"
          uint16
          default_private_rpc_port)
       (req "sequencer" Signature.Public_key_hash.encoding))

let encoding_observer =
  let open Data_encoding in
  conv
    (fun {preimages; evm_node_endpoint} ->
      (preimages, Uri.to_string evm_node_endpoint))
    (fun (preimages, evm_node_endpoint) ->
      {preimages; evm_node_endpoint = Uri.of_string evm_node_endpoint})
    (obj2
       (dft "preimages" string default_preimages)
       (dft
          "evm_node_endpoint"
          string
          (Uri.to_string default_evm_node_endpoint)))

let encoding : type a. a Data_encoding.t -> a t Data_encoding.t =
 fun mode_encoding ->
  let open Data_encoding in
  conv
    (fun {
           rpc_addr;
           rpc_port;
           devmode;
           cors_origins;
           cors_headers;
           log_filter;
           mode;
           max_active_connections;
         } ->
      ( rpc_addr,
        rpc_port,
        devmode,
        cors_origins,
        cors_headers,
        log_filter,
        mode,
        max_active_connections ))
    (fun ( rpc_addr,
           rpc_port,
           devmode,
           cors_origins,
           cors_headers,
           log_filter,
           mode,
           max_active_connections ) ->
      {
        rpc_addr;
        rpc_port;
        devmode;
        cors_origins;
        cors_headers;
        log_filter;
        mode;
        max_active_connections;
      })
    (obj8
       (dft "rpc-addr" ~description:"RPC address" string default_rpc_addr)
       (dft "rpc-port" ~description:"RPC port" uint16 default_rpc_port)
       (dft "devmode" bool default_devmode)
       (dft "cors_origins" (list string) default_cors_origins)
       (dft "cors_headers" (list string) default_cors_headers)
       (dft "log_filter" log_filter_config_encoding default_filter_config)
       (req "mode" mode_encoding)
       (dft "max_active_connections" int31 default_max_active_connections))

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
  let encoding = encoding encoding_proxy in
  save ~force ~data_dir encoding config

let save_sequencer ~force ~data_dir config =
  let encoding = encoding encoding_sequencer in
  save ~force ~data_dir encoding config

let save_observer ~force ~data_dir config =
  let encoding = encoding encoding_observer in
  save ~force ~data_dir encoding config

let load ~data_dir encoding =
  let open Lwt_result_syntax in
  let+ json = Lwt_utils_unix.Json.read_file (config_filename ~data_dir) in
  let config = Data_encoding.Json.destruct encoding json in
  config

let load_proxy ~data_dir =
  let encoding = encoding encoding_proxy in
  load ~data_dir encoding

let load_sequencer ~data_dir =
  let encoding = encoding encoding_sequencer in
  load ~data_dir encoding

let load_observer ~data_dir =
  let encoding = encoding encoding_observer in
  load ~data_dir encoding

module Cli = struct
  let create ~devmode ?rpc_addr ?rpc_port ?cors_origins ?cors_headers
      ?log_filter ~mode () =
    {
      rpc_addr = Option.value ~default:default_rpc_addr rpc_addr;
      rpc_port = Option.value ~default:default_rpc_port rpc_port;
      devmode;
      cors_origins = Option.value ~default:default_cors_origins cors_origins;
      cors_headers = Option.value ~default:default_cors_headers cors_headers;
      log_filter = Option.value ~default:default_filter_config log_filter;
      mode;
      max_active_connections = default_max_active_connections;
    }

  let create_proxy ~devmode ?rpc_addr ?rpc_port ?cors_origins ?cors_headers
      ?log_filter ~rollup_node_endpoint =
    create
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~mode:{rollup_node_endpoint}

  let create_sequencer ?private_rpc_port ~devmode ?rpc_addr ?rpc_port
      ?cors_origins ?cors_headers ?log_filter ?rollup_node_endpoint ?preimages
      ?preimages_endpoint ?time_between_blocks ~sequencer =
    let mode =
      {
        rollup_node_endpoint =
          Option.value
            ~default:default_rollup_node_endpoint
            rollup_node_endpoint;
        preimages = Option.value ~default:default_preimages preimages;
        preimages_endpoint;
        time_between_blocks =
          Option.value ~default:default_time_between_blocks time_between_blocks;
        private_rpc_port =
          Option.value ~default:default_private_rpc_port private_rpc_port;
        sequencer;
      }
    in
    create
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~mode

  let create_observer ~devmode ?rpc_addr ?rpc_port ?cors_origins ?cors_headers
      ?log_filter ?evm_node_endpoint ?preimages =
    let mode =
      {
        evm_node_endpoint =
          Option.value ~default:default_rollup_node_endpoint evm_node_endpoint;
        preimages = Option.value ~default:default_preimages preimages;
      }
    in
    create
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~mode

  let patch_configuration_from_args ~devmode ?rpc_addr ?rpc_port ?cors_origins
      ?cors_headers ?log_filter ~mode configuration =
    {
      rpc_addr = Option.value ~default:configuration.rpc_addr rpc_addr;
      rpc_port = Option.value ~default:configuration.rpc_port rpc_port;
      devmode;
      cors_origins =
        Option.value ~default:configuration.cors_origins cors_origins;
      cors_headers =
        Option.value ~default:configuration.cors_headers cors_headers;
      log_filter = Option.value ~default:default_filter_config log_filter;
      mode;
      max_active_connections = configuration.max_active_connections;
    }

  let patch_proxy_configuration_from_args ~devmode ?rpc_addr ?rpc_port
      ?cors_origins ?cors_headers ?log_filter ?rollup_node_endpoint
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
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~mode
      configuration

  let patch_sequencer_configuration_from_args ?private_rpc_port ~devmode
      ?rpc_addr ?rpc_port ?cors_origins ?cors_headers ?log_filter
      ?rollup_node_endpoint ?preimages ?preimages_endpoint ?time_between_blocks
      configuration ~sequencer =
    let mode =
      {
        rollup_node_endpoint =
          Option.value
            ~default:configuration.mode.rollup_node_endpoint
            rollup_node_endpoint;
        preimages = Option.value ~default:configuration.mode.preimages preimages;
        preimages_endpoint =
          Option.either preimages_endpoint configuration.mode.preimages_endpoint;
        time_between_blocks =
          Option.value
            ~default:configuration.mode.time_between_blocks
            time_between_blocks;
        private_rpc_port =
          Option.value ~default:default_private_rpc_port private_rpc_port;
        sequencer;
      }
    in
    patch_configuration_from_args
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~mode
      configuration

  let patch_observer_configuration_from_args ~devmode ?rpc_addr ?rpc_port
      ?cors_origins ?cors_headers ?log_filter ?evm_node_endpoint ?preimages
      configuration =
    let mode =
      {
        evm_node_endpoint =
          Option.value
            ~default:configuration.mode.evm_node_endpoint
            evm_node_endpoint;
        preimages = Option.value ~default:configuration.mode.preimages preimages;
      }
    in
    patch_configuration_from_args
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~mode
      configuration

  let create_or_read_config ~data_dir ~devmode ?rpc_addr ?rpc_port
      ?private_rpc_port ?cors_origins ?cors_headers ?log_filter ~load
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
          ?private_rpc_port
          ~devmode
          ?rpc_addr
          ?rpc_port
          ?cors_origins
          ?cors_headers
          ?log_filter
          configuration
      in
      return configuration
    else
      let config =
        create
          ?private_rpc_port
          ~devmode
          ?rpc_addr
          ?rpc_port
          ?cors_origins
          ?cors_headers
          ?log_filter
          ()
      in
      return config

  let create_or_read_proxy_config ~data_dir ~devmode ?rpc_addr ?rpc_port
      ?cors_origins ?cors_headers ?log_filter ~rollup_node_endpoint () =
    create_or_read_config
      ~data_dir
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~load:load_proxy
      ~patch_configuration_from_args:(fun ?private_rpc_port:_ ->
        patch_proxy_configuration_from_args ~rollup_node_endpoint)
      ~create:(fun ?private_rpc_port:_ -> create_proxy ~rollup_node_endpoint)
      ()

  let create_or_read_sequencer_config ~data_dir ~devmode ?rpc_addr ?rpc_port
      ?private_rpc_port ?cors_origins ?cors_headers ?log_filter
      ?rollup_node_endpoint ?preimages ?preimages_endpoint ?time_between_blocks
      ~sequencer () =
    create_or_read_config
      ~data_dir
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?private_rpc_port
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~load:load_sequencer
      ~patch_configuration_from_args:
        (patch_sequencer_configuration_from_args
           ?rollup_node_endpoint
           ?preimages
           ?preimages_endpoint
           ?time_between_blocks
           ~sequencer)
      ~create:
        (create_sequencer
           ?rollup_node_endpoint
           ?preimages
           ?preimages_endpoint
           ?time_between_blocks
           ~sequencer)
      ()

  let create_or_read_observer_config ~data_dir ~devmode ?rpc_addr ?rpc_port
      ?cors_origins ?cors_headers ?log_filter ?evm_node_endpoint ?preimages () =
    create_or_read_config
      ~data_dir
      ~devmode
      ?rpc_addr
      ?rpc_port
      ?cors_origins
      ?cors_headers
      ?log_filter
      ~load:load_observer
      ~patch_configuration_from_args:(fun ?private_rpc_port:_ ->
        patch_observer_configuration_from_args ?evm_node_endpoint ?preimages)
      ~create:(fun ?private_rpc_port:_ ->
        create_observer ?evm_node_endpoint ?preimages)
      ()
end
