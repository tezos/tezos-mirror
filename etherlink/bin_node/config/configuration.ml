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

type sequencer = {
  rollup_node_endpoint : Uri.t;
  preimages : string;
  preimages_endpoint : Uri.t option;
  time_between_blocks : time_between_blocks;
  max_number_of_chunks : int;
  private_rpc_port : int option;
  sequencer : Signature.public_key_hash;
}

type observer = {
  evm_node_endpoint : Uri.t;
  preimages : string;
  preimages_endpoint : Uri.t option;
}

type t = {
  rpc_addr : string;
  rpc_port : int;
  devmode : bool;
  cors_origins : string list;
  cors_headers : string list;
  log_filter : log_filter_config;
  proxy : proxy option;
  sequencer : sequencer option;
  observer : observer option;
  max_active_connections :
    Tezos_rpc_http_server.RPC_server.Max_active_rpc_connections.t;
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

let default_max_active_connections =
  Tezos_rpc_http_server.RPC_server.Max_active_rpc_connections.default

let default_proxy = {rollup_node_endpoint = Uri.empty}

let default_preimages =
  Filename.Infix.(default_data_dir // "_evm_installer_preimages")

let default_time_between_blocks = Time_between_blocks 5.

let hard_maximum_number_of_chunks =
  (* The kernel doesn't accept blueprints whose cumulated chunk size is higher
     than 512kb. *)
  let max_cumulated_chunks_size = 512 * 1024 in
  (* External message size *)
  let chunk_size = 4095 in
  max_cumulated_chunks_size / chunk_size

let default_max_number_of_chunks = hard_maximum_number_of_chunks

let sequencer_config_dft ?preimages ?preimages_endpoint ?time_between_blocks
    ?max_number_of_chunks ?private_rpc_port ~rollup_node_endpoint ~sequencer ()
    =
  {
    rollup_node_endpoint;
    preimages = Option.value ~default:default_preimages preimages;
    preimages_endpoint;
    time_between_blocks =
      Option.value ~default:default_time_between_blocks time_between_blocks;
    max_number_of_chunks =
      Option.value ~default:default_max_number_of_chunks max_number_of_chunks;
    private_rpc_port;
    sequencer;
  }

let observer_config_dft ?preimages ?preimages_endpoint ~evm_node_endpoint () =
  {
    evm_node_endpoint;
    preimages = Option.value ~default:default_preimages preimages;
    preimages_endpoint;
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

let encoding_time_between_blocks : time_between_blocks Data_encoding.t =
  let open Data_encoding in
  def "time_between_blocks"
  @@ conv
       (function Nothing -> None | Time_between_blocks f -> Some f)
       (function None -> Nothing | Some f -> Time_between_blocks f)
       (option float)

let proxy_encoding =
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

let sequencer_encoding =
  let open Data_encoding in
  conv
    (fun {
           preimages;
           preimages_endpoint;
           rollup_node_endpoint;
           time_between_blocks;
           max_number_of_chunks;
           private_rpc_port;
           sequencer;
         } ->
      ( preimages,
        preimages_endpoint,
        Uri.to_string rollup_node_endpoint,
        time_between_blocks,
        max_number_of_chunks,
        private_rpc_port,
        sequencer ))
    (fun ( preimages,
           preimages_endpoint,
           rollup_node_endpoint,
           time_between_blocks,
           max_number_of_chunks,
           private_rpc_port,
           sequencer ) ->
      {
        preimages;
        preimages_endpoint;
        rollup_node_endpoint = Uri.of_string rollup_node_endpoint;
        time_between_blocks;
        max_number_of_chunks;
        private_rpc_port;
        sequencer;
      })
    (obj7
       (dft "preimages" string default_preimages)
       (opt "preimages_endpoint" Tezos_rpc.Encoding.uri_encoding)
       (req "rollup_node_endpoint" string)
       (dft
          "time_between_blocks"
          encoding_time_between_blocks
          default_time_between_blocks)
       (dft "max_number_of_chunks" int31 default_max_number_of_chunks)
       (opt
          "private-rpc-port"
          ~description:"RPC port for private server"
          uint16)
       (req "sequencer" Signature.Public_key_hash.encoding))

let observer_encoding =
  let open Data_encoding in
  conv
    (fun {preimages; preimages_endpoint; evm_node_endpoint} ->
      (preimages, preimages_endpoint, Uri.to_string evm_node_endpoint))
    (fun (preimages, preimages_endpoint, evm_node_endpoint) ->
      {
        preimages;
        preimages_endpoint;
        evm_node_endpoint = Uri.of_string evm_node_endpoint;
      })
    (obj3
       (dft "preimages" string default_preimages)
       (opt "preimages_endpoint" Tezos_rpc.Encoding.uri_encoding)
       (req "evm_node_endpoint" string))

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           rpc_addr;
           rpc_port;
           devmode;
           cors_origins;
           cors_headers;
           log_filter;
           proxy;
           sequencer;
           observer;
           max_active_connections;
         } ->
      ( rpc_addr,
        rpc_port,
        devmode,
        cors_origins,
        cors_headers,
        log_filter,
        proxy,
        sequencer,
        observer,
        max_active_connections ))
    (fun ( rpc_addr,
           rpc_port,
           devmode,
           cors_origins,
           cors_headers,
           log_filter,
           proxy,
           sequencer,
           observer,
           max_active_connections ) ->
      {
        rpc_addr;
        rpc_port;
        devmode;
        cors_origins;
        cors_headers;
        log_filter;
        proxy;
        sequencer;
        observer;
        max_active_connections;
      })
    (obj10
       (dft "rpc-addr" ~description:"RPC address" string default_rpc_addr)
       (dft "rpc-port" ~description:"RPC port" uint16 default_rpc_port)
       (dft "devmode" bool default_devmode)
       (dft "cors_origins" (list string) default_cors_origins)
       (dft "cors_headers" (list string) default_cors_headers)
       (dft "log_filter" log_filter_config_encoding default_filter_config)
       (opt "proxy" proxy_encoding)
       (opt "sequencer" sequencer_encoding)
       (opt "observer" observer_encoding)
       (dft
          "max_active_connections"
          Tezos_rpc_http_server.RPC_server.Max_active_rpc_connections.encoding
          default_max_active_connections))

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

let error_missing_config ~name = [error_of_fmt "missing %s config" name]

let proxy_config_exn {proxy; _} =
  Option.to_result ~none:(error_missing_config ~name:"proxy") proxy

let sequencer_config_exn {sequencer; _} =
  Option.to_result ~none:(error_missing_config ~name:"sequencer") sequencer

let observer_config_exn {observer; _} =
  Option.to_result ~none:(error_missing_config ~name:"observer") observer

module Cli = struct
  let create ~devmode ?rpc_addr ?rpc_port ?cors_origins ?cors_headers
      ?log_filter ?proxy ?sequencer ?observer () =
    {
      rpc_addr = Option.value ~default:default_rpc_addr rpc_addr;
      rpc_port = Option.value ~default:default_rpc_port rpc_port;
      devmode;
      cors_origins = Option.value ~default:default_cors_origins cors_origins;
      cors_headers = Option.value ~default:default_cors_headers cors_headers;
      log_filter = Option.value ~default:default_filter_config log_filter;
      proxy;
      sequencer;
      observer;
      max_active_connections = default_max_active_connections;
    }

  let patch_configuration_from_args ~devmode ?rpc_addr ?rpc_port ?cors_origins
      ?cors_headers ?log_filter ?proxy ?sequencer ?observer configuration =
    {
      rpc_addr = Option.value ~default:configuration.rpc_addr rpc_addr;
      rpc_port = Option.value ~default:configuration.rpc_port rpc_port;
      devmode;
      cors_origins =
        Option.value ~default:configuration.cors_origins cors_origins;
      cors_headers =
        Option.value ~default:configuration.cors_headers cors_headers;
      log_filter = Option.value ~default:default_filter_config log_filter;
      proxy = Option.either configuration.proxy proxy;
      sequencer = Option.either configuration.sequencer sequencer;
      observer = Option.either configuration.observer observer;
      max_active_connections = configuration.max_active_connections;
    }

  let create_or_read_config ~data_dir ~devmode ?rpc_addr ?rpc_port ?cors_origins
      ?cors_headers ?log_filter ?proxy ?sequencer ?observer () =
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
          ?cors_origins
          ?cors_headers
          ?log_filter
          ?proxy
          ?sequencer
          ?observer
          configuration
      in
      return configuration
    else
      let config =
        create
          ~devmode
          ?rpc_addr
          ?rpc_port
          ?cors_origins
          ?cors_headers
          ?log_filter
          ?proxy
          ?sequencer
          ?observer
          ()
      in
      return config
end
