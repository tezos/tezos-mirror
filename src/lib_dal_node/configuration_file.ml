(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2023 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(* Version history:
   - 0: initial version, comes with octez v20 release
   - 1: changed format of 'profile' field; added 'version' field
   - 2: removed fields network_name and neighbors. *)
let current_version = 2

type neighbor = {addr : string; port : int}

type history_mode = Rolling of {blocks : [`Auto | `Some of int]} | Full

type experimental_features = unit

let history_mode_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"rolling"
        ~description:""
        (Tag 0)
        (obj2
           (req "kind" (Data_encoding.constant "rolling"))
           (req "blocks" (Data_encoding.option Data_encoding.int31)))
        (function
          | Rolling {blocks} -> (
              match blocks with
              | `Auto -> Some ((), None)
              | `Some n -> Some ((), Some n))
          | Full -> None)
        (function
          | (), None -> Rolling {blocks = `Auto}
          | (), Some n -> Rolling {blocks = `Some n});
      case
        ~title:"full"
        ~description:""
        (Tag 1)
        (obj1 (req "kind" (Data_encoding.constant "full")))
        (function Full -> Some () | _ -> None)
        (fun () -> Full);
    ]

type t = {
  data_dir : string;
  rpc_addr : P2p_point.Id.t;
  listen_addr : P2p_point.Id.t;
  public_addr : P2p_point.Id.t;
  peers : string list;
  expected_pow : float;
  endpoint : Uri.t;
  slots_backup_uris : Uri.t list;
  trust_slots_backup_uris : bool;
  metrics_addr : P2p_point.Id.t option;
  profile : Profile_manager.unresolved_profile;
  history_mode : history_mode;
  version : int;
  service_name : string;
  service_namespace : string;
  experimental_features : experimental_features;
  fetch_trusted_setup : bool;
  verbose : bool;
  ignore_l1_config_peers : bool;
  disable_amplification : bool;
}

let default_data_dir = Filename.concat (Sys.getenv "HOME") ".tezos-dal-node"

let store_path {data_dir; _} = Filename.concat data_dir "store"

let default_rpc_addr = P2p_point.Id.of_string_exn ~default_port:10732 "0.0.0.0"

let default_listen_addr =
  let open Gossipsub.Transport_layer.Default_parameters.P2p_config in
  let default_net_host = P2p_addr.to_string listening_addr in
  let default_net_port = listening_port in
  P2p_point.Id.of_string_exn ~default_port:default_net_port default_net_host

let default_public_addr =
  let open Gossipsub.Transport_layer.Default_parameters.P2p_config in
  P2p_point.Id.of_string_exn ~default_port:listening_port "127.0.0.1"

let default_peers = []

let default_expected_pow =
  Gossipsub.Transport_layer.Default_parameters.P2p_config.expected_pow

let legacy_network_name = "dal-sandbox"

let default_endpoint = Uri.of_string "http://localhost:8732"

let default_metrics_port =
  Gossipsub.Transport_layer.Default_parameters.P2p_config.listening_port + 1

let default_history_mode = Rolling {blocks = `Auto}

let default_service_name = "octez-dal-node"

let default_service_namespace = "octez-dal-node"

let default_experimental_features = ()

let default_fetch_trusted_setup = true

let default =
  {
    data_dir = default_data_dir;
    rpc_addr = default_rpc_addr;
    listen_addr = default_listen_addr;
    public_addr = default_public_addr;
    peers = default_peers;
    expected_pow = default_expected_pow;
    endpoint = default_endpoint;
    slots_backup_uris = [];
    trust_slots_backup_uris = false;
    metrics_addr = None;
    history_mode = default_history_mode;
    profile = Profile_manager.Empty;
    version = current_version;
    service_name = default_service_name;
    service_namespace = default_service_namespace;
    experimental_features = default_experimental_features;
    fetch_trusted_setup = default_fetch_trusted_setup;
    verbose = false;
    ignore_l1_config_peers = false;
    disable_amplification = false;
  }

let uri_encoding : Uri.t Data_encoding.t =
  let open Data_encoding in
  conv_with_guard
    (fun uri -> Uri.to_string uri)
    (fun str ->
      try Uri.of_string str |> Result.ok
      with exn ->
        Format.asprintf
          "uri decoding failed:@.%a@."
          Error_monad.pp_print_trace
          [Exn exn]
        |> Result.error)
    string

let experimental_features_encoding : experimental_features Data_encoding.t =
  Data_encoding.unit

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           data_dir;
           rpc_addr;
           listen_addr;
           public_addr;
           peers;
           expected_pow;
           endpoint;
           slots_backup_uris;
           trust_slots_backup_uris;
           metrics_addr;
           history_mode;
           profile;
           version;
           service_name;
           service_namespace;
           experimental_features;
           fetch_trusted_setup;
           verbose;
           ignore_l1_config_peers;
           disable_amplification;
         } ->
      ( ( data_dir,
          rpc_addr,
          listen_addr,
          public_addr,
          peers,
          expected_pow,
          endpoint,
          slots_backup_uris,
          trust_slots_backup_uris,
          metrics_addr ),
        ( history_mode,
          profile,
          version,
          service_name,
          service_namespace,
          experimental_features,
          fetch_trusted_setup,
          verbose,
          ignore_l1_config_peers,
          disable_amplification ) ))
    (fun ( ( data_dir,
             rpc_addr,
             listen_addr,
             public_addr,
             peers,
             expected_pow,
             endpoint,
             slots_backup_uris,
             trust_slots_backup_uris,
             metrics_addr ),
           ( history_mode,
             profile,
             version,
             service_name,
             service_namespace,
             experimental_features,
             fetch_trusted_setup,
             verbose,
             ignore_l1_config_peers,
             disable_amplification ) ) ->
      {
        data_dir;
        rpc_addr;
        listen_addr;
        public_addr;
        peers;
        expected_pow;
        endpoint;
        slots_backup_uris;
        trust_slots_backup_uris;
        metrics_addr;
        history_mode;
        profile;
        version;
        service_name;
        service_namespace;
        experimental_features;
        fetch_trusted_setup;
        verbose;
        ignore_l1_config_peers;
        disable_amplification;
      })
    (merge_objs
       (obj10
          (dft
             "data-dir"
             ~description:"Location of the data dir"
             string
             default_data_dir)
          (dft
             "rpc-addr"
             ~description:"RPC address"
             P2p_point.Id.encoding
             default_rpc_addr)
          (dft
             "net-addr"
             ~description:"P2P address of this node"
             P2p_point.Id.encoding
             default_listen_addr)
          (dft
             "public-addr"
             ~description:"P2P address of this node"
             P2p_point.Id.encoding
             default_listen_addr)
          (dft
             "peers"
             ~description:"P2P addresses of remote peers"
             (list string)
             default_peers)
          (dft
             "expected-pow"
             ~description:"Expected P2P identity's PoW"
             float
             default_expected_pow)
          (dft
             "endpoint"
             ~description:"The Tezos node endpoint"
             uri_encoding
             default_endpoint)
          (dft
             "slots_backup_uris"
             ~description:"Optional HTTP endpoints to fetch missing slots from."
             (list uri_encoding)
             [])
          (dft
             "trust_slots_backup_uris"
             ~description:
               "Whether to trust the data downlaoded from the provided HTTP \
                backup URIs."
             bool
             false)
          (dft
             "metrics-addr"
             ~description:"The point for the DAL node metrics server"
             (Encoding.option P2p_point.Id.encoding)
             None))
       (obj10
          (dft
             "history_mode"
             ~description:"The history mode for the DAL node"
             history_mode_encoding
             default_history_mode)
          (dft
             "profiles"
             ~description:"The Octez DAL node profiles"
             Profile_manager.unresolved_encoding
             Profile_manager.Empty)
          (req "version" ~description:"The configuration file version" int31)
          (dft
             "service_name"
             ~description:"Name of the service"
             Data_encoding.string
             default.service_name)
          (dft
             "service_namespace"
             ~description:"Namespace for the service"
             Data_encoding.string
             default.service_namespace)
          (dft
             "experimental_features"
             ~description:"Experimental features"
             experimental_features_encoding
             default_experimental_features)
          (dft
             "fetch_trusted_setup"
             ~description:"Install trusted setup"
             bool
             true)
          (dft
             "verbose"
             ~description:
               "Whether to emit details about frequent logging events"
             bool
             default.verbose)
          (dft
             "ignore_l1_config_peers"
             ~description:"Ignore the boot(strap) peers provided by L1"
             bool
             default.ignore_l1_config_peers)
          (dft
             "disable_amplification"
             ~description:"Disable amplification"
             bool
             default.disable_amplification)))

type error += DAL_node_unable_to_write_configuration_file of string

let () =
  register_error_kind
    ~id:"dal.node.unable_to_write_configuration_file"
    ~title:"Unable to write configuration file"
    ~description:"Unable to write configuration file"
    ~pp:(fun ppf file ->
      Format.fprintf ppf "Unable to write the configuration file %s" file)
    `Permanent
    Data_encoding.(obj1 (req "file" string))
    (function
      | DAL_node_unable_to_write_configuration_file path -> Some path
      | _ -> None)
    (fun path -> DAL_node_unable_to_write_configuration_file path)

let filename ~data_dir = Filename.concat data_dir "config.json"

let save config =
  let open Lwt_syntax in
  let file = filename ~data_dir:config.data_dir in
  protect @@ fun () ->
  let* v =
    let* () = Lwt_utils_unix.create_dir config.data_dir in
    Lwt_utils_unix.with_atomic_open_out file @@ fun chan ->
    let json = Data_encoding.Json.construct encoding config in
    let content = Data_encoding.Json.to_string json in
    Lwt_utils_unix.write_string chan content
  in
  Lwt.return
    (Result.map_error
       (fun _ -> [DAL_node_unable_to_write_configuration_file file])
       v)

let load =
  let open Lwt_result_syntax in
  let destruct = Data_encoding.Json.destruct in
  let config_versions =
    [
      (2, destruct encoding);
      (* We can add cases here of the form:

         <version x>, fun json -> destruct Vx.encoding json |> Vx.to_latest_version)

         If we need to migrate the configuration format. See how it was done for previous
         migrations for more insight. *)
    ]
  in
  let rec try_decode json = function
    | [] -> failwith "Unreachable. Expecting to have at least one version"
    | (version, version_decoder) :: older_versions -> (
        try
          let res = version_decoder json in
          let*! () =
            if version = current_version then Lwt.return_unit
            else
              Event.emit_upgrading_configuration
                ~from:version
                ~into:current_version
          in
          return res
        with e ->
          if List.is_empty older_versions then tzfail (Exn e)
          else try_decode json older_versions)
  in
  fun ~data_dir ->
    let* json =
      let*! json = Lwt_utils_unix.Json.read_file (filename ~data_dir) in
      match json with
      | Ok json -> return json
      | Error (Exn _ :: _ as e) | Error e -> fail e
    in
    let* config = try_decode json config_versions in
    let config = {config with data_dir} in
    (* We save the config so that its format is that of the latest version. *)
    let* () = save config in
    return config

let identity_file {data_dir; _} = Filename.concat data_dir "identity.json"

let peers_file {data_dir; _} = Filename.concat data_dir "peers.json"
