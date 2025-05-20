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
  http_backup_uris : Uri.t list;
  trust_http_backup_uris : bool;
  metrics_addr : P2p_point.Id.t option;
  profile : Profile_manager.unresolved_profile;
  history_mode : history_mode;
  version : int;
  service_name : string option;
  service_namespace : string option;
  experimental_features : experimental_features;
  fetch_trusted_setup : bool;
  verbose : bool;
  ignore_l1_config_peers : bool;
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

let default_neighbors = []

let default_peers = []

let default_expected_pow =
  Gossipsub.Transport_layer.Default_parameters.P2p_config.expected_pow

let legacy_network_name = "dal-sandbox"

let default_endpoint = Uri.of_string "http://localhost:8732"

let default_metrics_port =
  Gossipsub.Transport_layer.Default_parameters.P2p_config.listening_port + 1

let default_metrics_addr =
  P2p_point.Id.of_string_exn ~default_port:default_metrics_port "0.0.0.0"

let default_history_mode = Rolling {blocks = `Auto}

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
    http_backup_uris = [];
    trust_http_backup_uris = false;
    metrics_addr = None;
    history_mode = default_history_mode;
    profile = Profile_manager.Empty;
    version = current_version;
    service_name = None;
    service_namespace = None;
    experimental_features = default_experimental_features;
    fetch_trusted_setup = default_fetch_trusted_setup;
    verbose = false;
    ignore_l1_config_peers = false;
  }

let neighbor_encoding : neighbor Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {addr; port} -> (addr, port))
    (fun (addr, port) -> {addr; port})
    (obj2 (req "rpc-addr" string) (req "rpc-port" uint16))

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
           http_backup_uris;
           trust_http_backup_uris;
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
         } ->
      ( ( data_dir,
          rpc_addr,
          listen_addr,
          public_addr,
          peers,
          expected_pow,
          endpoint,
          http_backup_uris,
          trust_http_backup_uris,
          metrics_addr ),
        ( history_mode,
          profile,
          version,
          service_name,
          service_namespace,
          experimental_features,
          fetch_trusted_setup,
          verbose,
          ignore_l1_config_peers ) ))
    (fun ( ( data_dir,
             rpc_addr,
             listen_addr,
             public_addr,
             peers,
             expected_pow,
             endpoint,
             http_backup_uris,
             trust_http_backup_uris,
             metrics_addr ),
           ( history_mode,
             profile,
             version,
             service_name,
             service_namespace,
             experimental_features,
             fetch_trusted_setup,
             verbose,
             ignore_l1_config_peers ) ) ->
      {
        data_dir;
        rpc_addr;
        listen_addr;
        public_addr;
        peers;
        expected_pow;
        endpoint;
        http_backup_uris;
        trust_http_backup_uris;
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
             "http_backup_uris"
             ~description:"Optional HTTP endpoints to fetch missing slots from."
             (list uri_encoding)
             [])
          (dft
             "trust_http_backup_uris"
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
       (obj9
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
             (Data_encoding.option Data_encoding.string)
             None)
          (dft
             "service_namespace"
             ~description:"Namespace for the service"
             (Data_encoding.option Data_encoding.string)
             None)
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
             default.ignore_l1_config_peers)))

module V0 = struct
  type v0_profile =
    | Bootstrap
    | Controller of Controller_profiles.t
    | Random_observer

  let v0_profile_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"Bootstrap node"
          (Tag 1)
          (obj1 (req "kind" (constant "bootstrap")))
          (function Bootstrap -> Some () | _ -> None)
          (function () -> Bootstrap);
        case
          ~title:"Controller"
          (Tag 2)
          (obj2
             (req "kind" (constant "controller"))
             (req "controller_profiles" Controller_profiles.encoding))
          (function
            | Controller controller_profiles -> Some ((), controller_profiles)
            | _ -> None)
          (function (), controller_profiles -> Controller controller_profiles);
        case
          ~title:"Random_observer"
          (Tag 3)
          (obj1 (req "kind" (constant "random_observer")))
          (function Random_observer -> Some () | _ -> None)
          (function () -> Random_observer);
      ]

  let to_latest_profile = function
    | Random_observer -> Profile_manager.Random_observer
    | Bootstrap -> Profile_manager.bootstrap
    | Controller profile -> Profile_manager.controller profile

  (* Legacy V0 configuration type used solely for migration purposes.

     This type represents the legacy (V0) version of the configuration,
     originally defined as a record. It is intentionally rewritten as a tuple
     for the following reasons:

     - Simplified Encoding/Decoding: Using a tuple allows removing
     [Data_encoding.conv] and its field-by-field mapping, significantly reducing
     boilerplate in the migration code.

     - Read-Only & Migration-Only: The V0 type is no longer edited or used
     beyond JSON decoding and transformation into the current configuration
     version. Record semantics (field names, accessors) are unnecessary.

     This design reflects the temporary, transitional nature of the V0
     configuration and isolates legacy logic from the active codebase. *)
  type t = tup1 * tup2

  and tup1 =
    string (* data_dir *)
    * P2p_point.Id.t (* rpc_addr *)
    * P2p_point.Id.t (* listen_addr *)
    * P2p_point.Id.t (* public_addr *)
    * neighbor list (* neighbors *)
    * string list (* peers *)
    * float (* expected_pow *)
    * string (* network_name *)
    * Uri.t (* endpoint *)
    * P2p_point.Id.t (* metrics_addr *)

  and tup2 = history_mode (* history_mode *) * v0_profile (* profile *)

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    merge_objs
      (obj10
         (dft "data-dir" string default_data_dir)
         (dft "rpc-addr" P2p_point.Id.encoding default_rpc_addr)
         (dft "net-addr" P2p_point.Id.encoding default_listen_addr)
         (dft "public-addr" P2p_point.Id.encoding default_listen_addr)
         (dft "neighbors" (list neighbor_encoding) default_neighbors)
         (dft "peers" (list string) default_peers)
         (dft "expected-pow" float default_expected_pow)
         (dft "network-name" string legacy_network_name)
         (dft "endpoint" uri_encoding default_endpoint)
         (dft "metrics-addr" P2p_point.Id.encoding default_metrics_addr))
      (obj2
         (dft "history_mode" history_mode_encoding default_history_mode)
         (dft
            "profiles"
            v0_profile_encoding
            (Controller Controller_profiles.empty)))

  let to_latest_version
      ( ( data_dir,
          rpc_addr,
          listen_addr,
          public_addr,
          _neighbors,
          peers,
          expected_pow,
          _network_name,
          endpoint,
          metrics_addr ),
        (history_mode, profile) ) =
    {
      data_dir;
      rpc_addr;
      listen_addr;
      public_addr;
      peers;
      expected_pow;
      endpoint;
      metrics_addr = Some metrics_addr;
      history_mode;
      profile = to_latest_profile profile;
      version = current_version;
      service_name = None;
      service_namespace = None;
      experimental_features = default_experimental_features;
      fetch_trusted_setup = true;
      verbose = false;
      ignore_l1_config_peers = false;
      http_backup_uris = [];
      trust_http_backup_uris = false;
    }
end

module V1 = struct
  (* Legacy V1 configuration type used solely for migration purposes.

     This type represents the legacy (V1) version of the configuration,
     originally defined as a record. It is intentionally rewritten as a tuple
     for the following reasons:

     - Simplified Encoding/Decoding: Using a tuple allows removing
     [Data_encoding.conv] and its field-by-field mapping, significantly reducing
     boilerplate in the migration code.

     - Read-Only & Migration-Only: The V1 type is no longer edited or used
     beyond JSON decoding and transformation into the current configuration
     version. Record semantics (field names, accessors) are unnecessary.

     This design reflects the temporary, transitional nature of the V1
     configuration and isolates legacy logic from the active codebase. *)
  type t = tup1 * tup2

  and tup1 =
    string (* data_dir *)
    * P2p_point.Id.t (* rpc_addr *)
    * P2p_point.Id.t (*listen_addr  *)
    * P2p_point.Id.t (* public_addr *)
    * neighbor list (* neighbors *)
    * string list (* peers *)
    * float (* expected_pow *)
    * string (*network_name  *)
    * Uri.t (*endpoint  *)
    * P2p_point.Id.t option (*metrics_addr  *)

  and tup2 =
    history_mode (* history_mode *)
    * V0.v0_profile (* profile *)
    * int (*version  *)
    * string option (* service_name *)
    * string option (*service_namespace  *)
    * experimental_features (* experimental_features *)
    * bool (* fetch_trusted_setup *)
    * bool (* verbose *)

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    merge_objs
      (obj10
         (dft "data-dir" string default_data_dir)
         (dft "rpc-addr" P2p_point.Id.encoding default_rpc_addr)
         (dft "net-addr" P2p_point.Id.encoding default_listen_addr)
         (dft "public-addr" P2p_point.Id.encoding default_listen_addr)
         (dft "neighbors" (list neighbor_encoding) default_neighbors)
         (dft "peers" (list string) default_peers)
         (dft "expected-pow" float default_expected_pow)
         (dft "network-name" string legacy_network_name)
         (dft "endpoint" uri_encoding default_endpoint)
         (dft "metrics-addr" (Encoding.option P2p_point.Id.encoding) None))
      (obj8
         (dft "history_mode" history_mode_encoding default_history_mode)
         (dft
            "profiles"
            V0.v0_profile_encoding
            (V0.Controller Controller_profiles.empty))
         (req "version" int31)
         (dft "service_name" (Data_encoding.option Data_encoding.string) None)
         (dft
            "service_namespace"
            (Data_encoding.option Data_encoding.string)
            None)
         (dft
            "experimental_features"
            experimental_features_encoding
            default_experimental_features)
         (dft "fetch_trusted_setup" bool true)
         (dft "verbose" bool default.verbose))

  let to_latest_version
      ( ( data_dir,
          rpc_addr,
          listen_addr,
          public_addr,
          _neighbors,
          peers,
          expected_pow,
          _network_name,
          endpoint,
          metrics_addr ),
        ( history_mode,
          profile,
          version,
          service_name,
          service_namespace,
          experimental_features,
          fetch_trusted_setup,
          verbose ) ) =
    {
      data_dir;
      rpc_addr;
      listen_addr;
      public_addr;
      peers;
      expected_pow;
      endpoint;
      metrics_addr;
      history_mode;
      profile = V0.to_latest_profile profile;
      version;
      service_name;
      service_namespace;
      experimental_features;
      fetch_trusted_setup;
      verbose;
      ignore_l1_config_peers = false;
      http_backup_uris = [];
      trust_http_backup_uris = false;
    }
end

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
      (1, fun json -> destruct V1.encoding json |> V1.to_latest_version);
      (0, fun json -> destruct V0.encoding json |> V0.to_latest_version);
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
