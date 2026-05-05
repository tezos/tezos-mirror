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
   - 2: removed fields network_name and neighbors.
   - 3: renamed [history_mode] variant [Full] to [Archive] (with [Full] kept as
        a read-only legacy alias); changed the default [history_mode] to
        [Archive]. [history_mode] now governs slot retention only; shards
        always follow [Constants.shard_retention_period_in_levels]. *)
let current_version = 3

type neighbor = {addr : string; port : int}

(* The history mode controls slot retention only. Shards have their own
   (short) lifetime, given by [Constants.shard_retention_period_in_levels],
   regardless of this setting.

   - [Archive] keeps slots indefinitely (default).
   - [Rolling { blocks = `Some n }] keeps slots for about [n] levels.
   - [Rolling { blocks = `Auto }] infers the number of levels depending on
     the L1 parametric constants and the profile. *)
type history_mode = Rolling of {blocks : [`Auto | `Some of int]} | Archive

type batching_configuration = Disabled | Enabled of {time_interval : int}

type experimental_features = unit

type publish_slots_regularly = {
  frequency : int;
  slot_index : int;
  secret_key : Signature.Secret_key.t;
}

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
          | Archive -> None)
        (function
          | (), None -> Rolling {blocks = `Auto}
          | (), Some n -> Rolling {blocks = `Some n});
      case
        ~title:"archive"
        ~description:"Keep slot payloads indefinitely."
        (Tag 1)
        (obj1 (req "kind" (Data_encoding.constant "archive")))
        (function Archive -> Some () | _ -> None)
        (fun () -> Archive);
      (* Legacy alias for [archive], for reading v2 configuration files. Never
         encoded. *)
      case
        ~title:"full"
        ~description:"Deprecated alias for archive."
        (Tag 2)
        (obj1 (req "kind" (Data_encoding.constant "full")))
        (fun _ -> None)
        (fun () -> Archive);
    ]

let batching_configuration_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"disabled"
        ~description:""
        (Tag 0)
        unit
        (function Disabled -> Some () | Enabled _ -> None)
        (fun () -> Disabled);
      case
        ~title:"enabled"
        ~description:""
        (Tag 1)
        int31
        (function
          | Disabled -> None | Enabled {time_interval} -> Some time_interval)
        (fun time_interval -> Enabled {time_interval});
    ]

let publish_slots_regularly_encoding =
  let open Data_encoding in
  conv
    (fun {frequency; slot_index; secret_key} ->
      (frequency, slot_index, secret_key))
    (fun (frequency, slot_index, secret_key) ->
      {frequency; slot_index; secret_key})
    (obj3
       (req "frequency" uint16)
       (req "slot_index" uint16)
       (req "secret_key" Signature.Secret_key.encoding))

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
  telemetry_env : string option;
  experimental_features : experimental_features;
  fetch_trusted_setup : bool;
  verbose : bool;
  ignore_l1_config_peers : bool;
  disable_amplification : bool;
  banned_addrs : P2p_addr.t list;
  batching_configuration : batching_configuration;
  publish_slots_regularly : publish_slots_regularly option;
  profiling : Tezos_profiler.Profiler.profiling_config option;
  rpc_acl_policy : Tezos_rpc_http_server.RPC_server.Acl.policy;
}

let default_data_dir = Filename.concat (Sys.getenv "HOME") ".tezos-dal-node"

let default_config_file data_dir = Filename.concat data_dir "config.json"

let store_path {data_dir; _} = Filename.concat data_dir "store"

let default_rpc_addr =
  P2p_point.Id.of_string_exn ~default_port:10732 "127.0.0.1"

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

let default_endpoint = Uri.of_string "http://localhost:8732"

let default_metrics_port =
  Gossipsub.Transport_layer.Default_parameters.P2p_config.listening_port + 1

let default_history_mode = Archive

let default_service_name = "octez-dal-node"

let default_service_namespace = "octez-dal-node"

let default_experimental_features = ()

let default_fetch_trusted_setup = true

(* By default, when a shard is received, we wait for 0.1 seconds for other
   shards of the same commitment before launching the cryptographic validation
   of the shards.
   Since there shards are supposed to be received several levels in advance,
   the risk that this 0.1 second delay makes the validation happen too late
   is very low.
   It also slows down gossiping a bit, since messages are advertised only after
   validation, so if a message has to go through several nodes before reaching
   its final destination, the waiting delay accumulates and may be of a few
   seconds. It looks fine with 8 blocks of attestation lag and 6 seconds block
   time but if those values are reduced a lot, this might become an issue.*)
let default_batching_configuration = Enabled {time_interval = 100}

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
    telemetry_env = None;
    experimental_features = default_experimental_features;
    fetch_trusted_setup = default_fetch_trusted_setup;
    verbose = false;
    ignore_l1_config_peers = false;
    disable_amplification = false;
    banned_addrs = [];
    batching_configuration = default_batching_configuration;
    publish_slots_regularly = None;
    profiling = None;
    rpc_acl_policy = Tezos_rpc_http_server.RPC_server.Acl.empty_policy;
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
           telemetry_env;
           experimental_features;
           fetch_trusted_setup;
           verbose;
           ignore_l1_config_peers;
           disable_amplification;
           banned_addrs;
           batching_configuration;
           publish_slots_regularly;
           profiling;
           rpc_acl_policy;
         }
       ->
      ( ( ( data_dir,
            rpc_addr,
            listen_addr,
            public_addr,
            peers,
            expected_pow,
            endpoint,
            slots_backup_uris,
            trust_slots_backup_uris,
            metrics_addr ),
          ( (history_mode, profile, version),
            ( service_name,
              service_namespace,
              telemetry_env,
              experimental_features,
              fetch_trusted_setup,
              verbose,
              ignore_l1_config_peers,
              disable_amplification ) ) ),
        ( banned_addrs,
          batching_configuration,
          publish_slots_regularly,
          profiling,
          rpc_acl_policy ) ))
    (fun ( ( ( data_dir,
               rpc_addr,
               listen_addr,
               public_addr,
               peers,
               expected_pow,
               endpoint,
               slots_backup_uris,
               trust_slots_backup_uris,
               metrics_addr ),
             ( (history_mode, profile, version),
               ( service_name,
                 service_namespace,
                 telemetry_env,
                 experimental_features,
                 fetch_trusted_setup,
                 verbose,
                 ignore_l1_config_peers,
                 disable_amplification ) ) ),
           ( banned_addrs,
             batching_configuration,
             publish_slots_regularly,
             profiling,
             rpc_acl_policy ) )
       ->
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
        telemetry_env;
        experimental_features;
        fetch_trusted_setup;
        verbose;
        ignore_l1_config_peers;
        disable_amplification;
        banned_addrs;
        batching_configuration;
        publish_slots_regularly;
        profiling;
        rpc_acl_policy;
      })
    (merge_objs
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
                ~description:"P2P listening address of this node"
                P2p_point.Id.encoding
                default_listen_addr)
             (dft
                "public-addr"
                ~description:"P2P public address of this node"
                P2p_point.Id.encoding
                default_public_addr)
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
                ~description:
                  "Optional HTTP endpoints to fetch missing slots from."
                (list uri_encoding)
                [])
             (dft
                "trust_slots_backup_uris"
                ~description:
                  "Whether to trust the data downlaoded from the provided HTTP \
                   backup URIs."
                bool
                false)
             (opt
                "metrics-addr"
                ~description:"The point for the DAL node metrics server"
                P2p_point.Id.encoding))
          (merge_objs
             (obj3
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
                (req
                   "version"
                   ~description:"The configuration file version"
                   int31))
             (obj8
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
                (opt
                   "telemetry_environment"
                   ~description:"Environment name for telemetry"
                   Data_encoding.string)
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
                   default.disable_amplification))))
       (obj5
          (dft
             "banned_addrs"
             ~description:
               "List of IP addresses to ban. Connections from/to these \
                addresses will be rejected by the P2P layer."
             (list P2p_addr.encoding)
             [])
          (dft
             "batching_configuration"
             ~description:"Set the batching delay for shard verification"
             batching_configuration_encoding
             default.batching_configuration)
          (opt
             "publish_slots_regularly"
             ~description:
               "Set the frequency, the slot and the secret key used for \
                automatic production"
             publish_slots_regularly_encoding)
          (opt
             "profiling"
             ~description:"Configuration of profiling output"
             Tezos_profiler.Profiler.profiling_config_encoding)
          (dft
             "acl"
             ~description:
               "A list of RPC ACL overrides per bind address. When an entry \
                matches the configured rpc-addr, its ACL replaces the default \
                dal_secure (public) / allow_all (loopback) policy."
             Tezos_rpc_http_server.RPC_server.Acl.policy_encoding
             default.rpc_acl_policy)))

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

type error += DAL_node_unable_to_overwrite_configuration_file of string

let () =
  register_error_kind
    ~id:"dal.node.unable_to_overwrite_configuration_file"
    ~title:"Unable to overwrite configuration file"
    ~description:"Configuration file already exists, overwriting forbidden."
    ~pp:(fun ppf file ->
      Format.fprintf
        ppf
        "Configuration file %s already exists, overwriting is forbidden, \
         please use `config update` command"
        file)
    `Permanent
    Data_encoding.(obj1 (req "file" string))
    (function
      | DAL_node_unable_to_overwrite_configuration_file path -> Some path
      | _ -> None)
    (fun path -> DAL_node_unable_to_overwrite_configuration_file path)

let save ~allow_overwrite ~config_file config =
  let open Lwt_syntax in
  protect @@ fun () ->
  let* file_exists = Lwt_unix.file_exists config_file in
  if file_exists && not allow_overwrite then
    Lwt_result_syntax.tzfail
      (DAL_node_unable_to_overwrite_configuration_file config_file)
  else
    let* v =
      let* () = Lwt_utils_unix.create_dir config.data_dir in
      Lwt_utils_unix.with_atomic_open_out config_file @@ fun chan ->
      let json = Data_encoding.Json.construct encoding config in
      let content = Data_encoding.Json.to_string json in
      Lwt_utils_unix.write_string chan content
    in
    Lwt.return
      (Result.map_error
         (fun _ ->
           Error_monad.TzTrace.make
           @@ DAL_node_unable_to_write_configuration_file config_file)
         v)

(* Whether the [history_mode] field is explicitly present in the raw
   configuration JSON. The config encoding uses [merge_objs], which flattens
   all fields to the top level of the JSON object, so the field (if set)
   appears as a top-level key. Used to decide whether a default change to
   [history_mode] silently affects this configuration. *)
let history_mode_field_is_set json =
  match json with
  | `O fields -> List.mem_assoc ~equal:String.equal "history_mode" fields
  | _ -> false

let load =
  let open Lwt_result_syntax in
  let destruct = Data_encoding.Json.destruct in
  let config_versions =
    [
      (current_version, destruct encoding);
      (* The current encoding is backward compatible with v2 configuration
         files: it accepts the legacy ["kind": "full"] history-mode and maps
         it transparently to [Archive]. The [version] field stored in the
         file is overridden to [current_version] after decoding (see
         [try_decode] below). *)
    ]
  in
  let rec try_decode json = function
    | [] -> failwith "Unreachable. Expecting to have at least one version"
    | (_version, version_decoder) :: older_versions -> (
        try
          let res = version_decoder json in
          let stored_version = res.version in
          (* Always set [version] to [current_version] so that the subsequent
             [save] writes the latest version number. *)
          let res = {res with version = current_version} in
          let*! () =
            if stored_version = current_version then Lwt.return_unit
            else
              let open Lwt_syntax in
              let* () =
                Event.emit_upgrading_configuration
                  ~from:stored_version
                  ~into:current_version
              in
              (* Version 3 changed the default [history_mode] from a bounded
                 rolling window to [Archive]. Warn precisely when this
                 actually affects the node: (1) upgrading from an earlier
                 version, (2) with no explicit [history_mode] in the file (so
                 the new default now applies), and (3) the node has an
                 operator profile (only operator slots get the
                 [history_mode]-driven retention; without one, every slot
                 still follows the bounded shard window regardless). The
                 configuration profile suffices for check (3): CLI arguments
                 only ever add profiles, so a config without an operator
                 profile cannot gain one that changes this. *)
              if
                stored_version < 3 && current_version >= 3
                && (not (history_mode_field_is_set json))
                && Profile_manager.unresolved_supports_refutations res.profile
              then Event.emit_history_mode_default_changed ()
              else Lwt.return_unit
          in
          return res
        with e ->
          if List.is_empty older_versions then tzfail (Exn e)
          else try_decode json older_versions)
  in
  fun ?on_file_not_found ~config_file () ->
    let*! file_exists = Lwt_unix.file_exists config_file in
    if not file_exists then
      match on_file_not_found with
      | None ->
          Lwt_result_syntax.tzfail
          @@ Errors.Missing_configuration_file {file = config_file}
      | Some handler -> handler ()
    else
      let* json = Lwt_utils_unix.Json.read_file config_file in
      let* config = try_decode json config_versions in
      (* We save the config so that its format is that of the latest version. *)
      let* () = save ~allow_overwrite:true ~config_file config in
      return config

let identity_file {data_dir; _} = Filename.concat data_dir "identity.json"

let peers_file {data_dir; _} = Filename.concat data_dir "peers.json"

let exit_on_configuration_error ~emit result_p : 'a tzresult Lwt.t =
  let open Lwt_syntax in
  let* result = result_p in
  match result with
  | Ok _ -> result_p
  | Error error_trace ->
      let* () = emit ~error_trace in
      Lwt_exit.exit_and_raise Errors.Exit_codes.invalid_configuration_file_code
