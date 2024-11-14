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
   - 1: changed format of 'profile' field; added 'version' field *)
let current_version = 1

type neighbor = {addr : string; port : int}

type history_mode = Rolling of {blocks : [`Auto | `Some of int]} | Full

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
  neighbors : neighbor list;
  listen_addr : P2p_point.Id.t;
  public_addr : P2p_point.Id.t;
  peers : string list;
  expected_pow : float;
  network_name : string;
  endpoint : Uri.t;
  metrics_addr : P2p_point.Id.t option;
  profile : Profile_manager.t;
  history_mode : history_mode;
  version : int;
  service_name : string option;
  service_namespace : string option;
}

let default_data_dir = Filename.concat (Sys.getenv "HOME") ".tezos-dal-node"

let store_path {data_dir; _} = Filename.concat data_dir "store"

let default_rpc_addr = P2p_point.Id.of_string_exn ~default_port:10732 "0.0.0.0"

let default_listen_addr =
  let open Gossipsub.Transport_layer.Default_parameters.P2p_config in
  let default_net_host = P2p_addr.to_string listening_addr in
  let default_net_port = listening_port in
  P2p_point.Id.of_string_exn ~default_port:default_net_port default_net_host

let default_neighbors = []

let default_peers = []

let default_expected_pow =
  Gossipsub.Transport_layer.Default_parameters.P2p_config.expected_pow

let default_network_name = "dal-sandbox"

let default_endpoint = Uri.of_string "http://localhost:8732"

let default_metrics_port =
  Gossipsub.Transport_layer.Default_parameters.P2p_config.listening_port + 1

let default_metrics_addr =
  P2p_point.Id.of_string_exn ~default_port:default_metrics_port "0.0.0.0"

let default_history_mode = Rolling {blocks = `Auto}

let default =
  {
    data_dir = default_data_dir;
    rpc_addr = default_rpc_addr;
    neighbors = default_neighbors;
    listen_addr = default_listen_addr;
    public_addr = default_listen_addr;
    peers = default_peers;
    expected_pow = default_expected_pow;
    network_name = default_network_name;
    endpoint = default_endpoint;
    metrics_addr = None;
    history_mode = default_history_mode;
    profile = Profile_manager.empty;
    version = current_version;
    service_name = None;
    service_namespace = None;
  }

let neighbor_encoding : neighbor Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {addr; port} -> (addr, port))
    (fun (addr, port) -> {addr; port})
    (obj2 (req "rpc-addr" string) (req "rpc-port" uint16))

let endpoint_encoding : Uri.t Data_encoding.t =
  let open Data_encoding in
  conv_with_guard
    (fun uri -> Uri.to_string uri)
    (fun str ->
      try Uri.of_string str |> Result.ok
      with exn ->
        Format.asprintf
          "endpoint decoding failed:@.%a@."
          Error_monad.pp_print_trace
          [Exn exn]
        |> Result.error)
    string

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           data_dir;
           rpc_addr;
           listen_addr;
           public_addr;
           neighbors;
           peers;
           expected_pow;
           network_name;
           endpoint;
           metrics_addr;
           history_mode;
           profile;
           version;
           service_name;
           service_namespace;
         } ->
      ( ( data_dir,
          rpc_addr,
          listen_addr,
          public_addr,
          neighbors,
          peers,
          expected_pow,
          network_name,
          endpoint,
          metrics_addr ),
        (history_mode, profile, version, service_name, service_namespace) ))
    (fun ( ( data_dir,
             rpc_addr,
             listen_addr,
             public_addr,
             neighbors,
             peers,
             expected_pow,
             network_name,
             endpoint,
             metrics_addr ),
           (history_mode, profile, version, service_name, service_namespace) ) ->
      {
        data_dir;
        rpc_addr;
        listen_addr;
        public_addr;
        neighbors;
        peers;
        expected_pow;
        network_name;
        endpoint;
        metrics_addr;
        history_mode;
        profile;
        version;
        service_name;
        service_namespace;
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
             "neighbors"
             ~description:"DAL Neighbors"
             (list neighbor_encoding)
             default_neighbors)
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
             "network-name"
             ~description:"The name that identifies the network"
             string
             default_network_name)
          (dft
             "endpoint"
             ~description:"The Tezos node endpoint"
             endpoint_encoding
             default_endpoint)
          (dft
             "metrics-addr"
             ~description:"The point for the DAL node metrics server"
             (Encoding.option P2p_point.Id.encoding)
             None))
       (obj5
          (dft
             "history_mode"
             ~description:"The history mode for the DAL node"
             history_mode_encoding
             default_history_mode)
          (dft
             "profiles"
             ~description:"The Octez DAL node profiles"
             Profile_manager.encoding
             Profile_manager.empty)
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
             None)))

module V0 = struct
  type t = {
    data_dir : string;
    rpc_addr : P2p_point.Id.t;
    neighbors : neighbor list;
    listen_addr : P2p_point.Id.t;
    public_addr : P2p_point.Id.t;
    peers : string list;
    expected_pow : float;
    network_name : string;
    endpoint : Uri.t;
    metrics_addr : P2p_point.Id.t;
    profile : Profile_manager.t;
    history_mode : history_mode;
  }

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {
             data_dir;
             rpc_addr;
             listen_addr;
             public_addr;
             neighbors;
             peers;
             expected_pow;
             network_name;
             endpoint;
             metrics_addr;
             history_mode;
             profile;
           } ->
        ( ( data_dir,
            rpc_addr,
            listen_addr,
            public_addr,
            neighbors,
            peers,
            expected_pow,
            network_name,
            endpoint,
            metrics_addr ),
          (history_mode, profile) ))
      (fun ( ( data_dir,
               rpc_addr,
               listen_addr,
               public_addr,
               neighbors,
               peers,
               expected_pow,
               network_name,
               endpoint,
               metrics_addr ),
             (history_mode, profile) ) ->
        {
          data_dir;
          rpc_addr;
          listen_addr;
          public_addr;
          neighbors;
          peers;
          expected_pow;
          network_name;
          endpoint;
          metrics_addr;
          history_mode;
          profile;
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
               "neighbors"
               ~description:"DAL Neighbors"
               (list neighbor_encoding)
               default_neighbors)
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
               "network-name"
               ~description:"The name that identifies the network"
               string
               default_network_name)
            (dft
               "endpoint"
               ~description:"The Tezos node endpoint"
               endpoint_encoding
               default_endpoint)
            (dft
               "metrics-addr"
               ~description:"The point for the DAL node metrics server"
               P2p_point.Id.encoding
               default_metrics_addr))
         (obj2
            (dft
               "history_mode"
               ~description:"The history mode for the DAL node"
               history_mode_encoding
               default_history_mode)
            (dft
               "profiles"
               ~description:"The Octez DAL node profiles"
               Profile_manager.encoding
               Profile_manager.empty)))
end

let from_v0 v0 =
  {
    data_dir = v0.V0.data_dir;
    rpc_addr = v0.rpc_addr;
    listen_addr = v0.listen_addr;
    public_addr = v0.public_addr;
    neighbors = v0.neighbors;
    peers = v0.peers;
    expected_pow = v0.expected_pow;
    network_name = v0.network_name;
    endpoint = v0.endpoint;
    metrics_addr = Some v0.metrics_addr;
    history_mode = v0.history_mode;
    profile = v0.profile;
    version = current_version;
    service_name = None;
    service_namespace = None;
  }

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

let load ~data_dir =
  let open Lwt_result_syntax in
  let* json =
    let*! json = Lwt_utils_unix.Json.read_file (filename ~data_dir) in
    match json with
    | Ok json -> return json
    | Error (Exn _ :: _ as e) -> fail e
    | Error e -> fail e
  in
  let* config =
    Lwt.catch
      (fun () -> Data_encoding.Json.destruct encoding json |> return)
      (fun _e ->
        Data_encoding.Json.destruct V0.encoding json |> from_v0 |> return)
  in
  let config = {config with data_dir} in
  (* We save the config so that its format is that of the latest version. *)
  let* () = save config in
  return config

let identity_file {data_dir; _} = Filename.concat data_dir "identity.json"

let peers_file {data_dir; _} = Filename.concat data_dir "peers.json"
