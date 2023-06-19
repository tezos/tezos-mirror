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

type neighbor = {addr : string; port : int}

type t = {
  use_unsafe_srs : bool;
  data_dir : string;
  rpc_addr : P2p_point.Id.t;
  neighbors : neighbor list;
  listen_addr : P2p_point.Id.t;
  peers : P2p_point.Id.t list;
  expected_pow : float;
  network_name : string;
  endpoint : Uri.t;
}

let default_data_dir = Filename.concat (Sys.getenv "HOME") ".tezos-dal-node"

let data_dir_path config subpath = Filename.concat config.data_dir subpath

let relative_filename data_dir = Filename.concat data_dir "config.json"

let filename config = relative_filename config.data_dir

let default_rpc_addr =
  P2p_point.Id.of_string_exn ~default_port:10732 "127.0.0.1"

let default_listen_addr =
  let open Gossipsub.Transport_layer.Default_parameters.P2p_config in
  let default_net_host = P2p_addr.to_string listening_addr in
  let default_net_port = listening_port in
  P2p_point.Id.of_string_exn ~default_port:default_net_port default_net_host

let default_neighbors = []

let default_peers = []

let default_use_unsafe_srs = false

let default_expected_pow =
  Gossipsub.Transport_layer.Default_parameters.P2p_config.expected_pow

let default_network_name = "dal-sandbox"

let default_endpoint = Uri.of_string "http://localhost:9732"

let default =
  {
    use_unsafe_srs = false;
    data_dir = default_data_dir;
    rpc_addr = default_rpc_addr;
    neighbors = default_neighbors;
    listen_addr = default_listen_addr;
    peers = default_peers;
    expected_pow = default_expected_pow;
    network_name = default_network_name;
    endpoint = default_endpoint;
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
           use_unsafe_srs;
           data_dir;
           rpc_addr;
           listen_addr;
           neighbors;
           peers;
           expected_pow;
           network_name;
           endpoint;
         } ->
      ( use_unsafe_srs,
        data_dir,
        rpc_addr,
        listen_addr,
        neighbors,
        peers,
        expected_pow,
        network_name,
        endpoint ))
    (fun ( use_unsafe_srs,
           data_dir,
           rpc_addr,
           listen_addr,
           neighbors,
           peers,
           expected_pow,
           network_name,
           endpoint ) ->
      {
        use_unsafe_srs;
        data_dir;
        rpc_addr;
        listen_addr;
        neighbors;
        peers;
        expected_pow;
        network_name;
        endpoint;
      })
    (obj9
       (dft
          "use_unsafe_srs"
          ~description:"use unsafe srs for tests"
          bool
          default_use_unsafe_srs)
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
          "neighbors"
          ~description:"DAL Neighbors"
          (list neighbor_encoding)
          default_neighbors)
       (dft
          "peers"
          ~description:"P2P addresses of remote peers"
          (list P2p_point.Id.encoding)
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
          default_endpoint))

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

let save config =
  let open Lwt_syntax in
  let file = filename config in
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
  let+ json =
    let*! json = Lwt_utils_unix.Json.read_file (relative_filename data_dir) in
    match json with
    | Ok json -> return json
    | Error (Exn _ :: _ as e) ->
        let*! () = Event.(emit data_dir_not_found data_dir) in
        fail e
    | Error e -> fail e
  in
  let config = Data_encoding.Json.destruct encoding json in
  {config with data_dir}

let identity_file ~data_dir = Filename.concat data_dir "identity.json"

let peers_file ~data_dir = Filename.concat data_dir "peers.json"
