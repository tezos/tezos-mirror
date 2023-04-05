(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                        *)
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

type host_and_port = {host : string; port : int}

let default_data_dir = Filename.concat (Sys.getenv "HOME") ".tezos-dac-node"

let relative_filename data_dir = Filename.concat data_dir "config.json"

let default_rpc_address = "127.0.0.1"

let default_rpc_port = 10832

let default_dac_threshold = 0

let default_dac_addresses = []

let default_reveal_data_dir =
  Filename.concat
    (Filename.concat (Sys.getenv "HOME") ".tezos-smart-rollup-node")
    "wasm_2_0_0"

module Coordinator = struct
  type t = {
    threshold : int;
    committee_members_addresses :
      Tezos_crypto.Aggregate_signature.public_key_hash list;
  }

  let make threshold committee_members_addresses =
    {threshold; committee_members_addresses}

  let encoding =
    Data_encoding.(
      conv
        (fun {threshold; committee_members_addresses} ->
          (threshold, committee_members_addresses))
        (fun (threshold, committee_members_addresses) ->
          {threshold; committee_members_addresses})
        (obj2
           (req "threshold" uint8)
           (req
              "committee_members"
              (list Tezos_crypto.Aggregate_signature.Public_key_hash.encoding))))

  let committee_members_addresses t = t.committee_members_addresses
end

module Committee_member = struct
  type t = {
    coordinator_rpc_address : string;
    coordinator_rpc_port : int;
    address : Tezos_crypto.Aggregate_signature.public_key_hash;
  }

  let make coordinator_rpc_address coordinator_rpc_port address =
    {coordinator_rpc_address; coordinator_rpc_port; address}

  let encoding =
    Data_encoding.(
      conv
        (fun {coordinator_rpc_address; coordinator_rpc_port; address} ->
          (coordinator_rpc_address, coordinator_rpc_port, address))
        (fun (coordinator_rpc_address, coordinator_rpc_port, address) ->
          {coordinator_rpc_address; coordinator_rpc_port; address})
        (obj3
           (req "coordinator_rpc_address" string)
           (req "coordinator_rpc_port" uint16)
           (req
              "address"
              Tezos_crypto.Aggregate_signature.Public_key_hash.encoding)))
end

module Observer = struct
  type t = {coordinator_rpc_address : string; coordinator_rpc_port : int}

  let make coordinator_rpc_address coordinator_rpc_port =
    {coordinator_rpc_address; coordinator_rpc_port}

  let encoding =
    Data_encoding.(
      conv
        (fun {coordinator_rpc_address; coordinator_rpc_port} ->
          (coordinator_rpc_address, coordinator_rpc_port))
        (fun (coordinator_rpc_address, coordinator_rpc_port) ->
          {coordinator_rpc_address; coordinator_rpc_port})
        (obj2
           (req "coordinator_rpc_address" string)
           (req "coordinator_rpc_port" uint16)))
end

module Legacy = struct
  type t = {
    threshold : int;
    committee_members_addresses :
      Tezos_crypto.Aggregate_signature.public_key_hash list;
    dac_cctxt_config : host_and_port option;
    committee_member_address_opt :
      Tezos_crypto.Aggregate_signature.public_key_hash option;
  }

  let make ?coordinator_host_and_port threshold committee_members_addresses
      committee_member_address_opt =
    {
      threshold;
      committee_members_addresses;
      dac_cctxt_config = coordinator_host_and_port;
      committee_member_address_opt;
    }

  let committee_members_addresses t = t.committee_members_addresses

  let threshold t = t.threshold

  let dac_cctxt_config t = t.dac_cctxt_config

  let committee_member_address_opt t = t.committee_member_address_opt

  let host_and_port_encoding =
    let open Data_encoding in
    conv
      (fun {host; port} -> (host, port))
      (fun (host, port) -> {host; port})
      (obj2 (req "rpc-host" string) (req "rpc-port" uint16))

  let encoding =
    Data_encoding.(
      conv
        (fun {
               threshold;
               committee_members_addresses;
               dac_cctxt_config;
               committee_member_address_opt;
             } ->
          ( threshold,
            committee_members_addresses,
            dac_cctxt_config,
            committee_member_address_opt ))
        (fun ( threshold,
               committee_members_addresses,
               dac_cctxt_config,
               committee_member_address_opt ) ->
          {
            threshold;
            committee_members_addresses;
            dac_cctxt_config;
            committee_member_address_opt;
          })
        (obj4
           (dft "threshold" uint8 default_dac_threshold)
           (dft
              "committee_members"
              (list Tezos_crypto.Aggregate_signature.Public_key_hash.encoding)
              default_dac_addresses)
           (opt "dac_cctxt_config" host_and_port_encoding)
           (opt
              "committee_member_address_opt"
              Tezos_crypto.Aggregate_signature.Public_key_hash.encoding)))
end

type mode =
  | Coordinator of Coordinator.t
  | Committee_member of Committee_member.t
  | Observer of Observer.t
  | Legacy of Legacy.t

let make_coordinator threshold committee_members_addresses =
  Coordinator (Coordinator.make threshold committee_members_addresses)

let make_committee_member coordinator_rpc_address coordinator_rpc_port
    committee_member_address =
  Committee_member
    (Committee_member.make
       coordinator_rpc_address
       coordinator_rpc_port
       committee_member_address)

let make_observer coordinator_rpc_address coordinator_rpc_port =
  Observer (Observer.make coordinator_rpc_address coordinator_rpc_port)

let make_legacy ?coordinator_host_and_port threshold committee_members_addresses
    committee_member_address_opt =
  Legacy
    (Legacy.make
       ?coordinator_host_and_port
       threshold
       committee_members_addresses
       committee_member_address_opt)

type t = {
  data_dir : string;  (** The path to the DAC node data directory. *)
  rpc_address : string;  (** The address the DAC node listens to. *)
  rpc_port : int;  (** The port the DAC node listens to. *)
  reveal_data_dir : string;
      (** The directory where the DAC node saves pages. *)
  mode : mode;
      (** Configuration parameters specific to the operating mode of the
          DAC. *)
}

let make ~data_dir ~reveal_data_dir rpc_address rpc_port mode =
  {data_dir; reveal_data_dir; rpc_address; rpc_port; mode}

let data_dir_path config subpath = Filename.concat config.data_dir subpath

let filename config = relative_filename config.data_dir

let data_dir config = config.data_dir

let reveal_data_dir config = config.reveal_data_dir

let mode config = config.mode

let mode_encoding =
  Data_encoding.With_JSON_discriminant.(
    union
      [
        case
          ~title:"Coordinator"
          (Tag (0, "coordinator"))
          Coordinator.encoding
          (function Coordinator t -> Some t | _ -> None)
          (fun t -> Coordinator t);
        case
          ~title:"Committee_member"
          (Tag (1, "committee_member"))
          Committee_member.encoding
          (function Committee_member t -> Some t | _ -> None)
          (fun t -> Committee_member t);
        case
          ~title:"Observer"
          (Tag (2, "observer"))
          Observer.encoding
          (function Observer t -> Some t | _ -> None)
          (fun t -> Observer t);
        case
          ~title:"Legacy"
          (Tag (3, "legacy"))
          Legacy.encoding
          (function Legacy t -> Some t | _ -> None)
          (fun t -> Legacy t);
      ])

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {data_dir; rpc_address; rpc_port; reveal_data_dir; mode} ->
      (data_dir, rpc_address, rpc_port, reveal_data_dir, mode))
    (fun (data_dir, rpc_address, rpc_port, reveal_data_dir, mode) ->
      {data_dir; rpc_address; rpc_port; reveal_data_dir; mode})
    (obj5
       (dft
          "data-dir"
          ~description:"Location of the data dir"
          string
          default_data_dir)
       (dft "rpc-addr" ~description:"RPC address" string default_rpc_address)
       (dft "rpc-port" ~description:"RPC port" uint16 default_rpc_port)
       (dft
          "reveal_data_dir"
          ~description:"Reveal data directory"
          string
          default_reveal_data_dir)
       (req "mode" ~description:"Running mode" mode_encoding))

type error += DAC_node_unable_to_write_configuration_file of string

let () =
  register_error_kind
    ~id:"dac.node.unable_to_write_configuration_file"
    ~title:"Unable to write configuration file"
    ~description:"Unable to write configuration file"
    ~pp:(fun ppf file ->
      Format.fprintf ppf "Unable to write the configuration file %s" file)
    `Permanent
    Data_encoding.(obj1 (req "file" string))
    (function
      | DAC_node_unable_to_write_configuration_file path -> Some path
      | _ -> None)
    (fun path -> DAC_node_unable_to_write_configuration_file path)

let save config =
  let open Lwt_syntax in
  let file = filename config in
  protect @@ fun () ->
  let* v =
    let* () = Lwt_utils_unix.create_dir @@ data_dir config in
    Lwt_utils_unix.with_atomic_open_out file @@ fun chan ->
    let json = Data_encoding.Json.construct encoding config in
    let content = Data_encoding.Json.to_string json in
    Lwt_utils_unix.write_string chan content
  in
  Lwt.return
    (Result.map_error
       (fun _ -> [DAC_node_unable_to_write_configuration_file file])
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
