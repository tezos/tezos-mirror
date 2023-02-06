(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

type coordinator = {
  threshold : int;
  dac_members_addresses : Tezos_crypto.Aggregate_signature.public_key_hash list;
}

type dac_member = {
  coordinator_rpc_address : string;
  coordinator_rpc_port : int;
  address : Tezos_crypto.Aggregate_signature.public_key_hash;
}

type observer = {coordinator_rpc_address : string; coordinator_rpc_port : int}

type legacy = {
  threshold : int;
  dac_members_addresses : Tezos_crypto.Aggregate_signature.public_key_hash list;
}

type mode =
  | Coordinator of coordinator
  | Dac_member of dac_member
  | Observer of observer
  | Legacy of legacy

type t = {
  data_dir : string;
  rpc_address : string;
  rpc_port : int;
  reveal_data_dir : string;
  mode : mode;
}

let default_data_dir = Filename.concat (Sys.getenv "HOME") ".tezos-dac-node"

let data_dir_path config subpath = Filename.concat config.data_dir subpath

let relative_filename data_dir = Filename.concat data_dir "config.json"

let filename config = relative_filename config.data_dir

let default_rpc_address = "127.0.0.1"

let default_rpc_port = 10832

let default_dac_threshold = 0

let default_dac_addresses = []

let default_reveal_data_dir =
  Filename.concat
    (Filename.concat (Sys.getenv "HOME") ".tezos-smart-rollup-node")
    "wasm_2_0_0"

let coordinator_encoding =
  Data_encoding.(
    conv_with_guard
      (fun ({threshold; dac_members_addresses} : coordinator) ->
        (threshold, dac_members_addresses, false))
      (fun (threshold, dac_members_addresses, legacy) ->
        if legacy then Error "legacy flag should be set to false"
        else Ok {threshold; dac_members_addresses})
      (obj3
         (req "threshold" uint8)
         (req
            "dac_members"
            (list Tezos_crypto.Aggregate_signature.Public_key_hash.encoding))
         (req "legacy" bool)))

let dac_member_encoding =
  Data_encoding.(
    conv
      (fun {coordinator_rpc_address; coordinator_rpc_port; address} ->
        (coordinator_rpc_address, coordinator_rpc_port, address))
      (fun (coordinator_rpc_address, coordinator_rpc_port, address) ->
        {coordinator_rpc_address; coordinator_rpc_port; address})
      (obj3
         (req "coordinator_rpc_address" string)
         (req "coordinator_rpc_port" int16)
         (req
            "address"
            Tezos_crypto.Aggregate_signature.Public_key_hash.encoding)))

let observer_encoding =
  Data_encoding.(
    conv
      (fun {coordinator_rpc_address; coordinator_rpc_port} ->
        (coordinator_rpc_address, coordinator_rpc_port))
      (fun (coordinator_rpc_address, coordinator_rpc_port) ->
        {coordinator_rpc_address; coordinator_rpc_port})
      (obj2
         (req "coordinator_rpc_address" string)
         (req "coordinator_rpc_port" int16)))

let legacy_encoding =
  Data_encoding.(
    conv_with_guard
      (fun {threshold; dac_members_addresses} ->
        (threshold, dac_members_addresses, true))
      (fun (threshold, dac_members_addresses, legacy) ->
        if legacy then Ok {threshold; dac_members_addresses}
        else Error "legacy flag should be set to true")
      (obj3
         (dft "threshold" uint8 default_dac_threshold)
         (dft
            "dac_members"
            (list Tezos_crypto.Aggregate_signature.Public_key_hash.encoding)
            default_dac_addresses)
         (req "legacy" bool)))

let mode_config_encoding =
  Data_encoding.(
    union
      [
        case
          ~title:"coordinator"
          (Tag 0)
          coordinator_encoding
          (function Coordinator config -> Some config | _ -> None)
          (function config -> Coordinator config);
        case
          ~title:"dac_member"
          (Tag 1)
          dac_member_encoding
          (function Dac_member config -> Some config | _ -> None)
          (function config -> Dac_member config);
        case
          ~title:"observer"
          (Tag 2)
          observer_encoding
          (function Observer config -> Some config | _ -> None)
          (function config -> Observer config);
        case
          ~title:"legacy"
          (Tag 3)
          legacy_encoding
          (function Legacy config -> Some config | _ -> None)
          (function config -> Legacy config);
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
       (req "mode" ~description:"Running mode" mode_config_encoding))

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
    let* () = Lwt_utils_unix.create_dir config.data_dir in
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
