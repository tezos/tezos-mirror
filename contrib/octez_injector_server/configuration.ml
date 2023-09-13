(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  data_dir : string;
  rpc_address : string;
  rpc_port : int;
  block_delay : float;
  signer : string;
}

let default_data_dir =
  Filename.concat (Sys.getenv "HOME") ".octez-injector-server"

let relative_filename data_dir = Filename.concat data_dir "config.json"

let default_rpc_address = "127.0.0.1"

let default_rpc_port = 6732

let default_block_delay = 0.5

let make ~data_dir ~rpc_address ~rpc_port ~block_delay signer =
  let rpc_port = int_of_string rpc_port in
  let block_delay = float_of_string block_delay in
  {data_dir; rpc_address; rpc_port; block_delay; signer}

let data_dir_path config subpath = Filename.concat config.data_dir subpath

let filename config = relative_filename config.data_dir

let data_dir config = config.data_dir

let signers config = config.signer

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {data_dir; rpc_address; rpc_port; block_delay; signer} ->
      (data_dir, rpc_address, rpc_port, block_delay, signer))
    (fun (data_dir, rpc_address, rpc_port, block_delay, signer) ->
      {data_dir; rpc_address; rpc_port; block_delay; signer})
    (obj5
       (dft
          "data-dir"
          ~description:"Location of the data dir"
          string
          default_data_dir)
       (req "rpc-addr" ~description:"RPC address" string)
       (req "rpc-port" ~description:"RPC port" uint16)
       (req "block-delay" ~description:"Block delay" float)
       (req
          "signer"
          ~description:
            "Signer for injected operations (only one signer currently \
             supported)"
          string))

type error += Injector_server_unable_to_write_configuration_file of string

let () =
  register_error_kind
    ~id:"injector-server.unable_to_write_configuration_file"
    ~title:"Unable to write configuration file"
    ~description:"Unable to write configuration file"
    ~pp:(fun ppf file ->
      Format.fprintf ppf "Unable to write the configuration file %s" file)
    `Permanent
    Data_encoding.(obj1 (req "file" string))
    (function
      | Injector_server_unable_to_write_configuration_file path -> Some path
      | _ -> None)
    (fun path -> Injector_server_unable_to_write_configuration_file path)

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
       (fun _ -> [Injector_server_unable_to_write_configuration_file file])
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
