(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Opentelemetry

let instance_id_filename = "telemetry_id"

let setup ~data_dir ~service_namespace ~service_name
    {Opentelemetry_config.enable; instance_id; config} =
  let open Lwt_syntax in
  let+ instance_id =
    match instance_id with
    | Some id -> return id
    | None ->
        let file = Filename.concat data_dir instance_id_filename in
        let* exists = Lwt_unix.file_exists file in
        if exists then Lwt_utils_unix.read_file file
        else
          let _, _, id = Tezos_crypto.Crypto_box.random_keypair () in
          let sid = Tezos_crypto.Crypto_box.Public_key_hash.to_b58check id in
          let* () = Lwt_utils_unix.create_file file sid in
          return sid
  in
  Globals.service_name := service_name ;
  Globals.service_namespace := Some service_namespace ;
  Globals.service_instance_id := Some instance_id ;
  Opentelemetry_ambient_context.set_storage_provider
    (Opentelemetry_ambient_context_lwt.storage ()) ;
  Opentelemetry_client_cohttp_lwt.setup ~enable ~config ()
