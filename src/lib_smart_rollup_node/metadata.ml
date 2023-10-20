(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type t = {rollup_address : Address.t; context_version : Context.Version.t}

let encoding =
  let open Data_encoding in
  conv
    (fun {rollup_address; context_version} -> (rollup_address, context_version))
    (fun (rollup_address, context_version) -> {rollup_address; context_version})
    (obj2
       (req "rollup_address" Address.encoding)
       (req "context_version" Context.Version.encoding))

let path ~dir = Filename.concat dir "metadata"

let read_metadata_file ~dir =
  let open Lwt_result_syntax in
  protect @@ fun () ->
  let filename = path ~dir in
  let*! exists = Lwt_unix.file_exists filename in
  if not exists then return_none
  else
    let* json = Lwt_utils_unix.Json.read_file filename in
    return_some (Data_encoding.Json.destruct encoding json)

let write_metadata_file ~dir version =
  let open Lwt_result_syntax in
  protect @@ fun () ->
  let filename = path ~dir in
  let*! () = Lwt_utils_unix.create_dir dir in
  let json = Data_encoding.Json.construct encoding version in
  Lwt_utils_unix.Json.write_file filename json
