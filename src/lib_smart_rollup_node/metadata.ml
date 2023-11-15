(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type genesis_info = {level : int32; commitment_hash : Commitment.Hash.t}

let genesis_info_encoding =
  let open Data_encoding in
  conv
    (fun {level; commitment_hash} -> (level, commitment_hash))
    (fun (level, commitment_hash) -> {level; commitment_hash})
    (obj2 (req "level" int32) (req "commitment_hash" Commitment.Hash.encoding))

let path ~dir = Filename.concat dir "metadata"

let read_metadata_file encoding ~dir =
  let open Lwt_result_syntax in
  protect @@ fun () ->
  let filename = path ~dir in
  let*! exists = Lwt_unix.file_exists filename in
  if not exists then return_none
  else
    let* json = Lwt_utils_unix.Json.read_file filename in
    return_some (Data_encoding.Json.destruct encoding json)

let write_metadata_file encoding ~dir version =
  let open Lwt_result_syntax in
  protect @@ fun () ->
  let filename = path ~dir in
  let*! () = Lwt_utils_unix.create_dir dir in
  let json = Data_encoding.Json.construct encoding version in
  Lwt_utils_unix.Json.write_file filename json

module V0 = struct
  type t = {rollup_address : Address.t; context_version : Context.Version.t}

  let encoding =
    let open Data_encoding in
    conv
      (fun {rollup_address; context_version} ->
        (rollup_address, context_version))
      (fun (rollup_address, context_version) ->
        {rollup_address; context_version})
      (obj2
         (req "rollup_address" Address.encoding)
         (req "context_version" Context.Version.encoding))

  let read_metadata_file = read_metadata_file encoding

  let write_metadata_file = write_metadata_file encoding
end

module V1 = struct
  type t = {
    rollup_address : Address.t;
    context_version : Context.Version.t;
    kind : Kind.t;
    genesis_info : genesis_info;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {rollup_address; context_version; kind; genesis_info} ->
        (rollup_address, context_version, kind, genesis_info))
      (fun (rollup_address, context_version, kind, genesis_info) ->
        {rollup_address; context_version; kind; genesis_info})
      (obj4
         (req "rollup_address" Address.encoding)
         (req "context_version" Context.Version.encoding)
         (req "kind" Kind.encoding)
         (req "genesis_info" genesis_info_encoding))

  let read_metadata_file = read_metadata_file encoding

  let write_metadata_file = write_metadata_file encoding
end

module Versioned = struct
  type t = V0 of V0.t | V1 of V1.t

  let encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"metadata.v1"
          Json_only
          V1.encoding
          (function V1 m -> Some m | _ -> None)
          (fun m -> V1 m);
        case
          ~title:"metadata.v0"
          Json_only
          V0.encoding
          (function V0 m -> Some m | _ -> None)
          (fun m -> V0 m);
      ]

  let read_metadata_file = read_metadata_file encoding

  let write_metadata_file = write_metadata_file encoding
end

include V0
