(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type backend = Irmin | Brassaia | Tezedge

type error += Metadata_file_read_error of string

type error += Metadata_file_parse_error of string

type error += Metadata_file_write_error of string

let () =
  register_error_kind
    `Permanent
    ~id:"context_metadata.metadata_file_read_error"
    ~title:"Context metadata file read error"
    ~description:"Failed to read metadata file."
    Data_encoding.(obj1 (req "metadata_file_path" string))
    ~pp:(fun ppf path ->
      Format.fprintf ppf "Failed to read metadata file at '%s'." path)
    (function Metadata_file_read_error path -> Some path | _ -> None)
    (fun path -> Metadata_file_read_error path) ;
  register_error_kind
    `Permanent
    ~id:"context_metadata.metadata_file_parse_error"
    ~title:"Context metadata file parse error"
    ~description:"Failed to parse metadata file."
    Data_encoding.(obj1 (req "metadata_file_path" string))
    ~pp:(fun ppf path ->
      Format.fprintf ppf "Failed to parse metadata file at '%s'." path)
    (function Metadata_file_parse_error path -> Some path | _ -> None)
    (fun path -> Metadata_file_parse_error path) ;
  register_error_kind
    `Permanent
    ~id:"context_metadata.metadata_file_write_error"
    ~title:"Context metadata file write error"
    ~description:"Failed to write metadata file."
    Data_encoding.(obj1 (req "metadata_file_path" string))
    ~pp:(fun ppf path ->
      Format.fprintf ppf "Failed to write metadata file at '%s'." path)
    (function Metadata_file_write_error path -> Some path | _ -> None)
    (fun path -> Metadata_file_write_error path)

let metadata_filename = "metadata.json"

let metadata_file context_dir = Filename.concat context_dir metadata_filename

let encoding =
  let open Data_encoding in
  obj1
    (req
       "backend"
       (string_enum
          [("irmin", Irmin); ("brassaia", Brassaia); ("tezedge", Tezedge)]))

let pp fmt = function
  | Irmin -> Format.fprintf fmt "irmin"
  | Brassaia -> Format.fprintf fmt "brassaia"
  | Tezedge -> Format.fprintf fmt "tezedge"

let read_metadata_file context_dir =
  let open Lwt_result_syntax in
  let metadata_file = metadata_file context_dir in
  let* json =
    trace
      (Metadata_file_read_error metadata_file)
      (Lwt_utils_unix.Json.read_file metadata_file)
  in
  try return (Data_encoding.Json.destruct encoding json)
  with _ -> tzfail (Metadata_file_parse_error metadata_file)

let write_metadata_file context_dir backend =
  let metadata_file = metadata_file context_dir in
  Lwt_utils_unix.Json.write_file
    metadata_file
    (Data_encoding.Json.construct encoding backend)
  |> trace (Metadata_file_write_error metadata_file)
