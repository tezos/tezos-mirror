(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Docker_embedded of string
  | Local_file of string
  | Url of string
  | No_snapshot

(** Parse raw snapshot CLI option. *)
let parse_snapshot =
  let fail path =
    Test.fail
      "wrong snapshot argument (--snapshot) [%s].@.Use:@.- \
       \"file:path/to/file\" to use a local file that will be uploaded to each \
       agent,@.- \"docker\" to use the docker_embedded_snapshot_file, that \
       must be located in the local path, to embed the snapshot file into the \
       docker image."
      path
  in
  function
  | None -> No_snapshot
  | Some s when Re.Str.(string_match (regexp "^https?://.+$") s 0) -> Url s
  | Some s when String.equal s "docker" ->
      (* This hardcoded path must be defined as the location path of the snapshot
         embedded in the docker image. See the associated dockerfile. *)
      Docker_embedded "/tmp/docker_embedded_snapshot_file"
  | Some s when String.starts_with ~prefix:"file:" s -> (
      match String.split_on_char ':' s with
      | [_; path] -> Local_file path
      | _ -> fail s)
  | Some s -> fail s

let to_string = function
  | No_snapshot -> "No_snapshot"
  | Docker_embedded path -> Format.sprintf "Docker_embedded: %s" path
  | Local_file path -> Format.sprintf "Local_file: %s" path
  | Url url -> Format.sprintf "Url: %s" url

let encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"No_snapshot"
        (constant "no_snapshot")
        (function No_snapshot -> Some () | _ -> None)
        (fun () -> No_snapshot);
      case
        (Tag 1)
        ~title:"Docker_embedded"
        (obj1 (req "docker_embedded" string))
        (function Docker_embedded s -> Some s | _ -> None)
        (fun s -> Docker_embedded s);
      case
        (Tag 2)
        ~title:"Local_file"
        (obj1 (req "local_file" string))
        (function Local_file s -> Some s | _ -> None)
        (fun s -> Local_file s);
      case
        (Tag 3)
        ~title:"Url"
        (obj1 (req "url" string))
        (function Url s -> Some s | _ -> None)
        (fun s -> Url s);
    ]
