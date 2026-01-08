(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Scenarios_helpers

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

let get_snapshot_info_timestamp node snapshot_path =
  let* info = Node.snapshot_info node ~json:true snapshot_path in
  let json = JSON.parse ~origin:"snapshot_info" info in
  Lwt.return JSON.(json |-> "snapshot_header" |-> "timestamp" |> as_string)

let get_snapshot_info_level node snapshot_path =
  let* info = Node.snapshot_info node ~json:true snapshot_path in
  let json = JSON.parse ~origin:"snapshot_info" info in
  Lwt.return JSON.(json |-> "snapshot_header" |-> "level" |> as_int)

let get_snapshot_info_network node snapshot_path =
  let* info = Node.snapshot_info node ~json:true snapshot_path in
  let json = JSON.parse ~origin:"snapshot_info" info in
  (match JSON.(json |-> "snapshot_header" |-> "chain_name" |> as_string) with
  | "TEZOS_ITHACANET_2022-01-25T15:00:00Z" -> "ghostnet"
  | "TEZOS_SEOULNET_2025-07-11T08:00:00Z" -> "seoulnet"
  | "TEZOS_TALLINNNET_2025-11-18T21:00:00Z" -> "tallinnnet"
  | "TEZOS_MAINNET" -> "mainnet"
  | "TEZOS" | _ -> "sandbox")
  |> Lwt.return

let get_snapshot_info_version node snapshot_path =
  let* info = Node.snapshot_info node ~json:true snapshot_path in
  let json = JSON.parse ~origin:"snapshot_info" info in
  Lwt.return JSON.(json |-> "snapshot_header" |-> "version" |> as_int)

let download_snapshot ?agent ?(path = "snapshot_file") ~url ~name () =
  toplog "Trying to download snapshot for %s from %s" name url ;
  let* exit_status =
    Process.spawn
      ?runner:
        (match agent with None -> None | Some agent -> Agent.runner agent)
      "wget"
      ["-O"; path; sf "%s/rolling" url]
    |> Process.wait
  in
  let* () =
    match exit_status with
    | WEXITED 0 -> Lwt.return_unit
    | WEXITED code ->
        toplog
          "Could not download the snapshot for %s: wget exit code: %d\n\
           Starting without snapshot. It could last long before the node is \
           bootstrapped"
          name
          code ;
        Lwt.return_unit
    | status -> (
        match Process.validate_status status with
        | Ok () -> Lwt.return_unit
        | Error (`Invalid_status reason) ->
            failwith @@ Format.sprintf "wget: %s" reason)
  in
  Lwt.return path

let ensure_snapshot_opt ~agent ~name = function
  | Docker_embedded path ->
      toplog "Using locally stored snapshot file: %s" path ;
      Lwt.return_some path
  | Local_file path ->
      toplog "Copying snapshot to destination" ;
      let* path = Tezt_cloud.Agent.copy agent ~destination:path ~source:path in
      Lwt.return_some path
  | Url url ->
      let* path = download_snapshot ~agent ~url ~name () in
      Lwt.return_some path
  | No_snapshot -> Lwt.return_none

let ensure_snapshot ~agent ~name ~network = function
  | No_snapshot ->
      download_snapshot ~agent ~url:(Network.snapshot_service network) ~name ()
  | snapshot ->
      let* path = ensure_snapshot_opt ~agent ~name snapshot in
      Lwt.return @@ Option.get path

let import_snapshot ?env ?(delete_snapshot_file = false) ~no_check ~name node
    snapshot_file_path =
  toplog "Importing the snapshot for %s" name ;
  let* () =
    try
      let* () = Node.snapshot_import ?env ~no_check node snapshot_file_path in
      let () = toplog "Snapshot import succeeded for %s." name in
      let* () =
        if delete_snapshot_file then (
          (* Delete the snapshot downloaded locally *)
          toplog "Deleting downloaded snapshot (%s)" snapshot_file_path ;
          let* (_ignored_exit_status : Unix.process_status) =
            Process.wait (Process.spawn "rm" [snapshot_file_path])
          in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      Lwt.return_unit
    with _ ->
      (* Failing to import the snapshot could happen on a very young
         Weeklynet, before the first snapshot is available. In this
         case bootstrapping from the genesis block is OK. *)
      let () =
        toplog
          "Snapshot import failed for %s, the node will be bootstrapped from \
           genesis."
          name
      in
      Lwt.return_unit
  in
  Lwt.return_unit
