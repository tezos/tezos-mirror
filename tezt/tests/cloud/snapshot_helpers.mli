(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Here we provide common utilities for managing snapshot files
    in Tezt Cloud-based scenarios.

    Supported snapshot sources:
    - Embedded in a Docker image (constant local path)
    - Local files (copied to the agent)
    - URLs (downloaded)
    - No snapshot (fallback to downloading from a known network service)
*)

type t =
  | Docker_embedded of string
  | Local_file of string
  | Url of string
  | No_snapshot

(** Parse a raw snapshot CLI argument into a [Snapshot_helpers.t]. *)
val parse_snapshot : string option -> t

(** Pretty-print a snapshot value. *)
val to_string : t -> string

(** JSON encoding for snapshot representation. *)
val encoding : t Data_encoding.t

(** Extract the timestamp of a snapshot file using the node's [snapshot info] command. *)
val get_snapshot_info_timestamp : Node.t -> string -> string Lwt.t

(** Extract the level of a snapshot file using the node's [snapshot info] command. *)
val get_snapshot_info_level : Node.t -> string -> int Lwt.t

(** Extract the network name associated with a snapshot file. *)
val get_snapshot_info_network : Node.t -> string -> string Lwt.t

(** Extract the version associated with a snapshot file. *)
val get_snapshot_info_version : Node.t -> string -> int Lwt.t

(** [download_snapshot ?agent ~url ~name] downloads a snapshot from a given [~url]
    onto an [?agent] (or current local agent). The file is saved in the agent's
    working directory under [~name]. *)
val download_snapshot :
  ?agent:Agent.t ->
  ?path:string ->
  url:string ->
  name:string ->
  unit ->
  string Lwt.t

(** [ensure_snapshot ~agent ~name ~network snapshot] ensures a [snapshot] is available
    on the [~agent] by either:
    - Returning a local Docker-embedded path
    - Copying a local file to the [~agent]
    - Downloading it from a remote URL provided in [snapshot]
    - Fallback to [~network] snapshot service for [No_snapshot] *)
val ensure_snapshot :
  agent:Agent.t -> name:string -> network:Network.public -> t -> string Lwt.t

(** Like [ensure_snapshot], but returns [None] for [No_snapshot]
    cases instead of downloading. *)
val ensure_snapshot_opt :
  agent:Agent.t -> name:string -> t -> string option Lwt.t

(** [import_snapshot ?delete_snapshot_file ~no_check ~name node snapshot_path] imports
    a snapshot into a Tezos [node] at [snapshot_path], using [~name] for logging purposes.
    [~no_check] can be set to skip validity checks during import. If [?delete_snapshot_file]
    is [true], it deletes the snapshot file afterwards. *)
val import_snapshot :
  ?env:string String_map.t ->
  ?delete_snapshot_file:bool ->
  no_check:bool ->
  name:string ->
  Node.t ->
  string ->
  unit Lwt.t
