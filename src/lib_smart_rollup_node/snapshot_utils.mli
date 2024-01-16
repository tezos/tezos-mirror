(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** {2 Snapshot archives creation and extraction} *)

(** The type of snapshot archive readers. *)
type reader

(** The type of snapshot archive writers. *)
type writer

(** A reader for uncompressed files or snapshot archives. *)
val stdlib_reader : reader

(** A writer for uncompressed files or snapshot archives. *)
val stdlib_writer : writer

(** A reader for compressed files or snapshot archives. *)
val gzip_reader : reader

(** A writer for compressed files or snapshot archives. *)
val gzip_writer : writer

(** Versioning of snapshot format. Only one version for now. *)
type snapshot_version = V0

(** Snapshot metadata for version 0. This information is written as a header of
    the archive snapshot file. *)
type snapshot_metadata = {
  history_mode : Configuration.history_mode;
  address : Address.t;
  head_level : int32;
  last_commitment : Commitment.Hash.t;
}

(** [create reader writer metadata ~dir ~include_file ~dest] creates a snapshot
    archive with the header [metadata] and the hierarchy of files in directory
    [dir] for which [include_file] returns true. The archive is produced in file
    [dest]. *)
val create :
  reader ->
  writer ->
  snapshot_metadata ->
  dir:string ->
  include_file:(relative_path:string -> bool) ->
  dest:string ->
  unit

(** [extract reader writer check_metadata ~snapshot_file ~dest] extracts the
    snapshot archive [snapshot_file] in the directory [dest]. Existing files in
    [dest] with the same names are overwritten. The metadata header read from
    the snapshot is checked with [check_metadata] before beginning
    extraction, and returned. *)
val extract :
  reader ->
  writer ->
  (snapshot_metadata -> unit tzresult Lwt.t) ->
  snapshot_file:string ->
  dest:string ->
  snapshot_metadata tzresult Lwt.t

(** [compress ~snapshot_file] compresses the snapshot archive [snapshot_file] of
    the form ["path/to/snapshot.uncompressed"] to a new file
    ["path/to/snapshot"] whose path is returned. [snapshot_file] is removed upon
    successful compression. *)
val compress : snapshot_file:string -> string

(** [read_metadata reader ~snapshot_file] reads the metadata from the snapshot
    file without extracting it. *)
val read_metadata : reader -> snapshot_file:string -> snapshot_metadata
