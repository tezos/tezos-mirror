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

(** [create reader writer ~dir ~include_file ~dest] creates a snapshot archive
    with the hierarchy of files in directory [dir] for which [include_file]
    returns true. The archive is produced in file [dest]. *)
val create :
  reader ->
  writer ->
  dir:string ->
  include_file:(relative_path:string -> bool) ->
  dest:string ->
  unit

(** [extract reader writer ~snapshot_file ~dest] extracts the snapshot archive
    [snapshot_file] in the directory [dest]. Existing files in [dest] with the
    same names are overwritten. *)
val extract : reader -> writer -> snapshot_file:string -> dest:string -> unit

(** [compress ~snapshot_file] compresses the snapshot archive [snapshot_file] of
    the form ["path/to/snapshot.uncompressed"] to a new file
    ["path/to/snapshot"] whose path is returned. [snapshot_file] is removed upon
    successful compression. *)
val compress : snapshot_file:string -> string
