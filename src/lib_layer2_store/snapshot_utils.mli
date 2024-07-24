(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2023-2024 Functori <contact@functori.com>         *)
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

(** [is_compressed_snapshot f] returns [true] if [f] is the path of a compressed
    snapshot file, i.e. a gzip file. *)
val is_compressed_snapshot : string -> bool

module Make (Header : sig
  type t

  val encoding : t Data_encoding.t

  val size : int
end) : sig
  (** [create reader writer header ~dir ~include_file ~dest] creates a
      snapshot archive with the header [header] and the hierarchy of files in
      directory [dir] for which [include_file] returns true. The archive is
      produced in file [dest]. *)
  val create :
    reader ->
    writer ->
    Header.t ->
    dir:string ->
    include_file:(relative_path:string -> bool) ->
    dest:string ->
    unit

  (** [extract reader writer check_header ~snapshot_file ~dest] extracts the
      snapshot archive [snapshot_file] in the directory [dest]. Existing files
      in [dest] with the same names are overwritten. The header header read
      from the snapshot is checked with [check_header] before beginning
      extraction, and returned. *)
  val extract :
    reader ->
    writer ->
    (Header.t -> 'a tzresult Lwt.t) ->
    snapshot_file:string ->
    dest:string ->
    (Header.t * 'a) tzresult Lwt.t

  (** [compress ~snapshot_file] compresses the snapshot archive [snapshot_file]
      of the form ["path/to/snapshot.uncompressed"] to a new file
      ["path/to/snapshot"] whose path is returned. [snapshot_file] is removed
      upon successful compression. *)
  val compress : snapshot_file:string -> string

  (** [read_header reader ~snapshot_file] reads the header from the snapshot
      file without extracting it. *)
  val read_header : reader -> snapshot_file:string -> Header.t
end
