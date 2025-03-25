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
end) : sig
  (** [create writer header ~cancellable ~display_progress ~files ~dest] creates
      a snapshot archive with the header [header] with the contents of
      [files]. Each element of [files] is a pair whose first component is the
      path of the file to include and the second component is the "relative"
      path it should be registered to in the snapshot archive The archive is
      produced in file [dest].

      Setting [cancellable] to [true] ensures the promise returned by [create]
      can be canceled. How progress is advertized is controlled by the
      [display_progress] variable. Note that [`Bar] is not compatible with
      [Lwt_exit]. *)
  val create :
    writer ->
    Header.t ->
    cancellable:bool ->
    display_progress:
      [ `Bar
      | `Periodic_event of total:int -> progress:int -> Ptime.span -> unit Lwt.t
      ] ->
    files:(string * string) list ->
    dest:string ->
    unit ->
    unit Lwt.t

  (** [extract reader check_header ~cancellable ~display_progress ~snapshot_file
      ~dest] extracts the snapshot archive [snapshot_file] in the directory
      [dest]. Existing files in [dest] with the same names are overwritten. The
      header read from the snapshot is checked with [check_header] before
      beginning extraction, and returned.

      Setting [cancellable] to [true] ensures the promise returned by [extract]
      can be canceled. How progress is advertized is controlled by the
      [display_progress] variable. Note that [`Bar] is not compatible with
      [Lwt_exit]. *)
  val extract :
    reader ->
    (Header.t -> 'a tzresult Lwt.t) ->
    cancellable:bool ->
    display_progress:[`Bar | `Periodic_event of Ptime.span -> unit Lwt.t] ->
    snapshot_file:string ->
    dest:string ->
    (Header.t * 'a) tzresult Lwt.t

  (** [compress ~cancellable ~display_progress ~snapshot_file] compresses the
      snapshot archive [snapshot_file] of the form
      ["path/to/snapshot.uncompressed"] to a new file ["path/to/snapshot"]
      whose path is returned. [snapshot_file] is removed upon successful
      compression.

      Setting [cancellable] to [true] ensures the promise returned by
      [compress] can be canceled. How progress is advertized is controlled by
      the [display_progress] variable. Note that [`Bar] is not compatible with
      [Lwt_exit]. *)
  val compress :
    cancellable:bool ->
    display_progress:
      [ `Bar
      | `Periodic_event of total:int -> progress:int -> Ptime.span -> unit Lwt.t
      ] ->
    snapshot_file:string ->
    unit ->
    string Lwt.t

  (** [read_header reader ~snapshot_file] reads the header from the snapshot
      file without extracting it. *)
  val read_header : reader -> snapshot_file:string -> Header.t
end
