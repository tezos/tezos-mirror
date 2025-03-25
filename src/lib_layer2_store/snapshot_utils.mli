(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2023-2024 Functori <contact@functori.com>         *)
(*                                                                           *)
(*****************************************************************************)

(** {2 Snapshot archives creation and extraction} *)

(** The type of snapshot archive readers. *)
type reader

(** The type of snapshot archive readers with an input channel. *)
type reader_input

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

(** Returns whether the reader is for compressed or uncompressed files. *)
val reader_format : reader -> [`Compressed | `Uncompressed]

(** Returns whether the reader and input channel is for compressed or
    uncompressed files. *)
val input_format : reader_input -> [`Compressed | `Uncompressed]

(** [with_open_snapshot file f] opens the snapshot file for reading, executes
    [f] and closes the channels. *)
val with_open_snapshot :
  string -> (reader_input -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

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

  (** [extract reader_input check_header ~cancellable ~display_progress ~dest]
      extracts the snapshot archive opened in [reader_input] in the directory
      [dest]. Existing files in [dest] with the same names are overwritten. The
      header read from the snapshot is checked with [check_header] before
      beginning extraction, and returned.

      Setting [cancellable] to [true] ensures the promise returned by [extract]
      can be canceled. How progress is advertized is controlled by the
      [display_progress] variable. Note that [`Bar] is not compatible with
      [Lwt_exit]. *)
  val extract :
    reader_input ->
    (Header.t -> 'a tzresult Lwt.t) ->
    cancellable:bool ->
    display_progress:[`Bar | `Periodic_event of Ptime.span -> unit Lwt.t] ->
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

  (** [read_header reader_input] reads the header from an opened snapshot
      without extracting it. *)
  val read_snapshot_header : reader_input -> Header.t
end
