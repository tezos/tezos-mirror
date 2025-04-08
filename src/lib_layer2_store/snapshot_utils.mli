(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2023-2025 Functori <contact@functori.com>         *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
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

(** Returns from where the snapshot input was constructed. Either [`Local
    filename] when the snapshot is read from a local file [filename] or [`Remote
    url] when the snapshot file is downloaded from [url].  *)
val input_source :
  reader_input -> [`Local of string | `Remote of string | `Stdin]

(** Add a download snapshot command to the executable (only for internal use). *)
val add_download_command : unit -> unit

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

  (** [extract reader_input ~cancellable ~display_progress ~dest]
      extracts the snapshot archive opened in [reader_input] in the directory
      [dest]. Existing files in [dest] with the same names are overwritten.

      Setting [cancellable] to [true] ensures the promise returned by [extract]
      can be canceled. How progress is advertized is controlled by the
      [display_progress] variable. Note that [`Bar] is not compatible with
      [Lwt_exit]. *)
  val extract :
    reader_input ->
    cancellable:bool ->
    display_progress:[`Bar | `Periodic_event of Ptime.span -> unit Lwt.t] ->
    dest:string ->
    unit Lwt.t

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

  (** [with_open_snapshot file f] opens the snapshot file for reading, reads its
      header and executes [f] then finally closes the channels. [file] can be a
      local file, a URL (in which case the snapshot is downloaded) or ["-"] for
      reading from standard input. *)
  val with_open_snapshot :
    progress:bool ->
    string ->
    (Header.t -> reader_input -> 'a tzresult Lwt.t) ->
    'a tzresult Lwt.t
end
