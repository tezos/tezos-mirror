(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2020-2025 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

(** This module allows to create a tar archive by adding files to it,
    one by one. It can be seen as a list of contiguous files (made of a
    header followed by a raw data) closed by a specific end of file
    flag. *)

(** The type of a file contained in the tar archive. It is basically
    a header and a raw data. *)
type file

(** The type of an output tar archive. *)
type o

(** The type of an input tar archive. *)
type i

(** [open_out ~file] opens a tar archive as an output archive located at
    [file]. *)
val open_out : file:string -> o Lwt.t

(** [close_out tar] closes an output tar archive. *)
val close_out : o -> unit Lwt.t

(** [add_raw_and_finalize tar ~f ~filename] exposes a file
    descriptor of the tar archive through [f] to be able to write
    arbitrary data in the [tar]. When [f] terminates, a valid tar
    header referenced by [filename] is written *)
val add_raw_and_finalize :
  o -> f:(Lwt_unix.file_descr -> 'a Lwt.t) -> filename:string -> 'a Lwt.t

(** [add_file_and_finalize tar ~file ~filename ~buffer_size] copies the [file],
    and reference it through the given [filename], into a [tar]. It handles all
    specific operations an returns a handler ready to be enriched. *)
val add_file_and_finalize :
  o -> file:string -> filename:string -> buffer_size:int -> unit Lwt.t

(* [add_directory_and_finalize ?archive_prefix tar ~dir_path ~buffer_size]
   copies the [dir_path] and all its sub directories or files into a
   [tar]. By default, the tar archive file path are similar to the
   [dir_path]. They can be overridden using the [archive_prefix].
   It handles all specific operations an returns a handler ready to
   be enriched.
   For example,
   if the directory `/path/to/data` contains 2 files `a` and `b`:
   With the default behaviour, the tar will contain two files:
       - `/path/to/data/a`
       - `/path/to/data/b`
   If the archive_prefix is given with value `local_path`, the tar
   archive will contain:
      - `local_path/a`
      - `local_path/b`
  *)
val add_directory_and_finalize :
  ?archive_prefix:string ->
  o ->
  dir_path:string ->
  buffer_size:int ->
  unit Lwt.t

(** input utilities *)

(** [open_in ~file] opens a tar archive as an input archive located at
    [file]. *)
val open_in : file:string -> i Lwt.t

(** [close_in tar] closes an input tar archive. *)
val close_in : i -> unit Lwt.t

(** [list_files tar] returns the list of files contained in the
    [tar]. *)
val list_files : i -> file list Lwt.t

(** [get_file tar ~filename] returns the first occurrence of the
    file name [filename] from [tar]. *)
val get_file : i -> filename:string -> file option Lwt.t

(** [get_filename file] returns the file name of a [file] contained
    in a tar. *)
val get_filename : file -> string

(** [get_file_size file] returns the file size of a [file] contained
    in a tar. *)
val get_file_size : file -> int64

(** [get_raw_input_fd tar] returns the file descriptor to read
    directly in the tar file. It is no recommended to use it. *)
val get_raw_input_fd : i -> Lwt_unix.file_descr

(** [get_raw_file_ofs file] returns the position offset, from the
    beginning of the tar archive, of the given [file]. *)
val get_raw_file_ofs : file -> int64

(** [find_file tar ~filename] returns the file corresponding to the
    given [filename] within the given [tar]. *)
val find_file : i -> filename:string -> file option Lwt.t

(** [find_files_with_common_path tar ~pattern] returns, from the [tar] all
    the files matching the given [pattern]. *)
val find_files_with_common_path : i -> pattern:string -> file list Lwt.t

(** [load_file tar file] loads the [file] from the [tar] and returns
    it as bytes.
    Warning, this function loads the whole data in
    memory. *)
val load_file : i -> file -> string Lwt.t

(** [load_from_filename tar ~filename] loads the file with the name
    [filename] from the given [tar] and returns it as
    bytes.
    Warning, this function loads the whole data in memory *)
val load_from_filename : i -> filename:string -> string option Lwt.t

(** [copy_to_file tar file ~dst ~buffer_size] copies the [file] from the [tar]
    into new file designated by [dst]. *)
val copy_to_file : i -> file -> dst:string -> buffer_size:int -> unit Lwt.t
