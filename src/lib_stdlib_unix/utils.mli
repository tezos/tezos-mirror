(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2018-2024 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** {2 Displaying progress} *)

(** Print over the current [stdout] line. Takes a message formatting function of
    the form [(fun m -> m <format_string>)]. Message formatting occurs only when
    the line is actually printed (i.e. when [(fst refresh_rate) mod (snd
    refresh_rate) = 0]). [refresh_rate] defaults to always printing the supplied
    message.

    {3 Examples:}

    - [display_progress (fun m -> m "Loading... %d/100" percent)].

    - [display_progress ~refresh_rate:(index, 1000) (fun m -> m "Written %d
      bytes" total)]. Display progress message when [index] is divisible by
      1000.
 *)
val display_progress :
  ?refresh_rate:int * int ->
  ((('a, Format.formatter, unit, unit) format4 -> 'a) -> unit) ->
  unit

(** Finalizes progress display *)
val display_progress_end : unit -> unit

(** {2 Files manipulation} *)

(** [list_files dir ~include_file f] lists the files in directory [dir] which
    satisfy predicate [include_file] (by default all files) and applies [f] on
    each element. The result is returned as a stream.  *)
val list_files :
  string ->
  ?include_file:(relative_path:string -> bool) ->
  (full_path:string -> relative_path:string -> 'a) ->
  'a Stream.t

(** [directory_contents_size ~include_file] returns the total size of contents
    of directory [dir] which satisfy the predicate [include_file].  *)
val directory_contents_size :
  ?include_file:(relative_path:string -> bool) -> string -> int

(** [create_dir ~perm path] creates directory [path] with permissions [perm] if
    it does not exist. All directories in [path] are created if necessary, à la
    [mkdir -p]. *)
val create_dir : ?perm:int -> string -> unit

(** [copy_file ~src ~dst] copies the file [src] to [dst]. *)
val copy_file : src:string -> dst:string -> unit

(** [copy_dir ?perm ~progress:(message, color) src dst] copies the content of
    directory [src] in the directory [dst] (created with [perm], [0o755] by
    default). If [progress] is provided, a progress bar is displayed on terminal
    outputs with the given message and color. *)
val copy_dir :
  ?perm:int -> ?progress:string * Terminal.Color.t -> string -> string -> unit
