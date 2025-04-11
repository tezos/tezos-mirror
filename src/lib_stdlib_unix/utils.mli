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

(** [list_files dir] lists the files in directory [dir] and returns a list of
    file names with relative (to [dir]) paths.  *)
val list_files : string -> string list

(** [fold_files dir f acc] applies [f] on all files in [dir] (recursively) and
    accumulated on [acc].  *)
val fold_files : string -> (string -> 'a -> 'a) -> 'a -> 'a

(** [directory_contents_size dir] returns the total size of contents of
    directory [dir].  *)
val directory_contents_size : string -> int

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

(** [retry ?max_delay ~delay ~factor ~tries ~is_error ~emit ?msg f x]
    retries applying [f x] [tries] until it succeeds or returns an error
    when [is_error] is false, at most [tries] number of times. After
    each try it waits for a number of seconds, but not more than [max_delay], if
    given. The wait time between tries is given by the initial [delay],
    multiplied by [factor] at each subsequent try. At each failure, [msg]
    together with the current delay is printed using [emit]. *)
val retry :
  ?max_delay:float ->
  delay:float ->
  factor:float ->
  tries:int ->
  is_error:('err -> bool) ->
  emit:(string -> unit Lwt.t) ->
  ?msg:string ->
  ('a -> ('b, 'err list) result Lwt.t) ->
  'a ->
  ('b, 'err list) result Lwt.t
