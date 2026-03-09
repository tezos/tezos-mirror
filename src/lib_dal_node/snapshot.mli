(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [export ~data_dir ~config_file ~endpoint ~min_published_level
    ~max_published_level ~slots path] exports the DAL node store data for slots
    published between [min_published_level] and [max_published_level]
    (inclusive) to a snapshot at the given [path].

    The snapshot is exported as a plain data directory.

    If [min_published_level] is [None], the [first_seen_level] from the store
    is used as the default.
    If [max_published_level] is [None], the [last_processed_level] from the
    store is used as the default.
    The [endpoint] parameter, if provided, overrides the endpoint specified
    in the config file.

    [progress_display_mode] controls whether progress bars are displayed during
    the export. Defaults to [Auto] (display only when stdout is a TTY). *)
val export :
  ?progress_display_mode:Animation.progress_display_mode ->
  data_dir:string ->
  config_file:string ->
  endpoint:Uri.t option ->
  min_published_level:int32 option ->
  max_published_level:int32 option ->
  slots:int list option ->
  string ->
  unit tzresult Lwt.t

(** [import ~data_dir ~config_file ~endpoint ~min_published_level
    ~max_published_level ~slots path] imports DAL node store data from [path]
    and merges it into [data_dir].

    [path] is treated as a plain data directory.

    [progress_display_mode] controls whether progress bars are displayed during
    the import. Defaults to [Auto] (display only when stdout is a TTY). *)
val import :
  ?check:bool ->
  ?progress_display_mode:Animation.progress_display_mode ->
  data_dir:string ->
  config_file:string ->
  endpoint:Uri.t option ->
  min_published_level:int32 option ->
  max_published_level:int32 option ->
  slots:int list option ->
  string ->
  unit tzresult Lwt.t
