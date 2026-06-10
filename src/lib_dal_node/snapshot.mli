(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Current snapshot format version.

    Import rejects archives whose version does not match this value.
    Bump when making backward-incompatible changes to the archive format. *)
val current_snapshot_version : int

(** [export ~data_dir ~config_file ~endpoint ~min_published_level
    ~max_published_level ~slots path] exports the DAL node store data for slots
    published between [min_published_level] and [max_published_level]
    (inclusive) to a snapshot at the given [path].

    When [compress] is [true], the snapshot is written as a tar archive
    (with [.tar] extension automatically appended if missing) containing
    individually gzip-compressed entries. When [compress] is [false]
    (the default), the snapshot is exported as a plain data directory.

    If [min_published_level] is [None], the [first_seen_level] from the store
    is used as the default.
    If [max_published_level] is [None], the [last_processed_level] from the
    store is used as the default.
    The [endpoint] parameter, if provided, overrides the endpoint specified
    in the config file.

    [progress_display_mode] controls whether progress bars are displayed during
    the export. Defaults to [Auto] (display only when stdout is a TTY).

    When [skip_shards] is [true], shard data is not included in the snapshot.
    Defaults to [false]. *)
val export :
  ?compress:bool ->
  ?progress_display_mode:Animation.progress_display_mode ->
  ?skip_shards:bool ->
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
    the import. Defaults to [Auto] (display only when stdout is a TTY).

    When [skip_shards] is [true], shard entries from the snapshot are not
    imported. Defaults to [false]. *)
val import :
  ?check:bool ->
  ?progress_display_mode:Animation.progress_display_mode ->
  ?skip_shards:bool ->
  data_dir:string ->
  config_file:string ->
  endpoint:Uri.t option ->
  min_published_level:int32 option ->
  max_published_level:int32 option ->
  slots:int list option ->
  string ->
  unit tzresult Lwt.t

module Internal_for_tests : sig
  (** [is_shard_entry filename] returns [true] iff [filename], as it would
      appear inside a snapshot tar archive, is a shard entry.

      Exposed for testing the [--skip-shards] filtering logic. *)
  val is_shard_entry : string -> bool

  (** [is_slot_entry filename] returns [true] iff [filename], as it would
      appear inside a snapshot tar archive, is a slot entry.

      Exposed for testing the [--skip-shards] filtering logic. *)
  val is_slot_entry : string -> bool
end
