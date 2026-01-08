(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Module for handling incremental snapshots with
    {{:https://github.com/folbricht/desync}desync}. *)

(** Defines the chunking strategy for the desync tool. *)
type chunk_size = {
  min : int;  (** The minimum chunk size. *)
  avg : int;  (** The average chunk size. *)
  max : int;  (** The maximum chunk size. *)
}

val default_chunk_size_arg : string

(** [export ?desync_path ?chunk_size ?metadata ?metadata_encoding ?progress
    ~target_store ~index_file ~backup_items data_dir]
    exports the given [backup_items] from the [data_dir] to a desync store
    at [target_store] and creates an index file at [index_file].

    @param desync_path Path to the desync tool executable. If not provided,
      it will be looked for in the system's PATH.
    @param chunk_size Specifies the chunking strategy for the desync tool.
    @param metadata An optional value to be stored with the snapshot.
    @param metadata_encoding The encoding for the metadata.
    @param progress Displays a progress bar if true.
    @param target_store The path to the desync store.
    @param index_file The path where the index file will be created.
    @param backup_items A list of files and directories to be backed up.
    @param data_dir The directory where the items to backup are located.
*)
val export :
  ?desync_path:string ->
  ?chunk_size:chunk_size ->
  ?metadata:'a ->
  ?metadata_encoding:'a Data_encoding.t ->
  ?progress:bool ->
  target_store:string ->
  index_file:string ->
  backup_items:string list ->
  string ->
  unit tzresult Lwt.t

(** [import ?desync_path ?metadata_encoding ~source_store ~index_file
    mount_point] imports a snapshot from a desync store at [source_store] using
    the index file at [index_file] and restores it to the [mount_point].

    @param desync_path Path to the desync tool executable. If not provided,
      it will be looked for in the system's PATH.
    @param metadata_encoding The encoding for the metadata. If provided, the
      function will return the decoded metadata if it is present.
    @param source_store The path to the desync store.
    @param index_file The path to the index file.
    @param mount_point The directory where the snapshot will be restored.
*)
val import :
  ?desync_path:string ->
  ?metadata_encoding:'a Data_encoding.t ->
  source_store:string ->
  index_file:string ->
  string ->
  'a option tzresult Lwt.t

(** [read_metadata ?desync_path ~source_store ~index_file encoding] reads and
    decodes the metadata from the store without performing a full import.

    @param desync_path Path to the desync tool executable. If not provided,
      it will be looked for in the system's PATH.
    @param source_store The path to the desync store.
    @param index_file The path to the index file.
    @param encoding The data encoding for the metadata.
*)
val read_metadata :
  ?desync_path:string ->
  source_store:string ->
  index_file:string ->
  'a Data_encoding.t ->
  'a tzresult Lwt.t

(** [info ?desync_path index_file] reads and returns information about a
    snapshot from its index file as a JSON object.

    @param desync_path Path to the desync tool executable. If not provided,
      it will be looked for in the system's PATH.
    @param index_file The path to the index file. *)
val info : ?desync_path:string -> string -> Ezjsonm.value tzresult Lwt.t
