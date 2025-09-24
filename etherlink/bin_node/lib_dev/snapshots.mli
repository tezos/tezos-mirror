(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Invalid_snapshot_file of string
  | Invalid_snapshot_provider of string
  | Data_dir_populated of string
  | History_mode_mismatch of
      Configuration.history_mode * Configuration.history_mode
  | Incorrect_rollup of Address.t * Address.t
  | Outdated_snapshot of Z.t * Z.t

type metadata =
  | V1 of {
      rollup_address : Address.t;
      current_level : Ethereum_types.quantity;
      history_mode : Configuration.history_mode;
      first_level : Ethereum_types.quantity;
    }

(** [interpolate_snapshot_file block_number rollup_address history_mode path]
    replaces placeholders in the given snapshot [path] with the provided values.
    The supported placeholders are:
    - [%l] for the block number
    - [%r] for the rollup address (short form)
    - [%R] for the rollup address (long form)
    - [%h] for the history mode

    @param block_number The block number to use for interpolation.
    @param rollup_address The rollup address to use for interpolation.
    @param history_mode The history mode to use for interpolation.
    @param path The path to the snapshot file, possibly containing placeholders.
*)
val interpolate_snapshot_file :
  Ethereum_types.quantity ->
  Address.t ->
  Configuration.history_mode ->
  string ->
  string tzresult

(** [interpolate_snapshot_provider ?rollup_address ?network history_mode
    provider] replaces placeholders in the given snapshot [provider] with the
    provided values.
    The supported placeholders are:
    - [%n] for the network name
    - [%r] for the rollup address (short form)
    - [%R] for the rollup address (long form)
    - [%h] for the history mode

    @param rollup_address The rollup address to use for interpolation.
    @param network The network to use for interpolation.
    @param history_mode The history mode to use for interpolation.
    @param provider The snapshot provider string, possibly containing
      placeholders.
*)
val interpolate_snapshot_provider :
  ?rollup_address:Address.t ->
  ?network:Configuration.supported_network ->
  Configuration.history_mode ->
  string ->
  string tzresult

(** [export ?desync_path ?chunk_size ?progress ~target_store ?target_dir
    ?index_file ~data_dir ()] exports the data from an EVM node data directory
    to a snapshot index file and snapshot data in the [target_store] for
    deduplication.

    @param desync_path Path to the desync tool executable. If not provided,
      it will be looked for in the system's PATH.
    @param chunk_size Specifies the chunking strategy for the desync tool.
    @param progress Displays a progress bar if true.
    @param target_store The path to the desync store where snapshot data will
      be stored.
    @param target_dir The directory where the snapshot index file will be
      created.
    @param index_file The name of the snapshot index file.
    @param data_dir The path to the EVM node data directory.
*)
val export :
  ?desync_path:string ->
  ?chunk_size:Desync_snapshots.chunk_size ->
  ?progress:bool ->
  target_store:string ->
  ?target_dir:string ->
  ?index_file:string ->
  data_dir:string ->
  unit ->
  string tzresult Lwt.t

(** [info ?desync_path ~source_store ?index_dir ~index_file ()]
    returns the metadata and the size of a snapshot.

    @param desync_path Path to the desync tool executable. If not provided,
      it will be looked for in the system's PATH.
    @param source_store The path to the desync store containing the snapshot
      data.
    @param index_dir The directory where the snapshot index file is located.
    @param index_file The name of the snapshot index file.
*)
val info :
  ?desync_path:string ->
  source_store:string ->
  ?index_dir:string ->
  index_file:string ->
  unit ->
  (metadata * int) tzresult Lwt.t

(** [import ?desync_path ~force ?history_mode ~source_store ?index_dir
    ~index_file ~data_dir ()] imports a snapshot into an EVM node data
    directory.

    @param desync_path Path to the desync tool executable. If not provided,
      it will be looked for in the system's PATH.
    @param force If true, existing data in the data directory will be
      overwritten.
    @param history_mode The history mode for the imported data.
    @param source_store The path to the desync store containing the snapshot
      data.
    @param index_dir The directory where the snapshot index file is located.
    @param index_file The name of the snapshot index file.
    @param data_dir The path to the EVM node data directory.
*)
val import :
  ?desync_path:string ->
  force:bool ->
  ?history_mode:Configuration.history_mode ->
  source_store:string ->
  ?index_dir:string ->
  index_file:string ->
  data_dir:string ->
  unit ->
  Configuration.history_mode option tzresult Lwt.t
