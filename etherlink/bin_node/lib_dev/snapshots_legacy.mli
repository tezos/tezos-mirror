(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Compression strategy for snapshot archives. *)
type compression =
  | No  (** Produce uncompressed archive. Takes more space. *)
  | On_the_fly
      (** Compress archive on the fly. The rollup node will use less disk space
          to produce the snapshot but will lock the rollup node (if running) for
          a longer time. *)
  | After
      (** Compress archive after snapshot creation. Uses more disk space
          temporarily than {!On_the_fly} but does not lock the rollup node for
          very long. *)

module Header : sig
  (** Snapshot metadata. This information is written as a header
      of the archive snapshot file. *)
  type t =
    | V0_legacy of {
        rollup_address : Address.t;
        current_level : Ethereum_types.quantity;
      }  (** Snapshots with legacy block storage *)
    | V1 of {
        rollup_address : Address.t;
        current_level : Ethereum_types.quantity;
        history_mode : Configuration.history_mode;
        first_level : Ethereum_types.quantity;
      }  (** Snapshots with Sqlite3 block storage *)

  (** Fixed size metadata encoding. *)
  val encoding : t Data_encoding.t
end

(** [export ?snapshot_file ~compression ~data_dir () ] creates a
    tar gzipped archive with name [snapshot_file] (or a generated name in the
    current directory) containing a snapshot of the data of the rollup node
    with data directory [data_dir]. The path of the snapshot archive is
    returned.  *)
val export :
  ?snapshot_file:string ->
  compression:compression ->
  data_dir:string ->
  unit ->
  string tzresult Lwt.t

(** [import_from ~force ~data_dir ~snapshot_file] imports the snapshot from file
    [snapshot_file] (but also takes care of downloading the file if
    [snapshot_file] is an url) at path [snapshot_file] into the data directory
    [data_dir]. Import will fail if [data_dir] is already populated unless
    [force] is set to [true]. *)
val import_from :
  force:bool ->
  ?history_mode:Configuration.history_mode ->
  data_dir:string ->
  snapshot_file:string ->
  unit ->
  Configuration.history_mode option tzresult Lwt.t

(** [info ~snapshot_file] returns information that can be used to inspect the
    snapshot file. *)
val info :
  snapshot_file:string ->
  (Header.t * [`Compressed | `Uncompressed]) tzresult Lwt.t
