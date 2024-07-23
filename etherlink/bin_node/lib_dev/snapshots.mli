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
  (** Versioning of snapshot format. Only one version for now. *)
  type version = V0

  (** Snapshot metadata for version 0. This information is written as a header of
    the archive snapshot file. *)
  type t = {version : version}

  (** Fixed size metadata encoding. *)
  val encoding : t Data_encoding.t
end

(** [export ?dest ?filename ~compression ~data_dir () ] creates a
    tar gzipped archive with name [filename] (or a generated name) in [dest] (or
    the current directory) containing a snapshot of the data of the rollup node
    with data directory [data_dir]. The path of the snapshot archive is
    returned.  *)
val export :
  ?dest:string ->
  ?filename:string ->
  compression:compression ->
  data_dir:string ->
  unit ->
  string tzresult Lwt.t
