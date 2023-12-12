(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** [export ~no_checks ~data_dir ~dest] creates a tar gzipped archive in [dest]
    (or the current directory) containing a snapshot of the data of the rollup
    node with data directory [data_dir]. The path of the snapshot archive is
    returned. If [no_checks] is [true], the integrity of the snapshot is not
    checked at the end. *)
val export :
  no_checks:bool ->
  data_dir:string ->
  dest:string option ->
  string tzresult Lwt.t

(** [import ?no_checks cctxt ~data_dir ~snapshot_file] imports the snapshot at
    path [snapshot_file] into the data directory [data_dir]. If [no_checks] is
    [true], the integrity of the imported data is not checked at the end. *)
val import :
  no_checks:bool ->
  #Client_context.full ->
  data_dir:string ->
  snapshot_file:string ->
  unit tzresult Lwt.t
