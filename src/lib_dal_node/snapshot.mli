(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [export ~data_dir ~config_file ~min_published_level ~max_published_level path]
    exports the DAL node store data for slots published between
    [min_published_level] and [max_published_level] (inclusive) to a snapshot
    at the given [path]. If [min_published_level] is [None], the
    first_seen_level from the store is used as the default. If
    [max_published_level] is [None], the last_processed_level from the store
    is used as the default. *)
val export :
  data_dir:string ->
  config_file:string ->
  min_published_level:int32 option ->
  max_published_level:int32 option ->
  string ->
  unit tzresult Lwt.t
