(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [export data_dir ~min_published_level ~max_published_level path]
    exports the DAL node store data for slots published between
    [min_published_level] and [max_published_level] (inclusive) to a snapshot
    file at the given [path]. *)
val export :
  data_dir:string ->
  min_published_level:int32 ->
  max_published_level:int32 ->
  string ->
  unit tzresult Lwt.t
