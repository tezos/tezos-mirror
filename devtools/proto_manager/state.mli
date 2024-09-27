(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Type of target label representation for snapshot. *)
type snapshot_label = {name : string; version : int}

(** Type of different target label representation for different actions. *)
type label = Snapshot of snapshot_label | Stabilise of string

type t = {
  commits : int;  (** Count of commit done by this process. *)
  protocol_source : string;
  protocol_target : string;
  target_label : label;
  capitalize_label : string;
  git_dir : string;
}

(** [pp_label fmt l] prints the full representation of [l] in [fmt]. *)
val pp_label : Format.formatter -> label -> unit

(** [pp_label_name fmt l] prints the name of [l] in [fmt]. *)
val pp_label_name : Format.formatter -> label -> unit

(** Log an error message and how to clean up created files and exits the
    process. *)
val exit : __LOC__:string -> t -> 'a

(** Creates a state. *)
val create :
  git_dir:string ->
  protocol_source:string ->
  protocol_target:string ->
  target_pattern:string ->
  t
