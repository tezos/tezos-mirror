(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** A printable regular expression. *)
type pattern

(** Pretty printer for [pattern]. *)
val pp_pattern : Format.formatter -> pattern -> unit

(** Regular expression to specify a protocol. *)
val protocol_pattern : pattern

module Stabilise : sig
  (** [run git_dir from target] stabilises the protocol [from] into the
      protocol [target] in the git repository [git_dir]. *)
  val run : string -> string -> string -> unit
end

module Snapshot : sig
  (** Regular expression to specify a protocol target for snapshot. *)
  val target_pattern : pattern

  (** [run git_dir from target] snapshots the protocol [from] into the protocol
      [target] in the git repository [git_dir]. *)
  val run : string -> string -> string -> unit
end

module Hash : sig
  (** [run git_dir from] updates the hash of the protocol [from] in the git
      repository [git_dir]. *)
  val run : string -> string -> unit
end
