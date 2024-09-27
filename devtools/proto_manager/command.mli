(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [t] is a command that can be described or executed. *)
type t = {describe : State.t -> State.t; execute : State.t -> State.t}

(** Module that represent a queue of commands. This queue can be encapsulated
    into a single command. *)
module FIFO : sig
  type elt = t

  type t

  val empty : t

  (** [register elt t] adds [elt] in the FIFO. *)
  val register : elt -> t -> t

  (** [describe t state] applies describe function of each command contained in
      queue in FIFO order. *)
  val describe : t -> State.t -> State.t

  (** [describe t state] applies execute function of each command contained in
      queue in FIFO order. *)
  val execute : t -> State.t -> State.t

  (** [to_command ?desc t] encapsulates [t] in a single command using
      [describe] and [execute].

      If [desc] is provided, it is used instead of [describe]. *)
  val to_command : ?desc:(State.t -> State.t) -> t -> elt
end

(** Module that contains commands related to shell commands. *)
module Shell : sig
  (** [create ~__LOC__ ?desc s] creates a command that executes the string
      returned by [s] in a shell environment. If the command fails, call
      [State.exit].

      If [desc] is provided, it is used for describe field of the returned command. *)
  val create :
    __LOC__:string ->
    ?desc:(State.t -> State.t) ->
    ?error_msg:string ->
    (State.t -> string) ->
    t
end

(** Module that contains commands related to file manipulations. *)
module File : sig
  (** [check_exists get_path] checks that the file at the path returned by
      [get_path] exists. If it does not, call [State.exit]. *)
  val check_exists : ?error_msg:(State.t -> string) -> (State.t -> string) -> t

  (** [check_not_exist get_path] checks that the file at the path returned by
      [get_path] does not exist. If it does, call [State.exit]. *)
  val check_not_exist :
    ?error_msg:(State.t -> string) -> (State.t -> string) -> t
end

module Git : sig
  (** Command that checks that there is no uncommitted changes. *)
  val check_no_uncommitted_changes : t

  (** [commit_no_hooks msg] add changes to stage and commit them.*)
  val commit_no_hooks : string -> t
end
