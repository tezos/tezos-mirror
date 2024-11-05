(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [t] is a command.
    It can be:
    - registered,
    - described or,
    - executed.
    Commands are not executed at their creation.
    To execute a command see [execute]. *)
type t

(** [create ?desc exec] returns a new command. *)
val create : desc:(State.t -> State.t) -> (State.t -> State.t) -> t

(** [execute c state] executes [c] with [state]. *)
val execute : t -> State.t -> State.t

(** [describe c state] prints the description of [c] with [state]. *)
val describe : t -> State.t -> State.t

(** Module that represent a queue of commands. This queue can be encapsulated
    into a single command. *)
module FIFO : sig
  type elt

  type t

  val empty : t

  (** [describe t state] applies describe function of each command contained in
      queue in FIFO order. *)
  val describe : t -> State.t -> State.t

  (** [describe t state] applies execute function of each command contained in
      queue in FIFO order. *)
  val execute : t -> State.t -> State.t
end

(** [register c cmds] adds [c] in [cmds]. *)
val register : t -> FIFO.t -> FIFO.t

(** [to_command ?desc cmds] encapsulates [cmds] in a single command using
    [FIFO.describe] and [FIFO.execute].

    If [desc] is provided, it is used instead of [FIFO.describe]. *)
val of_fifo : ?desc:(State.t -> State.t) -> FIFO.t -> t

module Log : sig
  (** [log get_msg] creates a command that logs the value returned by [get_msg]. *)
  val printfln : (State.t -> string) -> t

  (** Same as [log] but using cyan color. *)
  val cyan : (State.t -> string) -> t

  (** Same as [log] but using yellow color. *)
  val yellow : (State.t -> string) -> t

  (** Same as [log] but using blue color. *)
  val blue : (State.t -> string) -> t
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

  val ocamlformat : __LOC__:string -> (State.t -> string list) -> t
end

module Git : sig
  (** [git state args] returns a git command with [args] running in the
      [state]’s git repository. *)
  val git : State.t -> string -> string

  (** Command that checks that there is no uncommitted changes. *)
  val check_no_uncommitted_changes : __LOC__:string -> t

  (** [commit ~__LOC__ ?no_verify msg] add changes to stage and commit them. If
      [no_verify] is provided, pre-commit are not run. *)
  val commit : __LOC__:string -> ?no_verify:unit -> (State.t -> string) -> t

  (** [cp ~__LOC__ get_src get_dest] returns a command that copies the
      versioned files from [get_src] to [get_dest]. *)
  val cp : __LOC__:string -> (State.t -> string) -> (State.t -> string) -> t
end
