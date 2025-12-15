(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A command taking ['a] as arguments, and requiring to be run against a given
    ['state] type. *)
type ('a, 'state) t

type 'state command = Command : ('a, 'state) t -> 'state command

(** General-purpose command arguments parsers that are likely to be re-used
    to build ad-hoc commands. *)
module Cli : sig
  (** A parser for Irmin path using a filesystem convention (directory
      names separated by the ['/'] character). A leading ['/'] is required). *)
  val path : string Cli_parser.t
end

(** [command ~name ~parse ~run] creates a new command from a parser and a
    handler that can be executed on top of a particular state. *)
val command :
  name:string ->
  parse:'args Cli_parser.t ->
  run:(Printer.t -> 'state -> 'args -> unit tzresult Lwt.t) ->
  ('args, 'state) t

(** [run p state cmd arg] executes the command [cmd] with the provided [arg] on
    top of [state], using [p] to print. *)
val run : Printer.t -> 'state -> ('a, 'state) t -> 'a -> unit tzresult Lwt.t

(** [read_eval ~args cmds p state] evaluates the unparsed arguments in [args]
    as the first command from [cmds] able to parse them, on top of [state],
    using [p] to print. *)
val read_eval :
  args:string list -> 'state command list -> Printer.t -> 'state -> unit Lwt.t

val name : ('a, 'state) t -> string

(** [ls ~subkeys ~inspect] is a command listing the sub-directories of a given
    path. *)
val ls :
  subkeys:('state -> string -> string trace Lwt.t) ->
  inspect:('state -> string -> bytes option Lwt.t) ->
  (string, 'state) t

(** [cat ~inspect] is a command printing the contents stored under a given path,
    using a specified pretty-printer (hex by default). *)
val cat :
  inspect:('state -> string -> bytes option Lwt.t) -> (string * Pp.t, 'state) t
