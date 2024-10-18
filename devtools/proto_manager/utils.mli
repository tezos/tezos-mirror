(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Infix : sig
  (** Infix operator for [Filename.concat]. *)
  val ( // ) : string -> string -> string
end

module Process : sig
  (** [exec ~error ~dir ~bin ~args] runs the program [bin] found in [dir] with
      arguments [args], waits its termination, calls [error ()] if the process
      does not terminate successfully and returns what [bin] outputted in
      stdout. *)
  val exec :
    error:(unit -> unit) ->
    dir:string ->
    bin:string ->
    args:string list ->
    string

  (** [shell ~error cmd] interprets [cmd] with /bin/sh, waits its termination,
      calls [error ()] if the process does not terminate successfully and
      returns what [bin] outputted in stdout. *)
  val shell : error:(unit -> unit) -> string -> string

  (** [stdout_by_line output] returns output trimmed and split on newlines. *)
  val stdout_by_line : string -> string list
end

module File : sig
  (** [find ~error ~dir ~opts] invokes find shell command. *)
  val find : error:(unit -> unit) -> dir:string -> opts:string -> string list

  module Content : sig
    (** [search ~regex file] returns all matches of [regex] in [file]. *)
    val search : regex:Re.re -> string -> Re.Group.t list

    (** [append ~content file] writes [content] at the end of [file]. *)
    val append : content:string -> string -> unit

    (** [replace_string ~regex ~by file] replaces each match of [regex] in
        [file] by [by], and returns the count of replacements. *)
    val replace_string : regex:Re.re -> by:string -> string -> int

    (** Same as [replace_string] but for a list of files. Returns the sum of
        replacement in each file. *)
    val replace_string_all : regex:Re.re -> by:string -> string list -> int

    (** [replace_f ~regex ~f file] replaces each match of [regex] in [file] by
        the result of [f], and returns the count of replacements. *)
    val replace_f : regex:Re.re -> f:(Re.Group.t -> string) -> string -> int

    (** Same as [replace_f] but for a list of files. Returns the sum of
        replacement in each file. *)
    val replace_f_all :
      regex:Re.re -> f:(Re.Group.t -> string) -> string list -> int

    (** [replace_assoc ~error ~regex ~assoc file] replaces each match of
        [regex] in [file] by the value associated to the group name in [assoc],
        and returns the count of replacements. If it fails, for example a group
        name is not found in [assoc], [error ()] is called. *)
    val replace_assoc :
      error:(unit -> unit) ->
      regex:Re.re ->
      assoc:(string * string) list ->
      string ->
      int

    (** Same as [replace_assoc] but for a list of files. Returns the sum of
        replacement in each file. *)
    val replace_assoc_all :
      error:(unit -> unit) ->
      regex:Re.re ->
      assoc:(string * string) list ->
      string list ->
      int

    (** [ocamlformat ~error files] formats all [files]. Calls [error ()] if
        fails. *)
    val ocamlformat : error:(unit -> unit) -> string list -> unit
  end
end
