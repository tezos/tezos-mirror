(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Runner specifications for processes spawned remotely using SSH. *)

(** Connection information for a runner.

    See {!create}. *)
type t = private {
  id : int;
  address : string;
  ssh_alias : string option;
  ssh_user : string option;
  ssh_port : int option;
  ssh_id : string option;
}

(** Create a runner.

    This function does not start any SSH connection, it only stores
    SSH connection information in a record.

    - [ssh_alias] is the alias in the ssh config file.
    - [ssh_user] is the username on the remote machine.
    - [ssh_port] is the port on the remote machine.
    - [ssh_id] is the path to the identity file.
    - [address] is the IP address of the remote machine. *)
val create :
  ?ssh_alias:string ->
  ?ssh_user:string ->
  ?ssh_port:int ->
  ?ssh_id:string ->
  address:string ->
  unit ->
  t

(** Get the IP address of the local machine as perceived by other runners.

    Use this if you need a runner to reach a process that is running on the
    local machine.

    This returns ["127.0.0.1"] by default, but you can set it with
    {!set_local_public_ip}. *)
val get_local_public_ip : unit -> string

(** Set the IP address of the local machine as perceived by other runners. *)
val set_local_public_ip : string -> unit

(** Get the IP / DNS address of a runner.

    Usage: [address ~from runner]

    Return the address at which a source node [from] can contact [runner].

    If the source node [from] is not specified, return the [address] of [runner]
    as given to {!create}. If [runner] itself is [None], return ["127.0.0.1"]
    or ["localhost"] if the [hostname] variable is set to true (default false).

    If [from] is specified:
    - if [runner] is [None], return [get_local_public_ip ()];
    - if [from] and [runner] are the same runner, return ["127.0.0.1"];
    - else, return the [address] of [runner]. *)
val address : ?hostname:bool -> ?from:t -> t option -> string

module Shell : sig
  (** Shell command representation.

      This module is used for the subset of the shell language that is
      needed to operate remote runners through SSH. It makes sure that
      shell command are properly quoted. *)

  (** Commands.

      Commands execute a program (whose executable name is [name]),
      with some [arguments], possibly with some specific environment
      variables [local_env] to add to the current environment.  *)
  type command = {
    local_env : (string * string) list;
    name : string;
    arguments : string list;
  }

  (** Shell programs. *)
  type t =
    | Cmd of command  (** run a command *)
    | Seq of t * t  (** run something, then something else ([;]) *)
    | Echo_pid  (** echo the current process PID ([echo $$]) *)
    | Redirect_stdout of t * string  (** redirect stdout to a file ([>]) *)
    | Redirect_stderr of t * string  (** redirect stderr to a file ([2>]) *)
    | Or_echo_false of t
        (** run something, if it fails, print "false" ([|| echo false]) *)

  (** Convert a shell program into a string.

      The result is quoted using shell syntax.

      [context] specifies whether parentheses are needed:
      - [`top] means no parentheses are needed (default);
      - [`operator] means parentheses may be needed because this command is
        inside of an operator such as [;] or [||]. *)
  val to_string : ?context:[`operator | `top] -> t -> string

  (** Make a command to execute a program.

      Usage: [cmd local_env name arguments]

      Same as: [Cmd { local_env; name; arguments }] *)
  val cmd : (string * string) list -> string -> string list -> t

  (** Make a sequence.

      Usage: [seq command_1 command_2]

      Same as: [Seq (command_1, command_2)] *)
  val seq : t -> t -> t

  (** Make an stdout redirection.

      Usage: [redirect_stdout command path]

      Same as: [Redirect_stdout (command, path)] *)
  val redirect_stdout : t -> string -> t

  (** Make an stderr redirection.

      Usage: [redirect_stderr command path]

      Same as: [Redirect_stderr (command, path)] *)
  val redirect_stderr : t -> string -> t

  (** Make a shell program that prints "false" if another program fails.

      Usage: [or_echo_false command]

      Same as: [Or_echo_false command] *)
  val or_echo_false : t -> t
end

(** Wrap a shell command into an SSH call.

    Usage: [wrap_with_ssh runner shell]

    Return [(name, arguments)] where [name] is ["ssh"] and [arguments]
    are arguments to pass to SSH to execute [shell] on [runner]. *)
val wrap_with_ssh : t -> Shell.t -> string * string list

(** Same as {!wrap_with_ssh}, but print the PID on stdout first.

    The PID can be used later on to, for instance, kill the remote process.
    Indeed, killing the local SSH process will not necessarily kill
    the remote process. *)
val wrap_with_ssh_pid : t -> Shell.command -> string * string list

module Sys : sig
  (** Extension to [Stdlib.Sys] that can also execute on remote runners.

      Most functions from this module just call their [Stdlib.Sys] equivalent
      when [runner] is not specified. When [runner] is specified, they
      instead use SSH to execute a shell command remotely with a similar effect. *)

  (** Errors that can occur when executing a [Sys] command on a remote runner. *)
  type error

  (** Exception raised when failing to execute a [Sys] command remotely.

      This is not raised when running a command without a [runner].
      Instead, [Sys_error] is raised. *)
  exception Remote_error of error

  (** Convert an error to an error message. *)
  val show_error : error -> string

  (** Check if a file exists on the system.

      For the local version, see {{: https://ocaml.org/api/Sys.html} Sys.file_exists}. *)
  val file_exists : ?runner:t -> string -> bool

  (** Create a new directory.

      For the local version, see {{: https://ocaml.org/api/Unix.html} Unix.mkdir}. *)
  val mkdir : ?runner:t -> ?perms:int -> string -> unit

  (** Check if a file exists and is a directory.

      For the local version, see {{: https://ocaml.org/api/Sys.html} Sys.is_directory}. *)
  val is_directory : ?runner:t -> string -> bool

  (** Return the contents of a directory.

      For the local version, see {{: https://ocaml.org/api/Sys.html} Sys.readdir}. *)
  val readdir : ?runner:t -> string -> string array

  (** Remove a file.

      For the local version, see {{: https://ocaml.org/api/Sys.html} Sys.remove}. *)
  val remove : ?runner:t -> string -> unit

  (** Remove recursively with [rm -rf].

      Only implemented for remote runners. *)
  val rm_rf : t -> string -> unit

  (** Remove a directory.

      For the local version, see {{: https://ocaml.org/api/Unix.html} Unix.rmdir}. *)
  val rmdir : ?runner:t -> string -> unit

  (** Create a named pipe.

      For the local version, see {{: https://ocaml.org/api/Unix.html} Unix.mkfifo}. *)
  val mkfifo : ?runner:t -> ?perms:int -> string -> unit
end
