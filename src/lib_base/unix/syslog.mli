(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Syslog primitives based on the [syslog(3)] function.

    See {{:https://www.rfc-editor.org/rfc/rfc3164}RFC 3164} for more
    information. *)

(** The various kinds of syslog output. *)
type facility =
  | Auth
  | Authpriv
  | Cron
  | Daemon
  | FTP
  | Kernel
  | Local0
  | Local1
  | Local2
  | Local3
  | Local4
  | Local5
  | Local6
  | Local7
  | LPR
  | Mail
  | News
  | Syslog
  | User
  | UUCP
  | NTP
  | Security
  | Console

val facility_to_string : facility -> string

(** Converts from string. Possible string values are:
    ["auth", "authpriv", "cron", "daemon", "ftp", "kernel", "local0", "local1",
    "local2", "local3", "local4", "local5", "local6", "local7", "lpr", "mail",
    "news", "syslog", "user", "uucp", "ntp", "security", "console"] *)
val facility_of_string_opt : string -> facility option

exception Syslog_error of string

(** A syslog logger *)
type t = {
  mutable fd : Lwt_unix.file_descr;
  tag : string;
  facility : facility;
  with_pid : bool;
  path : string;
}

(** Creates a syslog logger.

    [create tag ?path ?with_pid facility] opens the socket or the file [path]
    and returns a syslog logger. Messages logged through this logger with
    {!syslog} are prepended with [tag] and send to [facility].

    - [path] default value is, if exists, in this order: ["dev/log"],
    ["/var/run/syslog"], [""]

    - If [with_pid] is [true] (the default [false]), include the caller's PID in
    each message.
*)
val create : tag:string -> ?path:string -> ?with_pid:bool -> facility -> t Lwt.t

(** [format_message ?max_buflen logger level message] formats the given [message]
    depending on [logger] and [level]

    - [max_buflen] is the maximum length of the complete message after which
      text is replaced by [...]. Default to [1024] due to original standard.
    - [timestamp] is the timestamp to display. Uses [Unix.time] if not provided.
*)
val format_message :
  ?max_buflen:int ->
  ?timestamp:float ->
  tag:string ->
  facility:facility ->
  with_pid:bool ->
  Tezos_event_logging.Internal_event.level ->
  string ->
  string

(** [syslog ?max_buflen logger level message] formats [message] (using
    [format_msg]) and sends the result to the [logger] file descriptor with the
    appropriate [level]. *)
val syslog :
  ?max_buflen:int ->
  ?timestamp:float ->
  t ->
  Tezos_event_logging.Internal_event.level ->
  string ->
  unit Lwt.t

(** [close logger] closes the logger's file-descriptor. The logger cannot be used
    after a call to this function. A call to [syslog logger ...] would result in
    a raised exception of kind [Unix.Unix_error]. *)
val close : t -> unit Lwt.t
