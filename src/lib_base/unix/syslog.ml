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

open Error_monad

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

let level_code = function
  | Tezos_event_logging.Internal_event.Fatal -> 0
  | Error -> 3
  | Warning -> 4
  | Notice -> 5
  | Info -> 6
  | Debug -> 7

let facility_code = function
  | Kernel -> 0
  | User -> 1
  | Mail -> 2
  | Daemon -> 3
  | Auth -> 4
  | Syslog -> 5
  | LPR -> 6
  | News -> 7
  | UUCP -> 8
  | Cron -> 9
  | Authpriv -> 10
  | FTP -> 11
  | NTP -> 12
  | Security -> 13
  | Console -> 14
  | Local0 -> 16
  | Local1 -> 17
  | Local2 -> 18
  | Local3 -> 19
  | Local4 -> 20
  | Local5 -> 21
  | Local6 -> 22
  | Local7 -> 23

let facility_to_string = function
  | Auth -> "auth"
  | Authpriv -> "authpriv"
  | Cron -> "cron"
  | Daemon -> "daemon"
  | FTP -> "ftp"
  | Kernel -> "kernel"
  | Local0 -> "local0"
  | Local1 -> "local1"
  | Local2 -> "local2"
  | Local3 -> "local3"
  | Local4 -> "local4"
  | Local5 -> "local5"
  | Local6 -> "local6"
  | Local7 -> "local7"
  | LPR -> "lpr"
  | Mail -> "mail"
  | News -> "news"
  | Syslog -> "syslog"
  | User -> "user"
  | UUCP -> "uucp"
  | NTP -> "ntp"
  | Security -> "security"
  | Console -> "console"

let facility_of_string_opt = function
  | "auth" -> Some Auth
  | "authpriv" -> Some Authpriv
  | "cron" -> Some Cron
  | "daemon" -> Some Daemon
  | "ftp" -> Some FTP
  | "kernel" -> Some Kernel
  | "local0" -> Some Local0
  | "local1" -> Some Local1
  | "local2" -> Some Local2
  | "local3" -> Some Local3
  | "local4" -> Some Local4
  | "local5" -> Some Local5
  | "local6" -> Some Local6
  | "local7" -> Some Local7
  | "lpr" -> Some LPR
  | "mail" -> Some Mail
  | "news" -> Some News
  | "syslog" -> Some Syslog
  | "user" -> Some User
  | "uucp" -> Some UUCP
  | "ntp" -> Some NTP
  | "security" -> Some Security
  | "console" -> Some Console
  | _ -> None

exception Syslog_error of string

let show_date Unix.{tm_sec; tm_min; tm_hour; tm_mday; tm_mon; _} =
  let asc_mon =
    match tm_mon with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _ ->
        Format.ksprintf
          Stdlib.failwith
          "Lwt_log.date_string: invalid month, %d"
          tm_mon
  in
  Format.sprintf "%s %2d %02d:%02d:%02d" asc_mon tm_mday tm_hour tm_min tm_sec

let open_fd path =
  let open Lwt_syntax in
  (match path with
  | "" -> raise (Syslog_error "unable to find the syslog socket or pipe")
  | _ -> ()) ;
  let* {st_kind; _} = Lwt_unix.stat path in
  match st_kind with
  | Unix.S_SOCK ->
      let logaddr = Unix.ADDR_UNIX path in
      let fd =
        try Lwt_unix.socket Unix.PF_UNIX SOCK_DGRAM 0
        with Unix.Unix_error (Unix.EPROTOTYPE, _, _) ->
          Lwt_unix.socket Unix.PF_UNIX SOCK_STREAM 0
      in
      let* () =
        Lwt.catch
          (fun () -> Lwt_unix.connect fd logaddr)
          (function
            | Unix.Unix_error (error, _, _) ->
                let* () = Lwt_unix.close fd in
                raise
                  (Syslog_error
                     (Format.sprintf
                        "can not connect to \"%s\": %s"
                        path
                        (Unix.error_message error)))
            | exn -> raise exn)
      in
      Lwt.return fd
  | Unix.S_FIFO -> Lwt_unix.openfile path [Unix.O_WRONLY] 0o666
  | _ -> raise (Syslog_error "invalid log path, not a socket or pipe")

(* Write the whole contents of a string on the given file
   descriptor *)
let write_string fd str =
  let open Lwt_syntax in
  let len = String.length str in
  let rec aux start_ofs =
    assert (start_ofs <= len) ;
    if start_ofs = len then Lwt.return_unit
    else
      let* n = Lwt_unix.write_string fd str start_ofs (len - start_ofs) in
      if n <> 0 then aux (start_ofs + n) else Lwt.return_unit
  in
  aux 0

let shutdown fd =
  Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL ;
  Lwt_unix.close fd

type t = {
  mutable fd : Lwt_unix.file_descr;
  tag : string;
  facility : facility;
  with_pid : bool;
  path : string;
}

let create ~tag ?path ?(with_pid = false) facility =
  let path =
    match path with
    | Some p -> p
    | None ->
        if Sys.file_exists "/dev/log" then "/dev/log"
        else if Sys.file_exists "/var/run/syslog" then "/var/run/syslog"
        else ""
  in
  let open Lwt_syntax in
  let* fd = open_fd path in
  Lwt.return {fd; tag; facility; with_pid; path}

let format_message ?(max_buflen = 1024) ?timestamp ~tag ~facility ~with_pid
    level str =
  (* For efficiency reason, the buffer is used both directy using
     Buffer.add_* functions, and as a formatter. *)
  let msg_buf = Buffer.create 128 in
  let msg_fmt = Format.formatter_of_buffer msg_buf in
  let level_facility = (facility_code facility lsl 3) lor level_code level in
  let timestamp = match timestamp with None -> Unix.time () | Some t -> t in
  let now = show_date Unix.(localtime timestamp) in
  Format.fprintf msg_fmt "<%d>%s @?" level_facility now ;
  if String.length tag > 32 then Buffer.add_substring msg_buf tag 0 32
  else Buffer.add_string msg_buf tag ;
  if with_pid then Format.fprintf msg_fmt "[%d]@?" (Unix.getpid ()) ;
  Buffer.add_string msg_buf ": " ;
  let buf_len = Buffer.length msg_buf in
  if buf_len + String.length str > max_buflen then (
    let blit_len = max_buflen - 3 - buf_len in
    Buffer.add_substring msg_buf str 0 blit_len ;
    Buffer.add_string msg_buf "...")
  else Buffer.add_string msg_buf str ;
  Buffer.contents msg_buf

let syslog ?max_buflen ?timestamp logger level str =
  let open Lwt_syntax in
  let msg =
    format_message
      ?timestamp
      ?max_buflen
      ~tag:logger.tag
      ~facility:logger.facility
      ~with_pid:logger.with_pid
      level
      str
  in
  Lwt.catch
    (fun () -> write_string logger.fd msg)
    (function
      | Unix.Unix_error (_, _, _) ->
          let* () = shutdown logger.fd in
          let* fd = open_fd logger.path in
          logger.fd <- fd ;
          write_string logger.fd msg
      | exn -> raise exn)

let close logger = shutdown logger.fd
