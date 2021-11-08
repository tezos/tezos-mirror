(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(* logging facility to monitor sockets *)

module Events = P2p_events.P2p_fd

let is_not_windows = Sys.os_type <> "Win32"

let () =
  (* Otherwise some writes trigger a SIGPIPE instead of raising an
     Lwt_unit exception. In the node, this is already done by
     Cohttp, so this is only useful when using the P2P layer as a
     stand alone library. *)
  if is_not_windows then Sys.(set_signal sigpipe Signal_ignore)

type t = {
  fd : Lwt_unix.file_descr;
  id : int;
  mutable nread : int;
  mutable nwrit : int;
}

let create =
  let counter = ref 0 in
  fun fd ->
    let open Lwt_syntax in
    incr counter ;
    let t = {fd; id = !counter; nread = 0; nwrit = 0} in
    let* () = Events.(emit create_fd) t.id in
    Lwt.return t

let string_of_sockaddr addr =
  match addr with
  | Lwt_unix.ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port
  | Lwt_unix.ADDR_UNIX file -> Printf.sprintf "@%s" file

let id t = t.id

let socket proto kind arg =
  create
    (let fd = Lwt_unix.socket proto kind arg in
     Lwt_unix.set_close_on_exec fd ;
     fd)

let close t =
  let open Lwt_syntax in
  let* () = Events.(emit close_fd) (t.id, t.nread, t.nwrit) in
  Lwt_utils_unix.safe_close t.fd

let read t buf pos len =
  let open Lwt_syntax in
  let* () = Events.(emit try_read) (t.id, len) in
  let* nread = Lwt_unix.read t.fd buf pos len in
  t.nread <- t.nread + nread ;
  let* () = Events.(emit read_fd) (t.id, nread, t.nread) in
  Lwt.return nread

let write t buf =
  let open Lwt_syntax in
  let len = Bytes.length buf in
  let* () = Events.(emit try_write) (t.id, len) in
  let* () = Lwt_utils_unix.write_bytes t.fd buf in
  t.nwrit <- t.nwrit + len ;
  let* () = Events.(emit written_fd) (t.id, len, t.nwrit) in
  Lwt.return_unit

let connect t saddr =
  let open Lwt_syntax in
  let* () = Events.(emit connect_fd) (t.id, string_of_sockaddr saddr) in
  Lwt_unix.connect t.fd saddr

let accept sock =
  let open Lwt_syntax in
  let* (fd, saddr) = Lwt_unix.accept sock in
  let* t = create fd in
  let* () = Events.(emit accept_fd) (t.id, string_of_sockaddr saddr) in
  Lwt.return (t, saddr)

module Table = Hashtbl.Make (struct
  type nonrec t = t

  let equal {id = x; _} {id = y; _} = x = y

  let hash {id; _} = Hashtbl.hash id
end)
