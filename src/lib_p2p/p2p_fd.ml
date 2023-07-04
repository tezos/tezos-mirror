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

(* This error is not declared in P2p_errors module of lib_p2p_services because
   Unix.error_encoding is part of lib_stdlib_unix that cannot be added easily
   as a dependency of lib_p2p_services. *)
type listening_socket_open_failure = {
  reason : Unix.error;
  address : P2p_addr.t;
  port : int;
}

type error += Failed_to_open_listening_socket of listening_socket_open_failure

let () =
  register_error_kind
    `Permanent
    ~id:"p2p.welcome.failed_to_open_listening_socket"
    ~title:"Failed to open listening socket"
    ~description:"The p2p listening socket could not be opened."
    ~pp:(fun ppf (reason, address, port) ->
      let tips ppf () =
        match reason with
        | Unix.EADDRINUSE ->
            Format.fprintf
              ppf
              "Another tezos node is probably running on this address.@;\
               Please choose another P2P port using --net-addr."
        | _ -> Format.fprintf ppf ""
      in
      Format.fprintf
        ppf
        "@[<v 2>An error occured while initializing P2P server on this \
         address: %a:%d.@;\
         Reason: %s.@;\
         %a@]"
        P2p_addr.pp
        address
        port
        (Unix.error_message reason)
        tips
        ())
    Data_encoding.(
      obj3
        (req "reason" Unix_error.encoding)
        (req "address" P2p_addr.encoding)
        (req "port" uint16))
    (function
      | Failed_to_open_listening_socket {reason; address; port} ->
          Some (reason, address, port)
      | _ -> None)
    (fun (reason, address, port) ->
      Failed_to_open_listening_socket {reason; address; port})

let is_not_windows = Sys.os_type <> "Win32"

let () =
  (* Otherwise some writes trigger a SIGPIPE instead of raising an
     Lwt_unit exception. In the node, this is already done by
     Cohttp, so this is only useful when using the P2P layer as a
     stand alone library. *)
  if is_not_windows then Sys.(set_signal sigpipe Signal_ignore)

type close_reason =
  [ `Connection_closed_by_peer
  | `Connection_lost of exn
  | `Connection_locally_closed
  | `Unexpected_error_when_closing of exn * exn
  | `Unexpected_error of exn ]

type t = {
  fd : Lwt_unix.file_descr;
  id : int;
  mutable nread : int;
  mutable nwrit : int;
}

let pp_close_reason fmt = function
  | `Connection_closed_by_peer -> Format.fprintf fmt "connection closed by peer"
  | `Connection_lost ex ->
      Format.fprintf
        fmt
        "connection to peer was lost with exception %s"
        (Printexc.to_string ex)
  | `Connection_locally_closed -> Format.fprintf fmt "connection closed by us"
  | `Unexpected_error_when_closing (ex_orig, ex_closing) ->
      Format.fprintf
        fmt
        "unexpected error (%s) whilst closing the socket following an error \
         (%s)"
        (Printexc.to_string ex_closing)
        (Printexc.to_string ex_orig)
  | `Unexpected_error ex ->
      Format.fprintf
        fmt
        "connection closed because of an unexpected error %s"
        (Printexc.to_string ex)

let create =
  let counter = ref 0 in
  fun fd ->
    let open Lwt_syntax in
    incr counter ;
    let t = {fd; id = !counter; nread = 0; nwrit = 0} in
    let* () = Events.(emit create_fd) t.id in
    return t

let string_of_sockaddr addr =
  match addr with
  | Lwt_unix.ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port
  | Lwt_unix.ADDR_UNIX file -> Printf.sprintf "@%s" file

let id t = t.id

let raw_socket () = Lwt_unix.socket ~cloexec:true PF_INET6 SOCK_STREAM 0

let socket () = create (raw_socket ())

let create_listening_socket ?(reuse_port = false) ~backlog
    ?(addr = Ipaddr.V6.unspecified) port =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let sock = raw_socket () in
      (if reuse_port then Lwt_unix.(setsockopt sock SO_REUSEPORT true)) ;
      Lwt_unix.(setsockopt sock SO_REUSEADDR true) ;
      let*! () =
        Lwt_unix.bind
          sock
          Unix.(ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port))
      in
      Lwt_unix.listen sock backlog ;
      return sock)
    (function
      | Unix.Unix_error (err, _, _) ->
          tzfail
            (Failed_to_open_listening_socket
               {reason = err; address = addr; port})
      | exn -> Lwt.fail exn)

let close t =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! () = Events.(emit close_fd) (t.id, t.nread, t.nwrit) in
      let*! () = Lwt_unix.close t.fd in
      return_unit)
    (function
      (* the connection was already closed *)
      | Unix.Unix_error (EBADF, _, _) -> return_unit
      | ex -> Lwt.return_error (`Unexpected_error ex))

(* helper function to close the fd in the read/write function in
   case of error *)
let close_rw t err =
  let open Lwt_result_syntax in
  let*! res = close t in
  match res with
  | Ok () -> Lwt.return_error (`Connection_lost err)
  | Error (`Unexpected_error ex) ->
      Lwt.return_error (`Unexpected_error_when_closing (err, ex))

let read t buf pos len =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! () = Events.(emit try_read) (t.id, len) in
      let*! nread = Lwt_unix.read t.fd buf pos len in
      t.nread <- t.nread + nread ;
      let*! () = Events.(emit read_fd) (t.id, nread, t.nread) in
      if nread = 0 then Lwt.return_error `Connection_closed_by_peer
      else return nread)
    (function
      | Unix.Unix_error (EBADF, _, _) ->
          Lwt.return_error `Connection_locally_closed
      (* return connection closed in case of a timeout, connection reset
         and if we detect that the file descriptor was already closed.
         Notice that bad file descriptor could also arise because fd has
         a wrong value, that is, it inconsistent with the value obtained
         from socket() api. We call close to cleanup the socket *)
      | (Unix.Unix_error ((ECONNRESET | ETIMEDOUT), _, _) | End_of_file) as err
        ->
          close_rw t err
      | ex -> Lwt.return_error (`Unexpected_error ex))

let write t buf =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let len = Bytes.length buf in
      let*! () = Events.(emit try_write) (t.id, len) in
      let*! () = Lwt_utils_unix.write_bytes t.fd buf in
      t.nwrit <- t.nwrit + len ;
      let*! () = Events.(emit written_fd) (t.id, len, t.nwrit) in
      return_unit)
    (function
      | Unix.Unix_error (EBADF, _, _) ->
          Lwt.return_error `Connection_locally_closed
      | Unix.Unix_error ((ECONNRESET | EPIPE), _, _) as err -> close_rw t err
      | ex -> Lwt.return_error (`Unexpected_error ex))

let connect t saddr =
  Lwt.catch
    (fun () ->
      let open Lwt_result_syntax in
      let*! () = Events.(emit connect_fd) (t.id, string_of_sockaddr saddr) in
      let*! () = Lwt_unix.connect t.fd saddr in
      return_unit)
    (function
      | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
          Lwt.return_error `Connection_refused
      | ex -> Lwt.return_error (`Unexpected_error ex))

let accept listening_socket =
  Lwt.catch
    (fun () ->
      let open Lwt_syntax in
      let* fd, saddr = Lwt_unix.accept ~cloexec:true listening_socket in
      let* t = create fd in
      let* () = Events.(emit accept_fd) (t.id, string_of_sockaddr saddr) in
      return_ok (t, saddr))
    (function
      (* Unix errors related to the failure to create one connection,
         No reason to abort just now, but we want to stress out that we
         have a problem preventing us from accepting new connections. *)
      | Unix.Unix_error
          ( ( EMFILE (* Too many open files by the process *)
            | ENFILE (* Too many open files in the system *)
            | ENETDOWN (* Network is down *) ),
            _,
            _ ) as err ->
          Lwt.return_error (`System_error err)
      (* These are socket-specific errors. Ignoring. *)
      | Unix.Unix_error
          ( ( EAGAIN (* Resource temporarily unavailable; try again *)
            | EWOULDBLOCK (* Operation would block *)
            | ENOPROTOOPT (* Protocol not available *)
            | EOPNOTSUPP (* Operation not supported on socket *)
            | ENETUNREACH (* Network is unreachable *)
            | ECONNABORTED (* Software caused connection abort *)
            | ECONNRESET (* Connection reset by peer *)
            | ETIMEDOUT (* Connection timed out *)
            | EHOSTDOWN (* Host is down *)
            | EHOSTUNREACH (* No route to host *)
            (* Ugly hack to catch EPROTO and ENONET, Protocol error, which
               are not defined in the Unix module (which is 20 years late on
               the POSIX standard). A better solution is to use the package
               ocaml-unix-errno or redo the work *)
            | EUNKNOWNERR (71 | 64)
            (* On Linux EPROTO is 71, ENONET is 64 On BSD systems, accept
               cannot raise EPROTO.  71 is EREMOTE   for openBSD, NetBSD,
               Darwin, which is irrelevant here 64 is EHOSTDOWN for openBSD,
               NetBSD, Darwin, which is already caught *) ),
            _,
            _ ) as err ->
          Lwt.return_error (`Socket_error err)
      | ex -> Lwt.return_error (`Unexpected_error ex))

module Table = Hashtbl.Make (struct
  type nonrec t = t

  let equal {id = x; _} {id = y; _} = x = y

  let hash {id; _} = Hashtbl.hash id
end)
