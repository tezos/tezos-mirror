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

type unexpected_error = [`Unexpected_error of exn]

type read_write_error =
  [ `Connection_closed_by_peer
  | `Connection_lost of exn
  | `Connection_locally_closed
  | unexpected_error ]

type connect_error = [`Connection_failed | unexpected_error]

type accept_error =
  [`System_error of exn | `Socket_error of exn | unexpected_error]

type t = {
  fd : Lwt_unix.file_descr;
  id : int;
  mutable point : P2p_point.Id.t option;
  mutable peer_id : P2p_peer.Id.t option;
  mutable nread : int;
  mutable nwrit : int;
  mutable closing_reasons : P2p_disconnection_reason.t list;
}

let set_point ~point t = t.point <- Some point

let set_peer_id ~peer_id t = t.peer_id <- Some peer_id

let add_closing_reason ~reason t =
  t.closing_reasons <- reason :: t.closing_reasons

let pp_read_write_error fmt = function
  | `Connection_closed_by_peer -> Format.fprintf fmt "connection closed by peer"
  | `Connection_lost ex ->
      Format.fprintf
        fmt
        "connection to peer was lost with exception %s"
        (Printexc.to_string ex)
  | `Connection_locally_closed -> Format.fprintf fmt "connection closed by us"
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
    let t =
      {
        fd;
        id = !counter;
        point = None;
        peer_id = None;
        nread = 0;
        nwrit = 0;
        closing_reasons = [];
      }
    in
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
      | exn -> Lwt.reraise exn)

let close ?reason t =
  let open Lwt_syntax in
  Option.iter (fun reason -> add_closing_reason ~reason t) reason ;
  let* () =
    Events.(emit close_fd) (t.id, t.closing_reasons, t.nread, t.nwrit)
  in
  let* () =
    match t.closing_reasons with
    | [] -> Events.(emit close_fd_unknown_reason) (t.point, t.peer_id)
    | _ -> Events.(emit close_fd_reason) (t.point, t.peer_id, t.closing_reasons)
  in
  Lwt.catch
    (fun () ->
      (* Guarantee idempotency. *)
      if Lwt_unix.state t.fd = Closed then return_unit else Lwt_unix.close t.fd)
    (function
      (* From [man 2 close] [close] can fail with [EBADF, EINTR, EIO, ENOSPC,
         EDQUOT] Unix errors.

         Retrying the [Unix.close] after a failure return is the wrong thing to
         do.

         [EDQUOT] is not defined in [Unix.error] type.*)
      | ( Unix.Unix_error ((EBADF | EIO | ENOSPC), _, _)
        (* [EINTR] error is a somewhat special case. [Unix.close] should or
           should not be called again, depending on the system.
           - Retrying [Unix.close] could lead to close a file descriptor that has
             been reused and may lead to unpredictable consequences.
           - Not retrying [Unix.close] in some Unix implementation (not Linux),
             could lead to leak the file descriptor.
             Therefore, not retrying [Unix.close] may be safer. *)
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5633
           Handle the [EINTR] case by system. *)
        | Unix.Unix_error (EINTR, _, _) ) as err ->
          let* () =
            Events.(emit close_fd_error) (t.id, Printexc.to_string err)
          in
          return_unit
      (* Other possible exceptions can come from [Lwt] lifecycle. These exceptions
         are rather meant to be fatal, so we let them raise to top level. *)
      | ex -> raise ex)

let read_write_error_handler ~rw t =
  let open Lwt_syntax in
  function
  (* from Lwt_unix *)
  | Unix.Unix_error (EBADF, _, _) when Lwt_unix.state t.fd = Closed ->
      Lwt.return_error `Connection_locally_closed
  (* from [man 7 tcp], [man 3 read], [man 3 write] *)
  | (Unix.Unix_error (ETIMEDOUT, _, _) | End_of_file) as e ->
      let* () = close ~reason:(Connection_lost rw) t in
      Lwt.return_error (`Connection_lost e)
  | Unix.Unix_error ((ECONNRESET | EPIPE), _, _) ->
      let* () = close ~reason:(Connection_closed_by_peer rw) t in
      Lwt.return_error `Connection_closed_by_peer
  | ex ->
      let* () =
        close
          ~reason:
            (Connection_closed_by_unexpected_error (rw, Printexc.to_string ex))
          t
      in
      Lwt.return_error (`Unexpected_error ex)

let read t buf pos len =
  let open Lwt_result_syntax in
  let*! () = Events.(emit try_read) (t.id, len) in
  let* nread =
    Lwt.catch
      (fun () ->
        if Lwt_unix.state t.fd = Closed then
          Lwt.return_error `Connection_locally_closed
        else
          let*! nread = Lwt_unix.read t.fd buf pos len in
          return nread)
      (read_write_error_handler ~rw:Read t)
  in
  t.nread <- t.nread + nread ;
  let*! () = Events.(emit read_fd) (t.id, nread, Int64.of_int t.nread) in
  if nread = 0 then Lwt.return_error `Connection_closed_by_peer
  else return nread

let write t buf =
  let open Lwt_result_syntax in
  let len = Bytes.length buf in
  let*! () = Events.(emit try_write) (t.id, len) in
  let* () =
    Lwt.catch
      (fun () ->
        if Lwt_unix.state t.fd = Closed then
          Lwt.return_error `Connection_locally_closed
        else
          let*! () = Lwt_utils_unix.write_bytes t.fd buf in
          return_unit)
      (read_write_error_handler ~rw:Write t)
  in
  t.nwrit <- t.nwrit + len ;
  let*! () = Events.(emit written_fd) (t.id, len, t.nwrit) in
  return_unit

let connect t saddr =
  let open Lwt_result_syntax in
  let*! () = Events.(emit connect_fd) (t.id, string_of_sockaddr saddr) in
  Lwt.catch
    (fun () -> Lwt_unix.connect t.fd saddr |> Lwt.map Result.ok)
    (function
      | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
          let*! () = close ~reason:TCP_connection_refused t in
          Lwt.return_error `Connection_failed
      | Unix.Unix_error (Unix.EHOSTUNREACH, _, _) ->
          let*! () = close ~reason:TCP_connection_unreachable t in
          Lwt.return_error `Connection_failed
      | Lwt.Canceled ->
          let*! () = close ~reason:TCP_connection_canceled t in
          Lwt.return_error `Connection_failed
      | ex ->
          let*! () =
            close
              ~reason:
                (TCP_connection_failed_unexpected_error (Printexc.to_string ex))
              t
          in
          Lwt.return_error (`Unexpected_error ex))

let accept listening_socket =
  let open Lwt_result_syntax in
  let* fd, saddr =
    Lwt.catch
      (fun () ->
        Lwt_unix.accept ~cloexec:true listening_socket |> Lwt.map Result.ok)
      (function
        (* Unix errors related to the failure to create one connection,
           No reason to abort just now, but we want to stress out that we
           have a problem preventing us from accepting new connections. *)
        | Unix.Unix_error
            ( ( EMFILE (* Too many open files by the process *)
              | ENFILE (* Too many open files in the system *)
              | ENETDOWN (* Network is down *) ),
              _,
              _ ) as e ->
            Lwt.return_error @@ `System_error e
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
              _ ) as e ->
            Lwt.return_error @@ `Socket_error e
        | ex -> Lwt.return_error @@ `Unexpected_error ex)
  in
  let*! t = create fd in
  let*! () = Events.(emit accept_fd) (t.id, string_of_sockaddr saddr) in
  return (t, saddr)

module Table = Hashtbl.Make (struct
  type nonrec t = t

  let equal {id = x; _} {id = y; _} = x = y

  let hash {id; _} = Hashtbl.hash id
end)
