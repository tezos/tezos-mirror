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

type error +=
  | Failed_to_open_listening_socket of listening_socket_open_failure
  | Full_waiting_queue

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
      Failed_to_open_listening_socket {reason; address; port}) ;
  let description =
    "Not only the maximum number of connection has been reached, but the \
     waiting queue for connections is already full."
  in
  register_error_kind
    `Permanent
    ~id:"p2p.fd.Full_waiting_queue"
    ~title:"Connection rejected since waiting queue is full"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Full_waiting_queue -> Some () | _ -> None)
    (fun () -> Full_waiting_queue)

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

type connect_error =
  [ `Connection_refused
  | `Connection_unreachable
  | `Network_unreachable
  | `Connection_canceled
  | unexpected_error ]

type accept_error =
  [ `System_error of exn
  | `Socket_error of exn
  | `Full_waiting_queue
  | unexpected_error ]

let accept_error_to_tzerror = function
  | `System_error exn | `Socket_error exn | `Unexpected_error exn ->
      error_of_exn exn
  | `Full_waiting_queue -> Full_waiting_queue

type t = {
  fd : Lwt_unix.file_descr;
  id : int;
  wakener : unit Lwt.u option;
      (** Used to put the token back in the fd_pool after closing the file descriptor. *)
  mutable point : P2p_point.Id.t option;
  mutable peer_id : P2p_peer.Id.t option;
  mutable nread : int;
  mutable nwrit : int;
  mutable closing_reasons : P2p_disconnection_reason.t list;
}

type fd_pool = unit Lwt_pool.t

let create_fd_pool ~capacity = Lwt_pool.create capacity Lwt.return

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
  fun ?fd_pool fd ->
    let open Lwt_result_syntax in
    let* wakener =
      match fd_pool with
      | None -> return None
      | Some fd_pool ->
          (* [1000] is quite arbitrary. It is chosen to be in the same range as
             the typical pools. The pools are expected to be around 1000
             as the number of opened file descriptors on Linux is limited to 1024. *)
          if Lwt_pool.wait_queue_length fd_pool > 1000 then
            Lwt_result.fail `Full_waiting_queue
          else
            let p, u = Lwt.task () in
            (* We grab an element of the pool, which will be freed only when
               [u] will be waken up. *)
            let _ : unit Lwt.t = Lwt_pool.use fd_pool (fun () -> p) in
            (* We wait until the pool is not full. *)
            let* () = Lwt_pool.use fd_pool return in
            return (Some u)
    in
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
        wakener;
      }
    in
    let*! () = Events.(emit create_fd) t.id in
    return t

let string_of_sockaddr addr =
  match addr with
  | Lwt_unix.ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port
  | Lwt_unix.ADDR_UNIX file -> Printf.sprintf "@%s" file

let id t = t.id

let socket_setopt_user_timeout sock =
  let open Lwt_syntax in
  (* By setting [TCP_USER_TIMEOUT], we ensure that a dead connection is reported
     after at most [ms] milliseconds. This option allows the connection timeout
     to be much shorter than the default behavior—which can last several minutes
     (typically between 5 and 15 minutes) due to TCP retransmission timeouts (RTO).

     Below, we set this value to 45 seconds. This value should not be
     too low otherwise we may drop valid connection that were
     temporarily busy. The higher it is, the longer it is to detect a
     dead connection. We believe 45 seconds is reasonable in practice
     (especially this acknowledgement is done at the OS level and so
     is quite independent of the Lwt scheduler).

     This value is intimatly linked to the keepalive value (see below).
     Following Cloudflare article recommendation,
      https://blog.cloudflare.com/when-tcp-sockets-refuse-to-die/
     We set the value to 45s, ie "should be set to a value slightly lower than
         TCP_KEEPIDLE + TCP_KEEPINTVL * TCP_KEEPCNT." *)
  let ms_opt =
    let default =
      45000
      (* 45s *)
    in
    try
      match Sys.getenv_opt "OCTEZ_P2P_TCP_USER_TIMEOUT" with
      | None -> Some default
      | Some "0" -> None
      | Some value -> Some (int_of_string value)
    with _ -> Some default
  in
  match ms_opt with
  | None -> (* The user opt-out from the socket option *) Lwt.return sock
  | Some ms -> (
      match Socket.set_tcp_user_timeout (Lwt_unix.unix_file_descr sock) ~ms with
      | Ok () | Error `Unsupported -> Lwt.return sock
      | Error (`Unix_error exn) ->
          (* Socket option [TCP_USER_TIMEOUT] is not mandatory, this is why we only emit an
             event at [Info] level. *)
          let* () =
            Events.(emit set_socket_option_tcp_user_timeout_failed)
              [Error_monad.error_of_exn exn]
          in
          Lwt.return sock)

let socket_setopt_keepalive sock =
  let open Lwt_syntax in
  (* By setting [TCP_KEEPALIVE], we ensure that the connection stays alive
     for NAT or firewalls between the node and the other peer. If no TCP
     packet is sent after [ms] milliseconds, an empty TCP message will be
     sent.

     If not acknowledged before intv, some retries will be made (number
     depending on OS, default 9 for linux) after [intv] seconds. The connection
     will be dropped if no ACK is received.

     See also the comment of the [socket_setopt_user_timeout] about the cloudflare
     article, to understand the rationale for the 10s and 5s interval default
     value, in conjunction with the user_timeout value. *)
  let keepalive_opts =
    let default =
      (* after 10s of inactivity, and every 5s if no ACK *)
      (10000, 5000)
    in
    match Sys.getenv_opt "OCTEZ_P2P_TCP_KEEPALIVE" with
    | Some "0" -> None
    | None ->
        (* Sends a keepalive starting at 10 seconds of inactivity and every 5
           seconds interval *)
        Some default
    | Some value -> (
        match String.split ',' value with
        | [ms] -> Some (int_of_string ms, snd default)
        | [ms; intv] -> Some (int_of_string ms, int_of_string intv)
        | _ -> None)
  in
  match keepalive_opts with
  | None -> Lwt.return sock
  | Some (ms, intv) -> (
      match
        Socket.set_tcp_keepalive (Lwt_unix.unix_file_descr sock) ~ms ~intv
      with
      | Ok () | Error `Unsupported -> Lwt.return sock
      | Error (`Unix_error exn) ->
          (* Socket option [TCP_KEEPALIVE] is not mandatory, this is why we
             only emit an event at [Info] level. *)
          let* () =
            Events.(emit set_socket_option_tcp_keepalive_failed)
              [Error_monad.error_of_exn exn]
          in
          Lwt.return sock)

let raw_socket () =
  let open Lwt_syntax in
  let sock = Lwt_unix.socket ~cloexec:true PF_INET6 SOCK_STREAM 0 in
  let* sock = socket_setopt_user_timeout sock in
  socket_setopt_keepalive sock

let socket ?fd_pool () =
  let open Lwt_syntax in
  let* socket = raw_socket () in
  Lwt_result.map_error
    (fun `Full_waiting_queue -> [Full_waiting_queue])
    (create ?fd_pool socket)

let create_listening_socket ?(reuse_port = false) ~backlog
    ?(addr = Ipaddr.V6.unspecified) port =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! sock = raw_socket () in
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

(* Taken from [Lwt_io] but is not exported. *)
let close_socket fd =
  Lwt.finalize
    (fun () ->
      (Lwt.catch
         (fun () ->
           (* This announces to the remote peer we won't send or
              receive anymore messages. This is particularly useful in
              the case of a client/server situation such as HTTP. In
              our case, this is not that much necessary, but seems to
              be a good practice. Any attempt to write on this socket
              will result in a [EPIPE] error (as well as a SIGPIPE
              signal, this file sets a ignore handler for this
              signal). *)
           Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL ;
           Lwt.return_unit)
         (function
           (* Occurs if the peer closes the connection first. *)
           | Unix.Unix_error (Unix.ENOTCONN, _, _) -> Lwt.return_unit
           | exn -> Lwt.reraise exn) [@ocaml.warning "-4"]))
    (fun () ->
      (* This call can be blocking since it waits for the current
         write buffer to be acknowledged by the remote peer. A way to
         prevent that could be to set the [SO_LINGER] option for this
         socket. *)
      Lwt_unix.close fd)

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
      if Lwt_unix.state t.fd = Closed then return_unit
      else
        let open Lwt_syntax in
        let* () = close_socket t.fd in
        Option.iter (fun wakener -> Lwt.wakeup wakener ()) t.wakener ;
        return_unit)
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
  let*! () = Events.(emit written_fd) (t.id, len, Int64.of_int t.nwrit) in
  return_unit

let connect t saddr =
  let open Lwt_result_syntax in
  let*! () = Events.(emit connect_fd) (t.id, string_of_sockaddr saddr) in
  Lwt.catch
    (fun () -> Lwt_unix.connect t.fd saddr |> Lwt.map Result.ok)
    (function
      | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
          let*! () = close ~reason:TCP_connection_refused t in
          Lwt.return_error `Connection_refused
      | Unix.Unix_error (Unix.EHOSTUNREACH, _, _) ->
          let*! () = close ~reason:TCP_connection_unreachable t in
          Lwt.return_error `Connection_unreachable
      | Lwt.Canceled ->
          let*! () = close ~reason:TCP_connection_canceled t in
          Lwt.return_error `Connection_canceled
      | Unix.Unix_error (ENETUNREACH, _, _) ->
          let*! () = close ~reason:TCP_network_unreachable t in
          Lwt.return_error `Network_unreachable
      | ex ->
          let*! () =
            close
              ~reason:
                (TCP_connection_failed_unexpected_error (Printexc.to_string ex))
              t
          in
          Lwt.return_error (`Unexpected_error ex))

let accept ?fd_pool listening_socket =
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
                 NetBSD, Darwin, which is already caught *)
                ),
              _,
              _ ) as e ->
            Lwt.return_error @@ `Socket_error e
        | ex -> Lwt.return_error @@ `Unexpected_error ex)
  in
  let* t = create ?fd_pool fd in
  let*! () = Events.(emit accept_fd) (t.id, string_of_sockaddr saddr) in
  return (t, saddr)

module Table = Hashtbl.Make (struct
  type nonrec t = t

  let equal {id = x; _} {id = y; _} = x = y

  let hash {id; _} = Hashtbl.hash id
end)
