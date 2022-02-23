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

module Events = P2p_events.P2p_welcome

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

type connect_handler =
  | Connect_handler :
      ('msg, 'meta, 'meta_conn) P2p_connect_handler.t
      -> connect_handler

type t = {
  socket : Lwt_unix.file_descr;
  canceler : Lwt_canceler.t;
  connect_handler : connect_handler;
  mutable worker : unit Lwt.t;
}

let rec worker_loop st =
  let (Connect_handler connect_handler) = st.connect_handler in
  Lwt.pause () >>= fun () ->
  protect ~canceler:st.canceler (fun () -> P2p_fd.accept st.socket >>= return)
  >>= function
  | Ok (fd, addr) ->
      let point =
        match addr with
        | Lwt_unix.ADDR_UNIX _ -> assert false
        | Lwt_unix.ADDR_INET (addr, port) ->
            (Ipaddr_unix.V6.of_inet_addr_exn addr, port)
      in
      P2p_connect_handler.accept connect_handler fd point ;
      worker_loop st
  (* Unix errors related to the failure to create one connection,
     No reason to abort just now, but we want to stress out that we
     have a problem preventing us from accepting new connections. *)
  | Error
      (Exn
         (Unix.Unix_error
           ( ( EMFILE (* Too many open files by the process *)
             | ENFILE (* Too many open files in the system *)
             | ENETDOWN (* Network is down *) ),
             _,
             _ ))
       :: _ as err) ->
      Events.(emit incoming_error) (err, "system") >>= fun () ->
      (* These are temporary system errors, giving some time for the system to
         recover *)
      Lwt_unix.sleep 5. >>= fun () -> worker_loop st
  | Error
      (Exn
         (Unix.Unix_error
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
             _ ))
       :: _ as err) ->
      (* These are socket-specific errors, ignoring. *)
      Events.(emit incoming_error) (err, "socket") >>= fun () -> worker_loop st
  | Error (Canceled :: _) -> Lwt.return_unit
  | Error err -> Events.(emit unexpected_error) err

let create_listening_socket ~backlog ?(addr = Ipaddr.V6.unspecified) port =
  Lwt.catch
    (fun () ->
      let main_socket = Lwt_unix.(socket PF_INET6 SOCK_STREAM 0) in
      Lwt_unix.(setsockopt main_socket SO_REUSEADDR true) ;
      Lwt_unix.bind
        main_socket
        Unix.(ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port))
      >>= fun () ->
      Lwt_unix.listen main_socket backlog ;
      return main_socket)
    (function
      | Unix.Unix_error (err, _, _) ->
          fail
          @@ Failed_to_open_listening_socket
               {reason = err; address = addr; port}
      | exn -> Lwt.fail exn)

let create ?addr ~backlog connect_handler port =
  Lwt.catch
    (fun () ->
      create_listening_socket ~backlog ?addr port >>=? fun socket ->
      let canceler = Lwt_canceler.create () in
      let st =
        {
          socket;
          canceler;
          connect_handler = Connect_handler connect_handler;
          worker = Lwt.return_unit;
        }
      in
      Lwt_canceler.on_cancel canceler (fun () ->
          st.worker >>= fun () ->
          Lwt_utils_unix.safe_close socket >>= function
          | Error trace -> Events.(emit unexpected_error_closing_socket) trace
          | Ok () -> Lwt.return_unit) ;
      return st)
    (fun exn ->
      let error = Error_monad.Exn exn in
      Events.(emit incoming_connection_error) error >>= fun () -> fail error)

let activate st =
  st.worker <-
    Lwt_utils.worker
      "welcome"
      ~on_event:Internal_event.Lwt_worker_event.on_event
      ~run:(fun () -> worker_loop st)
      ~cancel:(fun () -> Error_monad.cancel_with_exceptions st.canceler)

let shutdown st = Error_monad.cancel_with_exceptions st.canceler
