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
  let open Lwt_syntax in
  let (Connect_handler connect_handler) = st.connect_handler in
  let* () = Lwt.pause () in
  let* r =
    protect ~canceler:st.canceler (fun () ->
        let* r = P2p_fd.accept st.socket in
        Result.fold
          ~ok:(fun (fd, addr) ->
            let point =
              match addr with
              | Lwt_unix.ADDR_UNIX _ -> assert false
              | Lwt_unix.ADDR_INET (addr, port) ->
                  (Ipaddr_unix.V6.of_inet_addr_exn addr, port)
            in
            P2p_connect_handler.accept connect_handler fd point ;
            Lwt.return_ok ())
          ~error:(function
            (* These are temporary system errors, giving some time for
               the system to recover *)
            | `System_error ex ->
                let* () =
                  Events.(emit incoming_error)
                    (TzTrace.make (error_of_exn ex), "system")
                in

                let* () = Lwt_unix.sleep 5. in
                Lwt.return_ok ()
            (* These errors are specific to attempted incoming connection,
               ignoring them. *)
            | `Socket_error ex ->
                let* () =
                  Events.(emit incoming_error)
                    (TzTrace.make (error_of_exn ex), "socket")
                in
                Lwt.return (Ok ())
            | `Unexpected_error ex ->
                Lwt.return_error (TzTrace.make (error_of_exn ex)))
          r)
  in
  match r with
  | Ok () -> worker_loop st
  | Error (Canceled :: _) -> Lwt.return_unit
  | Error err -> Events.(emit unexpected_error) err

let create_listening_socket ?(reuse_port = false) ~backlog
    ?(addr = Ipaddr.V6.unspecified) port =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let main_socket = Lwt_unix.(socket PF_INET6 SOCK_STREAM 0) in
      (if reuse_port then Lwt_unix.(setsockopt main_socket SO_REUSEPORT true)) ;
      Lwt_unix.(setsockopt main_socket SO_REUSEADDR true) ;
      let*! () =
        Lwt_unix.bind
          main_socket
          Unix.(ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port))
      in
      Lwt_unix.listen main_socket backlog ;
      return main_socket)
    (function
      | Unix.Unix_error (err, _, _) ->
          tzfail
            (Failed_to_open_listening_socket
               {reason = err; address = addr; port})
      | exn -> Lwt.fail exn)

let create ?reuse_port ?addr ~backlog connect_handler port =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let* socket = create_listening_socket ?reuse_port ~backlog ?addr port in
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
          let*! () = st.worker in
          let*! r = Lwt_utils_unix.safe_close socket in
          Result.iter_error_s Events.(emit unexpected_error_closing_socket) r) ;
      return st)
    (fun exn ->
      let error = error_of_exn exn in
      let*! () = Events.(emit incoming_connection_error) error in
      tzfail error)

let activate st =
  st.worker <-
    Lwt_utils.worker
      "welcome"
      ~on_event:Internal_event.Lwt_worker_logger.on_event
      ~run:(fun () -> worker_loop st)
      ~cancel:(fun () -> Error_monad.cancel_with_exceptions st.canceler)

let shutdown st = Error_monad.cancel_with_exceptions st.canceler
