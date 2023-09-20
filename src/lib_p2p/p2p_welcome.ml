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

let create ?reuse_port ?addr ~backlog connect_handler port =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let* socket =
        P2p_fd.create_listening_socket ?reuse_port ~backlog ?addr port
      in
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
