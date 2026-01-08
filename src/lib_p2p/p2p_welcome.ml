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

type state = {socket : Lwt_unix.file_descr; connect_handler : connect_handler}

(* This worker must not be parallelized. The worker loops on {!P2p_fd.accept}
   then checks if not too many file descriptors are opened. If multiple
   {!P2p_fd.accept} are ran in parallel, we may crash because of too many opened
   file descriptors.

   Therefore we declare at toplevel a mutex, that prevents concurrent [accept]. *)
let accept_lock = Lwt_mutex.create ()

let accept st =
  let open Lwt_syntax in
  let (Connect_handler connect_handler) = st.connect_handler in
  let* () = Lwt.pause () in
  let* () = Lwt_mutex.lock accept_lock in
  let* res =
    protect @@ fun () ->
    let fd_pool =
      P2p_pool.get_fd_pool @@ P2p_connect_handler.get_pool connect_handler
    in
    let* r = P2p_fd.accept ?fd_pool st.socket in
    Result.fold
      ~ok:(fun (fd, addr) ->
        let point =
          match addr with
          | Lwt_unix.ADDR_UNIX _ -> assert false
          | Lwt_unix.ADDR_INET (addr, port) ->
              (Ipaddr_unix.V6.of_inet_addr_exn addr, port)
        in
        P2p_fd.set_point ~point fd ;
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
        | ex -> Lwt_result_syntax.tzfail (P2p_fd.accept_error_to_tzerror ex))
      r
  in
  Lwt_mutex.unlock accept_lock ;
  return res

module Name = P2p_workers.Unique_name_maker (struct
  let base = ["p2p"; "welcome"]
end)

module Types = struct
  type nonrec state = state

  type parameters = {
    socket : Lwt_unix.file_descr;
    connect_handler : connect_handler;
  }
end

module Worker = P2p_workers.Make (Name) (P2p_workers.Loop_request) (Types)

type t = Worker.activated_worker

module Handlers = struct
  module Request = Worker.Request

  type self = Worker.callback Worker.t

  type launch_error = tztrace

  let on_request : type r request_error.
      self -> (r, request_error) Request.t -> (r, request_error) result Lwt.t =
   fun w Loop -> accept (Worker.state w)

  let on_launch _w () ({socket; connect_handler} : Types.parameters) =
    Lwt_result_syntax.return {socket; connect_handler}

  let on_error (type a b) _w _st (req : (a, b) Request.t) (errs : b) :
      [`Shutdown | `Continue] tzresult Lwt.t =
    let open Lwt_result_syntax in
    match (req, errs) with
    | Loop, (Canceled | Exn Lwt.Canceled) :: _ -> return `Shutdown
    | Loop, trace ->
        let*! () = Events.(emit unexpected_error) trace in
        return `Shutdown

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close w =
    let open Lwt_result_syntax in
    let state = Worker.state w in
    let*! r = Lwt_utils_unix.safe_close state.socket in
    Result.iter_error_s Events.(emit unexpected_error_closing_socket) r
end

let create ?reuse_port ?addr ~backlog connect_handler port =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let* socket =
        P2p_fd.create_listening_socket ?reuse_port ~backlog ?addr port
      in
      Worker.create
        ()
        {socket; connect_handler = Connect_handler connect_handler}
        (module Handlers))
    (fun exn ->
      let error = error_of_exn exn in
      let*! () = Events.(emit incoming_connection_error) error in
      tzfail error)

let activate w = Worker.activate w

let shutdown (w : t) = Worker.shutdown w

module Internal_for_tests = struct
  let create ?reuse_port = create ?reuse_port
end

(* Shadowing of [create] that removes the [?reuse_port] argument *)
let create ?addr ~backlog connect_handler port =
  create ?addr ~backlog connect_handler port
