(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

include Internal_event.Legacy_logging.Make (struct let name = "p2p.answerer" end)

type 'msg callback = {
  bootstrap: unit -> P2p_point.Id.t list Lwt.t ;
  advertise: P2p_point.Id.t list -> unit Lwt.t ;
  message: int -> 'msg -> unit Lwt.t ;
  swap_request: P2p_point.Id.t -> P2p_peer.Id.t -> unit Lwt.t ;
  swap_ack: P2p_point.Id.t -> P2p_peer.Id.t -> unit Lwt.t ;
}

type ('msg, 'meta) t = {
  canceler: Lwt_canceler.t ;
  conn: ('msg P2p_message.t, 'meta) P2p_socket.t ;
  callback: 'msg callback ;
  mutable worker: unit Lwt.t ;
}

let rec worker_loop st =
  Lwt_unix.yield () >>= fun () ->
  protect ~canceler:st.canceler begin fun () ->
    P2p_socket.read st.conn
  end >>= function
  | Ok (_, Bootstrap) -> begin
      (* st.callback.bootstrap will return an empty list if the node
         is in private mode *)
      st.callback.bootstrap () >>= function
      | [] ->
          worker_loop st
      | points ->
          match P2p_socket.write_now st.conn (Advertise points) with
          | Ok _sent ->
              (* if not sent then ?? TODO count dropped message ?? *)
              worker_loop st
          | Error _ ->
              Lwt_canceler.cancel st.canceler
    end
  | Ok (_, Advertise points) ->
      (* st.callback.advertise will ignore the points if the node is
         in private mode *)
      st.callback.advertise points >>= fun () ->
      worker_loop st
  | Ok (_, Swap_request (point, peer)) ->
      st.callback.swap_request point peer >>= fun () ->
      worker_loop st
  | Ok (_, Swap_ack (point, peer)) ->
      st.callback.swap_ack point peer >>= fun () ->
      worker_loop st
  | Ok (size, Message msg) ->
      st.callback.message size msg >>= fun () ->
      worker_loop st
  | Ok (_, Disconnect) | Error (P2p_errors.Connection_closed :: _) ->
      Lwt_canceler.cancel st.canceler
  | Error (P2p_errors.Decoding_error :: _) ->
      (* TODO: Penalize peer... *)
      Lwt_canceler.cancel st.canceler
  | Error (Canceled  :: _) ->
      Lwt.return_unit
  | Error err ->
      lwt_log_error "@[Answerer unexpected error:@ %a@]"
        Error_monad.pp_print_error err >>= fun () ->
      Lwt_canceler.cancel st.canceler

let run conn canceler callback =
  let st = {
    canceler ; conn ; callback ;
    worker = Lwt.return_unit ;
  } in
  st.worker <-
    Lwt_utils.worker "answerer"
      ~on_event:Internal_event.Lwt_worker_event.on_event
      ~run:(fun () -> worker_loop st)
      ~cancel:(fun () -> Lwt_canceler.cancel canceler) ;
  st

let shutdown st =
  Lwt_canceler.cancel st.canceler >>= fun () ->
  st.worker
