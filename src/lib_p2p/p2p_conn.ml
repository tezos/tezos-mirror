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

include Internal_event.Legacy_logging.Make (struct
  let name = "p2p.conn"
end)

type ('msg, 'peer, 'conn) t = {
  canceler : Lwt_canceler.t;
  messages : (int * 'msg) Lwt_pipe.t;
  conn : ('msg P2p_message.t, 'conn) P2p_socket.t;
  peer_info : (('msg, 'peer, 'conn) t, 'peer, 'conn) P2p_peer_state.Info.t;
  point_info : ('msg, 'peer, 'conn) t P2p_point_state.Info.t option;
  negotiated_version : Network_version.t;
  mutable last_sent_swap_request : (Time.System.t * P2p_peer.Id.t) option;
  mutable wait_close : bool;
  mutable worker : unit Lwt.t option;
  peer_id : P2p_peer.Id.t;
  trusted_node : bool;
  private_node : bool;
}

let rec worker_loop (t : ('msg, 'peer, 'conn) t) callback =
  let open P2p_answerer in
  let request_info =
    P2p_answerer.{last_sent_swap_request = t.last_sent_swap_request}
  in
  Lwt_unix.yield ()
  >>= fun () ->
  protect ~canceler:t.canceler (fun () -> P2p_socket.read t.conn)
  >>= function
  | Ok (_, Bootstrap) -> (
      (* callback.bootstrap will return an empty list if the node
         is in private mode *)
      callback.bootstrap request_info
      >>= function
      | [] ->
          worker_loop t callback
      | points -> (
        match P2p_socket.write_now t.conn (Advertise points) with
        | Ok _sent ->
            (* if not sent then ?? TODO count dropped message ?? *)
            worker_loop t callback
        | Error _ ->
            Lwt_canceler.cancel t.canceler >>= fun () -> Lwt.return_unit ) )
  | Ok (_, Advertise points) ->
      (* callback.advertise will ignore the points if the node is
         in private mode *)
      callback.advertise request_info points
      >>= fun () -> worker_loop t callback
  | Ok (_, Swap_request (point, peer)) ->
      callback.swap_request request_info point peer
      >>= fun () -> worker_loop t callback
  | Ok (_, Swap_ack (point, peer)) ->
      callback.swap_ack request_info point peer
      >>= fun () -> worker_loop t callback
  | Ok (size, Message msg) ->
      callback.message request_info size msg
      >>= fun () -> worker_loop t callback
  | Ok (_, Disconnect) | Error (P2p_errors.Connection_closed :: _) ->
      Lwt_canceler.cancel t.canceler >>= fun () -> Lwt.return_unit
  | Error (P2p_errors.Decoding_error :: _) ->
      (* TODO: Penalize peer... *)
      Lwt_canceler.cancel t.canceler >>= fun () -> Lwt.return_unit
  | Error (Canceled :: _) ->
      Lwt.return_unit
  | Error err ->
      lwt_log_error
        "@[Answerer unexpected error:@ %a@]"
        Error_monad.pp_print_error
        err
      >>= fun () ->
      Lwt_canceler.cancel t.canceler >>= fun () -> Lwt.return_unit

let shutdown t =
  match t.worker with
  | None ->
      Lwt.return_unit
  | Some w ->
      Lwt_canceler.cancel t.canceler >>= fun () -> w

let write_swap_ack t point peer_id =
  P2p_socket.write_now t.conn (Swap_ack (point, peer_id))

let create conn point_info peer_info messages canceler callback
    negotiated_version =
  let private_node = P2p_socket.private_node conn in
  let trusted_node =
    P2p_peer_state.Info.trusted peer_info
    || Option.unopt_map
         ~default:false
         ~f:P2p_point_state.Info.trusted
         point_info
  in
  let peer_id = peer_info |> P2p_peer_state.Info.peer_id in
  let t =
    {
      conn;
      point_info;
      peer_info;
      messages;
      canceler;
      wait_close = false;
      last_sent_swap_request = None;
      negotiated_version;
      worker = None;
      peer_id;
      private_node;
      trusted_node;
    }
  in
  let conn_info =
    P2p_answerer.
      {
        peer_id = t.peer_info |> P2p_peer_state.Info.peer_id;
        is_private = P2p_socket.private_node t.conn;
        write_swap_ack = write_swap_ack t;
        messages;
      }
  in
  t.worker <-
    Some
      (Lwt_utils.worker
         "answerer"
         ~on_event:Internal_event.Lwt_worker_event.on_event
         ~run:(fun () -> worker_loop t (callback conn_info))
         ~cancel:(fun () -> Lwt_canceler.cancel t.canceler)) ;
  t

let pipe_exn_handler = function
  | Lwt_pipe.Closed ->
      fail P2p_errors.Connection_closed
  | exc ->
      Lwt.fail exc

(* see [Lwt_pipe.pop] *)

let read t =
  Lwt.catch
    (fun () ->
      Lwt_pipe.pop t.messages
      >>= fun (s, msg) ->
      lwt_debug
        "%d bytes message popped from queue %a\027[0m"
        s
        P2p_peer.Id.pp
        (P2p_socket.info t.conn).peer_id
      >>= fun () -> return msg)
    pipe_exn_handler

let is_readable t =
  Lwt.catch
    (fun () -> Lwt_pipe.values_available t.messages >>= return)
    pipe_exn_handler

let write t msg = P2p_socket.write t.conn (Message msg)

let write_sync t msg = P2p_socket.write_sync t.conn (Message msg)

let raw_write_sync t buf = P2p_socket.raw_write_sync t.conn buf

let write_now t msg = P2p_socket.write_now t.conn (Message msg)

let write_swap_request t point peer_id =
  t.last_sent_swap_request <- Some (Systime_os.now (), peer_id) ;
  P2p_socket.write_now t.conn (Swap_request (point, peer_id))

let write_bootstrap t = P2p_socket.write_now t.conn Bootstrap

let stat t = P2p_socket.stat t.conn

let info t = P2p_socket.info t.conn

let local_metadata t = P2p_socket.local_metadata t.conn

let remote_metadata t = P2p_socket.remote_metadata t.conn

let disconnect ?(wait = false) t =
  t.wait_close <- wait ;
  shutdown t

let close t = P2p_socket.close ~wait:t.wait_close t.conn

let equal_sock t t' = P2p_socket.equal t.conn t'.conn

let private_node t = t.private_node

let peer_id t = t.peer_id

let trusted_node t = t.trusted_node
