(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

module Events = P2p_events.P2p_conn

type ('msg, 'peer, 'conn) t = {
  canceler : Lwt_canceler.t;
  greylister : motive:string -> unit Lwt.t;
  messages : (int * 'msg) Lwt_pipe.Maybe_bounded.t;
  conn : ('msg P2p_message.t, 'conn) P2p_socket.t;
  peer_info : (('msg, 'peer, 'conn) t, 'peer, 'conn) P2p_peer_state.Info.t;
  point_info : ('msg, 'peer, 'conn) t P2p_point_state.Info.t option;
  negotiated_version : Network_version.t;
  mutable last_sent_swap_request : (Time.System.t * P2p_peer.Id.t) option;
  mutable wait_close : bool;
  mutable disconnect_reason : P2p_disconnection_reason.t option;
  mutable worker : unit Lwt.t option;
  peer_id : P2p_peer.Id.t;
  trusted_node : bool;
  private_node : bool;
  disable_peer_discovery : bool;
}

let rec worker_loop (t : ('msg, 'peer, 'conn) t) callback =
  let open Lwt_syntax in
  let open P2p_answerer in
  let* () = Lwt.pause () in
  let* r = protect ~canceler:t.canceler (fun () -> P2p_socket.read t.conn) in
  match r with
  | Ok (_, Bootstrap) -> (
      Prometheus.Counter.inc_one P2p_metrics.Messages.bootstrap_received ;
      let* () = Events.(emit bootstrap_received) t.peer_id in
      if t.disable_peer_discovery then worker_loop t callback
      else
        (* Since [request_info] may be modified in another Lwt thread, it is
           required that getting the [last_sent_swat_request] and invoquing the
           callback be atomic to avoid race conditions.
           e.g. there must not exist a Lwt cooperation point between this
           affectation and the invocation of the callback.
           The same statement is also true in the next cases. *)
        let request_info =
          P2p_answerer.{last_sent_swap_request = t.last_sent_swap_request}
        in
        let* r = callback.bootstrap request_info in
        match r with
        | Ok () -> worker_loop t callback
        | Error _ -> Error_monad.cancel_with_exceptions t.canceler)
  | Ok (_, Advertise points) ->
      Prometheus.Counter.inc_one P2p_metrics.Messages.advertise_received ;
      let* () = Events.(emit advertise_received) (t.peer_id, points) in
      let* () =
        if t.disable_peer_discovery then return_unit
        else
          let request_info =
            P2p_answerer.{last_sent_swap_request = t.last_sent_swap_request}
          in
          callback.advertise request_info points
      in
      worker_loop t callback
  | Ok (_, Swap_request (point, peer)) ->
      Prometheus.Counter.inc_one P2p_metrics.Messages.swap_request_received ;
      let* () = Events.(emit swap_request_received) (t.peer_id, point, peer) in
      let request_info =
        P2p_answerer.{last_sent_swap_request = t.last_sent_swap_request}
      in
      let* () = callback.swap_request request_info point peer in
      worker_loop t callback
  | Ok (_, Swap_ack (point, peer)) ->
      Prometheus.Counter.inc_one P2p_metrics.Messages.swap_ack_received ;
      let* () = Events.(emit swap_ack_received) (t.peer_id, point, peer) in
      let request_info =
        P2p_answerer.{last_sent_swap_request = t.last_sent_swap_request}
      in
      let* () = callback.swap_ack request_info point peer in
      worker_loop t callback
  | Ok (size, Message msg) ->
      Prometheus.Counter.inc_one P2p_metrics.Messages.user_message_received ;
      let request_info =
        P2p_answerer.{last_sent_swap_request = t.last_sent_swap_request}
      in
      let* () = callback.message request_info size msg in
      worker_loop t callback
  | Ok (_, Disconnect) | Error (P2p_errors.Connection_closed :: _) ->
      Error_monad.cancel_with_exceptions t.canceler
  | Error (Tezos_base.Data_encoding_wrapper.Decoding_error _ :: _) ->
      let* () = t.greylister ~motive:"decoding error" in
      Error_monad.cancel_with_exceptions t.canceler
  | Error (Canceled :: _) -> Lwt.return_unit
  | Error err ->
      let* () = Events.(emit unexpected_error) err in
      Error_monad.cancel_with_exceptions t.canceler

let shutdown t =
  let open Lwt_syntax in
  match t.worker with
  | None -> Lwt.return_unit
  | Some w ->
      let* () = Error_monad.cancel_with_exceptions t.canceler in
      w

let write_swap_ack t point peer_id =
  let result = P2p_socket.write_now t.conn (Swap_ack (point, peer_id)) in
  Prometheus.Counter.inc_one P2p_metrics.Messages.swap_ack_sent ;
  result

let write_advertise t points =
  if t.disable_peer_discovery then
    Result_syntax.tzfail P2p_errors.Peer_discovery_disabled
  else
    let result = P2p_socket.write_now t.conn (Advertise points) in
    Prometheus.Counter.inc_one P2p_metrics.Messages.advertise_sent ;
    result

let create ~conn ~point_info ~peer_info ~messages ~canceler ~greylister
    ~callback ~disable_peer_discovery negotiated_version =
  let private_node = P2p_socket.private_node conn in
  let trusted_node =
    P2p_peer_state.Info.trusted peer_info
    || Option.fold ~none:false ~some:P2p_point_state.Info.trusted point_info
  in
  let peer_id = peer_info |> P2p_peer_state.Info.peer_id in
  let t =
    {
      conn;
      point_info;
      peer_info;
      messages;
      canceler;
      greylister;
      wait_close = false;
      disconnect_reason = None;
      last_sent_swap_request = None;
      negotiated_version;
      worker = None;
      peer_id;
      private_node;
      trusted_node;
      disable_peer_discovery;
    }
  in
  let conn_info =
    P2p_answerer.
      {
        peer_id = t.peer_info |> P2p_peer_state.Info.peer_id;
        is_private = P2p_socket.private_node t.conn;
        write_advertise = write_advertise t;
        write_swap_ack = write_swap_ack t;
        messages;
      }
  in
  t.worker <-
    Some
      (Lwt_utils.worker
         "answerer"
         ~on_event:Internal_event.Lwt_worker_logger.on_event
         ~run:(fun () -> worker_loop t (callback conn_info))
         ~cancel:(fun () -> Error_monad.cancel_with_exceptions t.canceler)) ;
  t

let pipe_exn_handler = function
  | Lwt_pipe.Closed -> Lwt_result_syntax.tzfail P2p_errors.Connection_closed
  | exc -> Lwt.fail exc

(* see [Lwt_pipe.Maybe_bounded.pop] *)

let read t =
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let* s, msg = Lwt_pipe.Maybe_bounded.pop t.messages in
      let* () =
        Events.(emit bytes_popped_from_queue)
          (s, (P2p_socket.info t.conn).peer_id)
      in
      return_ok msg)
    (fun e ->
      Prometheus.Counter.inc_one
        P2p_metrics.Messages.user_message_received_error ;
      pipe_exn_handler e)

let is_readable t =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! _ = Lwt_pipe.Maybe_bounded.peek t.messages in
      return_unit)
    pipe_exn_handler

let write t msg =
  let result = P2p_socket.write t.conn (Message msg) in
  Prometheus.Counter.inc_one P2p_metrics.Messages.user_message_sent ;
  result

let write_sync t msg =
  let result = P2p_socket.write_sync t.conn (Message msg) in
  Prometheus.Counter.inc_one P2p_metrics.Messages.user_message_sent ;
  result

let encode t msg = P2p_socket.encode t.conn (Message msg)

let write_encoded_now t buf =
  let result = P2p_socket.write_encoded_now t.conn buf in
  Prometheus.Counter.inc_one P2p_metrics.Messages.broadcast_message_sent ;
  result

let write_now t msg =
  let result = P2p_socket.write_now t.conn (Message msg) in
  Prometheus.Counter.inc_one P2p_metrics.Messages.user_message_sent ;
  result

let write_swap_request t point peer_id =
  t.last_sent_swap_request <- Some (Time.System.now (), peer_id) ;
  let result = P2p_socket.write_now t.conn (Swap_request (point, peer_id)) in
  Prometheus.Counter.inc_one P2p_metrics.Messages.swap_request_sent ;
  result

let write_bootstrap t =
  if t.disable_peer_discovery then (
    Events.(emit__dont_wait__use_with_care peer_discovery_disabled) () ;
    Result_syntax.return_false)
  else
    let result = P2p_socket.write_now t.conn Bootstrap in
    Prometheus.Counter.inc_one P2p_metrics.Messages.bootstrap_sent ;
    result

let stat t = P2p_socket.stat t.conn

let info t = P2p_socket.info t.conn

let local_metadata t = P2p_socket.local_metadata t.conn

let remote_metadata t = P2p_socket.remote_metadata t.conn

let disconnect ?(wait = false) ~reason t =
  let open Lwt_syntax in
  let* () = Events.(emit disconnect) t.peer_id in
  t.wait_close <- wait ;
  t.disconnect_reason <- Some reason ;
  shutdown t

let close ~reason t = P2p_socket.close ~wait:t.wait_close ~reason t.conn

let disconnect_reason t = t.disconnect_reason

let equal_sock t t' = P2p_socket.equal t.conn t'.conn

let private_node t = t.private_node

let peer_id t = t.peer_id

let trusted_node t = t.trusted_node

let negotiated_version t = t.negotiated_version

module Internal_for_tests = struct
  let raw_write_sync t buf =
    P2p_socket.Internal_for_tests.raw_write_sync t.conn buf
end
