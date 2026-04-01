(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Events = P2p_events.P2p_protocol

type ('msg, 'peer, 'conn) config = {
  swap_linger : Time.System.Span.t option;
  pool : ('msg, 'peer, 'conn) P2p_pool.t;
  log : P2p_connection.P2p_event.t -> unit;
  connect : P2p_point.Id.t -> ('msg, 'peer, 'conn) P2p_conn.t tzresult Lwt.t;
  mutable latest_accepted_swap : Time.System.t;
  mutable latest_successful_swap : Time.System.t;
}

type throttle_entry = {
  mutable last_handled : Time.System.t;
      (** Timestamp of the last invocation of this callback. *)
  cooldown_s : float option;
      (** [None] = no rate limiting; the callback is always forwarded
          immediately.
          [Some s] = minimum time between consecutive calls, in seconds; a call
          arriving sooner than [s] after the previous one is delayed. *)
}

type throttle_state = {
  advertise : throttle_entry;
  bootstrap : throttle_entry;
  swap_request : throttle_entry;
  swap_ack : throttle_entry;
  message : throttle_entry;
}

(** [throttle_callbacks conn_lost state callback] wraps [callback] so that each
    callback type is subject to a per-type cooldown: if the same callback is
    invoked more than once within [cooldown_s] seconds, the new call sleeps for
    [cooldown_s] (or until [conn_lost] resolves, whichever comes first) before
    being forwarded. Calls are rate-limited, not dropped. *)
let throttle_callbacks ~conn_lost (throttle_state : throttle_state)
    (callback : _ P2p_answerer.callback) =
  let open Lwt_syntax in
  let throttle ({last_handled; cooldown_s} as entry) =
    match cooldown_s with
    | None -> return_unit
    | Some cooldown_s ->
        let now = Time.System.now () in
        let throttled =
          Ptime.Span.compare
            (Ptime.diff now last_handled)
            (Time.System.Span.of_seconds_exn cooldown_s)
          < 0
        in
        if throttled then (
          let* () =
            Lwt.choose
              [
                Lwt_unix.sleep cooldown_s;
                (let* () = conn_lost in
                 Lwt.pause ());
              ]
          in
          (* Stamp the time after the sleep so that the cooldown window restarts
             from the wake-up point: a sustained burst is continuously throttled. *)
          entry.last_handled <- Time.System.now () ;
          return_unit)
        else (
          entry.last_handled <- now ;
          return_unit)
  in
  P2p_answerer.
    {
      message =
        (fun info i msg ->
          let* () = throttle throttle_state.message in
          callback.message info i msg);
      advertise =
        (fun info points ->
          let* () = throttle throttle_state.advertise in
          callback.advertise info points);
      bootstrap =
        (fun info ->
          let* () = throttle throttle_state.bootstrap in
          callback.bootstrap info);
      swap_request =
        (fun info point k ->
          let* () = throttle throttle_state.swap_request in
          callback.swap_request info point k);
      swap_ack =
        (fun info point k ->
          let* () = throttle throttle_state.swap_ack in
          callback.swap_ack info point k);
    }

(* Use [epoch] so that the first message on a fresh connection is never
   throttled: [now - epoch] is always much larger than any [cooldown_s]. *)
let make_entry cooldown_s =
  {last_handled = Ptime.epoch; cooldown_s = Some cooldown_s}

let no_throttle_entry () = {last_handled = Ptime.epoch; cooldown_s = None}

(** Default per-connection throttle parameters.
    - [message]: no throttle — application messages are forwarded immediately.
    - [advertise], [bootstrap]: 100 ms — these messages are harmless but can
      arrive in rapid bursts.
    - [swap_request], [swap_ack]: 60 s — a connection swap is a single
      round-trip; there is no legitimate reason to process more than one per
      minute. *)
let throttler () =
  {
    message = no_throttle_entry ();
    advertise = make_entry 0.1;
    bootstrap = make_entry 0.1;
    swap_request = make_entry 60.;
    swap_ack = make_entry 60.;
  }

open P2p_answerer

let message conn _request size msg =
  Lwt_pipe.Maybe_bounded.push conn.messages (size, msg)

module Private_answerer = struct
  let advertise conn _request _points =
    Events.(emit private_node_new_peers) conn.peer_id

  let bootstrap conn _request =
    Lwt_result.ok @@ Events.(emit private_node_peers_request) conn.peer_id

  let swap_request conn _request _new_point _peer =
    Events.(emit private_node_swap_request) conn.peer_id

  let swap_ack conn _request _point _peer_id =
    Events.(emit private_node_swap_ack) conn.peer_id

  let create conn =
    throttle_callbacks
      ~conn_lost:conn.conn_lost
      (throttler ())
      P2p_answerer.
        {
          message = message conn;
          advertise = advertise conn;
          bootstrap = bootstrap conn;
          swap_request = swap_request conn;
          swap_ack = swap_ack conn;
        }
end

module Default_answerer = struct
  open P2p_connection.P2p_event

  let advertise config conn _request points =
    let log = config.log in
    let source_peer_id = conn.peer_id in
    log (Advertise_received {source = source_peer_id}) ;
    P2p_pool.register_list_of_new_points
      ~medium:"advertise"
      ~source:conn.peer_id
      config.pool
      points

  let bootstrap config conn _request_info =
    let open Lwt_result_syntax in
    let log = config.log in
    let source_peer_id = conn.peer_id in
    log (Bootstrap_received {source = source_peer_id}) ;
    if conn.is_private then
      let*! () = Events.(emit private_node_request) conn.peer_id in
      return_unit
    else
      let*! points =
        P2p_pool.list_known_points ~ignore_private:true config.pool
      in
      match points with
      | [] -> return_unit
      | points -> (
          match conn.write_advertise points with
          | Ok true ->
              log (Advertise_sent {source = source_peer_id}) ;
              return_unit
          | Ok false ->
              (* TODO: https://gitlab.com/tezos/tezos/-/issues/4594
                 if not sent then ?? TODO count dropped message ?? *)
              return_unit
          | Error err as error ->
              let*! () =
                Events.(emit advertise_sending_failed) (source_peer_id, err)
              in
              Lwt.return error)

  let swap t pool source_peer_id ~connect current_peer_id new_point =
    let open Lwt_syntax in
    t.latest_accepted_swap <- Time.System.now () ;
    let* r = connect new_point in
    match r with
    | Ok _new_conn -> (
        t.latest_successful_swap <- Time.System.now () ;
        t.log (Swap_success {source = source_peer_id}) ;
        Prometheus.Counter.inc_one P2p_metrics.Swap.success ;
        let* () = Events.(emit swap_succeeded) new_point in
        match P2p_pool.Connection.find_by_peer_id pool current_peer_id with
        | None -> return_unit
        | Some conn -> P2p_conn.disconnect ~reason:Peer_swapped conn)
    | Error err -> (
        t.latest_accepted_swap <- t.latest_successful_swap ;
        t.log (Swap_failure {source = source_peer_id}) ;
        Prometheus.Counter.inc_one P2p_metrics.Swap.fail ;
        match err with
        | [Timeout] -> Events.(emit swap_interrupted) (new_point, err)
        | _ -> Events.(emit swap_failed) (new_point, err))

  let swap_ack config conn request new_point _peer =
    let open Lwt_syntax in
    let source_peer_id = conn.peer_id in
    let pool = config.pool in
    let connect = config.connect in
    let log = config.log in
    log (Swap_ack_received {source = source_peer_id}) ;
    let do_swap =
      let open Option_syntax in
      let* _ = config.swap_linger (* Don't answer if swap is disabled. *) in
      let* _time, proposed_peer_id =
        (* Checks that a swap request has been sent to this connection. *)
        request.last_sent_swap_request
      in
      P2p_pool.Connection.find_by_point pool new_point
      (* Ignore the swap if the new point is already connected *)
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5211
         Should we accept the swap if we are not yet connected
         to the node, but already in the process of connecting to
         it? This can raise race conditions. *)
      |> Option.fold_f
           ~some:(fun _ -> None)
           ~none:(fun () ->
             Some
               (swap
                  config
                  pool
                  source_peer_id
                  ~connect
                  proposed_peer_id
                  new_point))
    in
    Option.value ~default:return_unit do_swap

  let swap_request config conn _request new_point _peer =
    let open Result_syntax in
    let source_peer_id = conn.peer_id in
    let pool = config.pool in
    let connect = config.connect in
    let log = config.log in
    log (Swap_request_received {source = source_peer_id}) ;
    let do_swap =
      let* swap_linger =
        config.swap_linger |> Option.to_result ~none:`Swap_is_disabled
      in
      (* Ignore if already connected to peer or already swapped less than <swap_linger> ago. *)
      let span_since_last_swap =
        Ptime.diff
          (Time.System.now ())
          (Time.System.max
             config.latest_successful_swap
             config.latest_accepted_swap)
      in
      (* We don't need to register the point here.
         Registering the point is the responsibility of the
         [P2p_connect_handler]. Registering the point will
         eventually be done in [P2p_connect_handler.connect].

         Moreover, registering the point here could lead to
         chaotic interactions with the maintainance. *)
      let new_point_info = P2p_pool.Points.info pool new_point in
      if
        Ptime.Span.compare span_since_last_swap swap_linger < 0
        || not
             (Option.fold
                ~none:true
                ~some:(fun new_point_info ->
                  P2p_point_state.is_disconnected new_point_info)
                new_point_info)
      then (
        log (Swap_request_ignored {source = source_peer_id}) ;
        Prometheus.Counter.inc_one P2p_metrics.Swap.ignored ;
        return @@ Events.(emit swap_request_ignored) source_peer_id)
      else
        let* source_conn =
          P2p_pool.Connection.find_by_peer_id pool source_peer_id
          |> Option.to_result ~none:`Couldnt_find_by_peer
        in
        let* proposed_point, proposed_peer_id =
          P2p_pool.Connection.random_addr
            pool
            ~different_than:source_conn
            ~no_private:true
          |> Option.to_result ~none:(`No_swap_candidate source_peer_id)
        in
        let* () =
          let* sent_succeeded =
            conn.write_swap_ack proposed_point proposed_peer_id
            |> Result.map_error (fun _ ->
                   `Couldnt_write_swap_ack "Connection closed")
          in
          if sent_succeeded then return_unit
          else
            fail @@ `Couldnt_write_swap_ack "Buffer is full. Message dropped."
        in
        log (Swap_ack_sent {source = source_peer_id}) ;
        return
        @@ swap config pool source_peer_id ~connect proposed_peer_id new_point
    in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5187
       Handle silently ignored error cases. *)
    Result.fold
      ~ok:Fun.id
      ~error:(function
        | `No_swap_candidate source_peer_id ->
            Events.(emit no_swap_candidate) source_peer_id
        | `Couldnt_find_by_peer
        (* The connection has been lost so ignore the request *)
        | `Swap_is_disabled | `Couldnt_write_swap_ack _ ->
            Lwt.return_unit)
      do_swap

  let create config conn =
    throttle_callbacks
      ~conn_lost:conn.conn_lost
      (throttler ())
      P2p_answerer.
        {
          message = message conn;
          advertise = advertise config conn;
          bootstrap = bootstrap config conn;
          swap_request = swap_request config conn;
          swap_ack = swap_ack config conn;
        }
end

let create_default = Default_answerer.create

let create_private () = Private_answerer.create
