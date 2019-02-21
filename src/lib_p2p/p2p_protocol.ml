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

include Internal_event.Legacy_logging.Make (struct
  let name = "p2p.protocol"
end)

type ('msg, 'peer, 'conn) config = {
  swap_linger : Time.System.Span.t;
  pool : ('msg, 'peer, 'conn) P2p_pool.t;
  log : P2p_connection.P2p_event.t -> unit;
  connect : P2p_point.Id.t -> ('msg, 'peer, 'conn) P2p_conn.t tzresult Lwt.t;
  mutable latest_accepted_swap : Time.System.t;
  mutable latest_successful_swap : Time.System.t;
}

let private_node_warn fmt =
  Format.kasprintf (fun s -> lwt_warn "[private node] %s" s) fmt

open P2p_answerer

let message conn _request size msg = Lwt_pipe.push conn.messages (size, msg)

module Private_answerer = struct
  let advertise conn _request _points =
    private_node_warn
      "Received new peers addresses from %a"
      P2p_peer.Id.pp
      conn.peer_id

  let bootstrap conn _request =
    private_node_warn
      "Receive requests for peers addresses from %a"
      P2p_peer.Id.pp
      conn.peer_id
    >>= fun () -> Lwt.return_nil

  let swap_request conn _request _new_point _peer =
    private_node_warn
      "Received swap requests from %a"
      P2p_peer.Id.pp
      conn.peer_id

  let swap_ack conn _request _point _peer_id =
    private_node_warn "Received swap ack from %a" P2p_peer.Id.pp conn.peer_id

  let create conn =
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
    P2p_pool.register_list_of_new_points
      ~medium:"advertise"
      ~source:conn.peer_id
      config.pool
      points ;
    Lwt.return_unit

  let bootstrap config conn _request_info =
    if conn.is_private then
      private_node_warn
        "Private peer (%a) asked other peers addresses"
        P2p_peer.Id.pp
        conn.peer_id
      >>= fun () -> Lwt.return_nil
    else P2p_pool.list_known_points ~ignore_private:true config.pool

  let swap t pool source_peer_id ~connect current_peer_id new_point =
    t.latest_accepted_swap <- Systime_os.now () ;
    connect new_point
    >>= function
    | Ok _new_conn -> (
        t.latest_successful_swap <- Systime_os.now () ;
        t.log (Swap_success {source = source_peer_id}) ;
        lwt_log_info "Swap to %a succeeded" P2p_point.Id.pp new_point
        >>= fun () ->
        match P2p_pool.Connection.find_by_peer_id pool current_peer_id with
        | None ->
            Lwt.return_unit
        | Some conn ->
            P2p_conn.disconnect conn )
    | Error err -> (
        t.latest_accepted_swap <- t.latest_successful_swap ;
        t.log (Swap_failure {source = source_peer_id}) ;
        match err with
        | [Timeout] ->
            lwt_debug
              "Swap to %a was interrupted: %a"
              P2p_point.Id.pp
              new_point
              pp_print_error
              err
        | _ ->
            lwt_log_error
              "Swap to %a failed: %a"
              P2p_point.Id.pp
              new_point
              pp_print_error
              err )

  let swap_ack config conn request new_point _peer =
    let source_peer_id = conn.peer_id in
    let pool = config.pool in
    let connect = config.connect in
    let log = config.log in
    log (Swap_ack_received {source = source_peer_id}) ;
    lwt_log_info "Swap ack received from %a" P2p_peer.Id.pp source_peer_id
    >>= fun () ->
    match request.last_sent_swap_request with
    | None ->
        Lwt.return_unit (* ignore *)
    | Some (_time, proposed_peer_id) -> (
      match P2p_pool.Connection.find_by_peer_id pool proposed_peer_id with
      | None ->
          swap config pool source_peer_id ~connect proposed_peer_id new_point
          >>= fun () -> Lwt.return_unit
      | Some _ ->
          Lwt.return_unit )

  let swap_request config conn _request new_point _peer =
    let source_peer_id = conn.peer_id in
    let pool = config.pool in
    let swap_linger = config.swap_linger in
    let connect = config.connect in
    let log = config.log in
    log (Swap_request_received {source = source_peer_id}) ;
    lwt_log_info "Swap request received from %a" P2p_peer.Id.pp source_peer_id
    >>= fun () ->
    (* Ignore if already connected to peer or already swapped less than <swap_linger> ago. *)
    let span_since_last_swap =
      Ptime.diff
        (Systime_os.now ())
        (Time.System.max
           config.latest_successful_swap
           config.latest_accepted_swap)
    in
    let new_point_info = P2p_pool.register_point pool new_point in
    if
      Ptime.Span.compare span_since_last_swap swap_linger < 0
      || not (P2p_point_state.is_disconnected new_point_info)
    then (
      log (Swap_request_ignored {source = source_peer_id}) ;
      lwt_log_info
        "Ignoring swap request from %a"
        P2p_peer.Id.pp
        source_peer_id )
    else
      match P2p_pool.Connection.random_addr pool ~no_private:true with
      | None ->
          lwt_log_info "No swap candidate for %a" P2p_peer.Id.pp source_peer_id
      | Some (proposed_point, proposed_peer_id) -> (
        match conn.write_swap_ack proposed_point proposed_peer_id with
        | Ok true ->
            log (Swap_ack_sent {source = source_peer_id}) ;
            swap config pool source_peer_id ~connect proposed_peer_id new_point
            >>= fun () -> Lwt.return_unit
        | Ok false ->
            log (Swap_request_received {source = source_peer_id}) ;
            Lwt.return_unit
        | Error _ ->
            log (Swap_request_received {source = source_peer_id}) ;
            Lwt.return_unit )

  let create config conn =
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
