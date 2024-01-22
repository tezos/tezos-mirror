(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open P2p_point

type 'data t =
  | Requested of {cancel : Lwt_canceler.t}
  | Accepted of {current_peer_id : P2p_peer.Id.t; cancel : Lwt_canceler.t}
  | Running of {data : 'data; current_peer_id : P2p_peer.Id.t}
  | Disconnected

type 'data state = 'data t

let pp ppf = function
  | Requested _ -> Format.fprintf ppf "requested"
  | Accepted {current_peer_id; _} ->
      Format.fprintf ppf "accepted %a" P2p_peer.Id.pp current_peer_id
  | Running {current_peer_id; _} ->
      Format.fprintf ppf "running %a" P2p_peer.Id.pp current_peer_id
  | Disconnected -> Format.fprintf ppf "disconnected"

module Info = struct
  type reconnection_info = {
    delay : Time.System.Span.t;
    end_time : Time.System.t;
  }

  type 'data t = {
    point : Id.t;
    mutable trusted : bool;
    mutable state : 'data state;
    mutable last_failed_connection : Time.System.t option;
    mutable last_rejected_connection : (P2p_peer.Id.t * Time.System.t) option;
    mutable last_established_connection :
      (P2p_peer.Id.t * Time.System.t) option;
    mutable known_public : bool;
    mutable last_disconnection : (P2p_peer.Id.t * Time.System.t) option;
    mutable reconnection_info : reconnection_info option;
    events : Pool_event.t Ringo.Ring.t;
    mutable expected_peer_id : P2p_peer.Id.t option;
    watchers : Pool_event.t Lwt_watcher.input;
  }

  type 'data point_info = 'data t

  let compare pi1 pi2 = Id.compare pi1.point pi2.point

  let log_size = 100

  let create ?(trusted = false) ?expected_peer_id addr port =
    {
      point = (addr, port);
      trusted;
      state = Disconnected;
      last_failed_connection = None;
      last_rejected_connection = None;
      last_established_connection = None;
      last_disconnection = None;
      known_public = false;
      events = Ringo.Ring.create log_size;
      reconnection_info = None;
      watchers = Lwt_watcher.create_input ();
      expected_peer_id;
    }

  let point s = s.point

  let trusted s = s.trusted

  let set_trusted gi = gi.trusted <- true

  let unset_trusted gi = gi.trusted <- false

  let reset_reconnection_delay gi = gi.reconnection_info <- None

  let get_expected_peer_id gi = gi.expected_peer_id

  let last_established_connection s = s.last_established_connection

  let last_disconnection s = s.last_disconnection

  let last_failed_connection s = s.last_failed_connection

  let last_rejected_connection s = s.last_rejected_connection

  let known_public s = s.known_public

  let cannot_reconnect_yet ~now {reconnection_info; _} =
    Option.fold
      ~none:false
      ~some:(fun gr -> Time.System.compare now gr.end_time <= 0)
      reconnection_info

  let reconnection_time {reconnection_info; _} =
    Option.map (fun gr -> gr.end_time) reconnection_info

  let last_seen s =
    Time.System.recent
      s.last_rejected_connection
      (Time.System.recent s.last_established_connection s.last_disconnection)

  let last_miss s =
    Option.merge
      Time.System.max
      s.last_failed_connection
      (Option.map snd
      @@ Time.System.recent s.last_rejected_connection s.last_disconnection)

  let log {events; watchers; _} ~timestamp kind =
    let event = Time.System.stamp ~time:timestamp kind in
    Ringo.Ring.add events event ;
    Lwt_watcher.notify watchers event

  let log_incoming_rejection ~timestamp point_info peer_id =
    log point_info ~timestamp (Rejecting_request peer_id)

  let events {events; _} = Ringo.Ring.elements events

  let watch {watchers; _} = Lwt_watcher.create_stream watchers
end

let get {Info.state; _} = state

let is_running {Info.state; _} =
  match state with
  | Running _ -> true
  | Disconnected | Requested _ | Accepted _ -> false

let is_disconnected {Info.state; _} =
  match state with
  | Disconnected -> true
  | Requested _ | Accepted _ | Running _ -> false

let is_accepted {Info.state; _} =
  match state with
  | Accepted _ -> true
  | Disconnected | Requested _ | Running _ -> false

let set_requested ~timestamp point_info cancel =
  assert (
    match point_info.Info.state with
    | Requested _ -> true
    | Accepted _ | Running _ -> false
    | Disconnected -> true) ;
  point_info.state <- Requested {cancel} ;
  Info.log point_info ~timestamp Outgoing_request

let set_accepted ~timestamp point_info current_peer_id cancel =
  (* log_notice "SET_ACCEPTED %a@." P2p_point.pp point_info.point ; *)
  assert (
    match point_info.Info.state with
    | Accepted _ | Running _ -> false
    | Requested _ | Disconnected -> true) ;
  point_info.state <- Accepted {current_peer_id; cancel} ;
  Info.log point_info ~timestamp (Accepting_request current_peer_id)

let set_private point_info known_private =
  point_info.Info.known_public <- not known_private

let set_running ~timestamp point_info peer_id data =
  assert (
    match point_info.Info.state with
    | Disconnected -> true (* request to unknown peer_id. *)
    | Running _ -> false
    | Accepted {current_peer_id; _} -> P2p_peer.Id.equal peer_id current_peer_id
    | Requested _ -> true) ;
  point_info.state <- Running {data; current_peer_id = peer_id} ;
  point_info.last_established_connection <- Some (peer_id, timestamp) ;
  Info.log point_info ~timestamp (Connection_established peer_id)

let maxed_time_add t s =
  match Ptime.add_span t s with Some t -> t | None -> Ptime.max

let set_reconnection_delay (reconnection_config : Point_reconnection_config.t)
    timestamp point_info =
  let disconnection_delay =
    match point_info.Info.reconnection_info with
    | None -> reconnection_config.initial_delay
    | Some gr -> gr.delay
  in
  let end_time = maxed_time_add timestamp disconnection_delay in
  let delay =
    let new_delay =
      Time.System.Span.multiply_exn
        reconnection_config.factor
        disconnection_delay
    in
    if Ptime.Span.compare reconnection_config.increase_cap new_delay > 0 then
      new_delay
    else reconnection_config.increase_cap
  in
  point_info.Info.reconnection_info <- Some {delay; end_time}

let set_disconnected ~timestamp ?(requested = false)
    (reconnection_config : Point_reconnection_config.t) point_info =
  let event : Pool_event.kind =
    match point_info.Info.state with
    | Requested _ ->
        set_reconnection_delay reconnection_config timestamp point_info ;
        point_info.last_failed_connection <- Some timestamp ;
        Request_rejected None
    | Accepted {current_peer_id; _} ->
        set_reconnection_delay reconnection_config timestamp point_info ;
        point_info.last_rejected_connection <- Some (current_peer_id, timestamp) ;
        Request_rejected (Some current_peer_id)
    | Running {current_peer_id; _} ->
        let delay = reconnection_config.initial_delay in
        let end_time =
          maxed_time_add timestamp reconnection_config.disconnection_delay
        in
        point_info.reconnection_info <- Some {delay; end_time} ;
        point_info.last_disconnection <- Some (current_peer_id, timestamp) ;
        if requested then Disconnection current_peer_id
        else External_disconnection current_peer_id
    | Disconnected -> assert false
  in
  point_info.state <- Disconnected ;
  Info.log point_info ~timestamp event

let set_expected_peer_id point_info id =
  point_info.Info.expected_peer_id <- Some id

let get_expected_peer_id point_info = point_info.Info.expected_peer_id

let info_of_point_info i =
  let open P2p_point.Info in
  let open P2p_point.State in
  let state =
    match get i with
    | Requested _ -> Requested
    | Accepted {current_peer_id; _} -> Accepted current_peer_id
    | Running {current_peer_id; _} -> Running current_peer_id
    | Disconnected -> Disconnected
  in
  Info.
    {
      trusted = trusted i;
      state;
      reconnection_time = reconnection_time i;
      last_failed_connection = last_failed_connection i;
      last_rejected_connection = last_rejected_connection i;
      last_established_connection = last_established_connection i;
      last_disconnection = last_disconnection i;
      last_seen = last_seen i;
      last_miss = last_miss i;
      expected_peer_id = get_expected_peer_id i;
    }
