(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/5165

   Add coverage unit tests *)

module Make (C : Gossipsub_intf.WORKER_CONFIGURATION) :
  Gossipsub_intf.WORKER with module GS = C.GS = struct
  open C
  module GS = GS
  module Topic = GS.Topic
  module Peer = GS.Peer
  module Message_id = GS.Message_id
  module Message = GS.Message

  (** A worker has one of the following statuses:
     - [Starting] in case it is initialized with {!make} but not started yet.
     - [Running] in case the function [start] has been called. *)
  type worker_status =
    | Starting
    | Running of {
        heartbeat_handle : unit Monad.t;
        event_loop_handle : unit Monad.t;
      }

  type full_message = {
    message : Message.t;
    topic : Topic.t;
    message_id : Message_id.t;
  }

  type p2p_message =
    | Graft of {topic : Topic.t}
    | Prune of {topic : Topic.t; px : Peer.t Seq.t}
    | IHave of {topic : Topic.t; message_ids : Message_id.t list}
    | IWant of {message_ids : Message_id.t list}
    | Subscribe of {topic : Topic.t}
    | Unsubscribe of {topic : Topic.t}
    | Publish of full_message
  (* FIXME: FIXME: https://gitlab.com/tezos/tezos/-/issues/5323

     The payloads of the variants about are quite simular to those of GS (types
     graft, prune, ...). We could reuse them if we move the peer field
     outside. *)

  (** The different kinds of events the Gossipsub worker handles. *)
  type event =
    | Heartbeat
    | New_connection of P2P.Connections_handler.connection
    | Disconnection of {peer : Peer.t}
    | Received_message of {peer : Peer.t; p2p_message : p2p_message}
    | Inject_message of full_message
    | Join of Topic.t
    | Leave of Topic.t

  (** The worker's state is made of its status, the gossipsub automaton's state,
      and a stream of events to process.  *)
  type t = {
    gossip_state : GS.state;
    status : worker_status;
    events_stream : event Stream.t;
  }

  (** This is the main function of the worker. It interacts with the Gossipsub
      automaton given an event. The outcome is a new automaton state and an
      output to be processed, depending on the kind of input event. *)
  let apply_event gossip_state = function
    | Heartbeat ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5170

           Do we want to detect cases where two successive [Heartbeat] events
           would be handled (e.g. because the first one is late)? *)
        let gossip_state, output = GS.heartbeat gossip_state in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5159

           Handle Heartbeat's output *)
        ignore output ;
        gossip_state
    | New_connection {peer; direct; outbound} ->
        let gossip_state, output =
          GS.add_peer {direct; outbound; peer} gossip_state
        in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5160

           Handle New_connection's output *)
        ignore output ;
        gossip_state
    | Disconnection {peer} ->
        let gossip_state, output = GS.remove_peer {peer} gossip_state in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5161

           Handle disconnection's output *)
        ignore output ;
        gossip_state
    | Inject_message {message; message_id; topic} ->
        let gossip_state, output =
          GS.publish {sender = None; topic; message_id; message} gossip_state
        in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5162

           Handle Inject_message's output *)
        ignore output ;
        gossip_state
    | Received_message m ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5164

           Handle received p2p messages. *)
        ignore (Received_message m) ;
        assert false
    | Join topic ->
        let gossip_state, output = GS.join {topic} gossip_state in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5191

           Handle Join's output. *)
        ignore output ;
        gossip_state
    | Leave topic ->
        let gossip_state, output = GS.leave {topic} gossip_state in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5191

           Handle Leave's output. *)
        ignore output ;
        gossip_state

  (** A helper function that pushes events in the state *)
  let push e t = Stream.push e t.events_stream

  let p2p_message t peer p2p_message =
    push (Received_message {peer; p2p_message}) t

  (** A set of functions that push different kinds of events in the worker's
      state. *)
  let inject t message_id message topic =
    push (Inject_message {message; message_id; topic}) t

  let new_connection t conn = push (New_connection conn) t

  let disconnection t peer = push (Disconnection {peer}) t

  let join t = List.iter (fun topic -> push (Join topic) t)

  let leave t = List.iter (fun topic -> push (Leave topic) t)

  (** This function returns a never-ending loop that periodically pushes
      [Heartbeat] events in the stream.  *)
  let heartbeat_events_producer ~heartbeat_span t =
    let stream = t.events_stream in
    let rec loop () =
      let open Monad in
      let* () = Monad.sleep heartbeat_span in
      Stream.push Heartbeat stream ;
      loop ()
    in
    loop ()

  (** This function returns a never-ending loop that processes the events of the
      worker's stream. *)
  let event_loop t =
    let rec loop t =
      let open Monad in
      let* event = Stream.pop t.events_stream in
      loop {t with gossip_state = apply_event t.gossip_state event}
    in
    loop t

  let start ~heartbeat_span topics t =
    match t.status with
    | Starting ->
        let heartbeat_handle = heartbeat_events_producer ~heartbeat_span t in
        let event_loop_handle = event_loop t in
        let status = Running {heartbeat_handle; event_loop_handle} in
        let t = {t with status} in
        let () = P2P.Connections_handler.on_connection (new_connection t) in
        let () = P2P.Connections_handler.on_diconnection (disconnection t) in
        join t topics ;
        t
    | Running _ ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5166

           Better error handling *)
        Format.eprintf "A worker is already running for this state!@." ;
        assert false

  let shutdown state =
    match state.status with
    | Starting -> ()
    | Running _ ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5171

           Implement worker shutdown.
           Should we unsubscribe from the callbacks called in start? *)
        ()

  let make rng limits parameters =
    {
      gossip_state = GS.make rng limits parameters;
      status = Starting;
      events_stream = Stream.empty;
    }
end
