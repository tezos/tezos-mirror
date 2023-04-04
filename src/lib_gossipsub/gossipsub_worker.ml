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
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5323

     The payloads of the variants about are quite simular to those of GS (types
     graft, prune, ...). We could reuse them if we move the peer field
     outside. *)

  type p2p_input =
    | In_message of {from_peer : Peer.t; p2p_message : p2p_message}
    | New_connection of {peer : Peer.t; direct : bool; outbound : bool}
    | Disconnection of {peer : Peer.t}

  type app_input =
    | Inject_message of full_message
    | Join of Topic.t
    | Leave of Topic.t

  type p2p_output =
    | Out_message of {to_peer : Peer.t; p2p_message : p2p_message}
    | Disconnect of {peer : Peer.t}
    | Kick of {peer : Peer.t}

  type app_output = full_message

  (** The different kinds of events the Gossipsub worker handles. *)
  type event = Heartbeat | P2P_input of p2p_input | App_input of app_input

  (** The worker's state is made of its status, the gossipsub automaton's state,
      and a stream of events to process. It also has two output streams to
      communicate with the application and P2P layers. *)
  type t = {
    gossip_state : GS.state;
    status : worker_status;
    events_stream : event Stream.t;
    p2p_output_stream : p2p_output Stream.t;
    app_output_stream : app_output Stream.t;
  }

  let message_is_from_network publish = Option.is_some publish.GS.sender

  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5327

     - Also send the full message to direct peers: filter those that have it (we
     received IHave from them)

     - Should we send IHave to some metadata only connections? If yes, filter
     those that have it (we received IHave from them)

     - For the peers in `GS.Publish_message peers`, we assume that the set only
     contains the peers in the mesh that don't have the message (we didn't
     receive IHave from them). *)
  let handle_full_message ~emit_p2p_msg ~emit_app_msg publish = function
    | gstate, GS.Publish_message peers ->
        let {GS.sender = _; topic; message_id; message} = publish in
        let full_message = {message; topic; message_id} in
        let p2p_message = Publish full_message in
        GS.Peer.Set.iter
          (fun to_peer -> emit_p2p_msg @@ Out_message {to_peer; p2p_message})
          peers ;
        let has_joined = GS.Introspection.(has_joined topic @@ view gstate) in
        if message_is_from_network publish && has_joined then
          emit_app_msg full_message ;
        gstate
    | gstate, GS.Already_published -> gstate

  (** This is the main function of the worker. It interacts with the Gossipsub
      automaton given an event. The function possibly sends messages to the P2P
      and application layers via the functions [emit_p2p_msg] and [emit_app_msg]
      and returns the new automaton's state. *)
  let apply_event ~emit_p2p_msg ~emit_app_msg gossip_state = function
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5326

       Notify the GS worker about the status of messages sent to peers. *)
    | Heartbeat ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5170

           Do we want to detect cases where two successive [Heartbeat] events
           would be handled (e.g. because the first one is late)? *)
        let gossip_state, output = GS.heartbeat gossip_state in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5159

           Handle Heartbeat's output *)
        ignore output ;
        gossip_state
    | P2P_input (New_connection {peer; direct; outbound}) ->
        let gossip_state, output =
          GS.add_peer {direct; outbound; peer} gossip_state
        in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5160

           Handle New_connection's output *)
        ignore output ;
        gossip_state
    | P2P_input (Disconnection {peer}) ->
        let gossip_state, output = GS.remove_peer {peer} gossip_state in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5161

           Handle disconnection's output *)
        ignore output ;
        gossip_state
    | App_input (Inject_message {message; message_id; topic}) ->
        let publish = {GS.sender = None; topic; message_id; message} in
        GS.publish publish gossip_state
        |> handle_full_message ~emit_p2p_msg ~emit_app_msg publish
    | P2P_input
        (In_message
          {from_peer; p2p_message = Publish {message; topic; message_id}}) ->
        let publish =
          {GS.sender = Some from_peer; topic; message_id; message}
        in
        GS.publish publish gossip_state
        |> handle_full_message ~emit_p2p_msg ~emit_app_msg publish
    | P2P_input (In_message m) ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5164

           Handle received p2p messages. *)
        ignore (In_message m) ;
        assert false
    | App_input (Join topic) ->
        let gossip_state, output = GS.join {topic} gossip_state in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5191

           Handle Join's output. *)
        ignore output ;
        gossip_state
    | App_input (Leave topic) ->
        let gossip_state, output = GS.leave {topic} gossip_state in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5191

           Handle Leave's output. *)
        ignore output ;
        gossip_state

  (** A helper function that pushes events in the state *)
  let push e t = Stream.push e t.events_stream

  let app_input t input = push (App_input input) t

  let p2p_input t input = push (P2P_input input) t

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
    let rev_push stream e = Stream.push e stream in
    let emit_p2p_msg = rev_push t.p2p_output_stream in
    let emit_app_msg = rev_push t.app_output_stream in
    let rec loop t =
      let open Monad in
      let* event = Stream.pop t.events_stream in
      let gossip_state =
        apply_event ~emit_p2p_msg ~emit_app_msg t.gossip_state event
      in
      loop {t with gossip_state}
    in
    loop t

  let start ~heartbeat_span topics t =
    match t.status with
    | Starting ->
        let heartbeat_handle = heartbeat_events_producer ~heartbeat_span t in
        let event_loop_handle = event_loop t in
        let status = Running {heartbeat_handle; event_loop_handle} in
        let t = {t with status} in
        List.iter (fun topic -> app_input t (Join topic)) topics ;
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
      p2p_output_stream = Stream.empty;
      app_output_stream = Stream.empty;
    }
end
