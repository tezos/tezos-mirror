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

(* Testing
   -------
   Component:  Gossipsub Worker
   Invocation: dune exec test/test_gossipsub.exe -- --file test_integration_worker.ml
   Subject:    Integration tests for gossipsub worker
*)

open Test_gossipsub_shared
open Tezt_core.Base
open Tezt
module Peer = Worker.GS.Peer
module Topic = Worker.GS.Topic
module Message_id = Worker.GS.Message_id
module Message = Worker.GS.Message
module Span = Worker.GS.Span

(* Helpers *)

(** These three types are used by the function {!step} below that feeds the
   Worker and pushes the expected outputs to the queue. *)
type output = Out_p2p of Worker.p2p_output | Out_app of Worker.app_output

type input =
  | Heartbeat
  | In_p2p of Worker.p2p_input
  | In_app of Worker.app_input

(** A testing context aggregates some stuctures used by the function {!step}
    below. *)
type context = {
  worker : Worker.t;
  expected_p2p_output : Worker.p2p_output Queue.t;
  expected_app_output : Worker.app_output Queue.t;
  heartbeat_span : Span.t;
}

(** [step worker expected_p2p_output expected_app_output input expected_outputs]
    provides the given input to the worker (either via p2p_input or via
    app_input) and pushes the [expected_outputs] of the fed events in the
    corresponding queue.

    The goal of the queues is to replicates what the internal output streams of
    the worker are expected to contain. *)
let step context input expected_outputs =
  let* () =
    match input with
    | In_p2p i -> return @@ Worker.p2p_input context.worker i
    | In_app i -> return @@ Worker.app_input context.worker i
    | Heartbeat ->
        Lwt_unix.sleep
          (1.1 *. Milliseconds.Span.to_float_s context.heartbeat_span)
  in
  List.iter (function
      | Out_p2p o -> Queue.push o context.expected_p2p_output
      | Out_app o -> Queue.push o context.expected_app_output)
  @@ List.rev expected_outputs ;
  unit

(** [check_output_stream ~pp expected_outputs stream_content] allows to check if the
    content of the given [expected_outputs] and the [stream_content] match. The function
    fails at the first two elements that are not (structurally) equal. In that
    case, the function [pp] is used to show the content of those values.
    The function also fails if the size of the two structures is different. *)
let check_output_stream ~__LOC__ ~pp expected_outputs stream_content =
  let rec aux expected_outputs stream_content =
    match (stream_content, Queue.take_opt expected_outputs) with
    | [], None -> unit
    | [], Some q_elt ->
        Test.fail
          ~__LOC__
          "The stream is empty, but expected_outputs has at least an extra \
           element (%a) "
          pp
          q_elt
    | s_elt :: _, None ->
        Test.fail
          ~__LOC__
          "expected_outputs is empty, but the stream has at least an extra \
           element (%a) "
          pp
          s_elt
    | s_elt :: stream_content, Some q_elt ->
        if s_elt = q_elt then aux expected_outputs stream_content
        else
          Test.fail
            ~__LOC__
            "Two elements at the same position are not equal:\n\
            \           expected_outputs' element = %a, stream/got element = \
             %a,"
            pp
            q_elt
            pp
            s_elt
  in
  aux expected_outputs stream_content

let check_streams_content =
  let stream_content stream = Worker.Stream.get_available stream in
  fun ~__LOC__ worker expected_p2p_output expected_app_output ->
    let* () =
      check_output_stream ~__LOC__ ~pp:Worker.pp_p2p_output expected_p2p_output
      @@ stream_content
      @@ Worker.p2p_output_stream worker
    in
    check_output_stream ~__LOC__ ~pp:Worker.pp_app_output expected_app_output
    @@ stream_content
    @@ Worker.app_output_stream worker

(* Tests *)

(** This simple test just starts a worker, waits for a heartbeat and then shuts
    it down. *)
let test_worker_start_and_stop rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"GS worker: Start and stop"
    ~tags:["gossipsub"; "worker"; "start"; "stop"]
  @@ fun () ->
  (* 0. Preparing the worker and the step function to feed the worker and log
     the expected outputs. *)
  let expected_p2p_output = Queue.create () in
  let expected_app_output = Queue.create () in
  let worker = Worker.make rng limits parameters in
  let heartbeat_span = limits.heartbeat_interval in
  let worker = Worker.start ["1"; "2"; "3"] worker in
  let context =
    {worker; expected_p2p_output; expected_app_output; heartbeat_span}
  in

  (* 1. Waiting for a Heartbeat event. *)
  let* () = step context Heartbeat [] in

  (* 2. Check that the expected outputs match the actual outputs. *)
  let* () =
    check_streams_content
      ~__LOC__
      worker
      expected_p2p_output
      expected_app_output
  in
  Worker.shutdown worker

(** In this test, we check:
    - The handling of new connections.
    - The handling of subscribe messages.
    - Heartbeat sends appropriate grafts. *)
let test_worker_connect_and_graft rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"GS worker: subscribe and graft"
    ~tags:["gossipsub"; "worker"; "subscribe"; "graft"]
  @@ fun () ->
  (* Some values using below. We have:
     - a topic "1"
     - two peers 33 and 44 *)
  let topic = "1" in
  let (peer as from_peer as to_peer) = 33 in
  let peer' = 44 in

  (* 0. Preparing the worker and the step function to feed the worker and log
     the expected outputs. *)
  let expected_p2p_output = Queue.create () in
  let expected_app_output = Queue.create () in
  let worker = Worker.make rng limits parameters in
  let heartbeat_span = limits.heartbeat_interval in
  (* 1. The worker joins topic "1" at startup. *)
  let worker = Worker.start [topic] worker in
  let context =
    {worker; expected_p2p_output; expected_app_output; heartbeat_span}
  in
  let step = step context in

  (* 2. We connect peer 33. We expect the worker to send him a Subscribe {topic
     = 1}. *)
  let* () =
    Worker.(
      step
        (In_p2p (New_connection {peer; direct = false; outbound = true}))
        [Out_p2p (Out_message {to_peer; p2p_message = Subscribe {topic}})])
  in

  (* 3. We connect peer 44. We expect the worker to send him a Subscribe {topic
     = 1}. *)
  let* () =
    Worker.(
      step
        (In_p2p (New_connection {peer = peer'; direct = false; outbound = true}))
        [
          Out_p2p
            (Out_message {to_peer = peer'; p2p_message = Subscribe {topic}});
        ])
  in

  (* 4. Peer 33 informs our worker that he is also subscribed to {topic = 1}. No
     message is expected from our side. *)
  let* () =
    Worker.(
      step (In_p2p (In_message {from_peer; p2p_message = Subscribe {topic}})) [])
  in

  (* 5. Let's wait for a heartbeat. Our worker should send a Graft {topic} to
     peer 33. *)
  let* () =
    Worker.(
      step
        Heartbeat
        [Out_p2p (Out_message {to_peer; p2p_message = Graft {topic}})])
  in

  (* 6. Check that the expected outputs match the actual outputs. *)
  let* () =
    check_streams_content
      ~__LOC__
      worker
      expected_p2p_output
      expected_app_output
  in
  Worker.shutdown worker

(** In this test, we check:
    - The handling of IHave messages.
    - The routing of full messages. *)
let test_worker_filter_messages_for_app rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"GS worker: filter full messages for the app"
    ~tags:["gossipsub"; "worker"; "publish"]
  @@ fun () ->
  (* Some values using below. We have:
     - two topics "1" and "2"
     - two messages ids 11 and 12
     - two mesages with a different content
     - a peer 33

     Note that, the we defined [get_topic] in {!Test_gossipsub_shared} implies that
     message 11 belongs to topic 1 and message 12 to topic 2.
  *)
  let topic = "1" in
  let topic' = "2" in
  let message_id = 11 in
  let message_id' = 12 in
  let message = "Hello messages!" in
  let message' = "Hello message 2!" in
  let (_peer as from_peer as to_peer) = 33 in

  (* 0. Preparing the worker and the step function to feed the worker and log
     the expected outputs. *)
  let expected_p2p_output = Queue.create () in
  let expected_app_output = Queue.create () in
  let worker = Worker.make rng limits parameters in
  (* 1. The worker joins topic "1" at startup. *)
  let worker = Worker.start [topic] worker in
  let heartbeat_span = limits.heartbeat_interval in
  let context =
    {worker; expected_p2p_output; expected_app_output; heartbeat_span}
  in
  let step = step context in

  (* 2. Peer 33 notifies us that he have message_id for topic. We expect to send
     him a message Iwant [message_id], as we are subscribed to that topic. *)
  let* () =
    Worker.(
      step
        (In_p2p
           (In_message
              {
                from_peer;
                p2p_message = IHave {topic; message_ids = [message_id]};
              }))
        [
          Out_p2p
            (Out_message
               {to_peer; p2p_message = IWant {message_ids = [message_id]}});
        ])
  in

  (* 3. Peer 33 notifies us that he have message_id' for topic'. We don't expect
     to request the message content with Iwant because we are not subscribed to
     that topic. *)
  let* () =
    Worker.(
      step
        (In_p2p
           (In_message
              {
                from_peer;
                p2p_message =
                  IHave {topic = topic'; message_ids = [message_id']};
              }))
        [])
  in

  (* 4. Peer 33 sends us the full message corresponding to message_id. We expect
     to forward it to the application layer, as we joined the message's
     topic. *)
  let full_message = Worker.{topic; message_id; message} in
  let* () =
    Worker.(
      step
        (In_p2p
           (In_message
              {from_peer; p2p_message = Message_with_header full_message}))
        [Out_app full_message])
  in

  (* 5. Peer 33 sends us the full message of message_id' belonging to topic'. We
     don't expect to forward it to the application layer, as we didn't join
     the message's topic. *)
  let full_message' =
    Worker.{topic = topic'; message_id = message_id'; message = message'}
  in
  let* () =
    Worker.(
      step
        (In_p2p
           (In_message
              {from_peer; p2p_message = Message_with_header full_message'}))
        [])
  in

  (* 6. Check that the expected outputs match the actual outputs. *)
  let* () =
    check_streams_content
      ~__LOC__
      worker
      expected_p2p_output
      expected_app_output
  in
  Worker.shutdown worker

let register rng limits parameters =
  test_worker_start_and_stop rng limits parameters ;
  test_worker_connect_and_graft rng limits parameters ;
  test_worker_filter_messages_for_app rng limits parameters
