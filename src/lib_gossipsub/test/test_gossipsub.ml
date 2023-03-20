(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_gossipsub
open Gossipsub_intf
open Tezt
open Tezt_core.Base
open Test_gossipsub_shared

(* Most of these limits are the default ones used by the Go implementation. *)
let default_limits =
  {
    max_recv_ihave_per_heartbeat = 10;
    max_sent_iwant_per_heartbeat = 5000;
    degree_optimal = 6;
    gossip_publish_threshold = 0.;
    accept_px_threshold = 0.;
    unsubscribe_backoff = 10;
    graft_flood_backoff = -50;
    prune_backoff = 60;
    retain_duration = 10;
    fanout_ttl = 60;
    heartbeat_interval = 1;
    backoff_cleanup_ticks = 15;
    degree_low = 5;
    degree_high = 12;
    degree_score = 4;
    degree_out = 2;
  }

let parameters = {peer_filter = (fun _peer _action -> true)}

(* This is to use a seed with Tezt. *)
let seed =
  match
    Tezt_core.Cli.get
      ~default:None
      (fun x ->
        try int_of_string x |> Option.some |> Option.some
        with _ -> Option.none)
      "seed"
  with
  | None ->
      Random.self_init () ;
      Random.bits ()
  | Some seed -> seed

(** [init_state ~peer_no ~topics ~direct ~outbound] initiates a gossipsub state,
    joins the topics in [topics], and connects to [peer_no] number of peers which
    will be direct/outbound depending on [direct] and [outbound]. *)
let init_state ~(peer_no : int) ~(topics : C.Topic.t list) ~(direct : bool)
    ~(outbound : bool) : GS.state * C.Peer.t array =
  let rng = Random.State.make [|seed|] in
  let state =
    List.fold_left
      (fun state topic ->
        let state, _output = GS.join {topic} state in
        state)
      (GS.make rng default_limits parameters)
      topics
  in
  let peers =
    List.init ~when_negative_length:() peer_no (fun i -> i)
    |> WithExceptions.Result.get_ok ~loc:__LOC__
  in
  let state =
    List.fold_left
      (fun state peer ->
        let state, _output = GS.add_peer {direct; outbound; peer} state in
        state)
      state
      peers
  in
  (state, Array.of_list peers)

let assert_subscribed_topics ~__LOC__ ~peer ~expected_topics state =
  let actual_topics = GS.Internal_for_tests.get_subscribed_topics peer state in
  Check.(
    (actual_topics = expected_topics)
      (list string)
      ~error_msg:"Expected %R, got %L"
      ~__LOC__)

(** Test that grafting an unknown topic is ignored.

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L4367 *)
let test_ignore_graft_from_unknown_topic () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Ignore graft from unknown topic"
    ~tags:["gossipsub"; "graft"]
  @@ fun () ->
  let state, peers =
    init_state ~peer_no:1 ~topics:[] ~direct:false ~outbound:false
  in
  let _state, output =
    GS.handle_graft {peer = peers.(0); topic = "unknown_topic"} state
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5079
     Use Tezt.Check to assert output *)
  match output with
  | Unknown_topic -> unit
  | _ -> Tezt.Test.fail "Expected output [Unknown_topic]"

(** Test that:
    - Subscribing a known peer to a topic adds the topic to their subscriptions.
    - Subscribing an unknown peer to a topic does nothing.
    - Unsubscribing a peer from a topic removes the topic from their subscriptions.
    - Unsubscribing a non-subscribed topic from a peer has no effect.

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L852
*)
let test_handle_received_subscriptions () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Handle received subscriptions"
    ~tags:["gossipsub"; "subscribe"]
  @@ fun () ->
  let topics = ["topic1"; "topic2"; "topic3"; "topic4"] in
  let state, peers =
    init_state ~peer_no:20 ~topics ~direct:false ~outbound:false
  in

  (* The first peer, second peer, and an unknown peer sends
     3 subscriptions and 1 unsubscription *)
  let unknown_peer = 99 in
  let state =
    [peers.(0); peers.(1); unknown_peer]
    |> List.fold_left
         (fun state peer ->
           let state =
             ["topic1"; "topic2"; "topic3"]
             |> List.fold_left
                  (fun state topic ->
                    let state, _ = GS.handle_subscribe {topic; peer} state in
                    state)
                  state
           in
           let state, _ =
             GS.handle_unsubscribe {topic = "topic4"; peer} state
           in
           state)
         state
  in

  (* First and second peer should be subscribed to three topics *)
  assert_subscribed_topics
    ~__LOC__
    ~peer:peers.(0)
    ~expected_topics:["topic1"; "topic2"; "topic3"]
    state ;
  assert_subscribed_topics
    ~__LOC__
    ~peer:peers.(1)
    ~expected_topics:["topic1"; "topic2"; "topic3"]
    state ;
  (* Unknown peer should not be subscribed to any topic *)
  assert_subscribed_topics ~__LOC__ ~peer:unknown_peer ~expected_topics:[] state ;

  (* Peer 0 unsubscribes from the first topic *)
  let state, _ =
    GS.handle_unsubscribe {topic = "topic1"; peer = peers.(0)} state
  in
  (* Peer 0 should be subscribed to two topics *)
  assert_subscribed_topics
    ~__LOC__
    ~peer:peers.(0)
    ~expected_topics:["topic2"; "topic3"]
    state ;
  unit

let () =
  test_ignore_graft_from_unknown_topic () ;
  test_handle_received_subscriptions () ;
  Tezt.Test.run ()
