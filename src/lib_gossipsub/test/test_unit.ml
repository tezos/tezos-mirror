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

(* Testing
   -------
   Component:  Gossipsub
   Invocation: dune exec test/test_gossipsub.exe -- --file test_unit.ml
   Subject:    Unit tests for gossipsub
*)

open Test_gossipsub_shared
open Gossipsub_intf
open Tezt
open Tezt_core.Base
module Peer = C.Subconfig.Peer
module Topic = C.Subconfig.Topic
module Message_id = C.Subconfig.Message_id
module Message = C.Subconfig.Message

let assert_output ~__LOC__ actual expected =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5079
     Use non-polymorphic compare and print actual/expected output on failure. *)
  if actual = expected then ()
  else Test.fail ~__LOC__ "Assert for output failed."

let assert_subscribed_topics ~__LOC__ ~peer ~expected_topics state =
  let actual_topics =
    GS.Introspection.(get_subscribed_topics peer (view state))
  in
  Check.(
    (actual_topics = expected_topics)
      (list string)
      ~error_msg:"Expected %R, got %L"
      ~__LOC__)

let assert_fanout_size ~__LOC__ ~topic ~expected_size state =
  let view = GS.Introspection.view state in
  let fanout_peers = GS.Introspection.get_fanout_peers topic view in
  Check.(
    (List.length fanout_peers = expected_size)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__)

(* Note: a new message cache state is returned when inspecting it, but this
   function does not return this updated state! *)
let assert_in_message_cache ~__LOC__ message_id ~peer ~expected_message state =
  let view = GS.Introspection.view state in
  match
    GS.Introspection.Message_cache.get_message_for_peer
      peer
      message_id
      view.message_cache
  with
  | None ->
      Test.fail "Expected entry in message cache for message id %d" message_id
  | Some (_message_cache_state, message, _access) ->
      Check.(
        (message = expected_message)
          string
          ~error_msg:"Expected %R, got %L"
          ~__LOC__)

let assert_mesh_inclusion ~__LOC__ ~topic ~peer ~is_included state =
  let view = GS.Introspection.view state in
  let topic_mesh = GS.Introspection.get_peers_in_topic_mesh topic view in
  Check.(
    (List.mem ~equal:Int.equal peer topic_mesh = is_included)
      bool
      ~error_msg:"Expected %R, got %L"
      ~__LOC__)

let assert_mesh_size ~__LOC__ ~topic ~expected_size state =
  let view = GS.Introspection.view state in
  let topic_mesh = GS.Introspection.get_peers_in_topic_mesh topic view in
  Check.(
    (List.length topic_mesh = expected_size)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__)

let assert_iwant_output_success ~__LOC__ output =
  match output with
  | GS.On_iwant_messages_to_route {routed_message_ids} -> routed_message_ids
  | Iwant_from_peer_with_low_score _ ->
      Test.fail ~__LOC__ "Expected IWant to succeed."

let assert_topic_mesh ~__LOC__ ~topic ~expected_peers state =
  let actual_peers =
    GS.Introspection.(get_peers_in_topic_mesh topic (view state))
  in
  Check.(
    (actual_peers = expected_peers)
      (list int)
      ~error_msg:"Expected %R, got %L"
      ~__LOC__)

let assert_peer_score ~__LOC__ ~expected_score peer state =
  let view = GS.Introspection.view state in
  let actual_score =
    GS.Introspection.get_peer_score peer view
    |> GS.Score.Internal_for_tests.to_float
  in
  Check.(
    (actual_score = expected_score)
      float
      ~error_msg:"Expected score %R, got %L"
      ~__LOC__)

let many_peers limits = (4 * limits.degree_optimal) + 1

let make_peers ~number =
  List.init ~when_negative_length:() number (fun i -> i)
  |> WithExceptions.Result.get_ok ~loc:__LOC__

(** [add_and_subscribe_peers topics peers] adds [peers] to the
    gossipsub connections and subscribes each peer to [topics]. *)
let add_and_subscribe_peers (topics : Topic.t list) (peers : Peer.t list)
    ~(to_subscribe : Peer.t * Topic.t -> bool)
    ?(direct : Peer.t -> bool = fun _ -> false)
    ?(outbound : Peer.t -> bool = fun _ -> false) state =
  let subscribe_peer_to_topics peer topics state =
    List.fold_left
      (fun state topic ->
        if not @@ to_subscribe (peer, topic) then state
        else
          let state, output = GS.handle_subscribe {topic; peer} state in
          assert_output ~__LOC__ output Subscribed ;
          state)
      state
      topics
  in
  List.fold_left
    (fun state peer ->
      let state, output =
        GS.add_peer {direct = direct peer; outbound = outbound peer; peer} state
      in
      assert_output ~__LOC__ output Peer_added ;
      subscribe_peer_to_topics peer topics state)
    state
    peers

let init_state ~rng ~limits ~parameters ~peers ~topics
    ?(to_join : Topic.t -> bool = fun _ -> true)
    ?(direct : Peer.t -> bool = fun _ -> false)
    ?(outbound : Peer.t -> bool = fun _ -> false)
    ~(to_subscribe : Peer.t * Topic.t -> bool) () =
  let state = GS.make rng limits parameters in
  (* Add and subscribe the given peers. *)
  let state =
    add_and_subscribe_peers topics peers ~to_subscribe ~direct ~outbound state
  in
  (* Join to the given topics. *)
  let state =
    List.fold_left
      (fun state topic ->
        if to_join topic then
          let state, _output = GS.join {topic} state in
          state
        else state)
      state
      topics
  in
  state

(** Test that grafting an unknown topic is ignored.

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L4367 *)
let test_ignore_graft_from_unknown_topic rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Ignore graft from unknown topic"
    ~tags:["gossipsub"; "graft"]
  @@ fun () ->
  let peers = make_peers ~number:1 in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[]
      ~to_subscribe:(fun _ -> false)
      ()
  in
  let peers = Array.of_list peers in
  let _state, output =
    GS.handle_graft {peer = peers.(0); topic = "unknown_topic"} state
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5079
     Use Tezt.Check to assert output *)
  match output with
  | Unsubscribed_topic -> unit
  | _ ->
      Tezt.Test.fail
        "Expected output [Unsubscribed_topic]; got [%a]"
        GS.pp_output
        output

(** Test that:
    - Subscribing a known peer to a topic adds the topic to their subscriptions.
    - Subscribing an unknown peer to a topic does nothing.
    - Unsubscribing a peer from a topic removes the topic from their subscriptions.
    - Unsubscribing a non-subscribed topic from a peer has no effect.

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L852
*)
let test_handle_received_subscriptions rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Handle received subscriptions"
    ~tags:["gossipsub"; "subscribe"]
  @@ fun () ->
  let topics = ["topic1"; "topic2"; "topic3"; "topic4"] in
  let peers = make_peers ~number:2 in
  let state =
    init_state
      ~peers
      ~rng
      ~limits
      ~parameters
      ~topics
      ~to_subscribe:(fun _ -> false)
      ()
  in
  let peers = Array.of_list peers in

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

  (* Heartbeat to fill the mesh. *)
  let state, _ = GS.heartbeat state in
  (* Both peer 0 and peer 1 should be in the mesh of "topic1". *)
  assert_topic_mesh
    ~__LOC__
    ~topic:"topic1"
    ~expected_peers:[peers.(0); peers.(1)]
    state ;

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
  (* Only peer 1 should be in the mesh of "topic1". *)
  assert_topic_mesh ~__LOC__ ~topic:"topic1" ~expected_peers:[peers.(1)] state ;
  unit

(* The Join function should:
   - Fill up mesh with known gossipsub peers in the topic
   - Returns GRAFT requests for all nodes added to the mesh

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L512
*)
let test_join_adds_peers_to_mesh rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test join adds peers to mesh"
    ~tags:["gossipsub"; "join"]
  @@ fun () ->
  let topics = ["topic0"] in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics
      ~to_subscribe:(fun _ -> true)
      ()
  in
  (* leave, then call join to invoke functionality *)
  let topic = "topic0" in
  let state, _ = GS.leave {topic} state in
  (* re-join - there should be peers associated with the topic *)
  let state, to_graft =
    match GS.join {topic} state with
    | state, Joining_topic {to_graft} -> (state, Peer.Set.elements to_graft)
    | _, _ -> Test.fail ~__LOC__ "Expected Join to succeed"
  in
  (* should have added [degree_optimal] nodes to the mesh *)
  let peers_in_topic =
    GS.Introspection.(get_peers_in_topic_mesh "topic0" (view state))
  in
  Check.(
    (List.length peers_in_topic = limits.degree_optimal)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  (* there should be [degree_optimal] GRAFT messages. *)
  Check.(
    (List.length to_graft = limits.degree_optimal)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  unit

(* The Join function should:
   - Remove peers from fanout[topic]
   - Add any fanout[topic] peers to the mesh
   - Fill up mesh with known gossipsub peers in the topic
   - Returns GRAFT requests for all nodes added to the mesh

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L512
*)
let test_join_adds_fanout_to_mesh rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test join adds fanout to mesh"
    ~tags:["gossipsub"; "join"; "fanout"]
  @@ fun () ->
  let topics = ["topic0"] in
  (* We initialize the state with [degree_optimal / 2] peers
     so the mesh won't be filled with just fanout peers when we call [GS.join]. *)
  let init_peers, additional_peers =
    List.split_n (limits.degree_optimal / 2)
    @@ make_peers ~number:(many_peers limits)
  in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers:init_peers
      ~topics
      ~to_join:(fun _ -> false)
      ~to_subscribe:(fun _ -> true)
      ()
  in
  (* Publish to topic0.
     We did not join the topic so the peers should be added to the fanout map.*)
  let state, _ =
    GS.publish_message
      {topic = "topic0"; message_id = 0; message = "message"}
      state
  in
  (* Check that all [init_peers] have been added to the fanout.  *)
  let fanout_peers =
    GS.Introspection.(get_fanout_peers "topic0" (view state))
  in
  Check.(
    (List.length fanout_peers = limits.degree_optimal / 2)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  (* Add additonal peers *)
  let state =
    add_and_subscribe_peers
      topics
      additional_peers
      state
      ~to_subscribe:(fun _ -> true)
  in
  (* Join to topic0 *)
  let state, to_graft =
    match GS.join {topic = "topic0"} state with
    | state, Joining_topic {to_graft} -> (state, Peer.Set.elements to_graft)
    | _, _ -> Test.fail ~__LOC__ "Expected Join to succeed"
  in
  let peers_in_topic =
    GS.Introspection.(get_peers_in_topic_mesh "topic0" (view state))
  in
  (* All [degree_optimal / 2] fanout peers should have been added to the mesh,
     along with [degree_optimal / 2] more from the pool. *)
  Check.(
    (List.length peers_in_topic = limits.degree_optimal)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  List.iter
    (fun peer ->
      if not @@ List.mem ~equal:Int.equal peer peers_in_topic then
        Test.fail
          "Fanout peer %d should be included in the topic mesh [%a]"
          peer
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
             Format.pp_print_int)
          peers_in_topic
      else ())
    fanout_peers ;
  (* There should be [degree_optimal] additional GRAFT messages. *)
  Check.(
    (List.length to_graft = limits.degree_optimal)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  (* Check that the fanout map has been cleared.  *)
  let fanout_peers =
    GS.Introspection.(get_fanout_peers "topic0" (view state))
  in
  Check.(
    (List.length fanout_peers = 0) int ~error_msg:"Expected %R, got %L" ~__LOC__) ;
  unit

(** Tests that publishing to a subscribed topic:
    - Returns peers to publish to.
    - Inserts message into message cache.

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L629
*)
let test_publish_without_flood_publishing rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test publish without flood publishing"
    ~tags:["gossipsub"; "publish"]
  @@ fun () ->
  let topic = "test_publish" in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_join:(fun _ -> false)
      ~to_subscribe:(fun _ -> true)
      ()
  in
  let publish_data = "some_data" in
  let message_id = 0 in
  (* Publish to a joined topic. *)
  let state, output =
    GS.publish_message {topic; message_id; message = publish_data} state
  in
  let peers_to_publish =
    match output with
    | Already_published -> Test.fail ~__LOC__ "Publish should succeed."
    | Publish_message {to_publish} -> to_publish
  in
  (* Should return [degree_optimal] peers to publish to. *)
  Check.(
    (Peer.Set.cardinal peers_to_publish = limits.degree_optimal)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  (* [message_id] should be added to the message cache. *)
  assert_in_message_cache
    ~__LOC__
    message_id
    ~peer:(Stdlib.List.hd peers)
    ~expected_message:publish_data
    state ;
  unit

(** Tests that publishing to an unsubscribed topic:
    - Populate fanout peers.
    - Return peers to publish to.
    - Inserts message into the message cache.

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L715
*)
let test_fanout rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test fanout"
    ~tags:["gossipsub"; "publish"; "fanout"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_join:(fun _ -> false)
      ~to_subscribe:(fun _ -> true)
      ()
  in
  (* Leave the topic. *)
  let state, _ = GS.leave {topic} state in
  (* Publish to the topic we left. *)
  let publish_data = "some data" in
  let message_id = 0 in
  let state, output =
    GS.publish_message {topic; message_id; message = publish_data} state
  in
  let peers_to_publish =
    match output with
    | Already_published -> Test.fail ~__LOC__ "Publish should succeed."
    | Publish_message {to_publish} -> to_publish
  in
  (* Fanout should contain [degree_optimal] peers. *)
  assert_fanout_size ~__LOC__ ~topic ~expected_size:limits.degree_optimal state ;
  (* Should return [degree_optimal] peers to publish to. *)
  Check.(
    (Peer.Set.cardinal peers_to_publish = limits.degree_optimal)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  (* [message_id] should be added to the message cache. *)
  assert_in_message_cache
    ~__LOC__
    message_id
    ~peer:(Stdlib.List.hd peers)
    ~expected_message:publish_data
    state ;
  unit

(** Tests that receiving a message for a subscribed topic:
    - Returns peers to publish to.
    - Inserts message into message cache. *)
let test_receiving_message rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test receiving message"
    ~tags:["gossipsub"; "receiving_message"]
  @@ fun () ->
  let topic = "test" in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_join:(fun _ -> true)
      ~to_subscribe:(fun _ -> true)
      ()
  in
  let sender = 99 in
  let message = "some_data" in
  let message_id = 0 in
  (* Receive a message for a joined topic. *)
  let state, output =
    GS.handle_receive_message {sender; topic; message_id; message} state
  in
  let peers_to_route =
    match output with
    | Already_received | Not_subscribed | Invalid_message | Unknown_validity ->
        Test.fail ~__LOC__ "Handling of received message should succeed."
    | Route_message {to_route} -> to_route
  in
  (* Should return [degree_optimal] peers to route the message to. *)
  Check.(
    (Peer.Set.cardinal peers_to_route = limits.degree_optimal)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  (* [message_id] should be added to the message cache. *)
  assert_in_message_cache
    ~__LOC__
    message_id
    ~peer:sender
    ~expected_message:message
    state ;
  unit

(** Tests that we do not route the message when receiving a message
    for an unsubscribed topic. *)
let test_receiving_message_for_unsusbcribed_topic rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test receiving message for unsubscribed topic"
    ~tags:["gossipsub"; "receive_message"; "fanout"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_join:(fun _ -> false)
      ~to_subscribe:(fun _ -> true)
      ()
  in
  (* Leave the topic. *)
  let state, _ = GS.leave {topic} state in
  (* Receive message for the topic we left. *)
  let sender = Stdlib.List.hd peers in
  let message = "some data" in
  let message_id = 0 in
  let _state, output =
    GS.handle_receive_message {sender; topic; message_id; message} state
  in
  match output with
  | Already_received | Route_message _ | Invalid_message | Unknown_validity ->
      Test.fail
        ~__LOC__
        "Handling of received message should fail with [Not_subscribed]."
  | Not_subscribed -> unit

(** Tests that a peer is added to our mesh on graft when we are both
    joined/subscribed to the same topic.

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L1250
*)
let test_handle_graft_for_joined_topic rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test handle graft for subscribed topic"
    ~tags:["gossipsub"; "graft"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  let peers = Array.of_list peers in
  (* Prune peer with backoff 0 to be sure that the peer is not in mesh. *)
  let peer = peers.(7) in
  let state, _ =
    GS.handle_prune
      {peer; topic; px = Seq.empty; backoff = Milliseconds.Span.zero}
      state
  in
  assert_mesh_inclusion ~__LOC__ ~peer ~topic state ~is_included:false ;
  (* Graft peer. *)
  let state, _ = GS.handle_graft {peer; topic} state in
  (* Check that the grafted peer is in mesh. *)
  assert_mesh_inclusion ~__LOC__ ~peer ~topic state ~is_included:true ;
  unit

(** Tests that a peer is not added to our mesh on graft when
    we have not joined the topic.

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L1263
*)
let test_handle_graft_for_not_joined_topic rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test handle graft for not joined topic"
    ~tags:["gossipsub"; "graft"]
  @@ fun () ->
  let topic = "topic" in
  let peer_number = many_peers limits in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  (* Add new peer and graft it with an unknown topic. *)
  let new_peer = peer_number + 1 in
  let state =
    add_and_subscribe_peers
      [topic]
      [new_peer]
      ~to_subscribe:(fun _ -> true)
      state
  in
  let state, output =
    GS.handle_graft {peer = new_peer; topic = "not joined topic"} state
  in
  (* Check that the graft did not take effect. *)
  assert_mesh_inclusion ~__LOC__ ~peer:new_peer ~topic state ~is_included:false ;
  assert_output ~__LOC__ output Unsubscribed_topic ;
  unit

(** Tests sending a graft without subscribing to the topic results in susbcribing to the topic. *)
let test_handle_graft_from_unsubscribed_peer rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test handle graft without subscribe"
    ~tags:["gossipsub"; "graft"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:1 in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> false)
      ()
  in
  let peers = Array.of_list peers in
  let peer = peers.(0) in
  (* Check that the peers is not subscribed to the topic before sending graft. *)
  assert_subscribed_topics ~__LOC__ ~peer:peers.(0) ~expected_topics:[] state ;
  (* Send graft. *)
  let state, _output = GS.handle_graft {peer; topic} state in
  (* Check that the peers is now subscribed to the topic. *)
  assert_subscribed_topics
    ~__LOC__
    ~peer:peers.(0)
    ~expected_topics:[topic]
    state ;
  unit

(** Tests that prune removes peer from our mesh.

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L1323
*)
let test_handle_prune_peer_in_mesh rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test prune removes peer from mesh"
    ~tags:["gossipsub"; "prune"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  let peers = Array.of_list peers in
  let peer = peers.(7) in
  (* First graft to be sure that the peer is in the mesh. *)
  let state, _ = GS.handle_graft {peer; topic} state in
  assert_mesh_inclusion ~__LOC__ ~peer ~topic state ~is_included:true ;
  (* Next prune the peer and check if the peer is removed from the mesh. *)
  let state, _ =
    GS.handle_prune
      {peer; topic; px = Seq.empty; backoff = limits.prune_backoff}
      state
  in
  assert_mesh_inclusion ~__LOC__ ~peer ~topic state ~is_included:false ;
  unit

(** Test mesh addition in maintainance heartbeat.

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L1745
*)
let test_mesh_addition rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test mesh addition in maintainance"
    ~tags:["gossipsub"; "heartbeat"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:(limits.degree_optimal + 2) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  assert_mesh_size ~__LOC__ ~topic ~expected_size:limits.degree_optimal state ;
  let peers_in_mesh =
    GS.Introspection.(get_peers_in_topic_mesh topic (view state))
  in
  (* Remove two peers from mesh via prune. *)
  let state =
    List.take_n 2 peers_in_mesh
    |> List.fold_left
         (fun state peer ->
           let state, _ =
             GS.handle_prune
               {peer; topic; px = Seq.empty; backoff = limits.prune_backoff}
               state
           in
           state)
         state
  in
  assert_mesh_size
    ~__LOC__
    ~topic
    ~expected_size:(limits.degree_optimal - 2)
    state ;
  (* Heartbeat. *)
  let state, Heartbeat {to_graft; _} = GS.heartbeat state in
  (* There should be two grafting requests to fill the mesh. *)
  let peers_to_graft =
    to_graft |> Peer.Map.bindings |> List.map (fun (peer, _topic) -> peer)
  in
  Check.(
    (List.length peers_to_graft = 2)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  (* Mesh size should be [degree_optimal] and the newly grafted peers should be in the mesh.  *)
  assert_mesh_size ~__LOC__ ~topic ~expected_size:limits.degree_optimal state ;
  List.iter
    (fun peer ->
      assert_mesh_inclusion ~__LOC__ ~topic ~peer ~is_included:true state)
    peers_to_graft ;
  unit

(** Test mesh subtraction in maintainance heartbeat.

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L1780
*)
let test_mesh_subtraction rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test mesh subtraction in maintainance"
    ~tags:["gossipsub"; "heartbeat"]
  @@ fun () ->
  let topic = "topic" in
  let peer_number = limits.degree_high + 10 in
  let peers = make_peers ~number:peer_number in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ~outbound:(fun _ -> true)
      ()
  in
  (* Graft all the peers. This works because the connections are outbound. *)
  let state =
    List.fold_left
      (fun state peer ->
        let state, _ = GS.handle_graft {peer; topic} state in
        state)
      state
      peers
  in
  assert_mesh_size ~__LOC__ ~topic ~expected_size:peer_number state ;
  (* Heartbeat. *)
  let state, Heartbeat {to_prune; _} = GS.heartbeat state in
  (* There should be enough prune requests to bring back the mesh size to [degree_optimal]. *)
  let peers_to_prune =
    to_prune |> Peer.Map.bindings |> List.map (fun (peer, _topic) -> peer)
  in
  Check.(
    (List.length peers_to_prune = peer_number - limits.degree_optimal)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  (* Mesh size should be [degree_optimal] and the pruned peers should not be in the mesh.  *)
  assert_mesh_size ~__LOC__ ~topic ~expected_size:limits.degree_optimal state ;
  List.iter
    (fun peer ->
      assert_mesh_inclusion ~__LOC__ ~topic ~peer ~is_included:false state)
    peers_to_prune ;
  unit

(** Tests that the heartbeat does not graft peers that are waiting the backoff period.

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L1943
*)
let test_do_not_graft_within_backoff_period rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Do not graft within backoff period"
    ~tags:["gossipsub"; "heartbeat"; "graft"; "prune"]
  @@ fun () ->
  let topic = "topic" in
  (* Only one peer => mesh too small and will try to regraft as early as possible *)
  let peers = make_peers ~number:1 in
  let state =
    init_state
      ~rng
      ~limits:
        {
          limits with
          (* Run backoff clearing on every heartbeat tick. *)
          backoff_cleanup_ticks = 1;
          (* We will run the heartbeat tick on each time tick to simplify the test. *)
          heartbeat_interval = Milliseconds.Span.of_int_s 1;
        }
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  let peers = Array.of_list peers in
  (* Prune peer with backoff of 30 time ticks. *)
  let backoff = Milliseconds.of_int_s 30 in
  let state, _ =
    GS.handle_prune {peer = peers.(0); topic; px = Seq.empty; backoff} state
  in
  (* No graft should be emitted until 32 time ticks pass.
     The additional 2 time ticks is due to the "backoff slack". *)
  let state =
    List.init
      ~when_negative_length:()
      (Milliseconds.to_int_s backoff + 1)
      (fun i -> i + 1)
    |> WithExceptions.Result.get_ok ~loc:__LOC__
    |> List.fold_left
         (fun state i ->
           Time.elapse @@ Milliseconds.of_int_s 1 ;
           Log.info "%d time tick(s) elapsed..." i ;
           let state, Heartbeat {to_graft; _} = GS.heartbeat state in
           let grafts = Peer.Map.bindings to_graft in
           Check.(
             (List.length grafts = 0)
               int
               ~error_msg:"Expected %R, got %L"
               ~__LOC__) ;
           state)
         state
  in
  (* After elapsing one more second,
     the backoff should be cleared and the graft should be emitted. *)
  Time.elapse @@ Milliseconds.of_int_s 1 ;
  let _state, Heartbeat {to_graft; _} = GS.heartbeat state in
  let grafts = Peer.Map.bindings to_graft in
  Check.((List.length grafts = 1) int ~error_msg:"Expected %R, got %L" ~__LOC__) ;
  unit

(* Tests that the node leaving a topic introduces a backoff period,
   and that the heartbeat respects the introduced backoff.

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L2041
*)
let test_unsubscribe_backoff rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Unsubscribe backoff"
    ~tags:["gossipsub"; "heartbeat"; "join"; "leave"]
  @@ fun () ->
  let topic = "topic" in
  let per_topic_score_limits =
    GS.Score.Internal_for_tests.get_topic_params limits.score_limits topic
  in
  let mesh_message_deliveries_activation =
    per_topic_score_limits.mesh_message_deliveries_activation
  in
  (* Only one peer => mesh too small and will try to regraft as early as possible *)
  let peers = make_peers ~number:1 in
  (* Number of ticks until the unsubscribe backoff expires. *)
  let backoff_ticks = 5 in
  (* We must set [heartbeat_interval] so that
     number_of_heartbeats_in_the_test * [heartbeat_interval] < [mesh_message_deliveries_activation]
     holds. This prevents surpassing [mesh_message_deliveries_activation] within the test,
     thus avoiding the activation of p3 penalty. Since number_of_heartbeats_in_the_test
     is [backoff_ticks + 2], we set [heartbeat_interval] as the following.*)
  let heartbeat_interval =
    (Milliseconds.to_int_ms mesh_message_deliveries_activation - 1)
    / (backoff_ticks + 2)
    |> Milliseconds.of_int_ms
  in
  (* Time required until unsubscribe backoff expires. *)
  let unsubscribe_backoff =
    Milliseconds.(of_int_ms @@ (backoff_ticks * to_int_ms heartbeat_interval))
  in
  let state =
    init_state
      ~rng
      ~limits:
        {
          limits with
          heartbeat_interval;
          (* Run backoff clearing on every heartbeat tick. *)
          backoff_cleanup_ticks = 1;
          unsubscribe_backoff;
        }
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  (* Peer unsubscribes then subscribes from topic. *)
  let state, _ = GS.leave {topic} state in
  let state, _ = GS.join {topic} state in
  (* No graft should be emitted until [(backoff_ticks + 2) * heartbeat_interval] elapse.
     The additional 2 [heartbeat_interval] is due to the "backoff slack". *)
  let state =
    List.init ~when_negative_length:() (backoff_ticks + 1) (fun i -> i + 1)
    |> WithExceptions.Result.get_ok ~loc:__LOC__
    |> List.fold_left
         (fun state i ->
           Time.elapse @@ heartbeat_interval ;
           Log.info "%d time tick(s) elapsed..." i ;
           let state, Heartbeat {to_graft; _} = GS.heartbeat state in
           let grafts = Peer.Map.bindings to_graft in
           Check.(
             (List.length grafts = 0)
               int
               ~error_msg:"Expected %R, got %L"
               ~__LOC__) ;
           state)
         state
  in
  (* After elapsing one more [heartbeat_interval],
     the backoff should be cleared and the graft should be emitted. *)
  Time.elapse @@ heartbeat_interval ;
  let _state, Heartbeat {to_graft; _} = GS.heartbeat state in
  let grafts = Peer.Map.bindings to_graft in
  Check.((List.length grafts = 1) int ~error_msg:"Expected %R, got %L" ~__LOC__) ;
  unit

(* Tests that only grafts for outbound peers are accepted when the mesh is full.

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L2254
*)
let test_accept_only_outbound_peer_grafts_when_mesh_full rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Accept only outbound peer grafts when mesh full"
    ~tags:["gossipsub"; "graft"; "outbound"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:limits.degree_high in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  (* Graft all the peers. This should fill the mesh. *)
  let state =
    List.fold_left
      (fun state peer ->
        let state, _ = GS.handle_graft {peer; topic} state in
        state)
      state
      peers
  in
  (* Assert that the mesh is full. *)
  assert_mesh_size ~__LOC__ ~topic ~expected_size:limits.degree_high state ;
  (* Add an outbound peer and an inbound peer. *)
  let inbound_peer = 99 in
  let outbound_peer = 98 in
  let state, _ =
    GS.add_peer {direct = false; outbound = false; peer = inbound_peer} state
  in
  let state, _ =
    GS.add_peer {direct = false; outbound = true; peer = outbound_peer} state
  in
  (* Send grafts. *)
  let state, _ = GS.handle_graft {peer = inbound_peer; topic} state in
  let state, _ = GS.handle_graft {peer = outbound_peer; topic} state in
  (* Assert that only the outbound has been added to the mesh *)
  assert_mesh_inclusion
    ~__LOC__
    ~topic
    ~peer:inbound_peer
    ~is_included:false
    state ;
  assert_mesh_inclusion
    ~__LOC__
    ~topic
    ~peer:outbound_peer
    ~is_included:true
    state ;
  unit

(* Tests that the number of kept outbound peers is at least [degree_out]
   when removing peers from mesh in heartbeat.

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L2291
*)
let test_do_not_remove_too_many_outbound_peers rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Do not remove too many outbound peers"
    ~tags:["gossipsub"; "heartbeat"; "outbound"]
  @@ fun () ->
  let topic = "topic" in
  (* Create [degree_high] inbound peers and [degree_out] outbound peers. *)
  let inbound_peers, outbound_peers =
    make_peers ~number:(limits.degree_high + limits.degree_out)
    |> List.split_n limits.degree_high
  in
  (* Initiate the state with inbound peers. *)
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers:inbound_peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ~outbound:(fun _ -> false)
      ()
  in
  (* Graft all the inbound peers.
     This works because the number of inbound peers is equal to [degree_high]. *)
  let state =
    List.fold_left
      (fun state peer ->
        let state, _ = GS.handle_graft {peer; topic} state in
        state)
      state
      inbound_peers
  in
  (* Connect to all [degree_out] outbound peers. The grafts will be accepted since
     outbound connections are accepted even when the mesh is full. *)
  let state =
    add_and_subscribe_peers
      [topic]
      outbound_peers
      ~to_subscribe:(fun _ -> true)
      ~outbound:(fun _ -> true)
      state
  in
  let state =
    List.fold_left
      (fun state peer ->
        let state, _ = GS.handle_graft {peer; topic} state in
        state)
      state
      outbound_peers
  in
  (* At this point the mesh should be overly full.
     It has [degree_high + degree_out] peers where the upper limit is [degree_high]. *)
  assert_mesh_size
    ~__LOC__
    ~topic
    ~expected_size:(limits.degree_high + limits.degree_out)
    state ;
  (* Run heartbeat. *)
  let _state, Heartbeat {to_prune; _} = GS.heartbeat state in
  (* There should be enough prune requests to bring back the mesh size to [degree_optimal]. *)
  let peers_to_prune =
    to_prune |> Peer.Map.bindings |> List.map (fun (peer, _topics) -> peer)
  in
  Check.(
    (List.length peers_to_prune
    = limits.degree_high + limits.degree_out - limits.degree_optimal)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  (* No outbound peer should have been pruned since pruning any of them would
     bring the number of outbound peers to below [degree_out]. *)
  List.iter
    (fun peer ->
      (* Outbound peer should continue to be in mesh. *)
      assert_mesh_inclusion ~__LOC__ ~topic ~peer state ~is_included:true ;
      (* Should be no prune request for the outbound peer.  *)
      if List.mem ~equal:Peer.equal peer peers_to_prune then
        Test.fail ~__LOC__ "Outbound peer should not be pruned."
      else ())
    outbound_peers ;
  unit

(* Tests that outbound peers are added to the mesh
   if the number of outbound peers is below [degree_out].

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L2338
*)
let test_add_outbound_peers_if_min_is_not_satisfied rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Add outbound peers if min is not satisfied"
    ~tags:["gossipsub"; "heartbeat"; "outbound"]
  @@ fun () ->
  let topic = "topic" in
  let inbound_peers, outbound_peers =
    make_peers ~number:(limits.degree_high + limits.degree_out)
    |> List.split_n limits.degree_high
  in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers:inbound_peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ~outbound:(fun _ -> false)
      ()
  in
  (* Graft all the inbound peers.
     This works because the number of inbound peers is equal to [degree_high]. *)
  let state =
    List.fold_left
      (fun state peer ->
        let state, _ = GS.handle_graft {peer; topic} state in
        state)
      state
      inbound_peers
  in
  (* Create [degree_out] outbound connections without grafting. *)
  let state =
    add_and_subscribe_peers
      [topic]
      outbound_peers
      ~to_subscribe:(fun _ -> true)
      ~outbound:(fun _ -> true)
      state
  in
  (* At this point the mesh is filled with [degree_high] inbound peers. *)
  assert_mesh_size ~__LOC__ ~topic ~expected_size:limits.degree_high state ;
  (* Heartbeat. *)
  let state, Heartbeat {to_prune; to_graft; _} = GS.heartbeat state in
  (* The outbound peers should have been additionally added. *)
  assert_mesh_size
    ~__LOC__
    ~topic
    ~expected_size:(limits.degree_high + limits.degree_out)
    state ;
  let peers_to_prune =
    to_prune |> Peer.Map.bindings |> List.map (fun (peer, _topics) -> peer)
  in
  let peers_to_graft =
    to_graft |> Peer.Map.bindings |> List.map (fun (peer, _topics) -> peer)
  in
  Check.(
    (List.length peers_to_prune = 0)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  Check.(
    (List.length peers_to_graft = limits.degree_out)
      int
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  unit

(* Tests that the correct message is returned when a peer asks for a message in our cache.

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L1025
*)
let test_handle_iwant_msg_cached rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test handle IWant for message in cache"
    ~tags:["gossipsub"; "iwant"; "cache"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  let peers = Array.of_list peers in
  let peer = peers.(7) in
  let message = "some message" in
  let message_id = 3 in
  (* Place message in cache by publishing. *)
  let state, _ = GS.publish_message {topic; message; message_id} state in
  (* Send IWant. *)
  let _state, output =
    GS.handle_iwant {peer; message_ids = [message_id]} state
  in
  let routed_message_ids = assert_iwant_output_success ~__LOC__ output in
  (* IWant should return the message in cache. *)
  match Message_id.Map.find message_id routed_message_ids with
  | None | Some `Ignored | Some `Not_found | Some `Too_many_requests ->
      Test.fail ~__LOC__ "Expected IWant to return the message in cache."
  | Some (`Message msg) ->
      Check.((msg = message) string ~error_msg:"Expected %R, got %L" ~__LOC__) ;
      unit

(* Tests that in IWant stops returning message after
   [history_length] heartbeats as it is shifted out from the cache.

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L1081
*)
let test_handle_iwant_msg_cached_shifted rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test handle IWant message cache shifted"
    ~tags:["gossipsub"; "iwant"; "cache"; "heartbeat"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits:{limits with max_gossip_retransmission = 100}
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  let peers = Array.of_list peers in
  let peer = peers.(7) in
  let message = "some message" in
  let message_id = 3 in
  (* Place message in cache by publishing. *)
  let state, _ = GS.publish_message {topic; message; message_id} state in
  (* Loop [2 * limits.history_length] times and check that IWant starts returning
     `Not_found after [history_length] heartbeat ticks. *)
  let _state =
    List.init ~when_negative_length:() (2 * limits.history_length) (fun i ->
        i + 1)
    |> WithExceptions.Result.get_ok ~loc:__LOC__
    |> List.fold_left
         (fun state heartbeat_count ->
           (* Heartbeat. *)
           let state, _output = GS.heartbeat state in
           (* Send IWant. *)
           let state, output =
             GS.handle_iwant {peer; message_ids = [message_id]} state
           in
           let routed_message_ids =
             assert_iwant_output_success ~__LOC__ output
           in
           match Message_id.Map.find message_id routed_message_ids with
           | None | Some `Ignored | Some `Too_many_requests ->
               Test.fail ~__LOC__ "Expected `Message or `Not_found."
           | Some (`Message _msg) ->
               if heartbeat_count < limits.history_length then
                 (* The expected case *)
                 state
               else
                 Test.fail
                   ~__LOC__
                   "Expected IWant to not return the message in cache at \
                    heartbeat count %d."
                   heartbeat_count
           | Some `Not_found ->
               if heartbeat_count >= limits.history_length then
                 (* The expected case *)
                 state
               else
                 Test.fail
                   ~__LOC__
                   "Expected IWant to not return `Not_found at heartbeat count \
                    %d."
                   heartbeat_count)
         state
  in
  unit

(* Tests that we do not return a message when a peers asks for a message not in our cache.

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L1146
*)
let test_handle_iwant_msg_not_cached rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Test handle IWant for message not in cache"
    ~tags:["gossipsub"; "iwant"; "cache"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  let peers = Array.of_list peers in
  let peer = peers.(7) in
  (* Some random message id. *)
  let message_id = 99 in
  (* Send IWant. *)
  let _state, output =
    GS.handle_iwant {peer; message_ids = [message_id]} state
  in
  let routed_message_ids = assert_iwant_output_success ~__LOC__ output in
  (* IWant should return `Not_found as the message is not in cache. *)
  match Message_id.Map.find message_id routed_message_ids with
  | None | Some `Ignored | Some (`Message _) | Some `Too_many_requests ->
      Test.fail ~__LOC__ "Expected IWant to return `Not_found."
  | Some `Not_found -> unit

(* Tests that receiving too many IWants from the same peer for the same message
   results in ignoring the request.

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L4387
*)
let test_ignore_too_many_iwants_from_same_peer_for_same_message rng limits
    parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Ignore too many IWants from same peer for same message"
    ~tags:["gossipsub"; "iwant"; "cache"]
  @@ fun () ->
  let topic = "topic" in
  (* Create state with an empty mesh. *)
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers:[]
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  (* Add a peer that is not in the mesh. *)
  let peer = 99 in
  let state =
    add_and_subscribe_peers [topic] [peer] ~to_subscribe:(fun _ -> true) state
  in
  (* Add message to cache by publishing. *)
  let message = "some message" in
  let message_id = 0 in
  let state, _ = GS.publish_message {topic; message; message_id} state in
  (* Send IWant from same peer for same message [(2 * limits.max_gossip_retransmission) + 10] times.
     Only the first [max_gossip_retransmission] IWant requests should be accepted. *)
  let _state =
    List.init
      ~when_negative_length:()
      ((2 * limits.max_gossip_retransmission) + 10)
      (fun i -> i + 1)
    |> WithExceptions.Result.get_ok ~loc:__LOC__
    |> List.fold_left
         (fun state iwant_count ->
           let state, output =
             GS.handle_iwant {peer; message_ids = [message_id]} state
           in
           let routed_message_ids =
             assert_iwant_output_success ~__LOC__ output
           in
           match Message_id.Map.find message_id routed_message_ids with
           | None | Some `Not_found | Some `Ignored ->
               Test.fail
                 ~__LOC__
                 "IWant should be either accepted or ignored due to too many \
                  requests."
           | Some (`Message _) ->
               if iwant_count <= limits.max_gossip_retransmission then
                 (* The expected case. *)
                 state
               else
                 Test.fail
                   ~__LOC__
                   "IWant should be ignored at iwant count of %d."
                   iwant_count
           | Some `Too_many_requests ->
               if iwant_count > limits.max_gossip_retransmission then
                 (* The expected case. *)
                 state
               else
                 Test.fail
                   ~__LOC__
                   "IWant should be accepted at iwant count of %d."
                   iwant_count)
         state
  in
  unit

(* Tests that handling an IHave message for a subscribed topic that has not been seen
   results in requesting the message.

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L1165
*)
let test_handle_ihave_subscribed_and_msg_not_seen rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Handle IHave for subscribed and not seen."
    ~tags:["gossipsub"; "ihave"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  let peers = Array.of_list peers in
  let peer = peers.(7) in
  (* Some unknown message id. *)
  let message_id = 99 in
  let _state, output =
    GS.handle_ihave {peer; topic; message_ids = [message_id]} state
  in
  (* IHave should request the message id. *)
  match output with
  | Message_requested_message_ids message_ids ->
      Check.(
        (message_ids = [message_id])
          (list int)
          ~error_msg:"Expected %R, got %L"
          ~__LOC__) ;
      unit
  | Ihave_from_peer_with_low_score _ | Too_many_recv_ihave_messages _
  | Too_many_sent_iwant_messages _ | Message_topic_not_tracked ->
      Test.fail ~__LOC__ "Expected to request message."

(* Tests that handling an IHave message for a subscribed topic that has been seen
   does not result in requesting the message.

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L1197
*)
let test_handle_ihave_subscribed_and_msg_seen rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Handle IHave for subscribed and seen."
    ~tags:["gossipsub"; "ihave"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  let peers = Array.of_list peers in
  let peer = peers.(7) in
  (* Publish to mark message as seen. *)
  let message = "some message" in
  let message_id = 0 in
  let state, _ = GS.publish_message {topic; message_id; message} state in
  (* Handle IHave for the seen message. *)
  let _state, output =
    GS.handle_ihave {peer; topic; message_ids = [message_id]} state
  in
  (* IHave should not request any messages. *)
  match output with
  | Ihave_from_peer_with_low_score _ | Too_many_recv_ihave_messages _
  | Too_many_sent_iwant_messages _ | Message_topic_not_tracked ->
      Test.fail ~__LOC__ "Expected Message_requested_message_ids."
  | Message_requested_message_ids message_ids ->
      Check.(
        (message_ids = []) (list int) ~error_msg:"Expected %R, got %L" ~__LOC__) ;
      unit

(* Tests that handling an IHave message for an unsubscribed topic is ignored.

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L1219
*)
let test_handle_ihave_not_subscribed rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Handle IHave for unsubscribed."
    ~tags:["gossipsub"; "ihave"]
  @@ fun () ->
  let peers = make_peers ~number:(many_peers limits) in
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  let peers = Array.of_list peers in
  let peer = peers.(7) in
  let topic = "some random topic" in
  let message_id = 0 in
  (* Handle IHave for an unsubscribed topic. *)
  let _state, output =
    GS.handle_ihave {peer; topic; message_ids = [message_id]} state
  in
  (* IHave should result in [Message_topic_not_tracked]. *)
  match output with
  | Ihave_from_peer_with_low_score _ | Too_many_recv_ihave_messages _
  | Too_many_sent_iwant_messages _ | Message_requested_message_ids _ ->
      Test.fail ~__LOC__ "Expected Message_requested_message_ids."
  | Message_topic_not_tracked -> unit

(* Tests that we start ignoring IHaves after receiving more than
   [max_recv_ihave_per_heartbeat] IHaves per heartbeat.

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L4439
*)
let test_ignore_too_many_ihaves rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Ignore too many IHaves."
    ~tags:["gossipsub"; "ihave"]
  @@ fun () ->
  let topic = "topic" in
  let peers = make_peers ~number:(many_peers limits) in
  let max_recv_ihave_per_heartbeat = 10 in
  let state =
    init_state
      ~rng
      ~limits:{limits with max_recv_ihave_per_heartbeat}
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  (* Add a peer that is not in the mesh. *)
  let peer = 99 in
  let state =
    add_and_subscribe_peers [topic] [peer] ~to_subscribe:(fun _ -> true) state
  in
  (* [2 * max_recv_ihave_per_heartbeat] message ids. *)
  let message_ids =
    List.init
      ~when_negative_length:()
      ((2 * max_recv_ihave_per_heartbeat) + 1)
      (fun i -> i)
    |> WithExceptions.Result.get_ok ~loc:__LOC__
  in
  (* Peers sends us [2 * max_recv_ihave_per_heartbeat + 1] IHaves.
     The IHave should start being ignored with [Too_many_recv_ihave_messages]
     after [max_recv_ihave_per_heartbeat] *)
  let state =
    message_ids
    |> List.fold_left_i
         (fun i state message_id ->
           let state, output =
             GS.handle_ihave {peer; topic; message_ids = [message_id]} state
           in
           let ihave_count = i + 1 in
           match output with
           | Ihave_from_peer_with_low_score _ | Too_many_sent_iwant_messages _
           | Message_topic_not_tracked ->
               Test.fail
                 ~__LOC__
                 "Expected [Too_many_recv_ihave_messages] or \
                  [Message_requested_message_ids]."
           | Message_requested_message_ids _ ->
               if ihave_count <= max_recv_ihave_per_heartbeat then
                 (* Expected case. *)
                 state
               else
                 Test.fail
                   ~__LOC__
                   "Expected [Too_many_recv_ihave_messages] at IHave count %d."
                   ihave_count
           | Too_many_recv_ihave_messages _ ->
               if ihave_count > max_recv_ihave_per_heartbeat then
                 (* Expected case. *)
                 state
               else
                 Test.fail
                   ~__LOC__
                   "Expected [Message_requested_message_ids] at IHave count %d."
                   ihave_count)
         state
  in
  (* After heartbeat the IHave count should have been reset. *)
  let state, _ = GS.heartbeat state in
  (* Take [max_recv_ihave_per_heartbeat] message_ids from the second half of the [message_ids]. *)
  let second_half_ids =
    List.split_n max_recv_ihave_per_heartbeat message_ids
    |> fun (_, second_half_ids) ->
    List.take_n max_recv_ihave_per_heartbeat second_half_ids
  in
  (* Resend IHaves for the message ids that were previously ignored with [Too_many_recv_ihave_messages].
     All the IHaves should result in requesting the message. *)
  let _state =
    second_half_ids
    |> List.fold_left
         (fun state message_id ->
           let state, output =
             GS.handle_ihave {peer; topic; message_ids = [message_id]} state
           in
           match output with
           | Ihave_from_peer_with_low_score _ | Too_many_sent_iwant_messages _
           | Message_topic_not_tracked | Too_many_recv_ihave_messages _ ->
               Test.fail ~__LOC__ "Expected [Message_requested_message_ids]."
           | Message_requested_message_ids _ ->
               (* Expected case *)
               state)
         state
  in
  unit

(* Tests that only up-to [max_sent_iwant_per_heartbeat] messages are requested per heartbeat per peer.

   Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L4513
*)
let test_ignore_too_many_messages_in_ihave rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Ignore too many messages in IHave."
    ~tags:["gossipsub"; "ihave"]
  @@ fun () ->
  let assert_requested_message_ids output ~check =
    match output with
    | GS.Message_requested_message_ids message_ids ->
        check message_ids ;
        ()
    | Ihave_from_peer_with_low_score _ | Too_many_recv_ihave_messages _
    | Too_many_sent_iwant_messages _ | Message_topic_not_tracked ->
        Test.fail ~__LOC__ "Expected to request messages."
  in
  let check_message_ids_length ~__LOC__ ids ~expected =
    Check.(
      (List.length ids = expected) int ~error_msg:"Expected %R, got %L" ~__LOC__)
  in
  let check_subset_message_ids ~__LOC__ ids1 ids2 =
    (* Check that the elements in [ids1] is a subset of [ids2] *)
    if
      Message_id.Set.subset
        (Message_id.Set.of_list ids1)
        (Message_id.Set.of_list ids2)
    then ()
    else Test.fail ~__LOC__ "Subset check failed."
  in
  let topic = "topic" in
  let peers = make_peers ~number:(many_peers limits) in
  let max_sent_iwant_per_heartbeat = 10 in
  let state =
    init_state
      ~rng
      ~limits:
        {
          limits with
          max_sent_iwant_per_heartbeat;
          (* We set [max_recv_ihave_per_heartbeat] high so the IHaves only get ignored
             due to requesting too many messsages and not due to the number of IHaves. *)
          max_recv_ihave_per_heartbeat = 999;
        }
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  (* Add a peer that is not in the mesh. *)
  let peer = 99 in
  let state =
    add_and_subscribe_peers [topic] [peer] ~to_subscribe:(fun _ -> true) state
  in
  (* 20 message ids. *)
  let message_ids =
    List.init
      ~when_negative_length:()
      (2 * max_sent_iwant_per_heartbeat)
      (fun i -> i)
    |> WithExceptions.Result.get_ok ~loc:__LOC__
  in
  (* Send IHave with message ids 0..7 *)
  let first_8 = List.take_n 8 message_ids in
  let state, output =
    GS.handle_ihave {peer; topic; message_ids = first_8} state
  in
  (* All messages should be requested. *)
  assert_requested_message_ids output ~check:(fun ids ->
      Check.(
        (List.sort Message_id.compare ids = List.sort Message_id.compare first_8)
          (list int)
          ~error_msg:"Expected %R, got %L"
          ~__LOC__)) ;
  (* Send IHave with message ids 0..11 *)
  let first_12 = List.take_n 12 message_ids in
  let state, output =
    GS.handle_ihave {peer; topic; message_ids = first_12} state
  in
  (* Since [8 + 2 >= max_sent_iwant_per_heartbeat], only 2 messages should be requested. *)
  assert_requested_message_ids output ~check:(fun ids ->
      check_message_ids_length ~__LOC__ ids ~expected:2 ;
      check_subset_message_ids ~__LOC__ ids first_12) ;
  (* Send IHave with message ids 0..19 *)
  let _state, output = GS.handle_ihave {peer; topic; message_ids} state in
  let () =
    (* The number of messages requested has already exceeded [max_sent_iwant_per_heartbeat]
       so the IHave should return [Too_many_sent_iwant_messages]. *)
    match output with
    | Too_many_sent_iwant_messages _ ->
        (* Expected case. *)
        ()
    | Message_requested_message_ids _ | Ihave_from_peer_with_low_score _
    | Too_many_recv_ihave_messages _ | Message_topic_not_tracked ->
        Test.fail ~__LOC__ "Expected [Too_many_sent_iwant_messages]."
  in
  (* After heartbeat the count should have been reset. *)
  let state, _ = GS.heartbeat state in
  (* IHave should result in requesting the remaining 10 messages. *)
  let _state, output = GS.handle_ihave {peer; topic; message_ids} state in
  assert_requested_message_ids output ~check:(fun ids ->
      check_message_ids_length ~__LOC__ ids ~expected:10 ;
      check_subset_message_ids ~__LOC__ ids message_ids) ;
  unit

(* Check the following scenario:
   * we joined a topic and we have a unique peer in the mesh
   * the peer has a negative score
   * the heartbeat removes that peer
   * a new peer subscribes to the same topic
   * a new heartbeat adds to the new peer to the mesh
*)
let test_heartbeat_scenario rng limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: simple heartbeat scenario"
    ~tags:["gossipsub"; "heartbeat"]
  @@ fun () ->
  let peer_topics_map_to_list map =
    map |> Peer.Map.bindings
    |> List.map (fun (peer, topics) -> (peer, Topic.Set.elements topics))
  in
  let topic = "topic" in
  let peer = 0 in
  let s = GS.make rng limits parameters in
  let s, output = GS.add_peer {direct = false; outbound = false; peer} s in
  assert_output ~__LOC__ output Peer_added ;
  let s, output = GS.handle_subscribe {topic; peer} s in
  assert_output ~__LOC__ output Subscribed ;
  let s, output = GS.join {topic} s in
  (match output with
  | Joining_topic {to_graft} when Peer.Set.elements to_graft = [peer] -> ()
  | _ -> Test.fail ~__LOC__ "Unexpected Join output.") ;
  assert_mesh_size ~__LOC__ ~topic ~expected_size:1 s ;
  let s, GS.Set_application_score =
    GS.set_application_score ({peer; score = -1.0} : GS.set_application_score) s
  in
  let s, Heartbeat {to_prune; _} = GS.heartbeat s in
  Check.(
    (peer_topics_map_to_list to_prune = [(peer, [topic])])
      (list (tuple2 int (list string)))
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  assert_mesh_size ~__LOC__ ~topic ~expected_size:0 s ;
  let peer = 1 in
  let s, output = GS.add_peer {direct = false; outbound = false; peer} s in
  assert_output ~__LOC__ output Peer_added ;
  let s, output = GS.handle_subscribe {peer; topic} s in
  assert_output ~__LOC__ output Subscribed ;
  let s, Heartbeat {to_graft; _} = GS.heartbeat s in
  Check.(
    (peer_topics_map_to_list to_graft = [(peer, [topic])])
      (list (tuple2 int (list string)))
      ~error_msg:"Expected %R, got %L"
      ~__LOC__) ;
  assert_mesh_size ~__LOC__ ~topic ~expected_size:1 s ;
  unit

(** Test for P1 (Time in Mesh).

    Ported from: https://github.com/libp2p/rust-libp2p/blob/12b785e94ede1e763dd041a107d3a00d5135a213/protocols/gossipsub/src/behaviour/tests.rs#L3166 *)
let test_scoring_p1 rng _limits parameters =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub: Scoring P1"
    ~tags:["gossipsub"; "scoring"; "p1"]
  @@ fun () ->
  let time_in_mesh_quantum = Milliseconds.of_int_ms 50 in
  let time_in_mesh_weight = 2.0 in
  let time_in_mesh_cap = 10.0 in
  let limits =
    Default_limits.default_limits
      ~time_in_mesh_weight
      ~time_in_mesh_quantum
      ~time_in_mesh_cap
      ()
  in
  let peers = make_peers ~number:1 in
  let peer = Stdlib.List.hd peers in
  let topic = "topic" in
  (* Build mesh with one peer. *)
  let state =
    init_state
      ~rng
      ~limits
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  (* sleep for 2 times the mesh_quantum *)
  Time.elapse GS.Span.(mul time_in_mesh_quantum 2) ;
  (* refresh scores *)
  let state, _ = GS.heartbeat state in
  (* score should be 2 * time_in_mesh_weight *)
  assert_peer_score
    ~__LOC__
    ~expected_score:(time_in_mesh_weight *. 2.0)
    peer
    state ;
  (* sleep again for 2 times the mesh_quantum *)
  Time.elapse GS.Span.(mul time_in_mesh_quantum 2) ;
  (* refresh scores *)
  let state, _ = GS.heartbeat state in
  (* score should be 4 * time_in_mesh_weight *)
  assert_peer_score
    ~__LOC__
    ~expected_score:(time_in_mesh_weight *. 4.0)
    peer
    state ;
  (* sleep for enough periods to reach maximum *)
  Time.elapse
    GS.Span.(
      mul time_in_mesh_quantum (Float.to_int @@ (time_in_mesh_cap +. 10.0))) ;
  (* refresh scores *)
  let state, _ = GS.heartbeat state in
  (* score should be exactly time_in_mesh_cap * time_in_mesh_weight *)
  assert_peer_score
    ~__LOC__
    ~expected_score:(time_in_mesh_weight *. time_in_mesh_cap)
    peer
    state ;
  unit

(* TODO: https://gitlab.com/tezos/tezos/-/issues/5293
   Add test the described test scenario *)

let register rng limits parameters =
  test_ignore_graft_from_unknown_topic rng limits parameters ;
  test_handle_received_subscriptions rng limits parameters ;
  test_join_adds_peers_to_mesh rng limits parameters ;
  test_join_adds_fanout_to_mesh rng limits parameters ;
  test_publish_without_flood_publishing rng limits parameters ;
  test_receiving_message_for_unsusbcribed_topic rng limits parameters ;
  test_receiving_message rng limits parameters ;
  test_fanout rng limits parameters ;
  test_handle_graft_for_joined_topic rng limits parameters ;
  test_handle_graft_for_not_joined_topic rng limits parameters ;
  test_handle_graft_from_unsubscribed_peer rng limits parameters ;
  test_handle_prune_peer_in_mesh rng limits parameters ;
  test_mesh_addition rng limits parameters ;
  test_mesh_subtraction rng limits parameters ;
  test_do_not_graft_within_backoff_period rng limits parameters ;
  test_unsubscribe_backoff rng limits parameters ;
  test_accept_only_outbound_peer_grafts_when_mesh_full rng limits parameters ;
  test_do_not_remove_too_many_outbound_peers rng limits parameters ;
  test_add_outbound_peers_if_min_is_not_satisfied rng limits parameters ;
  test_handle_iwant_msg_cached rng limits parameters ;
  test_handle_iwant_msg_cached_shifted rng limits parameters ;
  test_handle_iwant_msg_not_cached rng limits parameters ;
  test_ignore_too_many_iwants_from_same_peer_for_same_message
    rng
    limits
    parameters ;
  test_handle_ihave_subscribed_and_msg_not_seen rng limits parameters ;
  test_handle_ihave_subscribed_and_msg_seen rng limits parameters ;
  test_handle_ihave_not_subscribed rng limits parameters ;
  test_ignore_too_many_ihaves rng limits parameters ;
  test_ignore_too_many_messages_in_ihave rng limits parameters ;
  test_heartbeat_scenario rng limits parameters ;
  test_scoring_p1 rng limits parameters
