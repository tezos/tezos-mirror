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

(** {2 Unit tests for gossipsub.} *)

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
  | Unknown_topic -> unit
  | _ -> Tezt.Test.fail "Expected output [Unknown_topic]"

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
  let peers = make_peers ~number:(many_peers limits) in
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
    GS.publish
      {sender = None; topic = "topic0"; message_id = 0; message = "message"}
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
  let state, Publish_message peers_to_publish =
    GS.publish {sender = None; topic; message_id; message = publish_data} state
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
  let state, Publish_message peers_to_publish =
    GS.publish {sender = None; topic; message_id; message = publish_data} state
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
    GS.handle_prune {peer; topic; px = Seq.empty; backoff = 0} state
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
  assert_output ~__LOC__ output Unknown_topic ;
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
  let _state, Heartbeat {to_graft; _} = GS.heartbeat state in
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
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5274
     Uncomment once we change mesh size in heartbeat. *)
  (* assert_mesh_size ~__LOC__ ~topic ~expected_size:limits.degree_optimal state ; *)
  (* List.iter *)
  (*   (fun peer -> *)
  (*     assert_mesh_inclusion ~__LOC__ ~topic ~peer ~is_included:true state) *)
  (*   peers_to_graft ; *)
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
  let _state, Heartbeat {to_prune; _} = GS.heartbeat state in
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
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5274
     Uncomment once we change mesh size in heartbeat. *)
  (* assert_mesh_size ~__LOC__ ~topic ~expected_size:limits.degree_optimal state ; *)
  (* List.iter *)
  (*   (fun peer -> *)
  (*     assert_mesh_inclusion ~__LOC__ ~topic ~peer ~is_included:false state) *)
  (*   peers_to_prune ; *)
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
          heartbeat_interval = 1;
        }
      ~parameters
      ~peers
      ~topics:[topic]
      ~to_subscribe:(fun _ -> true)
      ()
  in
  let peers = Array.of_list peers in
  (* Prune peer with backoff of 30 time ticks. *)
  let backoff = 30 in
  let state, _ =
    GS.handle_prune {peer = peers.(0); topic; px = Seq.empty; backoff} state
  in
  (* No graft should be emitted until 32 time ticks pass.
     The additional 2 time ticks is due to the "backoff slack". *)
  let state =
    List.init ~when_negative_length:() (backoff + 1) (fun i -> i + 1)
    |> WithExceptions.Result.get_ok ~loc:__LOC__
    |> List.fold_left
         (fun state i ->
           Time.elapse 1 ;
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
  Time.elapse 1 ;
  let _state, Heartbeat {to_graft; _} = GS.heartbeat state in
  let grafts = Peer.Map.bindings to_graft in
  Check.((List.length grafts = 1) int ~error_msg:"Expected %R, got %L" ~__LOC__) ;
  unit

let register rng limits parameters =
  test_ignore_graft_from_unknown_topic rng limits parameters ;
  test_handle_received_subscriptions rng limits parameters ;
  test_join_adds_peers_to_mesh rng limits parameters ;
  test_join_adds_fanout_to_mesh rng limits parameters ;
  test_publish_without_flood_publishing rng limits parameters ;
  test_fanout rng limits parameters ;
  test_handle_graft_for_joined_topic rng limits parameters ;
  test_handle_graft_for_not_joined_topic rng limits parameters ;
  test_handle_prune_peer_in_mesh rng limits parameters ;
  test_mesh_addition rng limits parameters ;
  test_mesh_subtraction rng limits parameters ;
  test_do_not_graft_within_backoff_period rng limits parameters ;
