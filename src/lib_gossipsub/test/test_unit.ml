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
open Tezt
open Tezt_core.Base

let assert_output ~__LOC__ actual expected =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5079
     Use non-polymorphic compare and print actual/expected output on failure. *)
  if actual = expected then ()
  else Test.fail ~__LOC__ "Assert for output failed."

let assert_subscribed_topics ~__LOC__ ~peer ~expected_topics state =
  let actual_topics = GS.Internal_for_tests.get_subscribed_topics peer state in
  Check.(
    (actual_topics = expected_topics)
      (list string)
      ~error_msg:"Expected %R, got %L"
      ~__LOC__)

let many_peers limits = (4 * limits.Gossipsub_intf.degree_optimal) + 1

let make_peers ~number =
  List.init ~when_negative_length:() number (fun i -> i)
  |> WithExceptions.Result.get_ok ~loc:__LOC__

(** [add_and_subscribe_peers topics peers] adds [peers] to the
    gossipsub connections and subscribes each peer to [topics]. *)
let add_and_subscribe_peers (topics : C.Topic.t list) (peers : C.Peer.t list)
    ~(to_subscribe : C.Peer.t * C.Topic.t -> bool)
    ?(direct : C.Peer.t -> bool = fun _ -> false)
    ?(outbound : C.Peer.t -> bool = fun _ -> false) state =
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
    ?(to_join : C.Topic.t -> bool = fun _ -> true)
    ?(direct : C.Peer.t -> bool = fun _ -> false)
    ?(outbound : C.Peer.t -> bool = fun _ -> false)
    ~(to_subscribe : C.Peer.t * C.Topic.t -> bool) () =
  let state = GS.make rng limits parameters in
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
  (* Add and subscribe the given peers. *)
  let state =
    add_and_subscribe_peers topics peers ~to_subscribe ~direct ~outbound state
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

let register rng limits parameters =
  test_ignore_graft_from_unknown_topic rng limits parameters ;
  test_handle_received_subscriptions rng limits parameters
