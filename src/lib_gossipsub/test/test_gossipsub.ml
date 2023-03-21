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

let rng = Random.State.make [|seed|]

(** [init_state ~peer_no ~topics ~direct ~outbound] initiates a gossipsub state,
    joins the topics in [topics], and connects to [peer_no] number of peers which
    will be direct/outbound depending on [direct] and [outbound]. *)
let init_state ~(peer_no : int) ~(topics : C.Topic.t list) ~(direct : bool)
    ~(outbound : bool) : GS.state * C.Peer.t array =
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

module Basic_fragments = struct
  open Gossipsub_pbt_generators
  open Fragment

  let add_then_remove_peer ~gen_peer : t =
    of_input_gen (add_peer ~gen_peer) @@ fun ap ->
    [Add_peer ap; Remove_peer {peer = ap.peer}]

  let join_then_leave_topic ~gen_topic : t =
    of_input_gen (join ~gen_topic) @@ fun jp ->
    [Join jp; Leave {topic = jp.topic}]

  let graft_then_prune ~gen_peer ~gen_topic : t =
    of_input_gen (graft ~gen_peer ~gen_topic) @@ fun g ->
    [
      Graft g;
      Prune {peer = g.peer; topic = g.topic; px = Seq.empty; backoff = 10};
    ]

  let heartbeat : t = of_list [Heartbeat]
end

(** Test that removing a peer really removes it from the state *)
module Test_remove_peer = struct
  open Gossipsub_pbt_generators

  let predicate ~peer_id final_state _final_output =
    (* This predicate checks that [peer_id] does not appear in the [connections]
       field of the final state. *)
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/5190 *)
    let conns = GS.Internal_for_tests.connections final_state in
    if GS.Peer.Map.mem peer_id conns then Error `peer_not_removed_correctly
    else Ok ()

  let scenario =
    let open Fragment in
    let open Basic_fragments in
    let add_then_remove_peer_wait_and_clean ~peer_id =
      (* In order to pure a peer from the connection, we need to
         1. remove it
         2. wait until [retain_duration+slack]
         3. wait until the next round of cleanup in the heartbeat *)
      let expire =
        default_limits.retain_duration + (default_limits.heartbeat_interval * 2)
      in
      let heartbeat_cleanup_ticks = default_limits.backoff_cleanup_ticks in
      add_then_remove_peer ~gen_peer:(M.return peer_id)
      @% repeat expire tick
      @% repeat heartbeat_cleanup_ticks heartbeat
    in
    interleave
      [
        add_then_remove_peer_wait_and_clean ~peer_id:0;
        add_then_remove_peer_wait_and_clean ~peer_id:0;
        add_then_remove_peer_wait_and_clean ~peer_id:1;
        add_then_remove_peer_wait_and_clean ~peer_id:2;
        repeat 10
        @@ join_then_leave_topic ~gen_topic:(M.oneofl ["topicA"; "topicB"]);
        repeat 10 heartbeat;
        repeat 100 tick;
        of_input_gen
          (ihave
             ~gen_peer:(M.oneofl [0; 1; 2; 3])
             ~gen_topic:(M.oneofl ["topicA"; "topicB"; "topicC"])
             ~gen_message_id:(M.oneofl [42; 43; 44])
             ~gen_msg_count:(M.int_range 1 5))
          (fun ihave -> [Ihave ihave])
        |> repeat 5;
        of_input_gen
          (iwant
             ~gen_peer:(M.oneofl [0; 1; 2; 3])
             ~gen_message_id:(M.oneofl [42; 43; 44])
             ~gen_msg_count:(M.int_range 1 5))
          (fun iwant -> [Iwant iwant])
        |> repeat 5;
        graft_then_prune
          ~gen_peer:(M.oneofl [0; 1; 2])
          ~gen_topic:(M.oneofl ["topicC"; "topicD"])
        |> repeat 10;
      ]

  let test () =
    Tezt_core.Test.register
      ~__FILE__
      ~title:"Gossipsub: remove peer"
      ~tags:["gossipsub"; "control"]
    @@ fun () ->
    let state, _ =
      init_state ~peer_no:0 ~topics:[] ~direct:false ~outbound:false
    in
    let test =
      QCheck2.Test.make ~name:"Gossipsub: remove_peer" (run state scenario)
      @@ fun trace ->
      match check_final (predicate ~peer_id:0) trace with
      | Ok () -> true
      | Error e -> (
          match e with
          | `peer_not_removed_correctly ->
              Tezt.Test.fail
                ~__LOC__
                "Peer was not removed correctly. Dumping trace:\n%a"
                (pp_trace ())
                trace)
    in
    QCheck2.Test.check_exn ~rand:rng test ;
    unit
end

let () =
  test_ignore_graft_from_unknown_topic () ;
  test_handle_received_subscriptions () ;
  Test_remove_peer.test () ;
  Tezt.Test.run ()
