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
   Invocation: dune exec test/test_gossipsub.exe -- --file test_pbt.ml
   Subject:    PBT for gossipsub.
*)

open Test_gossipsub_shared
open Gossipsub_intf
open Tezt_core.Base
module Peer = C.Subconfig.Peer
module Topic = C.Subconfig.Topic
module Message_id = C.Subconfig.Message_id
module Message = C.Subconfig.Message

module Basic_fragments = struct
  open Gossipsub_pbt_generators
  open Fragment

  let prune_backoff = Milliseconds.Span.of_int_s 10

  let add_then_remove_peer ~gen_peer : t =
    of_input_gen (add_peer ~gen_peer ~gen_direct:M.bool ~gen_outbound:M.bool)
    @@ fun ap -> [I.add_peer ap; I.remove_peer {peer = ap.peer}]

  let join_then_leave_topic ~gen_topic : t =
    of_input_gen (join ~gen_topic) @@ fun jp ->
    [I.join jp; I.leave {topic = jp.topic}]

  let graft_then_prune ~gen_peer ~gen_topic : t =
    of_input_gen (graft ~gen_peer ~gen_topic) @@ fun g ->
    [
      I.graft g;
      I.prune
        {
          peer = g.peer;
          topic = g.topic;
          px = Seq.empty;
          backoff = prune_backoff;
        };
    ]

  let heartbeat : t = of_list [I.heartbeat]

  let ihave ~gen_peer ~gen_topic ~gen_message_id ~gen_msg_count : t =
    of_input_gen
      (ihave ~gen_peer ~gen_topic ~gen_message_id ~gen_msg_count)
      (fun ihave -> [I.ihave ihave])

  let iwant ~gen_peer ~gen_message_id ~gen_msg_count =
    of_input_gen (iwant ~gen_peer ~gen_message_id ~gen_msg_count) (fun iwant ->
        [I.iwant iwant])

  let receive_message ~gen_peer ~gen_topic ~gen_message_id ~gen_message : t =
    of_input_gen
      (receive_message ~gen_peer ~gen_topic ~gen_message_id ~gen_message)
      (fun receive_message -> [I.receive_message receive_message])

  let publish_message ~gen_topic ~gen_message_id ~gen_message : t =
    of_input_gen
      (publish_message ~gen_topic ~gen_message_id ~gen_message)
      (fun publish_message -> [I.publish_message publish_message])

  (** [add_distinct_peers_and_subscribe ~all_peers ~gen_direct ~gen_outbound ~topics_to_subscribe]
      returns [add_peer] inputs for peers [all_peers] using generators [gen_direct] and [gen_outbound]
      followed by [subscribe] inputs for all topics in [topics_to_subscribe] for all [all_peers]. *)
  let add_distinct_peers_and_subscribe ~all_peers ~gen_direct ~gen_outbound
      ~topics_to_subscribe =
    let many_peer_gens =
      all_peers
      |> List.map (fun peer_id ->
             let open M in
             let+ add_peer =
               add_peer ~gen_peer:(M.return peer_id) ~gen_direct ~gen_outbound
             in
             (peer_id, add_peer))
      |> M.flatten_l
    in
    of_input_gen many_peer_gens @@ fun peers ->
    let peer_ids, add_peers = List.split peers in
    let add_peer_inputs =
      List.map (fun add_peer -> I.add_peer add_peer) add_peers
    in
    let subscribe_inputs =
      peer_ids
      |> List.map (fun peer_id ->
             List.map
               (fun topic -> I.subscribe GS.{topic; peer = peer_id})
               topics_to_subscribe)
      |> List.flatten
    in
    add_peer_inputs @ subscribe_inputs

  (* This creates a list of [count] [add_peer;graft] with distinct peers
     such that exactly [count_outbound] of them are [outbound] peers. *)
  let add_distinct_peers_and_graft count count_outbound topic =
    assert (count_outbound <= count) ;
    let many_peer_gens =
      List.init ~when_negative_length:() count (fun i ->
          (* Setting [direct=false] otherwise the peers won't be grafted. *)
          let gen_direct = M.return false in
          let gen_outbound = M.return (i < count_outbound) in
          add_peer ~gen_peer:(M.return i) ~gen_direct ~gen_outbound)
      |> WithExceptions.Result.get_ok ~loc:__LOC__
      |> M.flatten_l
    in
    of_input_gen many_peer_gens @@ fun peers ->
    List.map (fun ap -> [I.add_peer ap; I.graft {topic; peer = ap.peer}]) peers
    |> List.flatten
end

let dump_trace_and_fail ~__LOC__ prefix limits pp_state msg =
  let open Gossipsub_pbt_generators in
  Tezt.Test.fail
    ~__LOC__
    "Limits:@;%a@;Dumping trace until failure:@;@[<hov>%a@]@;%s@;"
    pp_limits
    limits
    (pp_trace ~pp_output ~pp_state ())
    prefix
    msg

module Test_message_cache = struct
  module L = Message_cache

  module R = struct
    module M = Map.Make (Int)

    type t = {
      mutable ticks : int;
      cache : Message.t Message_id.Map.t Topic.Map.t M.t;
      history_slots : int;
      gossip_slots : int;
    }

    let create ~history_slots ~gossip_slots =
      assert (gossip_slots > 0) ;
      assert (gossip_slots <= history_slots) ;
      {ticks = 0; cache = M.empty; history_slots; gossip_slots}

    let add_message message_id message topic t =
      {
        t with
        cache =
          M.update
            t.ticks
            (function
              | None ->
                  Topic.Map.singleton
                    topic
                    (Message_id.Map.singleton message_id message)
                  |> Option.some
              | Some map ->
                  Topic.Map.update
                    topic
                    (function
                      | None ->
                          Message_id.Map.singleton message_id message
                          |> Option.some
                      | Some topic_map ->
                          Message_id.Map.add message_id message topic_map
                          |> Option.some)
                    map
                  |> Option.some)
            t.cache;
      }

    let get_message_for_peer _peer message_id t =
      let found = ref None in
      for x = max 0 (t.ticks - t.history_slots + 1) to t.ticks do
        match M.find x t.cache with
        | None -> ()
        | Some topic_map ->
            Topic.Map.iter
              (fun _topic map ->
                match Message_id.Map.find message_id map with
                | None -> ()
                | Some message -> found := Some message)
              topic_map
      done ;
      let r = !found in
      Option.map (fun message -> (t, message)) r

    let get_message_ids_to_gossip topic t =
      let found = ref Message_id.Set.empty in
      for x = max 0 (t.ticks - t.gossip_slots + 1) to t.ticks do
        match M.find x t.cache with
        | None -> ()
        | Some topic_map -> (
            match Topic.Map.find topic topic_map with
            | None -> ()
            | Some message_map ->
                let set =
                  message_map |> Message_id.Map.to_seq |> Seq.map fst
                  |> Message_id.Set.of_seq
                in
                found := Message_id.Set.union !found set)
      done ;
      !found

    let shift t =
      t.ticks <- t.ticks + 1 ;
      t
  end

  (* If those numbers are too large, we will miss scenarios with collisions. *)
  let history_slots = QCheck2.Gen.int_range (-1) 10

  let gossip_slots = history_slots

  let message_id = QCheck2.Gen.int_range 0 10

  (* The data-structure assumes that the id identifies uniquely the
     message. To ease the readibility of the test we consider a
     messsage constant. *)
  let message = QCheck2.Gen.return "m"

  let peer = QCheck2.Gen.return 0

  let topic =
    let open QCheck2.Gen in
    let* chars =
      QCheck2.Gen.list_size
        (QCheck2.Gen.int_range 1 2)
        (QCheck2.Gen.char_range 'a' 'c')
    in
    return (String.of_seq (List.to_seq chars))

  type action =
    | Add_message of {message_id : int; message : string; topic : string}
    | Get_message_for_peer of {peer : int; message_id : int}
    | Get_message_ids_to_gossip of {topic : string}
    | Shift

  let pp_action fmt = function
    | Add_message {message_id; message = _; topic} ->
        Format.fprintf fmt "ADD_MESSAGE {id:%d;topic:%s}" message_id topic
    | Get_message_for_peer {peer = _; message_id} ->
        Format.fprintf fmt "GET_MESSAGE {id:%d}" message_id
    | Get_message_ids_to_gossip {topic} ->
        Format.fprintf fmt "GET_FOR_TOPIC {topic:%s}" topic
    | Shift -> Format.fprintf fmt "SHIFT"

  let add_message =
    let open QCheck2.Gen in
    let* message_id in
    let* message in
    let* topic in
    return (Add_message {message_id; message; topic})

  let get_message_for_peer =
    let open QCheck2.Gen in
    let* message_id in
    let* peer in
    return (Get_message_for_peer {peer; message_id})

  let get_message_ids_to_gossip =
    let open QCheck2.Gen in
    let* topic in
    return (Get_message_ids_to_gossip {topic})

  let action =
    QCheck2.Gen.oneof
      [
        add_message;
        get_message_for_peer;
        get_message_ids_to_gossip;
        QCheck2.Gen.return Shift;
      ]

  let actions = QCheck2.Gen.(list_size (int_range 1 30) action)

  let rec run (left, right) actions =
    let remaining_steps = List.length actions in
    match actions with
    | [] -> None
    | Add_message {message_id; message; topic} :: actions ->
        let left = L.add_message message_id message topic left in
        let right = R.add_message message_id message topic right in
        run (left, right) actions
    | Get_message_for_peer {peer; message_id} :: actions -> (
        let left_result = L.get_message_for_peer peer message_id left in
        let right_result = R.get_message_for_peer peer message_id right in
        match (left_result, right_result) with
        | None, None -> run (left, right) actions
        | Some (left, _left_message, _left_counter), Some (right, _right_message)
          ->
            (* By definition of the message generator, messages are equal. *)
            run (left, right) actions
        | None, Some _ ->
            let message = Format.asprintf "Expected: A message. Got: None" in
            Some (remaining_steps, message)
        | Some _, None ->
            let message =
              Format.asprintf "Expected: No message. Got: A message"
            in
            Some (remaining_steps, message))
    | Get_message_ids_to_gossip {topic} :: actions ->
        let left_result = L.get_message_ids_to_gossip topic left in
        let right_result = R.get_message_ids_to_gossip topic right in
        let left_set = Message_id.Set.of_list left_result in
        if Message_id.Set.equal left_set right_result then
          run (left, right) actions
        else
          let pp_set fmt s =
            if Message_id.Set.is_empty s then Format.fprintf fmt "empty set"
            else
              s |> Message_id.Set.to_seq |> List.of_seq
              |> Format.fprintf
                   fmt
                   "%a"
                   (Format.pp_print_list
                      ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
                      Format.pp_print_int)
          in
          let message =
            Format.asprintf
              "Expected: %a@.Got: %a@."
              pp_set
              right_result
              pp_set
              left_set
          in
          Some (remaining_steps, message)
    | Shift :: actions ->
        let left = L.shift left in
        let right = R.shift right in
        run (left, right) actions

  let pp fmt trace =
    Format.fprintf
      fmt
      "%a@."
      (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_action)
      trace

  let test rng =
    Tezt_core.Test.register
      ~__FILE__
      ~title:"Gossipsub: check correction of message cache data structure"
      ~tags:["gossipsub"; "message_cache"]
    @@ fun () ->
    let scenario =
      let open QCheck2.Gen in
      let* history_slots in
      let* gossip_slots in
      let* actions in
      let left =
        try
          L.create ~history_slots ~gossip_slots ~seen_message_slots:10
          |> Either.left
        with exn -> Either.right exn
      in
      let right =
        try R.create ~history_slots ~gossip_slots |> Either.left
        with exn -> Either.right exn
      in
      match (left, right) with
      | Right _, Right _ -> return None
      | Left left, Left right -> (
          match run (left, right) actions with
          | None -> return None
          | Some (remaining_steps, explanation) ->
              let n = List.length actions - remaining_steps + 1 in
              let actions = List.take_n n actions in
              return @@ Some (history_slots, gossip_slots, explanation, actions)
          )
      | Right exn, Left _ ->
          let explanation =
            Format.asprintf
              "Initialisation failed unexpectedly: %s"
              (Printexc.to_string exn)
          in
          return @@ Some (history_slots, gossip_slots, explanation, [])
      | Left _, Right exn ->
          let explanation =
            Format.asprintf
              "Initialisation succeeded while it should not. Expected to fail \
               with: %s"
              (Printexc.to_string exn)
          in
          return @@ Some (history_slots, gossip_slots, explanation, [])
    in
    let test =
      QCheck2.Test.make ~count:500_000 ~name:"Gossipsub: message cache" scenario
      @@ function
      | None -> true
      | Some (history_slots, gossip_slots, explanation, trace) ->
          Tezt.Test.fail
            ~__LOC__
            "@[<v 2>Soundness check failed.@;\
             Limits:@;\
             history_slots: %d@;\
             gossip_slots: %d@;\
             @;\
             Dumping trace:@;\
             @[<v>%a@]@;\
             @;\
             Explanation:@;\
             %s@]"
            history_slots
            gossip_slots
            pp
            trace
            explanation
    in
    QCheck2.Test.check_exn ~rand:rng test ;
    unit
end

(** Test that removing a peer really removes it from the state *)
module Test_remove_peer = struct
  open Gossipsub_pbt_generators

  let all_peers = [0; 1; 2; 3]

  let fail_if_in_map peers map ~on_error =
    let fail = List.find_opt (fun peer -> GS.Peer.Map.mem peer map) peers in
    match fail with None -> Ok () | Some peer -> Error (on_error peer)

  let fail_if_in_set peers set ~on_error =
    let fail = List.find_opt (fun peer -> GS.Peer.Set.mem peer set) peers in
    match fail with None -> Ok () | Some peer -> Error (on_error peer)

  let not_in_view peers state =
    let open GS.Introspection in
    let open Result_syntax in
    let view = view state in
    let check_map str map =
      fail_if_in_map peers map ~on_error:(fun peer ->
          `peer_not_removed_correctly (view, str, peer))
    in
    let check_set str set =
      fail_if_in_set peers set ~on_error:(fun peer ->
          `peer_not_removed_correctly (view, str, peer))
    in
    let* () = check_map "connections" view.connections in
    let* () = check_map "scores" view.scores in
    let* () =
      Topic.Map.iter_e
        (fun topic backoff ->
          let str = Format.asprintf "backoff[topic=%a]" Topic.pp topic in
          check_map str backoff)
        view.backoff
    in
    (* The last step of the scenario is a heartbeat, which cleans the
       [iwant/ihave_per_heartbeat] maps. *)
    let* () = check_map "ihave_per_heartbeat" view.ihave_per_heartbeat in
    let* () = check_map "iwant_per_heartbeat" view.iwant_per_heartbeat in
    let* () =
      Topic.Map.iter_e
        (fun topic peer_set ->
          let str = Format.asprintf "mesh[topic=%a]" Topic.pp topic in
          check_set str peer_set)
        view.mesh
    in
    let* () =
      Topic.Map.iter_e
        (fun topic fanout_peers ->
          let str = Format.asprintf "fanout[topic=%a]" Topic.pp topic in
          check_set str fanout_peers.peers)
        view.fanout
    in
    Message_cache.Internal_for_tests.get_access_counters view.message_cache
    |> Seq.E.iter (fun (message_id, map) ->
           let place =
             Format.asprintf "message_cache[message_id=%d]" message_id
           in
           check_map place map)

  let predicate (Transition {state' = final_state; _}) =
    (* This predicate checks that [peer_id] does not appear in the [view]
       of the final state. *)
    not_in_view all_peers final_state

  let scenario (limits : ('a, 'b, 'c, Milliseconds.span) limits) =
    let open Fragment in
    let open Basic_fragments in
    let gen_peer = M.oneofl all_peers in
    let gen_topic =
      M.oneofl ["topicA"; "topicB"; "topicC"; "topicD"; "topicE"]
    in
    let gen_message_id = M.oneofl [42; 43; 44] in
    let gen_msg_count = M.int_range 1 5 in

    let add_then_remove_peer_wait_and_clean () =
      (* In order to purge a peer from the connections, we need to
         1. remove it
         2. wait until [expire=retain_duration+slack]
         3. wait until the next round of cleanup in the heartbeat *)
      let expire =
        Milliseconds.Span.to_int_s limits.retain_duration
        + (Milliseconds.Span.to_int_s limits.heartbeat_interval * 2)
      in
      let heartbeat_cleanup_ticks = limits.backoff_cleanup_ticks in
      add_then_remove_peer ~gen_peer
      @% repeat expire tick
      @% repeat heartbeat_cleanup_ticks heartbeat
    in
    let graft_then_prune_wait_and_clean () =
      (* A pruned peer will stay in the backoff table until the
         end of the backoff specified in the Prune message.
         After pruning, we wait for [backoff + slack] ticks then force
         triggering a cleanup of the backoffs in the heartbeat. *)
      let backoff =
        Milliseconds.to_int_s Basic_fragments.prune_backoff
        + (Milliseconds.Span.to_int_s limits.heartbeat_interval * 2)
      in
      let heartbeat_cleanup_ticks = limits.backoff_cleanup_ticks in
      graft_then_prune ~gen_peer ~gen_topic
      @% repeat backoff tick
      @% repeat heartbeat_cleanup_ticks heartbeat
    in
    interleave
      [
        fork_at_most
          4
          (repeat_at_most 2 (add_then_remove_peer_wait_and_clean ()));
        repeat_at_most 10 @@ join_then_leave_topic ~gen_topic;
        repeat_at_most 10 heartbeat;
        repeat_at_most 100 tick;
        repeat_at_most
          5
          (ihave ~gen_peer ~gen_topic ~gen_message_id ~gen_msg_count);
        repeat_at_most 5 (iwant ~gen_peer ~gen_message_id ~gen_msg_count);
        repeat_at_most 10 (graft_then_prune_wait_and_clean ());
      ]
    @% heartbeat

  let pp_backoff fmtr backoff =
    let list = backoff |> GS.Peer.Map.bindings in
    Format.pp_print_list
      ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ", ")
      (fun fmtr (topic, backoff) ->
        Format.fprintf
          fmtr
          "peer %a -> %a"
          GS.Peer.pp
          topic
          Milliseconds.pp
          backoff)
      fmtr
      list

  let pp_state fmtr state =
    let open Format in
    let v = GS.Introspection.view state in
    let cleanup =
      Int64.(rem v.heartbeat_ticks (of_int v.limits.backoff_cleanup_ticks)) = 0L
    in
    if cleanup then fprintf fmtr "heartbeat.clean; " ;
    Topic.Map.iter
      (fun topic backoff ->
        fprintf fmtr "%a: [%a]" Topic.pp topic pp_backoff backoff)
      v.backoff ;
    Peer.Map.iter
      (fun peer score ->
        let expires = GS.Score.expires score in
        fprintf
          fmtr
          "peer %a, expire=%a, cleanup=%b"
          GS.Peer.pp
          peer
          (pp_print_option Milliseconds.pp)
          expires
          cleanup)
      v.scores

  let test rng limits parameters =
    Tezt_core.Test.register
      ~__FILE__
      ~title:"Gossipsub: remove peer"
      ~tags:["gossipsub"; "control"]
    @@ fun () ->
    let scenario =
      let open M in
      let* limits =
        let+ retain_duration =
          M.int_range 0 (Milliseconds.Span.to_int_s limits.retain_duration * 2)
        and+ heartbeat_interval =
          M.int_range
            0
            (Milliseconds.Span.to_int_s limits.heartbeat_interval * 2)
        and+ backoff_cleanup_ticks =
          M.int_range 1 (limits.backoff_cleanup_ticks * 2)
        in
        let score_cleanup_ticks = backoff_cleanup_ticks in
        let retain_duration = Milliseconds.Span.of_int_s retain_duration in
        let heartbeat_interval =
          Milliseconds.Span.of_int_s heartbeat_interval
        in
        {
          limits with
          retain_duration;
          heartbeat_interval;
          backoff_cleanup_ticks;
          score_cleanup_ticks;
          prune_backoff = Basic_fragments.prune_backoff;
        }
      in
      let state = GS.make rng limits parameters in
      run state (scenario limits)
    in
    let test =
      QCheck2.Test.make ~count:1_000 ~name:"Gossipsub: remove_peer" scenario
      @@ fun trace ->
      match check_final predicate trace with
      | Ok () -> true
      | Error e -> (
          match e with
          | `peer_not_removed_correctly (v, msg, peer) ->
              Tezt.Test.fail
                ~__LOC__
                "@[<v 2>Peer %d was not removed correctly from %s.@;\
                 Limits:@;\
                 %a@;\
                 Dumping trace:@;\
                 @[<v>%a@]@]"
                peer
                msg
                pp_limits
                GS.Introspection.(limits v)
                (pp_trace ~pp_state ~pp_state':pp_state ())
                trace)
    in
    QCheck2.Test.check_exn ~rand:rng test ;
    unit
end

module Test_peers_below_degree_high = struct
  open Gossipsub_pbt_generators

  (* This test checks the logic around pruning overflowing peers.
     The test works as follows:
     1. Add many distinct peers (some direct, some not) on some unique topic.
        We do so by performing a sequence of [Add_peer] followed by [Graft].
        When the mesh becomes full, only [outbound] peers are added by [Graft] so
        we add more outbound peers than [degree_high].
     2. Perform a heartbeat.
     3.1 Check just before the heartbeat that the [to_prune] record is well-formed
         and that there are indeed too many peers in the mesh.
     3.2 Check in the heartbeat output that the automaton requests to prune enough
         peers to reach [degree_optimal].
  *)

  let topic = "dummy_topic"

  let pp_state fmtr state =
    let v = GS.Introspection.view state in
    Fmt.Dump.record
      Fmt.Dump.
        [
          field
            "mesh"
            (fun v -> v.GS.Introspection.mesh)
            GS.Introspection.(pp_topic_map pp_peer_set);
          field
            "connections"
            (fun v -> v.GS.Introspection.connections)
            GS.Introspection.pp_connections;
        ]
      fmtr
      v

  let predicate degree_high
      (Transition {time; input; state; state'; output} : transition) () =
    let open Result_syntax in
    match input with
    | Graft _ -> (
        match output with
        | Peer_filtered | Unsubscribed_topic | Unexpected_grafting_peer
        | Grafting_peer_with_negative_score | Peer_backed_off ->
            fail (`unexpected_output (O output))
        | Grafting_direct_peer | Peer_already_in_mesh | Grafting_successfully
        | Mesh_full ->
            return_unit)
    | Heartbeat -> (
        match output with
        | Heartbeat {to_prune; _} ->
            let open GS.Introspection in
            let previous_state_has_enough_peers_in_mesh s =
              (* The mesh should have too many peers in [s]. *)
              let v = view s in
              (* Get number of peers on the single topic. *)
              let peer_set = GS.Topic.Map.find topic v.mesh in
              let card =
                match peer_set with
                | None -> 0
                | Some set -> GS.Peer.Set.cardinal set
              in
              (* Cardinality should be > [degree_high]. *)
              if card <= degree_high then
                fail (`invalid_state_before_heartbeat s)
              else return_unit
            in
            let pruned_enough_peers s' =
              (* The automaton should have pruned enough peers. *)
              let v = view s' in
              (* First perform an unrelated, opportunistic check: the prune
                 record doesn't map empty topic sets to any peer. *)
              let inconsistent_prune_record =
                GS.Peer.Map.exists
                  (fun _peer topic_set -> GS.Topic.Set.is_empty topic_set)
                  to_prune
              in
              if inconsistent_prune_record then
                fail (`inconsistent_prune_record to_prune)
              else
                (* Check that we pruned enough peers to reach [target]. *)
                let target = v.limits.degree_optimal in
                let peer_set = GS.Topic.Map.find topic v.mesh in
                let card =
                  match peer_set with
                  | None -> 0
                  | Some set -> GS.Peer.Set.cardinal set
                in
                if card <> target then
                  fail (`too_many_peers (time, card, target, degree_high))
                else return_unit
            in
            let* () = previous_state_has_enough_peers_in_mesh state in
            pruned_enough_peers state')
    | _ -> return_unit

  let scenario limits =
    let open Fragment in
    let open Basic_fragments in
    let open M in
    (* In order to satisfy the predicate, [count_outbound > degree_high ] *)
    bind_gen
      (let* count_outbound =
         M.int_range (limits.degree_high + 1) (limits.degree_high * 2)
       in
       (* We need [peer_count >= count_outbound] *)
       let* peer_count = M.int_range count_outbound (count_outbound * 2) in
       return (count_outbound, peer_count))
    @@ fun (count_outbound, peer_count) ->
    of_list [I.join {topic}]
    @% add_distinct_peers_and_graft peer_count count_outbound topic
    @% heartbeat

  (* A generator that should satisfy [Tezos_gossipsub.check_limits]. *)
  let limit_generator (default_limits : (_, _, _, _) limits) =
    let open M in
    let* degree_optimal = M.int_range 1 20 in
    let* degree_low, degree_high =
      M.pair
        (M.int_range 1 degree_optimal)
        (M.int_range degree_optimal (2 * degree_optimal))
    in
    let* degree_out = M.int_range 0 (Int.min degree_low (degree_optimal / 2)) in
    let* degree_score = M.int_range 0 (degree_optimal - degree_out) in
    let* history_length = M.int_range 1 50 in
    let* history_gossip_length = M.int_range 1 history_length in
    return
      {
        default_limits with
        degree_optimal;
        degree_low;
        degree_high;
        degree_out;
        degree_score;
        history_length;
        history_gossip_length;
      }

  let test rng limits parameters =
    Tezt_core.Test.register
      ~__FILE__
      ~title:"Gossipsub: peers below degree high"
      ~tags:["gossipsub"; "heartbeat"; "degree"]
    @@ fun () ->
    let scenario =
      let open M in
      let* limits = limit_generator limits in
      let state = GS.make rng limits parameters in
      let+ trace = run state (scenario limits) in
      (trace, limits)
    in
    let test =
      QCheck2.Test.make
        ~count:1_000
        ~name:"Gossipsub: peers_below_degree_high"
        scenario
      @@ fun (trace, limits) ->
      match check_fold (predicate limits.degree_high) () trace with
      | Ok () -> true
      | Error (e, prefix) -> (
          match e with
          | `invalid_state_before_heartbeat s ->
              Tezt.Test.fail
                ~__LOC__
                "Dumping trace until failure:@;\
                 @[<hov>%a@]@;\
                 Limits:@;\
                 %a@;\
                 Faulty test: invalid state before heartbeat %a@;"
                (pp_trace ~pp_output ~pp_state ())
                prefix
                pp_limits
                limits
                pp_state
                s
          | `unexpected_output (O o) ->
              Tezt.Test.fail
                ~__LOC__
                "Faulty test: unexpected output %a"
                GS.pp_output
                o
          | `inconsistent_prune_record to_prune ->
              Tezt.Test.fail
                ~__LOC__
                "At heartbeat, the prune record is ill-formed: %a"
                GS.Introspection.(pp_peer_map pp_topic_set)
                to_prune
          | `too_many_peers (t, remaining, target, degree_high) ->
              Tezt.Test.fail
                ~__LOC__
                "@[<v 2>Dumping trace until failure:@;\
                 @[<hov>%a@]@;\
                 At time %a: peers in mesh=%d, target = %d, degree_high = %d@;\
                 Limits:@;\
                 %a@;\
                 @]"
                (pp_trace ~pp_state ~pp_output ())
                prefix
                Milliseconds.pp
                t
                remaining
                target
                degree_high
                pp_limits
                limits)
    in
    QCheck2.Test.check_exn ~rand:rng test ;
    unit
end

(** Check that when handling IHaves we:
    1. Don't send IWants to peers with negative score.
    2. Don't send more than [max_sent_iwant_per_heartbeat] IWants per heartbeat per peer.
    3. Don't respond to more than [max_recv_ihave_per_heartbeat] IHaves per heartbeat per peer.
    4. Only send IWants for messages we haven't seen.
    5. Don't send IWants for topics we have not joined.
    6. Correctly update IWant/IHave counters. *)
module Test_handle_ihave = struct
  open Gossipsub_pbt_generators

  let pp_state fmtr state =
    let v = GS.Introspection.view state in
    Fmt.Dump.record
      Fmt.Dump.
        [
          field
            "ihave_per_heartbeat"
            (fun v -> v.GS.Introspection.ihave_per_heartbeat)
            (GS.Introspection.pp_peer_map Format.pp_print_int);
          field
            "iwant_per_heartbeat"
            (fun v -> v.GS.Introspection.iwant_per_heartbeat)
            (GS.Introspection.pp_peer_map Format.pp_print_int);
        ]
      fmtr
      v

  let predicate
      {
        max_sent_iwant_per_heartbeat;
        max_recv_ihave_per_heartbeat;
        gossip_threshold;
        _;
      } (Transition {time = _; input; state; state'; output} : transition) () =
    let open Result_syntax in
    match input with
    | Ihave {peer; topic; message_ids = _} ->
        let open GS.Introspection in
        let gossip_threshold = GS.Score.of_float gossip_threshold in
        let v = view state in
        let sent_iwant_count = get_peer_iwant_per_heartbeat peer v in
        let recv_ihave_count = get_peer_ihave_per_heartbeat peer v in
        let iwant_message_ids =
          match output with
          | Message_requested_message_ids iwant_message_ids -> iwant_message_ids
          | _ -> []
        in
        let iwant_size = List.length iwant_message_ids in
        (* 1. Don't send IWants to peers with negative score. *)
        let score = GS.Introspection.get_peer_score peer v in
        let* () =
          if GS.Score.(score < gossip_threshold) && iwant_size != 0 then
            fail @@ `negative_score score
          else return_unit
        in
        (* 2. Don't send more than [max_sent_iwant_per_heartbeat] IWants per heartbeat per peer. *)
        let updated_sent_iwant_count = sent_iwant_count + iwant_size in
        let* () =
          if updated_sent_iwant_count > max_sent_iwant_per_heartbeat then
            fail
            @@ `too_many_iwants_sent
                 (peer, updated_sent_iwant_count, max_sent_iwant_per_heartbeat)
          else return_unit
        in
        (* 3. Don't respond to more than [max_recv_ihave_per_heartbeat] IHaves per heartbeat per peer. *)
        let updated_recv_ihave_count = recv_ihave_count + 1 in
        let* () =
          if
            updated_recv_ihave_count > max_recv_ihave_per_heartbeat
            && iwant_size != 0
          then
            fail
            @@ `too_many_ihaves_received
                 (peer, updated_recv_ihave_count, max_recv_ihave_per_heartbeat)
          else return_unit
        in
        (* 4. Only send IWants for messages we haven't seen. *)
        let* () =
          List.iter_e
            (fun message_id ->
              if Message_cache.seen_message message_id v.message_cache then
                fail @@ `sending_iwant_for_seen_message (peer, message_id)
              else return_unit)
            iwant_message_ids
        in
        (* 5. Don't send IWants for topics we have not joined. *)
        let* () =
          if (not @@ has_joined topic v) && iwant_size != 0 then
            fail @@ `sending_iwant_for_topic_not_joined (peer, topic)
          else return_unit
        in
        (* 6. Correctly update IWant/IHave counters. *)
        let* () =
          let v' = view state' in
          let sent_iwant_count' =
            GS.Introspection.get_peer_iwant_per_heartbeat peer v'
          in
          let recv_ihave_count' =
            GS.Introspection.get_peer_ihave_per_heartbeat peer v'
          in
          if GS.Score.(score < gossip_threshold) then
            (* We do not expect counter update if peer has negative score. *)
            if
              sent_iwant_count = sent_iwant_count'
              && recv_ihave_count = recv_ihave_count'
            then return_unit
            else fail `counter_updated_when_negative_score
          else if updated_sent_iwant_count != sent_iwant_count' then
            fail
            @@ `sent_iwant_count_not_updated_correctly
                 (updated_sent_iwant_count, sent_iwant_count')
          else if updated_recv_ihave_count != recv_ihave_count' then
            fail
            @@ `recv_ihave_count_not_updated_correctly
                 (updated_recv_ihave_count, recv_ihave_count')
          else return_unit
        in
        return_unit
    | _ -> return_unit

  let test rng _limits parameters =
    Tezt_core.Test.register
      ~__FILE__
      ~title:"Gossipsub: Handle IHaves"
      ~tags:["gossipsub"; "ihave"; "iwant"]
    @@ fun () ->
    let scenario =
      let open M in
      let total_ticks = 100 in
      let* limits =
        let* max_sent_iwant_per_heartbeat = int_range 1 100 in
        let* max_recv_ihave_per_heartbeat = int_range 1 10 in
        (* [mesh_message_deliveries_activation] controls when p3 is activated and starts
           applying negative scores to peers that don't send us enough messages.
           If it is too low, all peers will have negative scores early on in the test.
           If it is too high, no peer will have negative scores during the test.
           So we set it somewhere between [total_ticks / 3] and [total_ticks / 2] *)
        let* mesh_message_deliveries_activation =
          int_range (total_ticks / 3) (total_ticks / 2)
        in
        return
          {
            (Default_limits.default_limits
               ~mesh_message_deliveries_activation:
                 (Some mesh_message_deliveries_activation)
               ())
            with
            max_sent_iwant_per_heartbeat;
            max_recv_ihave_per_heartbeat;
          }
      in
      let state = GS.make rng limits parameters in
      let* scenario =
        let open Fragment in
        let open Basic_fragments in
        let open M in
        (* We separate peers that publish messages and peers that don't.
           This is to ensure that some subset of peers will have a negative score
           at some point due to P3. *)
        let* publishing_peer_count = M.int_range 1 5 in
        let+ not_publishing_peer_count = M.int_range 1 5 in
        let publishing_peers = range 0 publishing_peer_count in
        let not_publishing_peers = range 0 not_publishing_peer_count in
        let all_peers = publishing_peers @ not_publishing_peers in
        let gen_all_peer = M.oneofl all_peers in
        let gen_publishing_peers = M.oneofl publishing_peers in
        let joined_topics =
          ["topicA"; "topicB"; "topicC"; "topicD"; "topicE"]
        in
        let not_joined_topics = ["topicF"] in
        let all_topics = joined_topics @ not_joined_topics in
        let gen_topic = M.oneofl all_topics in
        let gen_message_id = M.int_range 1 100 in
        let gen_msg_count = M.int_range 1 5 in
        let gen_message = M.string in
        of_list (List.map (fun topic -> I.join {topic}) joined_topics)
        @% add_distinct_peers_and_subscribe
             ~all_peers
             ~topics_to_subscribe:all_topics
             ~gen_outbound:M.bool
             ~gen_direct:(M.return false)
        @% interleave
             [
               repeat_at_most total_ticks (heartbeat @% tick);
               repeat_at_most
                 100
                 (ihave
                    ~gen_peer:gen_all_peer
                    ~gen_topic
                    ~gen_message_id
                    ~gen_msg_count);
               repeat_at_most
                 50
                 (receive_message
                    ~gen_peer:gen_publishing_peers
                    ~gen_topic
                    ~gen_message_id
                    ~gen_message);
               repeat_at_most
                 10
                 (publish_message ~gen_topic ~gen_message_id ~gen_message);
             ]
      in
      let+ trace = run state scenario in
      (trace, limits)
    in
    let test =
      QCheck2.Test.make ~count:100 ~name:"Gossipsub: iwant_checks" scenario
      @@ fun (trace, limits) ->
      match check_fold (predicate limits) () trace with
      | Ok () -> true
      | Error (e, prefix) -> (
          match e with
          | `negative_score score ->
              let msg =
                Format.asprintf
                  "Sending IWant to a peer with negative score %a"
                  GS.Score.pp_value
                  score
              in
              dump_trace_and_fail ~__LOC__ prefix limits pp_state msg
          | `too_many_iwants_sent
              (peer, iwant_count, max_sent_iwant_per_heartbeat) ->
              let msg =
                Format.asprintf
                  "Too many IWants were sent: Peer = %a, IWant count = %d, \
                   max_sent_iwant_per_heartbeat = %d"
                  Peer.pp
                  peer
                  iwant_count
                  max_sent_iwant_per_heartbeat
              in
              dump_trace_and_fail ~__LOC__ prefix limits pp_state msg
          | `too_many_ihaves_received
              (peer, ihave_count, max_recv_ihave_per_heartbeat) ->
              let msg =
                Format.asprintf
                  "Too many IHaves were received: Peer = %a, IHave count = %d, \
                   max_recv_ihave_per_heartbeat = %d"
                  Peer.pp
                  peer
                  ihave_count
                  max_recv_ihave_per_heartbeat
              in
              dump_trace_and_fail ~__LOC__ prefix limits pp_state msg
          | `sending_iwant_for_seen_message (peer, message_id) ->
              let msg =
                Format.asprintf
                  "Sending IWant for a seen message: Peer = %a, message_id = %d"
                  Peer.pp
                  peer
                  message_id
              in
              dump_trace_and_fail ~__LOC__ prefix limits pp_state msg
          | `sending_iwant_for_topic_not_joined (peer, topic) ->
              let msg =
                Format.asprintf
                  "Sending IWant for a topic we have not joined: Peer = %a, \
                   topic = %a"
                  Peer.pp
                  peer
                  Topic.pp
                  topic
              in
              dump_trace_and_fail ~__LOC__ prefix limits pp_state msg
          | `sent_iwant_count_not_updated_correctly
              (expected_count, actual_count) ->
              let msg =
                Format.asprintf
                  "Sent IWant count not updated correctly. Expected %d got %d"
                  expected_count
                  actual_count
              in
              dump_trace_and_fail ~__LOC__ prefix limits pp_state msg
          | `counter_updated_when_negative_score ->
              let msg =
                "We should not update IWant/IHave counters when the peer has \
                 negative score."
              in
              dump_trace_and_fail ~__LOC__ prefix limits pp_state msg
          | `recv_ihave_count_not_updated_correctly
              (expected_count, actual_count) ->
              let msg =
                Format.asprintf
                  "Received IHave count not updated correctly. Expected %d got \
                   %d"
                  expected_count
                  actual_count
              in
              dump_trace_and_fail ~__LOC__ prefix limits pp_state msg)
    in
    QCheck2.Test.check_exn ~rand:rng test ;
    unit
end

let register rng limits parameters =
  Test_remove_peer.test rng limits parameters ;
  Test_message_cache.test rng ;
  Test_peers_below_degree_high.test rng limits parameters ;
  Test_handle_ihave.test rng limits parameters
