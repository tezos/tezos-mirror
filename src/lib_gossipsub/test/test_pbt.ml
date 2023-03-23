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

(** {2 PBT for gossipsub.} *)

open Test_gossipsub_shared
open Gossipsub_intf
open Tezt_core.Base

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

  let scenario limits =
    let open Fragment in
    let open Basic_fragments in
    let add_then_remove_peer_wait_and_clean ~peer_id =
      (* In order to pure a peer from the connection, we need to
         1. remove it
         2. wait until [retain_duration+slack]
         3. wait until the next round of cleanup in the heartbeat *)
      let expire = limits.retain_duration + (limits.heartbeat_interval * 2) in
      let heartbeat_cleanup_ticks = limits.backoff_cleanup_ticks in
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

  let test rng limits parameters =
    Tezt_core.Test.register
      ~__FILE__
      ~title:"Gossipsub: remove peer"
      ~tags:["gossipsub"; "control"]
    @@ fun () ->
    let state = GS.make rng limits parameters in
    let test =
      QCheck2.Test.make
        ~name:"Gossipsub: remove_peer"
        (run state (scenario limits))
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

let register rng limits parameters = Test_remove_peer.test rng limits parameters
