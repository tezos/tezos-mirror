(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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
   Component:  Gossipsub/Message Cache
   Invocation: dune exec test/test_gossipsub.exe -- --file test_message_cache.ml
   Subject:    Unit tests for message cache used in gossipsub
*)

open Test_gossipsub_shared
open Tezt
open Tezt_core.Base
module Peer = C.Subconfig.Peer
module Topic = C.Subconfig.Topic
module Message_id = C.Subconfig.Message_id
module Message = C.Subconfig.Message

(** [new_message ()] returns a fresh (message_id, message) pair. *)
let new_message : unit -> Message_id.t * Message.t =
  let i = ref 0 in
  fun () ->
    let new_message = (!i, Format.sprintf "message%d" !i) in
    incr i ;
    new_message

let new_messages n = Array.init n (fun _ -> new_message ()) |> Array.to_list

(** [add_messages topic messages message_cache] adds [messages] to [message_cache]
    under [topic]. *)
let add_messages topic messages message_cache =
  List.fold_left
    (fun m (message_id, message) ->
      Message_cache.add_message message_id message topic m)
    message_cache
    messages

(** [check_messages_in_cache messages message_cache ~expect_included]
    checks that [messages] are retrievable from [message_cache] if [expect_included = true].
    If [expect_included = false], it checks that [messages] are not retrievable from [message_cache] *)
let check_messages_inclusion ~__LOC__ messages message_cache ~expect_included =
  let random_peer = 99 in
  List.iter
    (fun (message_id, message) ->
      match
        Message_cache.get_message_for_peer random_peer message_id message_cache
      with
      | None ->
          if expect_included then
            Test.fail
              ~__LOC__
              "Expected message %a to be in cache."
              Message.pp
              message
          else (* Expected *)
            ()
      | Some (_, returned_message, _) ->
          if expect_included then
            Check.(
              (returned_message = message)
                string
                ~error_msg:"Expected %R, got %L"
                ~__LOC__)
          else
            Test.fail
              ~__LOC__
              "Expected message %a to not be in cache."
              Message.pp
              message)
    messages

(** [check_gossip_ids topic message_cache ~expected_message_ids] checks that
    [expected_message_ids] are in the gossiped message ids generated from [message_cache]. *)
let check_gossip_ids ~__LOC__ topic message_cache ~expected_message_ids =
  let gossip_message_ids =
    Message_cache.get_message_ids_to_gossip topic message_cache
  in
  Check.(
    (gossip_message_ids = expected_message_ids)
      (list int)
      ~error_msg:"Expected %R, got %L"
      ~__LOC__)

(* Tests the sliding window behavior of the message cache.

   Ported from: https://github.com/libp2p/go-libp2p-pubsub/blob/56c0e6c5c9dfcc6cd3214e59ef19456dc0171574/mcache_test.go#L11
*)
let test_message_cache () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Gossipsub/Message Cache: Test message cache"
    ~tags:["gossipsub"; "cache"; "message_cache"]
  @@ fun () ->
  let m = Message_cache.create ~gossip_slots:3 ~history_slots:5 in
  let topic = "topic" in
  let message_batches =
    [|
      new_messages 10;
      new_messages 10;
      new_messages 10;
      new_messages 10;
      new_messages 10;
      new_messages 10;
    |]
  in
  let ids_of_messages messages =
    List.map (fun (message_id, _) -> message_id) messages
  in
  (* Add 10 messages. *)
  let m = add_messages topic message_batches.(0) m in
  (* Should gossip 10 messages (10 total). *)
  check_gossip_ids
    ~__LOC__
    topic
    m
    ~expected_message_ids:(ids_of_messages message_batches.(0)) ;
  (* 1st shift *)
  let m = Message_cache.shift m in
  (* Add another 10 messages (20 total). *)
  let m = add_messages topic message_batches.(1) m in
  (* Should gossip all 20 messages. *)
  let expected_message_ids =
    List.concat [message_batches.(0); message_batches.(1)] |> ids_of_messages
  in
  check_gossip_ids ~__LOC__ topic m ~expected_message_ids ;
  (* 2nd shift *)
  let m = Message_cache.shift m in
  (* Add another 10 messages (30 total). *)
  let m = add_messages topic message_batches.(2) m in
  (* 3rd shift *)
  let m = Message_cache.shift m in
  (* Add another 10 messages (40 total). *)
  let m = add_messages topic message_batches.(3) m in
  (* 4th shift *)
  let m = Message_cache.shift m in
  (* Add another 10 messages (50 total). *)
  let m = add_messages topic message_batches.(4) m in
  (* 5th shift *)
  let m = Message_cache.shift m in
  (* Add another 10 messages (60 total). *)
  let m = add_messages topic message_batches.(5) m in
  (* Since the gossip window is [3], we should only gossip the last 30 messages. *)
  let expected_message_ids =
    List.concat [message_batches.(3); message_batches.(4); message_batches.(5)]
    |> ids_of_messages
  in
  check_gossip_ids ~__LOC__ topic m ~expected_message_ids ;
  (* Since the history window is [5], the first 10 messages should not be in the cache. *)
  let first_10 = message_batches.(0) in
  check_messages_inclusion ~__LOC__ first_10 m ~expect_included:false ;
  (* The later 50 messages should be in the cache. *)
  let later_50 =
    List.concat
      [
        message_batches.(1);
        message_batches.(2);
        message_batches.(3);
        message_batches.(4);
        message_batches.(5);
      ]
  in
  check_messages_inclusion ~__LOC__ later_50 m ~expect_included:true ;
  unit

let register () = test_message_cache ()
