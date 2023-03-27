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

(** Testing
    -------
    Component:  Lib_dac_node Data_streamer
    Invocation: dune exec src/lib_dac_node/test/main.exe   
    Subject:    Tests for the data streamer component.
*)

let dac_hash_1 = "hash_1"

let dac_hash_2 = "hash_2"

let assert_equal_string expected actual =
  Assert.equal
    ~loc:__LOC__
    ~eq:String.equal
    ~pp:Format.pp_print_string
    expected
    actual

let test_simple_pub_sub () =
  let open Lwt_result_syntax in
  let open Data_streamer in
  let streamer = init () in
  let stream, stopper = handle_subscribe streamer in
  let () = publish streamer dac_hash_1 in
  let*! next = Lwt_stream.next stream in
  let () = Lwt_watcher.shutdown stopper in
  let*! is_empty = Lwt_stream.is_empty stream in
  let () = Assert.assert_true "Expected empty stream." is_empty in
  return @@ assert_equal_string dac_hash_1 next

let test_subscription_time () =
  (* 1. subscriber_1 subscribes to the streamer component.
     2. [dac_hash_1] is published via the streamer component.
     3. subscriber_2 subscribes to the streamer component.
     4. [dac_hash_2] is published via streamer component.

     As such we expect:
     - subscriber_1 first element inside the stream is [dac_hash_1]
       and second one is [dac_hash_2].
     - subscriber_2 first element inside the stream is [dac_hash_2].

     Note that subscriber_2 does not receive [dac_hash_1] as it was published
     before his/her subscription to the streaming component.
  *)
  let open Lwt_result_syntax in
  let open Data_streamer in
  let streamer = init () in
  let stream_1, _stopper_1 = handle_subscribe streamer in
  let () = publish streamer dac_hash_1 in
  let stream_2, _stopper_2 = handle_subscribe streamer in
  let () = publish streamer dac_hash_2 in
  let*! subscriber_1_first = Lwt_stream.next stream_1 in
  let () = assert_equal_string dac_hash_1 subscriber_1_first in
  let*! subscriber_1_second = Lwt_stream.next stream_1 in
  let () = assert_equal_string dac_hash_2 subscriber_1_second in
  let*! subscriber_2_first = Lwt_stream.next stream_2 in
  return @@ assert_equal_string dac_hash_2 subscriber_2_first

let test_closing_subscriber_stream () =
  (* 1. subscriber_1 subscribes to the streamer component.
     2. subscriber_2 subscribes to the streamer component.
     3. subscriber_1's [stream_1] is closed.
     4. [dac_hash_1] is published via streamer component.

     As such we expect:
     - subscriber_1's [stream_1] is empty.
     - subscriber_2 first element inside the stream is [dac_hash_1].
  *)
  let open Lwt_result_syntax in
  let open Data_streamer in
  let streamer = init () in
  let stream_1, stopper_1 = handle_subscribe streamer in
  let stream_2, _stopper_2 = handle_subscribe streamer in
  let () = Lwt_watcher.shutdown stopper_1 in
  let () = publish streamer dac_hash_1 in
  let*! is_empty = Lwt_stream.is_empty stream_1 in
  let () = Assert.assert_true "[stream_1]: expected empty stream." is_empty in
  let*! subscriber_2_first = Lwt_stream.next stream_2 in
  return @@ assert_equal_string dac_hash_1 subscriber_2_first

let tests =
  [
    Tztest.tztest "Simple pub sub" `Quick test_simple_pub_sub;
    Tztest.tztest "Test order of subscription" `Quick test_subscription_time;
    Tztest.tztest
      "Test closing subscriber stream"
      `Quick
      test_closing_subscriber_stream;
  ]
