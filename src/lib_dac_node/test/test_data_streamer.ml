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
    Invocation: dune exec src/lib_dac_node/test/main.exe \
                  -- test "^\[Unit\] Data_streamer.ml$"   
    Subject:    Tests for the data streamer component.
*)

let dac_hash_1 = "hash_1"

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
  let* stream, stopper = handle_subscribe streamer in
  let* () = publish streamer dac_hash_1 in
  let*! next = Lwt_stream.next stream in
  let () = Lwt_watcher.shutdown stopper in
  let*! is_empty = Lwt_stream.is_empty stream in
  let () = Assert.assert_true "Expected empty stream." is_empty in
  return @@ assert_equal_string dac_hash_1 next

let tests = [Tztest.tztest "Simple pub sub" `Quick test_simple_pub_sub]
