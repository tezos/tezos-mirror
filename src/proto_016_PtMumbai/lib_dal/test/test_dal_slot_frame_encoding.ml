(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech <contact@trili.tech>                        *)
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
    Component:  Dal_node Slot_frame_encoding
    Invocation: dune exec src/proto_016_PtMumbai/lib_dal/test/main.exe
    Subject:    Tests for the SCORU storage module
*)

(* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3421
   Property based tests to check basic invariants of slot-frame encoding V0. *)

open Protocol
open Alpha_context
module Rollup_messages_map = Dal_slot_frame_encoding.Rollups_map
module V0 = Dal_slot_frame_encoding.V0

(* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3339
   Fetch this value from protocol default constants *)
let max_size = 1_048_576

let assert_fails_with ~loc k expected_err =
  let open Lwt_result_syntax in
  let*! res = k in
  Assert.error ~loc res (( = ) expected_err)

module Compare_list_string = Compare.List (String)
module Compare_list_list_string = Compare.List (Compare_list_string)
module Compare_list_rollup = Compare.List (Sc_rollup.Address)

let assert_equal_bytes ~loc msg =
  Assert.equal ~loc Bytes.equal msg String.pp_bytes_hex

let assert_equal_list_string ~loc msg =
  Assert.equal
    ~loc
    Compare_list_string.equal
    msg
    (Format.pp_print_list Format.pp_print_string)

let assert_equal_list_list_string ~loc msg =
  Assert.equal
    ~loc
    Compare_list_list_string.equal
    msg
    (Format.pp_print_list (Format.pp_print_list Format.pp_print_string))

let assert_equal_list_rollups ~loc msg =
  Assert.equal
    ~loc
    Compare_list_rollup.equal
    msg
    (Format.pp_print_list Sc_rollup.Address.pp)

let sc_rollup_1 =
  Sc_rollup.Address.of_b58check_exn "sr1BAwv191dVYeZg44ZxVy8dFwfRQKW6bSqc"

let sc_rollup_2 =
  Sc_rollup.Address.of_b58check_exn "sr1Fq8fPi2NjhWUXtcXBggbL6zFjZctGkmso"

let slot_frame_encoding_size_correct_single_v0 () =
  let open Lwt_result_syntax in
  let messages_rollup_1 =
    ["hello"; "is"; "it"; "me"; "you"; "are"; "looking"; "for"]
  in
  (* One rollup with offset should take 24 bytes *)
  let entry_size = V0.Internal.rollup_entry_size in
  let* () = Assert.equal_int ~loc:__LOC__ entry_size 24 in
  (* 1 byte for version *)
  let expected_version_size = 1 in
  (* 20 bytes for one rollup address + 4 bytes of offset + 4 bytes for frame length = 28 bytes *)
  let computed_rollups_frame_size = V0.Internal.rollups_frame_size 1 in
  let expected_rollups_frame_size = 28 in
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      computed_rollups_frame_size
      expected_rollups_frame_size
  in
  (*27 bytes total messages +
    4 * 8 = 32 bytes message length prefixes +
    4 bytes list length prefix +
    63 bytes for messages frame *)
  let computed_messages_frame_size =
    V0.Internal.messages_frame_size messages_rollup_1
  in
  let expected_messages_frame_size = 63 in
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      computed_messages_frame_size
      expected_messages_frame_size
  in
  (* 4 bytes of list length prefix +
     63 bytes of messages_frame_size = 67 bytes for all messages frames *)
  let computed_all_messages_frames_size =
    V0.Internal.all_messages_frames_size [messages_rollup_1]
  in
  let expected_all_messages_frames_size = 4 + expected_messages_frame_size in
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      computed_all_messages_frames_size
      expected_all_messages_frames_size
  in
  let expected_size =
    expected_version_size + expected_rollups_frame_size
    + expected_all_messages_frames_size
  in
  let map =
    Rollup_messages_map.empty
    |> Rollup_messages_map.add sc_rollup_1 messages_rollup_1
  in
  let computed_size = V0.expected_slot_size map in
  Assert.equal_int ~loc:__LOC__ expected_size computed_size

let slot_frame_encoding_size_correct_multiple_v0 () =
  let open Lwt_result_syntax in
  let messages_rollup_1 = ["Summer"; "loving"; "had"; "me"; "a"; "blast"] in
  let messages_rollup_2 = ["Summer"; "loving"; "happened"; "so"; "fast"] in
  (* 1 byte for version *)
  let expected_version_size = 1 in
  (* 24 * 2 = 48 bytes for two rollup entries +
     4 bytes for frame length prefix = 52 bytes
  *)
  let computed_rollups_frame_size = V0.Internal.rollups_frame_size 2 in
  let expected_rollups_frame_size = 52 in
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      computed_rollups_frame_size
      expected_rollups_frame_size
  in
  (* Frame 1:
     6 + 6 + 3 + 2 + 1 + 5 = 23 bytes for messages +
     4 * 6 = 24 bytes for message length prefix for 6 messages
     + 4 bytes for message frame prefix =
     51 bytes for the messages frame for rollup1.
     Frame 2:
     6 + 6 + 8 + 2 + 4 = 26 bytes for messages +
     4 * 5 = 20 bytes for message length prefix for 6 messages
     + 4 bytes for message frame prefix =
     50 bytes for the messages frame for rollup2.
     Messages frame length:
     51 bytes for rollup1 messages frame +
     50 bytes for rollup2 messages frame +
     4 bytes prefix length for all messages frames =
     105 bytes
  *)
  let computed_all_messages_frames_size =
    V0.Internal.all_messages_frames_size [messages_rollup_1; messages_rollup_2]
  in
  let expected_all_messages_frames_size = 105 in
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      computed_all_messages_frames_size
      expected_all_messages_frames_size
  in
  let expected_size =
    expected_version_size + expected_rollups_frame_size
    + expected_all_messages_frames_size
  in
  let map =
    Rollup_messages_map.empty
    |> Rollup_messages_map.add sc_rollup_1 messages_rollup_1
    |> Rollup_messages_map.add sc_rollup_2 messages_rollup_2
  in
  let computed_size = V0.expected_slot_size map in
  Assert.equal_int ~loc:__LOC__ expected_size computed_size

let slot_frame_encoding_decoding_correct_single_v0 () =
  let open Lwt_result_syntax in
  let messages_rollup_1 =
    ["hello"; "is"; "it"; "me"; "you"; "are"; "looking"; "for"]
  in
  let messages_map =
    Rollup_messages_map.empty
    |> Rollup_messages_map.add sc_rollup_1 messages_rollup_1
  in
  let* serialized = V0.serialize ~max_size messages_map in
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      (String.length serialized)
      (V0.expected_slot_size messages_map)
  in
  let* deserialized = V0.deserialize ~max_size serialized in
  let rollups_with_messages = Rollup_messages_map.bindings deserialized in
  let* () =
    assert_equal_list_rollups
      ~loc:__LOC__
      "Deserialized rollups are different from originals"
      [sc_rollup_1]
      (List.map fst rollups_with_messages)
  in
  assert_equal_list_list_string
    ~loc:__LOC__
    "Messages frames are different from originals"
    [messages_rollup_1]
    (List.map snd rollups_with_messages)

let slot_frame_encoding_decoding_correct_multiple_v0 () =
  let open Lwt_result_syntax in
  let messages_rollup_1 = ["Summer"; "loving"; "had"; "me"; "a"; "blast"] in
  let messages_rollup_2 = ["Summer"; "loving"; "happened"; "so"; "fast"] in
  let messages_map =
    Rollup_messages_map.empty
    |> Rollup_messages_map.add sc_rollup_1 messages_rollup_1
    |> Rollup_messages_map.add sc_rollup_2 messages_rollup_2
  in
  let* serialized = V0.serialize ~max_size messages_map in
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      (String.length serialized)
      (V0.expected_slot_size messages_map)
  in
  let* deserialized = V0.deserialize ~max_size serialized in
  let rollups_with_messages = Rollup_messages_map.bindings deserialized in
  let* () =
    assert_equal_list_rollups
      ~loc:__LOC__
      "Deserialized rollups are different from originals"
      [sc_rollup_1; sc_rollup_2]
      (List.map fst rollups_with_messages)
  in
  assert_equal_list_list_string
    ~loc:__LOC__
    "Messages frames are different from originals"
    [messages_rollup_1; messages_rollup_2]
    (List.map snd rollups_with_messages)

let slot_frame_encoding_fails_if_too_big () =
  let messages_rollup_1 = ["Summer"; "loving"; "had"; "me"; "a"; "blast"] in
  let messages_rollup_2 = ["Summer"; "loving"; "happened"; "so"; "fast"] in
  let messages_map =
    Rollup_messages_map.empty
    |> Rollup_messages_map.add sc_rollup_1 messages_rollup_1
    |> Rollup_messages_map.add sc_rollup_2 messages_rollup_2
  in
  let actual_size = V0.expected_slot_size messages_map in
  let max_size = actual_size - 1 in
  assert_fails_with
    ~loc:__LOC__
    (V0.serialize ~max_size messages_map)
    (Dal_slot_frame_encoding.Slot_size_is_too_big {actual_size; max_size})

let slot_frame_decoding_fails_if_too_big () =
  let open Lwt_result_syntax in
  let messages_rollup_1 = ["Summer"; "loving"; "had"; "me"; "a"; "blast"] in
  let messages_rollup_2 = ["Summer"; "loving"; "happened"; "so"; "fast"] in
  let messages_map =
    Rollup_messages_map.empty
    |> Rollup_messages_map.add sc_rollup_1 messages_rollup_1
    |> Rollup_messages_map.add sc_rollup_2 messages_rollup_2
  in
  let actual_size = V0.expected_slot_size messages_map in
  let* serialized = V0.serialize ~max_size:actual_size messages_map in
  let max_size = actual_size - 1 in
  assert_fails_with
    ~loc:__LOC__
    (V0.deserialize ~max_size serialized)
    (Dal_slot_frame_encoding.Slot_size_is_too_big {actual_size; max_size})

let slot_frame_decoding_fails_if_wrong_version () =
  let open Lwt_result_syntax in
  let messages_rollup_1 = ["Summer"; "loving"; "had"; "me"; "a"; "blast"] in
  let messages_rollup_2 = ["Summer"; "loving"; "happened"; "so"; "fast"] in
  let messages_map =
    Rollup_messages_map.empty
    |> Rollup_messages_map.add sc_rollup_1 messages_rollup_1
    |> Rollup_messages_map.add sc_rollup_2 messages_rollup_2
  in
  let* serialized =
    Dal_slot_frame_encoding.V0.serialize ~max_size messages_map
  in
  let serialized_wrong_version =
    "\001" ^ String.sub serialized 1 (String.length serialized - 1)
  in
  assert_fails_with
    ~loc:__LOC__
    (V0.deserialize ~max_size serialized_wrong_version)
    (Dal_slot_frame_encoding.Wrong_slot_frame_version
       {expected = 0; provided = 1})

let slot_frame_encoding_correct_offsets () =
  let open Lwt_result_syntax in
  let messages_rollup_1 = ["hello"; "world"] in
  let messages_rollup_2 = ["CAFEBABE"; "CAFEDEAD"] in
  let messages_map =
    Rollup_messages_map.empty
    |> Rollup_messages_map.add sc_rollup_1 messages_rollup_1
    |> Rollup_messages_map.add sc_rollup_2 messages_rollup_2
  in
  let* serialized = V0.serialize ~max_size messages_map in
  (* the value of the offset that denotes where the messages frame for
     sc_rollup_1 starts can be found at offset 25 until 29 (excluded):
     1 byte for version +
     4 bytes for rollups frame prefix +
     20 bytes for rollup address = 25.
  *)
  let first_offset =
    String.sub serialized 25 4 |> Data_encoding.(Binary.of_string_exn int32)
  in
  (* the value of the offset should be 57l:
     1 byte for version number + 4 bytes for rollups frame prefix +
     2 * 24 bytes for rollups frame +
     4 bytes for messages frames prefix = 57. *)
  let* () = Assert.equal_int32 ~loc:__LOC__ first_offset 57l in
  (* The length of the first messages frame should be 22 bytes:
     4 bytes for the messages frame prefix +
     4 + 5 bytes for the encoding of the message "hello" +
     4 + 5 bytes for the encoding of the message "world" = 22.
  *)
  let first_messages_frame =
    Data_encoding.(Binary.of_string_exn @@ list string)
    @@ String.sub serialized 57 22
  in
  let* () =
    assert_equal_list_string
      ~loc:__LOC__
      "Messages frame for sc_rollup_1 is not as expected"
      first_messages_frame
      ["hello"; "world"]
  in
  (* The value of the offset that denotes where the messages frame for
     sc_rollup_2 STARTS can be found at bytes 49 until 53 (excluded):
     29 offset where the entry for sc_rollup_2 starts +
     20 bytes for the encoding of sc_rollup_2 = 49.
  *)
  let second_offset =
    String.sub serialized 49 4 |> Data_encoding.(Binary.of_string_exn int32)
  in
  (* the value of the second offset should be 79
     57 offset where the messages frame for sc_rollup_1 starts +
     22 bytes length of the first messages frame = 79
  *)
  let* () = Assert.equal_int32 ~loc:__LOC__ second_offset 79l in
  (* The length of the first messages frame should be 28:
     4 bytes for the messages frame prefix +
     4 + 8 bytes for the encoding of the message "CAFEBABE" +
     4 + 8 bytes for the encoding of the message "CAFEDEAD" = 28
  *)
  let second_messages_frame =
    Data_encoding.(Binary.of_string_exn @@ list string)
    @@ String.sub serialized 79 28
  in
  assert_equal_list_string
    ~loc:__LOC__
    "Messages frame for sc_rollup_1 is not as expected"
    second_messages_frame
    ["CAFEBABE"; "CAFEDEAD"]

let tests =
  [
    Tztest.tztest
      "Encoded slot has expected size (V0, 1 rollup)"
      `Quick
      slot_frame_encoding_size_correct_single_v0;
    Tztest.tztest
      "Encoded slot has expected size (V0, 2 rollups)"
      `Quick
      slot_frame_encoding_size_correct_multiple_v0;
    Tztest.tztest
      "Encoded slot can be decoded (V0, 1 rollup)"
      `Quick
      slot_frame_encoding_decoding_correct_single_v0;
    Tztest.tztest
      "Encoded slot can be decoded (V0, 2 rollups)"
      `Quick
      slot_frame_encoding_decoding_correct_multiple_v0;
    Tztest.tztest
      "Encoding of a slot over maximum size fails (V0)"
      `Quick
      slot_frame_encoding_fails_if_too_big;
    Tztest.tztest
      "Offsets of messages frames are correct (V0)"
      `Quick
      slot_frame_encoding_correct_offsets;
    Tztest.tztest
      "Slot decoding fails when slot size is too big (V0)"
      `Quick
      slot_frame_decoding_fails_if_too_big;
    Tztest.tztest
      "Slot decoding fails when first byte has wrong version (V0)"
      `Quick
      slot_frame_decoding_fails_if_wrong_version;
  ]

let () =
  Alcotest_lwt.run
    "protocol > unit"
    [
      Test_helpers.Unit_test.spec
        (Protocol.name ^ ": Slot_framing_protocol.ml")
        tests;
    ]
  |> Lwt_main.run
