(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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
    Component:    lib_scoru_sequencer kernel encodings
    Invocation:   dune exec src/lib_scoru_sequencer/test/main.exe -- --file test_kernel_message.ml
    Subject:      Tests kernel message encodings for the octez-smart-rollup-sequencer library
*)

open Protocol
open Alpha_context
open Tztest
module KM = Octez_smart_rollup_sequencer.Kernel_message

let test_signed ?loc expected_hex encoded_msg =
  let open Lwt_result_syntax in
  let result_hex = Hex.show @@ Hex.of_string encoded_msg in
  Assert.equal ?loc ~msg:"Encoded hex don't match" expected_hex result_hex ;
  return_unit

let test_empty_suffix_n_prefix () =
  let open Lwt_result_syntax in
  let rollup_address =
    Sc_rollup_repr.Address.of_b58check_exn
      "sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb"
  in

  let empty_l2_messages_hex =
    "006227a8721213bd7ddb9b56227e3acb01161b1e67000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
  in
  let* () =
    test_signed
      ~loc:__LOC__
      empty_l2_messages_hex
      (KM.encode_sequence_message rollup_address ~prefix:0l ~suffix:0l [])
  in

  let single_empty_l2_message_hex =
    "006227a8721213bd7ddb9b56227e3acb01161b1e6700000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
  in
  let* () =
    test_signed
      ~loc:__LOC__
      single_empty_l2_message_hex
      (KM.encode_sequence_message
         rollup_address
         ~prefix:0l
         ~suffix:0l
         [Sc_rollup.Inbox_message.unsafe_of_string ""])
  in

  let hello_l2_message_hex =
    "006227a8721213bd7ddb9b56227e3acb01161b1e6700000000000000000000000000000000090000000568656c6c6f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
  in
  let* () =
    test_signed
      ~loc:__LOC__
      hello_l2_message_hex
      (KM.encode_sequence_message
         rollup_address
         ~prefix:0l
         ~suffix:0l
         [Sc_rollup.Inbox_message.unsafe_of_string "hello"])
  in

  let hello_world_l2_message_hex =
    "006227a8721213bd7ddb9b56227e3acb01161b1e6700000000000000000000000000000000120000000568656c6c6f00000005776f726c6400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
  in
  test_signed
    ~loc:__LOC__
    hello_world_l2_message_hex
    (KM.encode_sequence_message
       rollup_address
       ~prefix:0l
       ~suffix:0l
       (List.map Sc_rollup.Inbox_message.unsafe_of_string ["hello"; "world"]))

let tests =
  [
    tztest
      "Sequence with empty prefix and suffix"
      `Quick
      test_empty_suffix_n_prefix;
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib scoru sequencer message encodings"
    [("Encodings", tests)]
  |> Lwt_main.run
