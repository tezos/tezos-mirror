(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:  Protocol (smart contract rollup inbox)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "^\[Unit\] sc rollup inbox$"
    Subject:    These unit tests check the off-line inbox implementation for
                smart contract rollups
*)
open Protocol

let lift k = Environment.wrap_tzresult k

module Merkelized_payload_hashes =
  Alpha_context.Sc_rollup.Inbox_merkelized_payload_hashes

module Message = Alpha_context.Sc_rollup.Inbox_message

let assert_equal_payload ~__LOC__ found (expected : Message.serialized) =
  Assert.equal_string
    ~loc:__LOC__
    (Message.unsafe_to_string expected)
    (Message.unsafe_to_string found)

let assert_equal_payload_hash ~__LOC__ found expected =
  Assert.equal
    ~loc:__LOC__
    Message.Hash.equal
    "Protocol hashes aren't equal"
    Message.Hash.pp
    expected
    found

let assert_merkelized_payload ~__LOC__ ~payload_hash ~index found =
  let open Lwt_result_syntax in
  let found_payload_hash = Merkelized_payload_hashes.get_payload_hash found in
  let found_index = Merkelized_payload_hashes.get_index found in
  let* () =
    assert_equal_payload_hash ~__LOC__ found_payload_hash payload_hash
  in
  Assert.equal_z ~loc:__LOC__ found_index index

let assert_equal_merkelized_payload ~__LOC__ ~found ~expected =
  let payload_hash = Merkelized_payload_hashes.get_payload_hash expected in
  let index = Merkelized_payload_hashes.get_index expected in
  assert_merkelized_payload ~__LOC__ ~payload_hash ~index found

let gen_payload_size = QCheck2.Gen.(1 -- 10)

let gen_payload =
  let open QCheck2.Gen in
  let+ payload = string_size gen_payload_size in
  Message.unsafe_of_string payload

let gen_payloads =
  let open QCheck2.Gen in
  list_size (2 -- 50) gen_payload

let gen_index payloads =
  let open QCheck2.Gen in
  let max_index = List.length payloads - 1 in
  let+ index = 0 -- max_index in
  Z.of_int index

let gen_payloads_and_index =
  let open QCheck2.Gen in
  let* payloads = gen_payloads in
  let* index = gen_index payloads in
  return (payloads, index)

let gen_payloads_and_two_index =
  let open QCheck2.Gen in
  let* payloads = gen_payloads in
  let* index = gen_index payloads in
  let* index' = gen_index payloads in
  return (payloads, index, index')

let fill_merkelized_payload history payloads =
  let open Lwt_result_syntax in
  let* first, payloads =
    match payloads with
    | x :: xs -> return (x, xs)
    | [] -> failwith "empty payloads"
  in
  let*? history, merkelized_payload =
    lift @@ Merkelized_payload_hashes.genesis history first
  in
  Lwt.return @@ lift
  @@ List.fold_left_e
       (fun (history, payloads) payload ->
         Merkelized_payload_hashes.add_payload history payloads payload)
       (history, merkelized_payload)
       payloads

let construct_merkelized_payload_hashes payloads =
  let history = Merkelized_payload_hashes.History.empty ~capacity:1000L in
  fill_merkelized_payload history payloads

let test_merkelized_payload_hashes_history payloads =
  let open Lwt_result_syntax in
  let nb_payloads = List.length payloads in
  let* history, merkelized_payloads =
    construct_merkelized_payload_hashes payloads
  in
  let* () =
    Assert.equal_z
      ~loc:__LOC__
      (Z.of_int nb_payloads)
      (Z.succ (Merkelized_payload_hashes.get_index merkelized_payloads))
  in
  List.iteri_es
    (fun index (expected_payload : Message.serialized) ->
      let expected_payload_hash =
        Message.hash_serialized_message expected_payload
      in
      let found_merkelized_payload =
        WithExceptions.Option.get ~loc:__LOC__
        @@ Merkelized_payload_hashes.Internal_for_tests.find_predecessor_payload
             history
             ~index:(Z.of_int index)
             merkelized_payloads
      in
      let found_payload_hash =
        Merkelized_payload_hashes.get_payload_hash found_merkelized_payload
      in
      assert_equal_payload_hash
        ~__LOC__
        found_payload_hash
        expected_payload_hash)
    payloads

let test_merkelized_payload_hashes_proof (payloads, index) =
  let open Lwt_result_syntax in
  let* history, merkelized_payload =
    construct_merkelized_payload_hashes payloads
  in
  let ( Merkelized_payload_hashes.
          {merkelized = target_merkelized_payload; payload = proof_payload},
        proof ) =
    WithExceptions.Option.get ~loc:__LOC__
    @@ Merkelized_payload_hashes.produce_proof history ~index merkelized_payload
  in
  let payload : Message.serialized =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth payloads (Z.to_int index)
  in
  let payload_hash = Message.hash_serialized_message payload in
  let* () = assert_equal_payload ~__LOC__ proof_payload payload in
  let* () =
    assert_merkelized_payload
      ~__LOC__
      ~index
      ~payload_hash
      target_merkelized_payload
  in
  let*? proof_ancestor_merkelized, proof_current_merkelized =
    lift @@ Merkelized_payload_hashes.verify_proof proof
  in
  let* () =
    assert_equal_merkelized_payload
      ~__LOC__
      ~found:proof_ancestor_merkelized
      ~expected:target_merkelized_payload
  in
  let* () =
    assert_equal_merkelized_payload
      ~__LOC__
      ~found:proof_current_merkelized
      ~expected:merkelized_payload
  in
  return_unit

let merkelized_payload_hashes_tests =
  [
    Tztest.tztest_qcheck2
      ~count:1000
      ~name:"Merkelized messages: Add messages then retrieve them from history."
      gen_payloads
      test_merkelized_payload_hashes_history;
    Tztest.tztest_qcheck2
      ~count:1000
      ~name:"Merkelized messages: Produce proof and verify its validity."
      gen_payloads_and_index
      test_merkelized_payload_hashes_proof;
  ]

let tests = merkelized_payload_hashes_tests @ Test_sc_rollup_inbox_legacy.tests
