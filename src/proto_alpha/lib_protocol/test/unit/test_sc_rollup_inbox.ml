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

let opt_get ~__LOC__ = WithExceptions.Option.get ~loc:__LOC__

open Sc_rollup_helpers

module Merkelized_payload_hashes =
  Alpha_context.Sc_rollup.Inbox_merkelized_payload_hashes

module Message = Alpha_context.Sc_rollup.Inbox_message
module Inbox = Alpha_context.Sc_rollup.Inbox
open Alpha_context

let assert_equal_payload ~__LOC__ found (expected : Message.serialized) =
  Assert.equal_string
    ~loc:__LOC__
    (Message.unsafe_to_string expected)
    (Message.unsafe_to_string found)

let assert_equal_payload_option ~__LOC__ found expected =
  Assert.equal
    ~loc:__LOC__
    (Option.equal (fun v1 v2 ->
         String.equal
           (Message.unsafe_to_string v1)
           (Message.unsafe_to_string v2)))
    "Input returned by the production is not the expected one"
    (Format.pp_print_option
       ~none:(fun fmt () -> Format.pp_print_string fmt "None")
       (fun fmt v ->
         Format.fprintf fmt "Some \"%s\"" (Message.unsafe_to_string v)))
    found
    expected

let assert_inbox_message ~__LOC__ found expected =
  Assert.equal
    ~loc:__LOC__
    (Option.equal Sc_rollup.inbox_message_equal)
    "Input returns by the production is not the expected one"
    (Format.pp_print_option Sc_rollup.pp_inbox_message)
    found
    expected

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

let assert_merkelized_payload_proof_error ~__LOC__ expected_msg result =
  Assert.error ~loc:__LOC__ (Environment.wrap_tzresult result) (function
      | Environment.Ecoproto_error
          (Sc_rollup_inbox_merkelized_payload_hashes_repr
           .Merkelized_payload_hashes_proof_error msg) ->
          expected_msg = msg
      | _ -> false)

let assert_equal_history_proof ~__LOC__ found expected =
  Assert.equal
    ~loc:__LOC__
    Inbox.equal_history_proof
    "history_proof are not equal"
    Inbox.pp_history_proof
    expected
    found

let assert_inbox_proof_error ~__LOC__ expected_msg result =
  Assert.error ~loc:__LOC__ (Environment.wrap_tzresult result) (function
      | Environment.Ecoproto_error (Sc_rollup_inbox_repr.Inbox_proof_error msg)
        ->
          expected_msg = msg
      | _ -> false)

let gen_payload_size = QCheck2.Gen.(1 -- 10)

let gen_payload_string =
  let open QCheck2.Gen in
  string_size gen_payload_size

let gen_payload =
  let open QCheck2.Gen in
  let+ payload = gen_payload_string in
  Message.unsafe_of_string payload

let gen_payloads ?(min_size = 2) ?(max_size = 50) () =
  let open QCheck2.Gen in
  list_size (min_size -- max_size) gen_payload

let gen_index_of_payloads ?(max_index_offset = 0) payloads =
  let open QCheck2.Gen in
  let max_index = List.length payloads - 1 - max_index_offset in
  let+ index = 0 -- max_index in
  Z.of_int index

let gen_payloads_and_index ?min_size ?max_size ?max_index_offset () =
  let open QCheck2.Gen in
  let* payloads = gen_payloads ?min_size ?max_size () in
  let* index = gen_index_of_payloads ?max_index_offset payloads in
  return (payloads, index)

let gen_payloads_for_level ?(inbox_level = Raw_level.(succ root)) () =
  gen_messages inbox_level gen_payload_string

let gen_level ?(inbox_creation_level = 0) ~max_level () =
  let open QCheck2.Gen in
  let+ int_level = inbox_creation_level -- max_level in
  Raw_level.of_int32_exn (Int32.of_int int_level)

let gen_payloads_and_level ?inbox_creation_level ?(max_level = 100_000) () =
  let open QCheck2.Gen in
  let* inbox_level = gen_level ?inbox_creation_level ~max_level () in
  gen_payloads_for_level ~inbox_level ()

let gen_payloads_and_level_and_index ?inbox_creation_level ?max_level () =
  let open QCheck2.Gen in
  let* payloads_for_level =
    gen_payloads_and_level ?inbox_creation_level ?max_level ()
  in
  let* index = gen_index_of_payloads payloads_for_level.messages in
  return (payloads_for_level, index)

let gen_payloads_for_levels ?(inbox_creation_level = 0) ?(max_level = 15) () =
  gen_payloads_for_levels
    ~start_level:(inbox_creation_level + 1)
    ~max_level
    gen_payload_string

let gen_payloads_for_levels_and_level ?inbox_creation_level ?max_level
    ?(level_offset = 1) () =
  let open QCheck2.Gen in
  let* payloads_for_levels =
    gen_payloads_for_levels ?inbox_creation_level ?max_level ()
  in
  let* payloads_index =
    0 -- (List.length payloads_for_levels - 1 - level_offset)
  in
  let payloads =
    opt_get ~__LOC__ @@ List.nth payloads_for_levels payloads_index
  in
  return (payloads_for_levels, payloads.level)

let gen_level_and_index payloads_for_levels =
  let open QCheck2.Gen in
  let* payloads_index = 0 -- (List.length payloads_for_levels - 1) in
  let payloads =
    opt_get ~__LOC__ @@ List.nth payloads_for_levels payloads_index
  in
  let* index = gen_index_of_payloads payloads.Sc_rollup_helpers.inputs in
  return (payloads.level, index)

let gen_payloads_for_levels_and_level_and_index ?inbox_creation_level ?max_level
    () =
  let open QCheck2.Gen in
  let* payloads_for_levels =
    gen_payloads_for_levels ?inbox_creation_level ?max_level ()
  in
  let* level, index = gen_level_and_index payloads_for_levels in
  return (payloads_for_levels, level, index)

let gen_payloads_for_levels_and_two_levels_and_two_indexes ?inbox_creation_level
    ?max_level () =
  let open QCheck2.Gen in
  let* payloads_for_levels =
    gen_payloads_for_levels ?inbox_creation_level ?max_level ()
  in
  let* level, index = gen_level_and_index payloads_for_levels in
  let* level', index' = gen_level_and_index payloads_for_levels in
  return (payloads_for_levels, level, index, level', index')

let fill_merkelized_payload history payloads =
  let open Lwt_result_syntax in
  let* first, payloads =
    match payloads with
    | x :: xs -> return (x, xs)
    | [] -> failwith "empty payloads"
  in
  let*? history, merkelized_payload =
    Environment.wrap_tzresult @@ Merkelized_payload_hashes.genesis history first
  in

  Lwt.return @@ Environment.wrap_tzresult
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
        opt_get ~__LOC__
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
    opt_get ~__LOC__
    @@ Merkelized_payload_hashes.produce_proof history ~index merkelized_payload
  in
  let payload : Message.serialized =
    opt_get ~__LOC__ @@ List.nth payloads (Z.to_int index)
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
    Environment.wrap_tzresult @@ Merkelized_payload_hashes.verify_proof proof
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

(* Test multiple cases of invalid proof. This test is more about testing the
   skip list than testing merkelized payload. But it was easier to test with a
   skip-list with hashes as pointer. *)
let test_invalid_merkelized_payload_hashes_proof_fails (payloads, index) =
  let open Lwt_result_wrap_syntax in
  let make_proof = Merkelized_payload_hashes.Internal_for_tests.make_proof in
  let hd ~__LOC__ l = List.hd l |> opt_get ~__LOC__ in
  let tl ~__LOC__ l = List.tl l |> opt_get ~__LOC__ in
  let nth ~__LOC__ idx l = List.nth idx l |> opt_get ~__LOC__ in
  let* history, merkelized_payload_hash =
    construct_merkelized_payload_hashes payloads
  in
  let Merkelized_payload_hashes.{merkelized = _target; _}, proof =
    opt_get ~__LOC__
    @@ Merkelized_payload_hashes.produce_proof
         history
         ~index
         merkelized_payload_hash
  in
  let proof :> Merkelized_payload_hashes.t list = proof in
  (* We need a proof of more than 3 elements otherwise some tests does not make
     sense after. *)
  QCheck2.assume Compare.List_length_with.(proof > 3) ;
  let proof_len = List.length proof in
  let payload = Message.unsafe_of_string "I'm a disruptive payload" in
  let payloads' = payload :: payloads in
  let* history', merkelized_payload' =
    construct_merkelized_payload_hashes payloads'
  in
  let Merkelized_payload_hashes.{merkelized = target'; payload = _}, proof' =
    opt_get ~__LOC__
    @@ Merkelized_payload_hashes.produce_proof
         history'
         ~index
         merkelized_payload'
  in
  let proof' :> Merkelized_payload_hashes.t list = proof' in
  let proof_with_invalid_target =
    (* change the target cell. *)
    let rest = List.rev proof |> tl ~__LOC__ in
    make_proof @@ List.rev (target' :: rest)
  in
  let proof_with_invalid_cell =
    (* change the latest cell. *)
    let cell = proof' |> hd ~__LOC__ in
    let rest = proof |> tl ~__LOC__ in
    make_proof @@ (cell :: rest)
  in
  let proof_with_only_cell_and_target =
    let cell = proof |> hd ~__LOC__ in
    let target = List.rev proof |> hd ~__LOC__ in
    make_proof @@ [cell; target]
  in
  let proof_with_invalid_cell_in_path =
    let idx = proof_len / 2 in
    let rev_prefix, suffix = List.rev_split_n idx proof in
    let new_cell = nth ~__LOC__ proof' idx in
    let prefix = new_cell :: tl ~__LOC__ rev_prefix |> List.rev in
    make_proof @@ prefix @ suffix
  in
  let proof_with_missing_cell =
    let idx = proof_len / 2 in
    let rev_prefix, suffix = List.rev_split_n idx proof in
    let prefix = tl ~__LOC__ rev_prefix |> List.rev in
    make_proof @@ prefix @ suffix
  in
  let proof_with_extra_step =
    let idx = proof_len / 2 in
    let rev_prefix, suffix = List.rev_split_n idx proof in
    let new_cell = nth ~__LOC__ proof' idx in
    let prefix =
      match rev_prefix with
      | cell :: rest -> List.rev (new_cell :: cell :: rest)
      | _ -> assert false
    in
    make_proof @@ prefix @ suffix
  in
  let assert_fails ~__LOC__ proof =
    let res = Merkelized_payload_hashes.verify_proof proof in
    assert_merkelized_payload_proof_error ~__LOC__ "invalid proof" res
  in
  let* () = assert_fails ~__LOC__ proof_with_missing_cell in
  let* () = assert_fails ~__LOC__ proof_with_invalid_cell_in_path in
  let* () = assert_fails ~__LOC__ proof_with_invalid_target in
  let* () = assert_fails ~__LOC__ proof_with_invalid_cell in
  let* () = assert_fails ~__LOC__ proof_with_only_cell_and_target in
  let* () = assert_fails ~__LOC__ proof_with_extra_step in
  return_unit

let test_inclusion_proof_production (payloads_for_levels, level) =
  let open Lwt_result_wrap_syntax in
  let*? node_inbox, proto_inbox =
    construct_node_and_protocol_inbox
      ~inbox_creation_level:Raw_level.root
      payloads_for_levels
  in
  let node_inbox_snapshot = Inbox.old_levels_messages node_inbox.inbox in
  let* proof, node_old_level_messages =
    Node_inbox.produce_inclusion_proof node_inbox node_inbox_snapshot level
  in
  let proto_inbox_snapshot = Inbox.take_snapshot proto_inbox in
  let*? verified_old_levels_messages =
    Environment.wrap_tzresult
    @@ Inbox.Internal_for_tests.verify_inclusion_proof
         proof
         proto_inbox_snapshot
  in
  assert_equal_history_proof
    ~__LOC__
    verified_old_levels_messages
    node_old_level_messages

let test_inclusion_proof_verification (payloads_for_levels, level) =
  let open Lwt_result_wrap_syntax in
  let*? node_inbox, proto_inbox =
    construct_node_and_protocol_inbox
      ~inbox_creation_level:Raw_level.root
      payloads_for_levels
  in
  let node_inbox_snapshot = Inbox.old_levels_messages node_inbox.inbox in
  let* proof, _node_old_level_messages =
    Node_inbox.produce_inclusion_proof node_inbox node_inbox_snapshot level
  in
  let*? proto_inbox = Protocol_inbox.add_new_level proto_inbox [] in
  (* This snapshot is not the same one as node_inbox_snapshot because we
     added an empty level. *)
  let proto_inbox_snapshot = Inbox.take_snapshot proto_inbox in
  let result =
    Inbox.Internal_for_tests.verify_inclusion_proof proof proto_inbox_snapshot
  in
  assert_inbox_proof_error ~__LOC__ "invalid inclusion proof" result

let test_inbox_proof_production (payloads_for_levels, level, message_counter) =
  let open Lwt_result_wrap_syntax in
  let exp_message = first_after payloads_for_levels level message_counter in
  let*? node_inbox, proto_inbox =
    construct_node_and_protocol_inbox
      ~inbox_creation_level:Raw_level.root
      payloads_for_levels
  in
  let node_inbox_snapshot = Inbox.take_snapshot node_inbox.inbox in
  let* proof, input_of_produced_proof =
    Node_inbox.produce_proof
      node_inbox
      node_inbox_snapshot
      (level, message_counter)
  in
  let proto_inbox_snapshot = Inbox.take_snapshot proto_inbox in
  let* () =
    assert_equal_history_proof ~__LOC__ node_inbox_snapshot proto_inbox_snapshot
  in
  let*? input_in_proof =
    Environment.wrap_tzresult
    @@ Inbox.verify_proof (level, message_counter) proto_inbox_snapshot proof
  in
  let* () =
    assert_inbox_message ~__LOC__ input_of_produced_proof input_in_proof
  in
  assert_inbox_message ~__LOC__ exp_message input_in_proof

let test_inbox_proof_verification (payloads_for_levels, level, message_counter)
    =
  let open Lwt_result_wrap_syntax in
  let*? node_inbox, proto_inbox =
    construct_node_and_protocol_inbox
      ~inbox_creation_level:Raw_level.root
      payloads_for_levels
  in
  let node_inbox_snapshot = Inbox.old_levels_messages node_inbox.inbox in
  let* proof, _input =
    Node_inbox.produce_proof
      node_inbox
      node_inbox_snapshot
      (level, message_counter)
  in
  let proto_inbox_snapshot = Inbox.take_snapshot proto_inbox in
  (* The proto and node inboxes are synchronized, we make sure the verification
     refuses the invalid message index. *)
  let invalid_message_counter =
    if Z.(equal message_counter zero) then Z.succ message_counter
    else Z.pred message_counter
  in
  let* () =
    let result =
      Inbox.verify_proof
        (level, invalid_message_counter)
        proto_inbox_snapshot
        proof
    in
    assert_inbox_proof_error
      ~__LOC__
      "found index in message_proof is incorrect"
      result
  in

  (* We move the inbox one level forward so the inbox's inclusion proof fails. *)
  let*? proto_inbox = Protocol_inbox.add_new_level proto_inbox [] in
  let proto_inbox_snapshot = Inbox.take_snapshot proto_inbox in
  let* () =
    let result =
      Inbox.verify_proof (level, message_counter) proto_inbox_snapshot proof
    in
    assert_inbox_proof_error ~__LOC__ "invalid inclusion proof" result
  in

  return_unit

let test_messages_are_correctly_added_in_history
    {predecessor_timestamp; predecessor; messages; _} =
  let open Lwt_result_syntax in
  let inbox = dumb_init Raw_level.root in
  let messages = List.map (fun message -> Message.External message) messages in
  let*? payloads_history, _history, _inbox, witness, messages =
    Environment.wrap_tzresult
    @@ Inbox.add_all_messages
         ~predecessor_timestamp
         ~predecessor
         (Inbox.History.empty ~capacity:0L)
         inbox
         messages
  in
  List.iteri_es
    (fun i message ->
      let index = Z.of_int i in
      let*? expected_payload =
        Environment.wrap_tzresult @@ Message.serialize message
      in
      let expected_hash = Message.hash_serialized_message expected_payload in
      let found_merkelized_opt =
        Sc_rollup.Inbox_merkelized_payload_hashes.Internal_for_tests
        .find_predecessor_payload
          payloads_history
          ~index
          witness
      in
      let* found_hash =
        match found_merkelized_opt with
        | Some x ->
            return
              (Sc_rollup.Inbox_merkelized_payload_hashes.get_payload_hash x)
        | None ->
            failwith
              "The payload was not found in the payloads_history, this is \
               unexpected"
      in
      Assert.equal
        ~loc:__LOC__
        Message.Hash.equal
        "The message was not correctly added to the payloads history"
        Message.Hash.pp
        expected_hash
        found_hash)
    messages

let merkelized_payload_hashes_tests =
  [
    Tztest.tztest_qcheck2
      ~count:1000
      ~name:
        "Add payloads to merkelized payload hashes then retrieve them from \
         history."
      (gen_payloads ())
      test_merkelized_payload_hashes_history;
    Tztest.tztest_qcheck2
      ~count:1000
      ~name:"Produce a merkelized payload hashes proof and verify its validity."
      (gen_payloads_and_index ())
      test_merkelized_payload_hashes_proof;
    Tztest.tztest_qcheck2
      ~count:1000
      ~name:"Invalid merkelized payload hashes proof fails."
      (gen_payloads_and_index
         ~min_size:20
         ~max_size:100
         ~max_index_offset:10
         ())
      test_invalid_merkelized_payload_hashes_proof_fails;
  ]

let inbox_tests =
  [
    Tztest.tztest_qcheck2
      ~count:1000
      ~name:"produce inclusion proof and verifies it."
      (gen_payloads_for_levels_and_level ())
      test_inclusion_proof_production;
    Tztest.tztest_qcheck2
      ~count:1000
      ~name:"negative test of inclusion proof."
      (gen_payloads_for_levels_and_level ())
      test_inclusion_proof_verification;
    Tztest.tztest_qcheck2
      ~count:1000
      ~name:"produce inbox proof and verifies it."
      (gen_payloads_for_levels_and_level_and_index ())
      test_inbox_proof_production;
    Tztest.tztest_qcheck2
      ~count:1000
      ~name:"negative test of inbox proof."
      (gen_payloads_for_levels_and_level_and_index ())
      test_inbox_proof_verification;
    Tztest.tztest_qcheck2
      ~count:1000
      ~name:"messages are correctly added in payloads history"
      (gen_payloads_for_level ())
      test_messages_are_correctly_added_in_history;
  ]

let tests =
  merkelized_payload_hashes_tests @ inbox_tests
  @ Test_sc_rollup_inbox_legacy.tests
