(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Baker, operation worker (pre)quorum accounting
    Invocation:   dune exec src/proto_025_PsUshuai/lib_delegate/test/main.exe \
                   -- --file test_preattestation_quorum_amplification.ml
    Subject:      The quorum monitor must count each delegate's voting power
                  exactly once, and every distinct delegate that votes, across
                  one or several monitor updates.

                  Regression tests for the P-256 quorum-amplification finding:
                  several signature-distinct copies of the same vote in one
                  update must count once (no inflation), while
                  distinct delegates in the same update must all be counted (no
                  loss), and a vote already counted in a previous update must not
                  be counted again.
*)

module Alpha_context = Protocol.Alpha_context
open Alpha_context

let proto_tag =
  String.lowercase_ascii Protocol.name
  |> String.map (function '-' -> '_' | x -> x)

(** Hack to obtain distinct block hashes, see
    [test_dal_included_attestations_cache.ml]. Encodes [n]'s low 16 bits into a
    32-byte zero buffer. *)
let make_block_hash n =
  let bytes = Bytes.make 32 '\000' in
  Bytes.set_int8 bytes 0 (n land 0xFF) ;
  Bytes.set_int8 bytes 1 ((n lsr 8) land 0xFF) ;
  Block_hash.of_bytes_exn bytes

let slot_exn i = match Slot.of_int i with Ok s -> s | Error _ -> assert false

let level = Raw_level.of_int32_exn 42l

let round = Round.zero

let payload_hash = Protocol.Block_payload_hash.zero

(* Every slot carries the same voting power, so counts and powers stay in
   lockstep and are easy to predict. *)
let slot_power = 100L

let get_slot_voting_power ~slot =
  ignore slot ;
  Some slot_power

let candidate : Operation_worker.candidate =
  {
    hash = make_block_hash 0;
    level_watched = Raw_level.to_int32 level;
    round_watched = round;
    payload_hash_watched = payload_hash;
    branch_watched = None;
  }

let consensus_content ~slot : consensus_content =
  {slot = slot_exn slot; level; round; block_payload_hash = payload_hash}

(* Two votes for the same [slot] but a different [branch] are structurally
   distinct operations that the semantic comparator treats as equal (it ignores
   the branch, just as it ignores the signature that differs between real P-256
   copies). Votes for different [slot]s are distinct delegates. *)
let make_preattestation ~slot ~branch : Kind.preattestation operation =
  {
    shell = {branch = make_block_hash branch};
    protocol_data =
      {
        contents = Single (Preattestation (consensus_content ~slot));
        signature = None;
      };
  }

let make_attestation ~slot ~branch : Kind.attestation operation =
  {
    shell = {branch = make_block_hash branch};
    protocol_data =
      {
        contents =
          Single
            (Attestation
               {consensus_content = consensus_content ~slot; dal_content = None});
        signature = None;
      };
  }

(* [make_op] and [account] are the only kind-specific pieces; the three scenarios
   below are shared between preattestations and attestations. *)
let check_accounting ~make_op ~account =
  let power_of n = Int64.mul slot_power (Int64.of_int n) in
  let expect ?(candidate = candidate) ~scenario ~batches ~expected_count () =
    let voting_power, count, certificate =
      account ~get_slot_voting_power candidate batches
    in
    if
      count <> expected_count
      || voting_power <> power_of expected_count
      || List.compare_length_with certificate expected_count <> 0
    then
      Test.fail
        "%s: got count=%d voting_power=%Ld certificate_size=%d; expected \
         count=%d voting_power=%Ld"
        scenario
        count
        voting_power
        (List.length certificate)
        expected_count
        (power_of expected_count)
  in
  (* Five signature-distinct copies of one delegate's vote count once. *)
  expect
    ~scenario:"duplicates in one update"
    ~batches:[Stdlib.List.init 5 (fun i -> make_op ~slot:0 ~branch:(i + 1))]
    ~expected_count:1
    () ;
  (* Three distinct delegates in one update are all counted. *)
  expect
    ~scenario:"distinct delegates in one update"
    ~batches:
      [
        [
          make_op ~slot:0 ~branch:1;
          make_op ~slot:1 ~branch:2;
          make_op ~slot:2 ~branch:3;
        ];
      ]
    ~expected_count:3
    () ;
  (* A vote already counted in a previous update is not counted again, and fresh
     distinct delegates in the second update are added. *)
  expect
    ~scenario:"repeated votes across updates"
    ~batches:
      [
        [make_op ~slot:0 ~branch:1; make_op ~slot:1 ~branch:2];
        [
          make_op ~slot:2 ~branch:3;
          make_op ~slot:0 ~branch:4;
          make_op ~slot:1 ~branch:5;
        ];
      ]
    ~expected_count:3
    () ;
  (* When the candidate watches a branch, a copy of a vote on another branch is
     ineligible and must not shadow a valid copy of the same vote in the same
     update, whatever their order. *)
  let candidate = {candidate with branch_watched = Some (make_block_hash 2)} in
  expect
    ~candidate
    ~scenario:"ineligible branch copy before valid one"
    ~batches:[[make_op ~slot:0 ~branch:1; make_op ~slot:0 ~branch:2]]
    ~expected_count:1
    () ;
  expect
    ~candidate
    ~scenario:"ineligible branch copy after valid one"
    ~batches:[[make_op ~slot:0 ~branch:2; make_op ~slot:0 ~branch:1]]
    ~expected_count:1
    () ;
  Lwt.return_unit

let test_preattestation_accounting () =
  check_accounting
    ~make_op:make_preattestation
    ~account:Operation_worker.Internal_for_tests.account_preattestation_quorum

let test_attestation_accounting () =
  check_accounting
    ~make_op:make_attestation
    ~account:Operation_worker.Internal_for_tests.account_attestation_quorum

let () =
  Test.register
    ~__FILE__
    ~title:
      (Protocol.name ^ ": preattestation voting power counted once per delegate")
    ~tags:
      [
        proto_tag;
        "baker";
        "operation_worker";
        "prequorum";
        "quorum_amplification";
        "security";
      ]
    test_preattestation_accounting ;
  Test.register
    ~__FILE__
    ~title:
      (Protocol.name ^ ": attestation voting power counted once per delegate")
    ~tags:
      [
        proto_tag;
        "baker";
        "operation_worker";
        "quorum";
        "quorum_amplification";
        "security";
      ]
    test_attestation_accounting
