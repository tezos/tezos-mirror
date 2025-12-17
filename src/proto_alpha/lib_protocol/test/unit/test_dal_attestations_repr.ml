(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (dal attestations representation)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_dal_attestations_repr.ml
    Subject:    These unit tests check Dal_attestations_repr encoding and operations.
*)

open Protocol

let number_of_slots = 32

let number_of_lags = 4

(** Helper to create a slot index from an int *)
let slot_index_of_int i =
  match Dal_slot_index_repr.of_int ~number_of_slots i with
  | Ok idx -> idx
  | Error _ -> Alcotest.failf "Invalid slot index: %d" i

(** Helper for boolean assertions *)
let assert_equal_bool ~loc msg expected actual =
  Assert.equal ~loc Bool.equal msg Format.pp_print_bool expected actual

(** Helper for is_attested with pre-filled number_of_slots and number_of_lags *)
let is_attested t ~lag_index slot_index =
  Dal_attestations_repr.is_attested
    t
    ~number_of_slots
    ~number_of_lags
    ~lag_index
    (slot_index_of_int slot_index)

(** Helper for commit with pre-filled number_of_slots and number_of_lags *)
let commit t ~lag_index slot_index =
  Dal_attestations_repr.commit
    t
    ~number_of_slots
    ~number_of_lags
    ~lag_index
    (slot_index_of_int slot_index)

(** Test the example from the interface documentation:
    - number_of_lags = 4, number_of_slots = 32
    - Lag 0: empty
    - Lag 1: slots 0, 2 attested
    - Lag 2: empty
    - Lag 3: slots 0, 1 attested *)
let test_example_encoding () =
  let open Lwt_result_wrap_syntax in
  (* Build the example attestations structure *)
  let t = Dal_attestations_repr.empty in
  (* Lag 1: attest slots 0 and 2 *)
  let t = commit t ~lag_index:1 0 in
  let t = commit t ~lag_index:1 2 in
  (* Lag 3: attest slots 0 and 1 *)
  let t = commit t ~lag_index:3 0 in
  let t = commit t ~lag_index:3 1 in

  (* Check all combinations of lag_index and slot_index *)
  (* Expected attestations:
     - Lag 0: all empty (slots 0-31 not attested)
     - Lag 1: slots 0 and 2 attested, all others not attested
     - Lag 2: all empty (slots 0-31 not attested)
     - Lag 3: slots 0 and 1 attested, all others not attested *)

  (* Define expected attestations for each lag *)
  let expected lag_idx slot_idx =
    match lag_idx with
    | 0 -> false (* Lag 0: empty *)
    | 1 -> slot_idx = 0 || slot_idx = 2 (* Lag 1: slots 0 and 2 *)
    | 2 -> false (* Lag 2: empty *)
    | 3 -> slot_idx = 0 || slot_idx = 1 (* Lag 3: slots 0 and 1 *)
    | _ -> false
  in

  (* Check all lags *)
  let* () =
    List.iter_es
      (fun lag_idx ->
        List.iter_es
          (fun slot_idx ->
            let attested = is_attested t ~lag_index:lag_idx slot_idx in
            assert_equal_bool
              ~loc:__LOC__
              (Printf.sprintf "Lag %d, slot %d attestation" lag_idx slot_idx)
              (expected lag_idx slot_idx)
              attested)
          (0 -- (number_of_slots - 1)))
      (0 -- (number_of_lags - 1))
  in

  return_unit

(** Test that empty attestations work correctly *)
let test_empty () =
  let open Lwt_result_wrap_syntax in
  let t = Dal_attestations_repr.empty in

  (* Check that all lags and slots are not attested *)
  let* () =
    List.iter_es
      (fun lag_idx ->
        List.iter_es
          (fun slot_idx ->
            let attested = is_attested t ~lag_index:lag_idx slot_idx in
            assert_equal_bool
              ~loc:__LOC__
              (Printf.sprintf
                 "Empty attestation: lag %d, slot %d"
                 lag_idx
                 slot_idx)
              false
              attested)
          (0 -- (number_of_slots - 1)))
      (0 -- (number_of_lags - 1))
  in

  return_unit

(** Test committing the same slot multiple times (idempotent) *)
let test_commit_idempotent () =
  let open Lwt_result_wrap_syntax in
  let t = Dal_attestations_repr.empty in

  (* Commit slot 5 at lag 2 *)
  let t = commit t ~lag_index:2 5 in

  (* Commit the same slot again *)
  let t = commit t ~lag_index:2 5 in

  (* Check that slot 5 at lag 2 is attested *)
  let attested = is_attested t ~lag_index:2 5 in
  let* () =
    assert_equal_bool ~loc:__LOC__ "Slot 5 at lag 2 attestation" true attested
  in

  (* Check that other slots at lag 2 are not attested *)
  let* () =
    List.iter_es
      (fun slot_idx ->
        if slot_idx <> 5 then
          let attested = is_attested t ~lag_index:2 slot_idx in
          assert_equal_bool
            ~loc:__LOC__
            (Printf.sprintf "Slot %d at lag 2 attestation" slot_idx)
            false
            attested
        else return_unit)
      (0 -- (number_of_slots - 1))
  in

  return_unit

(** Test encoding and decoding *)
let test_encoding_roundtrip () =
  let open Lwt_result_wrap_syntax in
  (* Build a non-trivial attestations structure *)
  let t = Dal_attestations_repr.empty in
  let t = commit t ~lag_index:0 10 in
  let t = commit t ~lag_index:1 5 in
  let t = commit t ~lag_index:1 15 in
  let t = commit t ~lag_index:3 0 in

  (* Encode and decode *)
  let encoded =
    Data_encoding.Binary.to_bytes_exn Dal_attestations_repr.encoding t
  in
  let decoded =
    Data_encoding.Binary.of_bytes_exn Dal_attestations_repr.encoding encoded
  in

  (* Check that the decoded value has the same attestations *)
  let check_attestation lag_idx slot_idx expected =
    let attested_original = is_attested t ~lag_index:lag_idx slot_idx in
    let attested_decoded = is_attested decoded ~lag_index:lag_idx slot_idx in
    let* () =
      assert_equal_bool
        ~loc:__LOC__
        "is_attested"
        attested_original
        attested_decoded
    in
    assert_equal_bool
      ~loc:__LOC__
      "is_attested vs expected"
      expected
      attested_decoded
  in

  (* Check specific attestations *)
  let* () = check_attestation 0 10 true in
  let* () = check_attestation 0 5 false in
  let* () = check_attestation 1 5 true in
  let* () = check_attestation 1 15 true in
  let* () = check_attestation 1 10 false in
  let* () = check_attestation 2 0 false in
  let* () = check_attestation 3 0 true in
  let* () = check_attestation 3 1 false in

  return_unit

(** Test update_attestation fast path: non-empty → non-empty
    This tests the optimization where we update bits in place *)
let test_update_existing_attestation () =
  let open Lwt_result_wrap_syntax in
  (* Create an attestation at lag 1 with slot 5 *)
  let t = Dal_attestations_repr.empty in
  let t = commit t ~lag_index:1 5 in

  (* Verify slot 5 is attested *)
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Slot 5 initially attested"
      true
      (is_attested t ~lag_index:1 5)
  in

  (* Add more slots to the same lag (should use fast path) *)
  let t = commit t ~lag_index:1 10 in
  let t = commit t ~lag_index:1 15 in

  (* Verify all three slots are attested *)
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Slot 5 still attested"
      true
      (is_attested t ~lag_index:1 5)
  in
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Slot 10 attested"
      true
      (is_attested t ~lag_index:1 10)
  in
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Slot 15 attested"
      true
      (is_attested t ~lag_index:1 15)
  in

  (* Verify other slots are not attested *)
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Slot 0 not attested"
      false
      (is_attested t ~lag_index:1 0)
  in
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Slot 20 not attested"
      false
      (is_attested t ~lag_index:1 20)
  in

  return_unit

(** Test creating attestations at different lags (empty → non-empty path) *)
let test_multiple_lag_attestations () =
  let open Lwt_result_wrap_syntax in
  let t = Dal_attestations_repr.empty in

  (* Create attestations at different lags *)
  let t = commit t ~lag_index:0 1 in
  let t = commit t ~lag_index:2 2 in
  let t = commit t ~lag_index:3 3 in

  (* Verify each lag has only its attested slot *)
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Lag 0, slot 1"
      true
      (is_attested t ~lag_index:0 1)
  in
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Lag 0, slot 2"
      false
      (is_attested t ~lag_index:0 2)
  in
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Lag 1 empty"
      false
      (is_attested t ~lag_index:1 0)
  in
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Lag 2, slot 2"
      true
      (is_attested t ~lag_index:2 2)
  in
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Lag 3, slot 3"
      true
      (is_attested t ~lag_index:3 3)
  in

  return_unit

(** Test edge cases: first and last lag, first and last slot *)
let test_edge_cases () =
  let open Lwt_result_wrap_syntax in
  let t = Dal_attestations_repr.empty in

  (* Test first lag, first slot *)
  let t = commit t ~lag_index:0 0 in
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "First lag, first slot"
      true
      (is_attested t ~lag_index:0 0)
  in

  (* Test last lag, last slot *)
  let t = commit t ~lag_index:(number_of_lags - 1) (number_of_slots - 1) in
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Last lag, last slot"
      true
      (is_attested t ~lag_index:(number_of_lags - 1) (number_of_slots - 1))
  in

  (* Test first lag, last slot *)
  let t = commit t ~lag_index:0 (number_of_slots - 1) in
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "First lag, last slot"
      true
      (is_attested t ~lag_index:0 (number_of_slots - 1))
  in

  (* Test last lag, first slot *)
  let t = commit t ~lag_index:(number_of_lags - 1) 0 in
  let* () =
    assert_equal_bool
      ~loc:__LOC__
      "Last lag, first slot"
      true
      (is_attested t ~lag_index:(number_of_lags - 1) 0)
  in

  return_unit

(** Test many slots attested at same lag (exercises fast path extensively) *)
let test_many_slots_same_lag () =
  let open Lwt_result_wrap_syntax in
  let t = Dal_attestations_repr.empty in

  (* Attest slots 0, 2, 4, 6, ... (even slots) at lag 2 *)
  let t =
    List.fold_left
      (fun acc slot_idx -> commit acc ~lag_index:2 slot_idx)
      t
      (List.filter (fun x -> x mod 2 = 0) (0 -- (number_of_slots - 1)))
  in

  (* Verify even slots are attested *)
  let* () =
    List.iter_es
      (fun slot_idx ->
        let expected = slot_idx mod 2 = 0 in
        let attested = is_attested t ~lag_index:2 slot_idx in
        assert_equal_bool
          ~loc:__LOC__
          (Printf.sprintf "Slot %d at lag 2" slot_idx)
          expected
          attested)
      (0 -- (number_of_slots - 1))
  in

  (* Verify lag 2 doesn't affect other lags *)
  let* () =
    List.iter_es
      (fun lag_idx ->
        if lag_idx <> 2 then
          let attested = is_attested t ~lag_index:lag_idx 0 in
          assert_equal_bool
            ~loc:__LOC__
            (Printf.sprintf "Slot 0 at lag %d should be empty" lag_idx)
            false
            attested
        else return_unit)
      (0 -- (number_of_lags - 1))
  in

  return_unit

(** Test size calculation *)
let test_size_calculation () =
  let open Lwt_result_wrap_syntax in
  (* Empty bitset should be small *)
  let t_empty = Dal_attestations_repr.empty in
  let size_empty = Dal_attestations_repr.occupied_size_in_bits t_empty in
  let* () =
    Assert.equal
      ~loc:__LOC__
      Int.equal
      "Empty size"
      Format.pp_print_int
      0
      size_empty
  in

  (* Expected max size *)
  let expected_max =
    Dal_attestations_repr.expected_max_size_in_bits
      ~number_of_slots
      ~number_of_lags
  in
  let* () =
    Assert.equal
      ~loc:__LOC__
      Int.equal
      "Expected max size"
      Format.pp_print_int
      (number_of_lags * (1 + number_of_slots))
      expected_max
  in

  (* Create attestations and check size grows *)
  let t = commit t_empty ~lag_index:1 5 in
  let size_one = Dal_attestations_repr.occupied_size_in_bits t in
  let* () =
    if size_one > size_empty then return_unit
    else
      failwith
        "Size should grow after first commit: %d <= %d"
        size_one
        size_empty
  in

  (* Add more attestations *)
  let t = commit t ~lag_index:2 10 in
  let size_two = Dal_attestations_repr.occupied_size_in_bits t in
  let* () =
    if size_two > size_one then return_unit
    else
      failwith
        "Size should grow after second commit: %d <= %d"
        size_two
        size_one
  in

  return_unit

(** Test complex pattern: interleaved lags and slots.
    The commits are shuffled to test the slow path (inserting attestations
    in the middle of the bitset, which requires shifting existing data). *)
let test_complex_pattern () =
  let open Lwt_result_wrap_syntax in
  let t = Dal_attestations_repr.empty in

  (* Build a complex pattern:
     - Lag 0: slots 1, 3, 5
     - Lag 1: slot 10
     - Lag 2: empty
     - Lag 3: slots 0, 15, 31

     Order is shuffled to exercise the slow path: inserting lag 0 after lag 3
     exists forces shifting lag 3's data, and inserting lag 1 between them
     forces another shift. *)
  let t = commit t ~lag_index:3 15 in
  let t = commit t ~lag_index:0 5 in
  let t = commit t ~lag_index:0 1 in
  let t = commit t ~lag_index:3 0 in
  let t = commit t ~lag_index:1 10 in
  let t = commit t ~lag_index:0 3 in
  let t = commit t ~lag_index:3 31 in

  (* Verify the pattern *)
  let check_slot lag slot expected =
    let attested = is_attested t ~lag_index:lag slot in
    assert_equal_bool
      ~loc:__LOC__
      (Printf.sprintf "Lag %d, slot %d" lag slot)
      expected
      attested
  in

  let* () = check_slot 0 1 true in
  let* () = check_slot 0 3 true in
  let* () = check_slot 0 5 true in
  let* () = check_slot 0 0 false in
  let* () = check_slot 0 2 false in
  let* () = check_slot 1 10 true in
  let* () = check_slot 1 0 false in
  let* () = check_slot 2 0 false in
  let* () = check_slot 2 10 false in
  let* () = check_slot 3 0 true in
  let* () = check_slot 3 15 true in
  let* () = check_slot 3 31 true in
  let* () = check_slot 3 1 false in

  return_unit

let tests =
  [
    Tztest.tztest "example encoding from interface" `Quick test_example_encoding;
    Tztest.tztest "empty attestations" `Quick test_empty;
    Tztest.tztest "commit is idempotent" `Quick test_commit_idempotent;
    Tztest.tztest "encoding roundtrip" `Quick test_encoding_roundtrip;
    Tztest.tztest
      "update existing attestation (fast path)"
      `Quick
      test_update_existing_attestation;
    Tztest.tztest
      "multiple lag attestations"
      `Quick
      test_multiple_lag_attestations;
    Tztest.tztest "edge cases" `Quick test_edge_cases;
    Tztest.tztest "many slots at same lag" `Quick test_many_slots_same_lag;
    Tztest.tztest "size calculation" `Quick test_size_calculation;
    Tztest.tztest "complex pattern" `Quick test_complex_pattern;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("dal attestations repr", tests)]
  |> Lwt_main.run
