(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (DAL slot storage - bitset utilities)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_dal_slot_storage_bitset.ml
    Subject:    Unit tests for DAL bitset-based attestation storage utilities.
                Tests the conversion between Set(public_key_hash) and Bitset
                representations, shard counting, and threshold checking.
*)

open Protocol
module Signature = Environment.Signature
module Bitset = Environment.Bitset
open Dal_attestations_repr.Internal_for_tests
open Dal_slot_storage.Internal_for_tests

(** {1 Test Helpers} *)

(** Count set bits in a bitset *)
let count_set_bits bitset =
  let rec count acc i =
    if i < 0 then acc
    else
      let is_set =
        match Bitset.mem bitset i with Ok b -> b | Error _ -> false
      in
      count (acc + if is_set then 1 else 0) (i - 1)
  in
  (* Bitsets can be up to 1000+ bits for ~800 delegates, check first 1000 *)
  count 0 (Bitset.occupied_size_in_bits bitset)

module Helpers = struct
  open Lwt_result_wrap_syntax

  let get_delegate_ordering ctxt ~shard_assignment_level =
    let*@ result = get_delegate_ordering ctxt ~shard_assignment_level in
    return result

  let attesters_to_bitset ctxt ~shard_assignment_level ~attesters =
    let* ctxt, ordered_delegates =
      get_delegate_ordering ctxt ~shard_assignment_level
    in
    let bitset = attesters_to_bitset ~ordered_delegates ~attesters in
    return (ctxt, bitset)

  let bitset_to_attesters ctxt ~shard_assignment_level ~bitset =
    let* ctxt, ordered_delegates =
      get_delegate_ordering ctxt ~shard_assignment_level
    in
    let attesters = bitset_to_attesters ~ordered_delegates ~bitset in
    return (ctxt, attesters)

  let bitset_attested_shards ctxt ~shard_assignment_level ~bitset =
    let* delegate_to_shard_count =
      Assert.get_some
        ~loc:__LOC__
        (Raw_level_repr.Map.find shard_assignment_level
        @@ Raw_context.Consensus.delegate_to_shard_count ctxt)
    in
    let* _ctxt, attesters =
      bitset_to_attesters ctxt ~shard_assignment_level ~bitset
    in
    let total_shards = attested_shards ~delegate_to_shard_count ~attesters in
    return (ctxt, total_shards)

  let bitset_is_attested ctxt ~shard_assignment_level ~bitset ~threshold
      ~number_of_shards =
    let* ctxt, attested_shards =
      bitset_attested_shards ctxt ~shard_assignment_level ~bitset
    in
    let is_attested =
      Dal_attestations_repr.Accountability.is_threshold_reached
        ~threshold
        ~number_of_shards
        ~attested_shards
    in
    return (ctxt, is_attested)
end

(** {1 Test: Round-trip conversion (attesters → bitset → attesters)} *)

(** Test that converting attesters to bitset and back yields the same set *)
let test_roundtrip_conversion () =
  let open Lwt_result_wrap_syntax in
  (* Initialize test context *)
  let* b, _contracts = Context.init1 () in
  let* inc = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt inc in
  let ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in

  (* Get delegate ordering to understand the bitset structure *)
  let shard_assignment_level = Raw_level_repr.of_int32_exn 10l in
  let* ctxt, delegates =
    Helpers.get_delegate_ordering ctxt ~shard_assignment_level
  in

  (* Create an attester set with some delegates *)
  let attesters =
    match delegates with
    | [] -> Signature.Public_key_hash.Set.empty
    | first :: rest ->
        (* Take first 3 delegates if available *)
        let to_take = List.take_n 3 (first :: rest) in
        List.fold_left
          (fun acc pkh -> Signature.Public_key_hash.Set.add pkh acc)
          Signature.Public_key_hash.Set.empty
          to_take
  in

  (* Convert to bitset *)
  let* ctxt, bitset =
    Helpers.attesters_to_bitset ctxt ~shard_assignment_level ~attesters
  in

  (* Convert back to attesters *)
  let* _ctxt, recovered_attesters =
    Helpers.bitset_to_attesters ctxt ~shard_assignment_level ~bitset
  in

  (* Verify the sets are equal *)
  let* () =
    Assert.equal
      ~loc:__LOC__
      Signature.Public_key_hash.Set.equal
      "Round-trip attesters"
      (fun ppf set ->
        Format.fprintf
          ppf
          "{%d delegates}"
          (Signature.Public_key_hash.Set.cardinal set))
      attesters
      recovered_attesters
  in

  return_unit

(** {1 Test: Empty attesters} *)

(** Test that an empty attester set converts to an all-zero bitset *)
let test_empty_attesters () =
  let open Lwt_result_wrap_syntax in
  let* b, _contracts = Context.init1 () in
  let* inc = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt inc in
  let ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in

  let empty_attesters = Signature.Public_key_hash.Set.empty in
  let shard_assignment_level = Raw_level_repr.of_int32_exn 5l in

  (* Convert empty set to bitset *)
  let* ctxt, bitset =
    Helpers.attesters_to_bitset
      ctxt
      ~shard_assignment_level
      ~attesters:empty_attesters
  in

  (* Verify the bitset is empty (all bits are 0) *)
  let set_bit_count = count_set_bits bitset in
  let* () =
    Assert.equal
      ~loc:__LOC__
      Int.equal
      "Empty attesters should produce empty bitset"
      Format.pp_print_int
      0
      set_bit_count
  in

  (* Convert back and verify *)
  let* _ctxt, recovered_attesters =
    Helpers.bitset_to_attesters ctxt ~shard_assignment_level ~bitset
  in

  let* () =
    if Signature.Public_key_hash.Set.is_empty recovered_attesters then
      return_unit
    else failwith "Recovered attesters should be empty"
  in

  return_unit

(** {1 Test: All delegates attest} *)

(** Test converting when all delegates attest *)
let test_all_delegates_attest () =
  let open Lwt_result_wrap_syntax in
  let* b, _contracts = Context.init1 () in
  let* inc = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt inc in
  let ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in

  let shard_assignment_level = Raw_level_repr.of_int32_exn 15l in

  (* Get all delegates *)
  let* ctxt, delegates =
    Helpers.get_delegate_ordering ctxt ~shard_assignment_level
  in

  let n_delegates = List.length delegates in

  (* Create set with all delegates *)
  let all_attesters =
    List.fold_left
      (fun acc pkh -> Signature.Public_key_hash.Set.add pkh acc)
      Signature.Public_key_hash.Set.empty
      delegates
  in

  (* Convert to bitset *)
  let* ctxt, bitset =
    Helpers.attesters_to_bitset
      ctxt
      ~shard_assignment_level
      ~attesters:all_attesters
  in

  (* Count set bits - should match number of delegates *)
  let set_bit_count = count_set_bits bitset in
  let* () =
    if set_bit_count = n_delegates then return_unit
    else
      failwith "Expected at least %d set bits, got %d" n_delegates set_bit_count
  in

  (* Convert back and verify *)
  let* _ctxt, recovered_attesters =
    Helpers.bitset_to_attesters ctxt ~shard_assignment_level ~bitset
  in

  let* () =
    Assert.equal
      ~loc:__LOC__
      Signature.Public_key_hash.Set.equal
      "All delegates round-trip"
      (fun ppf set ->
        Format.fprintf
          ppf
          "<%d delegates>"
          (Signature.Public_key_hash.Set.cardinal set))
      all_attesters
      recovered_attesters
  in

  return_unit

(** {1 Test: Single attester} *)

(** Test converting a single attester *)
let test_single_attester () =
  let open Lwt_result_wrap_syntax in
  let* b, _contracts = Context.init1 () in
  let* inc = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt inc in
  let ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in

  let shard_assignment_level = Raw_level_repr.of_int32_exn 20l in

  (* Get delegates *)
  let* ctxt, delegates =
    Helpers.get_delegate_ordering ctxt ~shard_assignment_level
  in

  (* Create set with single delegate (first one) *)
  let single_attester =
    match delegates with
    | [] -> Signature.Public_key_hash.Set.empty
    | first :: _ -> Signature.Public_key_hash.Set.singleton first
  in

  (* Convert to bitset *)
  let* ctxt, bitset =
    Helpers.attesters_to_bitset
      ctxt
      ~shard_assignment_level
      ~attesters:single_attester
  in

  (* Should have exactly 1 set bit (unless empty) *)
  let set_bit_count = count_set_bits bitset in
  let expected_count =
    if Signature.Public_key_hash.Set.is_empty single_attester then 0 else 1
  in
  let* () =
    Assert.equal
      ~loc:__LOC__
      Int.equal
      "Single attester should produce 1 set bit"
      Format.pp_print_int
      expected_count
      set_bit_count
  in

  (* Convert back and verify *)
  let* _ctxt, recovered_attesters =
    Helpers.bitset_to_attesters ctxt ~shard_assignment_level ~bitset
  in

  let* () =
    Assert.equal
      ~loc:__LOC__
      Signature.Public_key_hash.Set.equal
      "Single attester round-trip"
      (fun ppf set ->
        Format.fprintf
          ppf
          "<%d delegates>"
          (Signature.Public_key_hash.Set.cardinal set))
      single_attester
      recovered_attesters
  in

  return_unit

(** {1 Test: Bitset shard calculation} *)

(** Test that bitset_attested_shards calculates correct shard counts *)
let test_bitset_attested_shards () =
  let open Lwt_result_wrap_syntax in
  let* genesis, _contracts = Context.init1 ~consensus_threshold_size:0 () in
  let* {parametric = {dal = {attestation_lag; _}; _}; _} =
    Context.get_constants (B genesis)
  in

  (* Bake [attestation_lag] blocks to properly set up the DAL committee *)
  let* b = Block.bake_n attestation_lag genesis in
  let* inc = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt inc in
  let ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in

  (* Get delegates *)
  let current_level = (Raw_context.current_level ctxt).level in
  let shard_assignment_level =
    match Raw_level_repr.pred current_level with
    | Some l -> l
    | None -> assert false
  in
  let* ctxt, delegates =
    Helpers.get_delegate_ordering ctxt ~shard_assignment_level
  in

  (* Create attester set with some delegates *)
  let attesters =
    let to_take = List.take_n 3 delegates in
    List.fold_left
      (fun acc pkh -> Signature.Public_key_hash.Set.add pkh acc)
      Signature.Public_key_hash.Set.empty
      to_take
  in

  (* Convert to bitset *)
  let* ctxt, bitset =
    Helpers.attesters_to_bitset ctxt ~shard_assignment_level ~attesters
  in

  (* Calculate attested shards *)
  let* _ctxt, attested_shards =
    Helpers.bitset_attested_shards ctxt ~shard_assignment_level ~bitset
  in

  let* () =
    if attested_shards > 0 then return_unit
    else
      failwith
        "The number of attested shards should be positive, got %d"
        attested_shards
  in

  return_unit

(** {1 Test: Threshold checking} *)

(** Test bitset_is_attested threshold logic *)
let test_bitset_is_attested () =
  let open Lwt_result_wrap_syntax in
  let* genesis, _contracts = Context.init1 ~consensus_threshold_size:0 () in
  let* {
         parametric =
           {
             dal =
               {
                 attestation_lag;
                 cryptobox_parameters = {number_of_shards; _};
                 _;
               };
             _;
           };
         _;
       } =
    Context.get_constants (B genesis)
  in

  (* Bake [attestation_lag] blocks to properly set up the DAL committee *)
  let* b = Block.bake_n attestation_lag genesis in
  let* inc = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt inc in
  let ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in

  let current_level = (Raw_context.current_level ctxt).level in
  let shard_assignment_level =
    match Raw_level_repr.pred current_level with
    | Some l -> l
    | None -> assert false
  in

  (* Test with no attesters - should not reach threshold *)
  let empty_attesters = Signature.Public_key_hash.Set.empty in
  let* ctxt, empty_bitset =
    Helpers.attesters_to_bitset
      ctxt
      ~shard_assignment_level
      ~attesters:empty_attesters
  in

  let* ctxt, is_attested_empty =
    Helpers.bitset_is_attested
      ctxt
      ~shard_assignment_level
      ~bitset:empty_bitset
      ~threshold:66
      ~number_of_shards
  in

  let* () =
    Assert.equal
      ~loc:__LOC__
      Bool.equal
      "Empty bitset should not reach threshold"
      Format.pp_print_bool
      false
      is_attested_empty
  in

  (* Test with all delegates - it should reach the threshold *)
  let* ctxt, delegates =
    Helpers.get_delegate_ordering ctxt ~shard_assignment_level
  in

  let all_attesters =
    List.fold_left
      (fun acc pkh -> Signature.Public_key_hash.Set.add pkh acc)
      Signature.Public_key_hash.Set.empty
      delegates
  in

  let* ctxt, full_bitset =
    Helpers.attesters_to_bitset
      ctxt
      ~shard_assignment_level
      ~attesters:all_attesters
  in

  let* _ctxt, is_attested_full =
    Helpers.bitset_is_attested
      ctxt
      ~shard_assignment_level
      ~bitset:full_bitset
      ~threshold:100
      ~number_of_shards
  in

  (* With all delegates attesting, should likely reach threshold *)
  (* Note: We can't guarantee true without knowing exact stake distribution,
     but we can verify the function returns a boolean *)
  let* () =
    Assert.equal
      ~loc:__LOC__
      Bool.equal
      "Slot should be attested"
      Format.pp_print_bool
      true
      is_attested_full
  in

  return_unit

(** {1 Test: Bitset edge cases} *)

(** Test empty bitset decoding and shard counting *)
let test_empty_bitset () =
  let open Lwt_result_wrap_syntax in
  let* genesis, _contracts = Context.init1 ~consensus_threshold_size:0 () in
  let* {parametric = {dal = {attestation_lag; _}; _}; _} =
    Context.get_constants (B genesis)
  in

  (* Bake [attestation_lag] blocks to properly set up the DAL committee *)
  let* b = Block.bake_n attestation_lag genesis in
  let* inc = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt inc in
  let ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in

  let current_level = (Raw_context.current_level ctxt).level in
  let shard_assignment_level =
    match Raw_level_repr.pred current_level with
    | Some l -> l
    | None -> assert false
  in

  (* Test with empty bitset directly *)
  let empty_bitset = Bitset.empty in

  let* _ctxt, empty_attesters =
    Helpers.bitset_to_attesters
      ctxt
      ~shard_assignment_level
      ~bitset:empty_bitset
  in

  let* () =
    if Signature.Public_key_hash.Set.is_empty empty_attesters then return_unit
    else failwith "Empty bitset should yield empty attesters"
  in

  (* Test shard calculation with empty bitset *)
  let* _ctxt, empty_shards =
    Helpers.bitset_attested_shards
      ctxt
      ~shard_assignment_level
      ~bitset:empty_bitset
  in

  let* () =
    Assert.equal
      ~loc:__LOC__
      Int.equal
      "Empty bitset should have 0 attested shards"
      Format.pp_print_int
      0
      empty_shards
  in

  return_unit

(** {1 Test Suite} *)

let tests =
  [
    Tztest.tztest
      "Round-trip conversion (attesters → bitset → attesters)"
      `Quick
      test_roundtrip_conversion;
    Tztest.tztest "Empty attesters to bitset" `Quick test_empty_attesters;
    Tztest.tztest "All delegates attest" `Quick test_all_delegates_attest;
    Tztest.tztest "Single attester" `Quick test_single_attester;
    Tztest.tztest
      "Bitset attested shards calculation"
      `Quick
      test_bitset_attested_shards;
    Tztest.tztest "Bitset threshold checking" `Quick test_bitset_is_attested;
    Tztest.tztest
      "Empty bitset decoding and shard counting"
      `Quick
      test_empty_bitset;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("dal slot storage bitset", tests)]
  |> Lwt_main.run
