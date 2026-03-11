(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Baker, DAL included attestations cache
    Invocation:   dune exec src/proto_alpha/lib_delegate/test/main.exe \
                   -- --file test_dal_included_attestations_cache.ml
    Subject:      Tests for the DAL included attestations cache.

    The cache tracks which DAL slot attestations have already been included
    in proposals to avoid redundant attestations across multiple lags.

    [filter_attestable_slots] uses the cache to remove already-attested slots.

    This file contains some unit tests exercising concrete fork scenarios.
*)

open Baking_state_types
module Cache = Dal_included_attestations_cache
module Internal = Cache.Internal_for_tests

let default_published_level = 10l

let default_delegate_id =
  let pkh_str = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" in
  match Signature.Public_key_hash.of_b58check pkh_str with
  | Ok pkh -> Delegate_id.of_pkh pkh
  | Error _trace -> assert false

let dummy_block_hash = Block_hash.of_bytes_exn (Bytes.make 32 '\000')

let make_cache () =
  Cache.create ~attestation_lags:[1; 2; 3; 4; 5] ~number_of_slots:32

(** Hack to obtain distinct block hashes. It encodes the parameter [n]'s low 16
    bits into a 32-byte zero buffer, giving up to 65536 distinct block
    hashes. *)
let make_block_hash n =
  let bytes = Bytes.make 32 '\000' in
  Bytes.set_int8 bytes 0 (n land 0xFF) ;
  Bytes.set_int8 bytes 1 ((n lsr 8) land 0xFF) ;
  Block_hash.of_bytes_exn bytes

let filter_attestable_slots cache ~attestable_slots ~head_level ~head_hash
    ~predecessor_hash =
  Cache.filter_attestable_slots
    cache
    ~delegate_id:default_delegate_id
    ~published_level:default_published_level
    ~attestable_slots:(Cache.SlotSet.of_list attestable_slots)
    ~head_level
    ~head_hash
    ~predecessor_hash

let update_from_attested_slots t ~attested_level ~block_hash ~predecessor_hash
    ~grandparent ~slots =
  Internal.update_from_attested_slots
    t
    ~delegate_id:default_delegate_id
    ~attested_level
    ~block_hash
    ~predecessor_hash
    ~grandparent
    ~attested_slots:[(default_published_level, slots)]

(** Helper: call [filter_attestable_slots], then assert expected set of slots. *)
let assert_filter cache ~attestable_slots ~head_level ~head_hash
    ~predecessor_hash ~expected_slots =
  match
    filter_attestable_slots
      cache
      ~attestable_slots
      ~head_level
      ~head_hash
      ~predecessor_hash
  with
  | Error trace ->
      Test.fail
        "filter_attestable_slots returned error: %a"
        Error_monad.pp_print_trace
        trace
  | Ok filtered ->
      let expected = Cache.SlotSet.of_list expected_slots in
      if not (Cache.SlotSet.equal filtered expected) then
        Test.fail
          "Expected slots [%s], got [%s]"
          (String.concat "," (List.map string_of_int expected_slots))
          (filtered |> Cache.SlotSet.elements |> List.map string_of_int
         |> String.concat ",")

(** Chain:

      A(10) -- B(11)
            \- B'(11) -- C'(12)

    published_level = 10 (A's level).
    Slot 0: attested at B and B'.
    Slot 1: attested at B and C'.
    Current chain: A -- B' -- C'.
    Replay blocks with [update_from_attested_slots]: B, then B', then C'.
    Filter at head C' -> both slots filtered out. *)
let test_fork_deduplication () =
  let h_a = make_block_hash 0 in
  let h_b = make_block_hash 1 in
  let h_b' = make_block_hash 2 in
  let h_c' = make_block_hash 3 in
  let h_0 = make_block_hash 4 in
  let cache = make_cache () in
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_b
    ~predecessor_hash:h_a
    ~grandparent:h_0
    ~slots:[0] ;
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_b'
    ~predecessor_hash:h_a
    ~grandparent:h_0
    ~slots:[0; 1] ;
  update_from_attested_slots
    cache
    ~attested_level:12l
    ~block_hash:h_c'
    ~predecessor_hash:h_b'
    ~grandparent:h_a
    ~slots:[1] ;
  assert_filter
    cache
    ~attestable_slots:[0; 1]
    ~head_level:12l
    ~head_hash:h_c'
    ~predecessor_hash:h_b'
    ~expected_slots:[] ;
  Lwt.return_unit

(** Chain:

    A(10) -- B(11) attests slot 5
          \- B'(11) -- C'(12) -- D'(13) -- E'(14)

    Replay: B attests slot 5; then B', C', D' with no attestation for slot 5.
    Filter at head E'(14) → slot 5 should be allowed. *)
let test_zombie_cleared_then_allowed () =
  let h_a = make_block_hash 0 in
  let h_b = make_block_hash 1 in
  let h_b' = make_block_hash 2 in
  let h_c' = make_block_hash 3 in
  let h_d' = make_block_hash 4 in
  let h_e' = make_block_hash 5 in
  let h_0 = make_block_hash 0 in
  let cache = make_cache () in
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_b
    ~predecessor_hash:h_a
    ~grandparent:h_0
    ~slots:[5] ;
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_b'
    ~predecessor_hash:h_a
    ~grandparent:h_0
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:12l
    ~block_hash:h_c'
    ~predecessor_hash:h_b'
    ~grandparent:h_a
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:13l
    ~block_hash:h_d'
    ~predecessor_hash:h_c'
    ~grandparent:h_b'
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:14l
    ~block_hash:h_e'
    ~predecessor_hash:h_d'
    ~grandparent:h_c'
    ~slots:[] ;
  assert_filter
    cache
    ~attestable_slots:[5]
    ~head_level:14l
    ~head_hash:h_e'
    ~predecessor_hash:h_d'
    ~expected_slots:[5] ;
  Lwt.return_unit

let () =
  Test.register
    ~__FILE__
    ~title:(Protocol.name ^ ": fork deduplication warm-up")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"]
    test_fork_deduplication ;
  Test.register
    ~__FILE__
    ~title:(Protocol.name ^ ": zombie cleared by depth-2 filter then allowed")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"]
    test_zombie_cleared_then_allowed
