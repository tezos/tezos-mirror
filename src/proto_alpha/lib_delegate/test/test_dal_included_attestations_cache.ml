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

    This file contains:
    - some unit tests exercising concrete fork scenarios
    - a property-based test simulating chain evolution with forks, checking the
      "no duplicate attestations" property.
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

(* ---- Depth 0: attested_level = head_level ---- *)

(** Depth 0: head_hash in block_hashes -> filter out. *)
let test_depth0_head_in_block_hashes_filtered () =
  let cache = make_cache () in
  let h_a = make_block_hash 11 in
  let h_10 = make_block_hash 10 in
  let h_9 = make_block_hash 9 in
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_a
    ~predecessor_hash:h_10
    ~grandparent:h_9
    ~slots:[0] ;
  assert_filter
    cache
    ~attestable_slots:[0]
    ~head_level:11l
    ~head_hash:h_a
    ~predecessor_hash:h_10
    ~expected_slots:[] ;
  Lwt.return_unit

(** Depth 0: head_hash not in block_hashes -> allow.

    Chain: A(11) is head. Slot attested at B(11) with h_b (B != A). Filter at
    head A -> slot allowed. *)
let test_depth0_head_not_in_block_hashes_allowed () =
  let cache = make_cache () in
  let h_a = make_block_hash 3 in
  let h_b = make_block_hash 2 in
  let h_1 = make_block_hash 1 in
  let h_0 = make_block_hash 0 in
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_b
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[0] ;
  (* This is a no-op on the cache, but mirrors the realistic scenario
     where A is the head and B's attestation was on a competing block. *)
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_a
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[] ;
  assert_filter
    cache
    ~attestable_slots:[0]
    ~head_level:11l
    ~head_hash:h_a
    ~predecessor_hash:h_1
    ~expected_slots:[0] ;
  Lwt.return_unit

(** Depth 0: same-level accumulation, head in list -> filter out.

    Chain: A(11). Two blocks at level 11 attest slot 0 (h_a, h_b).
    Filter at head h_a -> head in block_hashes -> filtered out. *)
let test_depth0_head_in_block_hashes_accumulation_filtered () =
  let cache = make_cache () in
  let h_a = make_block_hash 2 in
  let h_b = make_block_hash 3 in
  let h_1 = make_block_hash 1 in
  let h_0 = make_block_hash 0 in
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_a
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[0] ;
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_b
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[0] ;
  assert_filter
    cache
    ~attestable_slots:[0]
    ~head_level:11l
    ~head_hash:h_a
    ~predecessor_hash:h_1
    ~expected_slots:[] ;
  Lwt.return_unit

(* ---- Depth 1: attested_level = head_level - 1 ---- *)

(** Depth 1: predecessor_hash in block_hashes -> filter out.

    Chain: A(13) -- B(14). Slot attested at A. Filter at head B; predecessor
    h_a in block_hashes -> filtered out. *)
let test_depth1_predecessor_in_block_hashes_filtered () =
  let cache = make_cache () in
  let h_0 = make_block_hash 0 in
  let h_1 = make_block_hash 1 in
  let h_a = make_block_hash 13 in
  let h_b = make_block_hash 14 in
  update_from_attested_slots
    cache
    ~attested_level:13l
    ~block_hash:h_a
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[0] ;
  update_from_attested_slots
    cache
    ~attested_level:14l
    ~block_hash:h_b
    ~predecessor_hash:h_a
    ~grandparent:h_1
    ~slots:[] ;
  assert_filter
    cache
    ~attestable_slots:[0]
    ~head_level:14l
    ~head_hash:h_b
    ~predecessor_hash:h_a
    ~expected_slots:[] ;
  Lwt.return_unit

(** Depth 1: predecessor_hash not in block_hashes -> allow.

    Chain:
    -- A(13)  (slot attested here)
    \- A'(13) -- B'(14)  (head)

    Filter at head B' -> predecessor h_a' not in block_hashes -> allowed. *)
let test_depth1_predecessor_not_in_block_hashes_allowed () =
  let cache = make_cache () in
  let h_0 = make_block_hash 0 in
  let h_1 = make_block_hash 1 in
  let h_a = make_block_hash 10 in
  let h_a' = make_block_hash 11 in
  let h_b' = make_block_hash 12 in
  update_from_attested_slots
    cache
    ~attested_level:13l
    ~block_hash:h_a
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[0] ;
  update_from_attested_slots
    cache
    ~attested_level:13l
    ~block_hash:h_a'
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:14l
    ~block_hash:h_b'
    ~predecessor_hash:h_a'
    ~grandparent:h_1
    ~slots:[] ;
  assert_filter
    cache
    ~attestable_slots:[0]
    ~head_level:14l
    ~head_hash:h_b'
    ~predecessor_hash:h_a'
    ~expected_slots:[0] ;
  Lwt.return_unit

(* ---- Depth 2: attested_level = head_level - 2 ---- *)

(** Depth 2: block_hashes = [] (abandoned fork) -> allow.

    Chain:
    -- A'(13)
    \- A(13) -- B(14) -- C(15). Attest at 13 with h_a'; process C(15)
    with grandparent h_a, h_abandoned <> h_a so entry cleared. Filter at head C ->
    slot allowed. *)
let test_depth2_empty_block_hashes_allowed () =
  let cache = make_cache () in
  let h_0 = make_block_hash 0 in
  let h_1 = make_block_hash 1 in
  let h_a = make_block_hash 2 in
  let h_b = make_block_hash 3 in
  let h_c = make_block_hash 4 in
  let h_a' = make_block_hash 99 in
  update_from_attested_slots
    cache
    ~attested_level:13l
    ~block_hash:h_a'
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[0] ;
  update_from_attested_slots
    cache
    ~attested_level:13l
    ~block_hash:h_a
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:14l
    ~block_hash:h_b
    ~predecessor_hash:h_a
    ~grandparent:h_1
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:15l
    ~block_hash:h_c
    ~predecessor_hash:h_b
    ~grandparent:h_a
    ~slots:[] ;
  assert_filter
    cache
    ~attestable_slots:[0]
    ~head_level:15l
    ~head_hash:h_c
    ~predecessor_hash:h_b
    ~expected_slots:[0] ;
  Lwt.return_unit

(** Depth 2: block_hashes = [grandparent] (on canonical chain) -> filter out.

    Chain: A(13) -- B(14) -- C(15). Slot attested at A (h_a). Process C(15) with
    grandparent h_a keeps entry. Filter at head C -> filtered out. *)
let test_depth2_nonempty_block_hashes_filtered () =
  let cache = make_cache () in
  let h_0 = make_block_hash 0 in
  let h_1 = make_block_hash 1 in
  let h_a = make_block_hash 2 in
  let h_b = make_block_hash 3 in
  let h_c = make_block_hash 4 in
  update_from_attested_slots
    cache
    ~attested_level:13l
    ~block_hash:h_a
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[0] ;
  update_from_attested_slots
    cache
    ~attested_level:14l
    ~block_hash:h_b
    ~predecessor_hash:h_a
    ~grandparent:h_1
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:15l
    ~block_hash:h_c
    ~predecessor_hash:h_b
    ~grandparent:h_a
    ~slots:[] ;
  assert_filter
    cache
    ~attestable_slots:[0]
    ~head_level:15l
    ~head_hash:h_c
    ~predecessor_hash:h_b
    ~expected_slots:[] ;
  Lwt.return_unit

(* ---- Depth 3 ---- *)

(** Depth 3: block_hashes non-empty (on canonical chain at depth 2) -> filter out.

    Chain: A(11) -- B(12) -- C(13) -- D(14). Slot attested at A (h_a). Process
    C(13) with grandparent h_a keeps entry. Filter at head D -> filtered out. *)
let test_depth3_nonempty_block_hashes_filtered () =
  let cache = make_cache () in
  let h_0 = make_block_hash 0 in
  let h_1 = make_block_hash 1 in
  let h_a = make_block_hash 2 in
  let h_b = make_block_hash 3 in
  let h_c = make_block_hash 4 in
  let h_d = make_block_hash 5 in
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_a
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[0] ;
  update_from_attested_slots
    cache
    ~attested_level:12l
    ~block_hash:h_b
    ~predecessor_hash:h_a
    ~grandparent:h_1
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:13l
    ~block_hash:h_c
    ~predecessor_hash:h_b
    ~grandparent:h_a
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:14l
    ~block_hash:h_d
    ~predecessor_hash:h_c
    ~grandparent:h_b
    ~slots:[] ;
  assert_filter
    cache
    ~attestable_slots:[0]
    ~head_level:14l
    ~head_hash:h_d
    ~predecessor_hash:h_c
    ~expected_slots:[] ;
  Lwt.return_unit

(** Depth 3: block_hashes = [] (cleared by depth-2 filter) -> allow.

    Chain:
    -- A'(11)
    \- A(11) -- B(12) -- C(13) -- D(14)

    Slot attested at 11 on A' (h_a'). Process C(13) with grandparent h_a;
    h_a' != h_a so entry cleared. Filter at head D -> slot allowed. *)
let test_depth3_empty_block_hashes_allowed () =
  let cache = make_cache () in
  let h_0 = make_block_hash 0 in
  let h_1 = make_block_hash 1 in
  let h_a = make_block_hash 2 in
  let h_b = make_block_hash 3 in
  let h_c = make_block_hash 4 in
  let h_d = make_block_hash 5 in
  let h_a' = make_block_hash 99 in
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_a'
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[0] ;
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_a
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:12l
    ~block_hash:h_b
    ~predecessor_hash:h_a
    ~grandparent:h_1
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:13l
    ~block_hash:h_c
    ~predecessor_hash:h_b
    ~grandparent:h_a
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:14l
    ~block_hash:h_d
    ~predecessor_hash:h_c
    ~grandparent:h_b
    ~slots:[] ;
  assert_filter
    cache
    ~attestable_slots:[0]
    ~head_level:14l
    ~head_hash:h_d
    ~predecessor_hash:h_c
    ~expected_slots:[0] ;
  Lwt.return_unit

(** Multiple slots: mix of filter out and allow.

    Chain:
    -- A'(11) -- B'(12)
    \- A(11) -- B(12) -- C(13).

    Slot 0 attested at C (head) -> filtered.
    Slot 1 attested at B' -> allowed.
    Slot 2 no entry -> allowed.
    Slot 3 attested at A', process C with grandparent h_a clears entry -> allowed.
    Filter at head C. *)
let test_multiple_slots_mixed () =
  let cache = make_cache () in
  let h_0 = make_block_hash 0 in
  let h_1 = make_block_hash 1 in
  let h_c = make_block_hash 4 in
  let h_b = make_block_hash 3 in
  let h_a = make_block_hash 2 in
  let h_b' = make_block_hash 13 in
  let h_a' = make_block_hash 12 in
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_a'
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[3] ;
  update_from_attested_slots
    cache
    ~attested_level:12l
    ~block_hash:h_b'
    ~predecessor_hash:h_a'
    ~grandparent:h_1
    ~slots:[1] ;
  update_from_attested_slots
    cache
    ~attested_level:11l
    ~block_hash:h_a
    ~predecessor_hash:h_1
    ~grandparent:h_0
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:12l
    ~block_hash:h_b
    ~predecessor_hash:h_a
    ~grandparent:h_1
    ~slots:[] ;
  update_from_attested_slots
    cache
    ~attested_level:13l
    ~block_hash:h_c
    ~predecessor_hash:h_b
    ~grandparent:h_a
    ~slots:[0] ;
  assert_filter
    cache
    ~attestable_slots:[0; 1; 2; 3]
    ~head_level:13l
    ~head_hash:h_c
    ~predecessor_hash:h_b
    ~expected_slots:[1; 2; 3] ;
  Lwt.return_unit

let num_test_slots = 8

let max_test_level = 20

type step = {branch_level : int; block_slots : int list list}

type scenario = {init_slots : int list list; steps : step list}

let all_test_slots = Stdlib.List.init num_test_slots Fun.id

let gen_slot_subset =
  let open QCheck2.Gen in
  let+ flags = list_size (return num_test_slots) bool in
  let _, result =
    List.fold_left
      (fun (i, acc) b -> (i + 1, if b then i :: acc else acc))
      (0, [])
      flags
  in
  List.rev result

(** Generates a random scenario for the "no duplicate attestations" property.

    A scenario consists of:
    - [init_slots]: an initial chain of 3–6 blocks, each with a subset of slots
      attested at that block (levels [1..init_len] in the internal chain).
    - [steps]: 2–5 steps. Each step forks the chain from [branch_level]
      (between [head_level - 2] and [head_level]), then appends new blocks
      with their attested slot subsets. [branch_level] and block count are
      chosen so the chain length stays ≤ [max_test_level].

    When run with [run_scenario], the chain is replayed into the cache and
    [filter_attestable_slots] is checked against the expected set of slots
    (no duplicate attestations on the chosen branch). *)
let gen_scenario =
  let open QCheck2.Gen in
  let* init_len = 3 -- 6 in
  let* init_slots = list_size (return init_len) gen_slot_subset in
  let* num_steps = 2 -- 5 in
  let rec gen_steps head_level remaining acc =
    if remaining = 0 || head_level >= max_test_level then return (List.rev acc)
    else
      let min_branch = max 0 (head_level - 2) in
      let* branch_level = min_branch -- head_level in
      let min_blocks = head_level - branch_level in
      let max_extra = min 3 (max_test_level - branch_level - min_blocks) in
      let* extra = if max_extra > 0 then 0 -- max_extra else return 0 in
      let num_blocks = min_blocks + extra in
      let new_head = branch_level + num_blocks in
      let* block_slots = list_size (return num_blocks) gen_slot_subset in
      let s = {branch_level; block_slots} in
      gen_steps new_head (remaining - 1) (s :: acc)
  in
  let+ steps = gen_steps init_len num_steps [] in
  {init_slots; steps}

(** At each step: fork at [branch_level], add new blocks, replay the full chain
    into the cache via [update_from_attested_slots], call
    [filter_attestable_slots], and verify the result matches expected. *)
let run_scenario {init_slots; steps} =
  let cache = make_cache () in
  (* So that attested_level and head_level are always > default_published_level. *)
  let level_offset = Int32.to_int default_published_level in
  (* chain.(level) := (block_hash, slots attested at that block). *)
  let chain =
    Array.make (max_test_level + 1) (dummy_block_hash, Cache.SlotSet.empty)
  in
  let hash_counter = ref 0 in
  (* Allocate a fresh block hash for each block. *)
  let next_hash () =
    incr hash_counter ;
    make_block_hash !hash_counter
  in
  (* Current chain length (last block index). *)
  let head_level = ref 0 in
  (* Reference implementation: per-slot (level, hash) for expected filter result. *)
  let sim_cache = Array.make num_test_slots None in
  (* Update [sim_cache] as the cache would when processing one block. *)
  let simulate_update_from_proposal ~level ~hash ~predecessor_hash ~grandparent
      slots =
    Cache.SlotSet.iter
      (fun slot_index ->
        match sim_cache.(slot_index) with
        | None -> sim_cache.(slot_index) <- Some (level, hash)
        | Some (existing_level, existing_hash) ->
            let level_diff = level - existing_level in
            if level_diff = 0 then sim_cache.(slot_index) <- Some (level, hash)
            else if level_diff = 1 then
              if existing_hash = predecessor_hash then ()
              else sim_cache.(slot_index) <- Some (level, hash)
            else if level_diff = 2 then
              if existing_hash = grandparent then ()
              else sim_cache.(slot_index) <- Some (level, hash)
            else () (* depth > 2: keep earliest level *))
      slots
  in
  (* Replay chain [1..hl] into the cache; uses [level_offset] for attested_level. *)
  let replay_cache cache hl chain =
    for level = 1 to hl do
      let hash = fst chain.(level) in
      let slot_set = snd chain.(level) in
      let predecessor_hash =
        if level >= 2 then fst chain.(level - 1) else dummy_block_hash
      in
      let grandparent =
        if level >= 3 then fst chain.(level - 2) else dummy_block_hash
      in
      update_from_attested_slots
        cache
        ~attested_level:(Int32.of_int (level + level_offset))
        ~block_hash:hash
        ~predecessor_hash
        ~grandparent
        ~slots:(Cache.SlotSet.elements slot_set)
    done
  in
  (* Append one block to [chain] and [sim_cache]; [slots] = attested at this block. *)
  let add_block slots =
    incr head_level ;
    let level = !head_level in
    let hash = next_hash () in
    let attested_on_chain =
      let found = ref Cache.SlotSet.empty in
      for l = 1 to level - 1 do
        let _, s = chain.(l) in
        found := Cache.SlotSet.union !found s
      done ;
      !found
    in
    let valid_slots =
      List.filter (fun s -> not (Cache.SlotSet.mem s attested_on_chain)) slots
    in
    chain.(level) <- (hash, Cache.SlotSet.of_list valid_slots) ;
    let predecessor_hash =
      if level >= 2 then fst chain.(level - 1) else dummy_block_hash
    in
    let grandparent =
      if level >= 3 then fst chain.(level - 2) else dummy_block_hash
    in
    simulate_update_from_proposal
      ~level
      ~hash
      ~predecessor_hash
      ~grandparent
      (Cache.SlotSet.of_list valid_slots)
  in
  (* Run one scenario step: fork, add blocks, replay cache, check filter
     result. *)
  let execute_step {branch_level; block_slots} =
    (* Rewind chain to [branch_level] (drop blocks after it). *)
    Array.fill
      chain
      (branch_level + 1)
      (!head_level - branch_level)
      (dummy_block_hash, Cache.SlotSet.empty) ;
    head_level := branch_level ;
    (* Reset [sim_cache] and repopulate from chain [1..branch_level]. *)
    Array.fill sim_cache 0 num_test_slots None ;
    for level = 1 to !head_level do
      let hash = fst chain.(level) in
      let slots = snd chain.(level) in
      let predecessor_hash =
        if level >= 2 then fst chain.(level - 1) else dummy_block_hash
      in
      let grandparent =
        if level >= 3 then fst chain.(level - 2) else dummy_block_hash
      in
      simulate_update_from_proposal
        ~level
        ~hash
        ~predecessor_hash
        ~grandparent
        slots
    done ;
    List.iter add_block block_slots ;
    let hl = !head_level in
    replay_cache cache hl chain ;
    let head_hash = fst chain.(hl) in
    let predecessor_hash =
      if hl >= 2 then fst chain.(hl - 1) else dummy_block_hash
    in
    match
      filter_attestable_slots
        cache
        ~attestable_slots:all_test_slots
        ~head_level:(Int32.of_int (hl + level_offset))
        ~head_hash
        ~predecessor_hash
    with
    | Error _trace -> false
    | Ok filtered ->
        (* Mirror [filter_attestable_slots]: no entry → include; depth 0/1
           hash match → exclude; depth >= 2 on current chain → exclude. *)
        let expected_slots =
          List.filter
            (fun slot_index ->
              match sim_cache.(slot_index) with
              | None -> true
              | Some (existing_lvl, existing_hash) ->
                  let level_diff = hl - existing_lvl in
                  if level_diff >= 2 then
                    not
                      (Block_hash.equal
                         existing_hash
                         (fst chain.(existing_lvl)))
                  else if level_diff = 0 then
                    not (Block_hash.equal head_hash existing_hash)
                  else if level_diff = 1 then
                    not (Block_hash.equal predecessor_hash existing_hash)
                  else true)
            all_test_slots
        in
        Cache.SlotSet.equal filtered (Cache.SlotSet.of_list expected_slots)
  in
  List.iter add_block init_slots ;
  List.for_all execute_step steps

let print_scenario {init_slots; steps} =
  let print_slots slots =
    "[" ^ String.concat "; " (List.map string_of_int slots) ^ "]"
  in
  let print_blocks blocks =
    "[" ^ String.concat "; " (List.map print_slots blocks) ^ "]"
  in
  let print_step {branch_level; block_slots} =
    Format.sprintf
      "{branch_level=%d; block_slots=%s}"
      branch_level
      (print_blocks block_slots)
  in
  Format.sprintf
    "{init_slots=%s; steps=[%s]}"
    (print_blocks init_slots)
    (String.concat "; " (List.map print_step steps))

let test_no_duplicate_attestations =
  QCheck2.Test.make
    ~count:10000
    ~print:print_scenario
    ~name:"no duplicate attestations across forks"
    gen_scenario
    run_scenario

let () =
  Test.register
    ~__FILE__
    ~title:(Protocol.name ^ ": fork deduplication warm-up")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"]
    test_fork_deduplication ;
  (* Depth 0 *)
  Test.register
    ~__FILE__
    ~title:(Protocol.name ^ ": depth 0 — head in block_hashes -> filtered")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"; "depth0"]
    test_depth0_head_in_block_hashes_filtered ;
  Test.register
    ~__FILE__
    ~title:(Protocol.name ^ ": depth 0 — head not in block_hashes -> allowed")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"; "depth0"]
    test_depth0_head_not_in_block_hashes_allowed ;
  Test.register
    ~__FILE__
    ~title:(Protocol.name ^ ": depth 0 — same-level accumulation -> filtered")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"; "depth0"]
    test_depth0_head_in_block_hashes_accumulation_filtered ;
  (* Depth 1 *)
  Test.register
    ~__FILE__
    ~title:
      (Protocol.name ^ ": depth 1 — predecessor in block_hashes -> filtered")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"; "depth1"]
    test_depth1_predecessor_in_block_hashes_filtered ;
  Test.register
    ~__FILE__
    ~title:
      (Protocol.name ^ ": depth 1 — predecessor not in block_hashes -> allowed")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"; "depth1"]
    test_depth1_predecessor_not_in_block_hashes_allowed ;
  (* Depth 2 *)
  Test.register
    ~__FILE__
    ~title:(Protocol.name ^ ": depth 2 — empty block_hashes -> allowed")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"; "depth2"]
    test_depth2_empty_block_hashes_allowed ;
  Test.register
    ~__FILE__
    ~title:(Protocol.name ^ ": depth 2 — non-empty block_hashes -> filtered")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"; "depth2"]
    test_depth2_nonempty_block_hashes_filtered ;
  (* Depth 3 *)
  Test.register
    ~__FILE__
    ~title:(Protocol.name ^ ": depth 3 — non-empty block_hashes -> filtered")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"; "depth3"]
    test_depth3_nonempty_block_hashes_filtered ;
  Test.register
    ~__FILE__
    ~title:(Protocol.name ^ ": depth 3 — empty block_hashes -> allowed")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"; "depth3"]
    test_depth3_empty_block_hashes_allowed ;
  Test.register
    ~__FILE__
    ~title:(Protocol.name ^ ": multiple slots mixed depths")
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"]
    test_multiple_slots_mixed ;
  Qcheck_tezt.register
    ~__FILE__
    ~tags:[Protocol.name; "baker"; "dal"; "attestation_cache"]
    test_no_duplicate_attestations
