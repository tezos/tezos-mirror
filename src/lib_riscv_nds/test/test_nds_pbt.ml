(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Trilitech <contact@trili.tech>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    RISC-V NDS (New Durable Storage)
    Invocation:   dune exec src/lib_riscv_nds/test/main.exe \
                  -- --file test_nds_pbt.ml
    Subject:      Unit and property-based tests for in-memory and on-disk
                  backends
*)

open Nds_errors
open Helpers

(** {1 Unit tests}

    Deterministic tests for error handling, boundary conditions, and
    properties where randomization adds no value. *)

let unit_test name f = (name, f)

let test_fresh_size_zero =
  unit_test "fresh registry has size 0" @@ fun (module B : BACKEND) ->
  let r = B.create_registry_with_dbs 0 in
  let sz = B.Registry.size r in
  Check.((Int64.to_int sz = 0) int) ~error_msg:"expected size 0, got %L" ;
  unit

let test_resize_by_one =
  unit_test "resize by 1 succeeds" @@ fun (module B : BACKEND) ->
  let r = B.create_registry_with_dbs 0 in
  for i = 1 to 3 do
    let^? () = B.Registry.resize r (Int64.of_int i) in
    ()
  done ;
  let sz = B.Registry.size r in
  Check.((Int64.to_int sz = 3) int) ~error_msg:"expected size 3, got %L" ;
  for i = 2 downto 0 do
    let^? () = B.Registry.resize r (Int64.of_int i) in
    ()
  done ;
  let sz = B.Registry.size r in
  Check.((Int64.to_int sz = 0) int) ~error_msg:"expected size 0, got %L" ;
  unit

let test_resize_too_large =
  unit_test "resize by more than 1 fails" @@ fun (module B : BACKEND) ->
  let r = B.create_registry_with_dbs 0 in
  check_error
    ~msg:"resize(2)"
    Registry_resize_too_large
    (B.Registry.resize r 2L) ;
  unit

let test_key_too_long =
  unit_test "key too long errors" @@ fun (module B : BACKEND) ->
  let r = B.create_registry_with_dbs 1 in
  let key = Bytes.make 256 'x' in
  let value = Bytes.of_string "v" in
  B.Database.set r ~db_index:0L ~key ~value
  |> check_error ~msg:"set" Key_too_long ;
  B.Database.exists r ~db_index:0L ~key
  |> check_error ~msg:"exists" Key_too_long ;
  B.Database.read r ~db_index:0L ~key ~offset:0L ~len:10L
  |> check_error ~msg:"read" Key_too_long ;
  B.Database.write r ~db_index:0L ~key ~offset:0L ~value
  |> check_error ~msg:"write" Key_too_long ;
  B.Database.value_length r ~db_index:0L ~key
  |> check_error ~msg:"value_length" Key_too_long ;
  B.Database.delete r ~db_index:0L ~key
  |> check_error ~msg:"delete" Key_too_long ;
  unit

let test_db_index_oob =
  unit_test "database index out of bounds" @@ fun (module B : BACKEND) ->
  let r = B.create_registry_with_dbs 1 in
  let key = Bytes.of_string "k" in
  let value = Bytes.of_string "v" in
  check_error
    ~msg:"set"
    Database_index_out_of_bounds
    (B.Database.set r ~db_index:1L ~key ~value) ;
  check_error
    ~msg:"exists"
    Database_index_out_of_bounds
    (B.Database.exists r ~db_index:1L ~key) ;
  check_error
    ~msg:"read"
    Database_index_out_of_bounds
    (B.Database.read r ~db_index:1L ~key ~offset:0L ~len:1L) ;
  check_error
    ~msg:"write"
    Database_index_out_of_bounds
    (B.Database.write r ~db_index:1L ~key ~offset:0L ~value) ;
  check_error
    ~msg:"value_length"
    Database_index_out_of_bounds
    (B.Database.value_length r ~db_index:1L ~key) ;
  check_error
    ~msg:"delete"
    Database_index_out_of_bounds
    (B.Database.delete r ~db_index:1L ~key) ;
  check_error
    ~msg:"hash"
    Database_index_out_of_bounds
    (B.Database.hash r ~db_index:1L) ;
  unit

let test_registry_ops_oob =
  unit_test "registry ops with OOB index error" @@ fun (module B : BACKEND) ->
  let r = B.create_registry_with_dbs 1 in
  check_error
    ~msg:"copy_database"
    Database_index_out_of_bounds
    (B.Registry.copy_database r ~src:5L ~dst:0L) ;
  check_error
    ~msg:"move_database"
    Database_index_out_of_bounds
    (B.Registry.move_database r ~src:0L ~dst:5L) ;
  check_error ~msg:"clear" Database_index_out_of_bounds (B.Registry.clear r 5L) ;
  unit

let test_shrink_makes_oob =
  unit_test "shrink makes removed index out of bounds"
  @@ fun (module B : BACKEND) ->
  let r = B.create_registry_with_dbs 2 in
  let key = Bytes.of_string "k" in
  let^? () = B.Database.set r ~db_index:1L ~key ~value:(Bytes.of_string "v") in
  let^? () = B.Registry.resize r 1L in
  check_error
    ~msg:"exists after shrink"
    Database_index_out_of_bounds
    (B.Database.exists r ~db_index:1L ~key) ;
  unit

let test_delete_preserves_hash =
  unit_test "delete non-existent key preserves hash"
  @@ fun (module B : BACKEND) ->
  let r = B.create_registry_with_dbs 1 in
  let^? h_before = B.Database.hash r ~db_index:0L in
  let^? () = B.Database.delete r ~db_index:0L ~key:(Bytes.of_string "absent") in
  let^? h_after = B.Database.hash r ~db_index:0L in
  Check.((Bytes.to_string h_before = Bytes.to_string h_after) string)
    ~error_msg:"hash changed after deleting absent key" ;
  unit

(** Regression test for a disk backend bug uncovered by bisimulation:
    after deleting a key from an empty database and then copying that
    database over one that held the same key, [write] at a non-zero
    offset to the (now absent) key should return [Offset_not_found] but
    the Rust backend could return [Key_not_found]. The [delete] on the
    empty db is a no-op semantically but leaves stale state in the
    Rust AVL tree that [copy_database] propagates.
    {v Set(db=0, key="a", val="")    -- create key in db 0
       Delete(db=1, key="a")         -- no-op delete on empty db 1
       Copy_database(src=1, dst=0)   -- overwrite db 0 with db 1
       Write(db=0, key="a", off=1, val="") -- expects Offset_too_large v} *)
let test_write_after_copy_clears_key =
  unit_test "write after copy_database returns wrong error"
  @@ fun (module B : BACKEND) ->
  let r = B.create_registry_with_dbs 2 in
  let key = Bytes.of_string "a" in
  let^? () = B.Database.set r ~db_index:0L ~key ~value:Bytes.empty in
  let^? () = B.Database.delete r ~db_index:1L ~key in
  let^? () = B.Registry.copy_database r ~src:1L ~dst:0L in
  let res =
    B.Database.write r ~db_index:0L ~key ~offset:1L ~value:Bytes.empty
  in
  check_error ~msg:"write after copy" Offset_too_large res ;
  unit

let test_checkout_unknown_commit =
  unit_test "disk: checkout unknown commit fails" @@ fun () ->
  let repo = get_disk_repo () in
  let bogus = Bytes.make 32 '\x00' in
  let raised =
    try
      let _r = Octez_riscv_nds_disk.Normal.Registry.checkout repo bogus in
      false
    with Failure _ -> true
  in
  Check.((raised = true) bool)
    ~error_msg:"checkout with bogus commit should raise" ;
  unit

(** {1 Property-based tests}

    Randomized tests for properties where varying inputs improves
    coverage.  Single-operation key-value and registry properties are
    subsumed by the bisimulation test; the PBT tests below cover hash
    properties and stateful/cross-backend invariants that bisimulation
    does not check. *)

(** {2 Hash properties} *)

(** hash determinism *)
let test_hash_determinism ?long_factor (module B : BACKEND) =
  QCheck2.Test.make
    ?long_factor
    ~name:(B.name ^ ": hash determinism")
    QCheck2.Gen.(pair gen_valid_key gen_small_value)
  @@ fun (key, value) ->
  let r1 = B.create_registry_with_dbs 1 in
  let r2 = B.create_registry_with_dbs 1 in
  let^? () = B.Database.set r1 ~db_index:0L ~key ~value in
  let^? () = B.Database.set r2 ~db_index:0L ~key ~value in
  let^? h1 = B.Database.hash r1 ~db_index:0L in
  let^? h2 = B.Database.hash r2 ~db_index:0L in
  Bytes.equal h1 h2

(** hash sensitivity *)
let test_hash_sensitivity ?long_factor (module B : BACKEND) =
  QCheck2.Test.make
    ?long_factor
    ~name:(B.name ^ ": hash changes when value changes")
    QCheck2.Gen.(
      let* key = gen_valid_key in
      let* v1 = gen_nonempty_value in
      let* v2 = gen_nonempty_value in
      return (key, v1, v2))
  @@ fun (key, v1, v2) ->
  QCheck2.assume (not (Bytes.equal v1 v2)) ;
  let r = B.create_registry_with_dbs 1 in
  let^? () = B.Database.set r ~db_index:0L ~key ~value:v1 in
  let^? h1 = B.Database.hash r ~db_index:0L in
  let^? () = B.Database.set r ~db_index:0L ~key ~value:v2 in
  let^? h2 = B.Database.hash r ~db_index:0L in
  not (Bytes.equal h1 h2)

(** isolation between databases *)
let test_db_isolation ?long_factor (module B : BACKEND) =
  QCheck2.Test.make
    ?long_factor
    ~name:(B.name ^ ": isolation between databases")
    QCheck2.Gen.(pair gen_valid_key gen_small_value)
  @@ fun (key, value) ->
  let r = B.create_registry_with_dbs 2 in
  let^? h1_before = B.Database.hash r ~db_index:1L in
  let^? () = B.Database.set r ~db_index:0L ~key ~value in
  let^? h1_after = B.Database.hash r ~db_index:1L in
  Bytes.equal h1_before h1_after

(** registry hash determinism *)
let test_registry_hash_determinism ?long_factor (module B : BACKEND) =
  QCheck2.Test.make
    ?long_factor
    ~name:(B.name ^ ": registry hash determinism")
    QCheck2.Gen.(pair gen_valid_key gen_small_value)
  @@ fun (key, value) ->
  let r1 = B.create_registry_with_dbs 1 in
  let r2 = B.create_registry_with_dbs 1 in
  let^? () = B.Database.set r1 ~db_index:0L ~key ~value in
  let^? () = B.Database.set r2 ~db_index:0L ~key ~value in
  let h1 = B.Registry.hash r1 in
  let h2 = B.Registry.hash r2 in
  Bytes.equal h1 h2

(** Hash revert: set(k, v1), set(k, v2), set(k, v1) restores original hash *)
let test_hash_revert ?long_factor (module B : BACKEND) =
  QCheck2.Test.make
    ?long_factor
    ~name:(B.name ^ ": hash reverts when value is restored")
    QCheck2.Gen.(
      let* key = gen_valid_key in
      let* v1 = gen_nonempty_value in
      let* v2 = gen_nonempty_value in
      return (key, v1, v2))
  @@ fun (key, v1, v2) ->
  QCheck2.assume (not (Bytes.equal v1 v2)) ;
  let r = B.create_registry_with_dbs 1 in
  let^? () = B.Database.set r ~db_index:0L ~key ~value:v1 in
  let^? h_original = B.Database.hash r ~db_index:0L in
  let^? () = B.Database.set r ~db_index:0L ~key ~value:v2 in
  let^? () = B.Database.set r ~db_index:0L ~key ~value:v1 in
  let^? h_reverted = B.Database.hash r ~db_index:0L in
  Bytes.equal h_original h_reverted

(** Hash determinism: two fresh registries that undergo the same operation
    sequence must have identical hashes. *)
let test_sequence_hash_determinism ?long_factor (module B : BACKEND) =
  QCheck2.Test.make
    ?long_factor
    ~name:(B.name ^ ": same ops produce same hash")
    ~print:print_scenario
    (gen_scenario ~max_dbs:2 ~max_keys:4 ~max_ops:30)
  @@ fun (num_dbs, ops) ->
  let r1 = B.create_registry_with_dbs num_dbs in
  let r2 = B.create_registry_with_dbs num_dbs in
  apply_ops (module B) r1 ops ;
  apply_ops (module B) r2 ops ;
  let h1 = B.Registry.hash r1 in
  let h2 = B.Registry.hash r2 in
  Bytes.equal h1 h2

(** Commutativity: operations on different databases commute. *)
let test_independent_ops_commute ?long_factor (module B : BACKEND) =
  QCheck2.Test.make
    ?long_factor
    ~name:(B.name ^ ": ops on different databases commute")
    QCheck2.Gen.(
      let* key = gen_valid_key in
      let* v0 = gen_small_value in
      let* v1 = gen_small_value in
      return (key, v0, v1))
  @@ fun (key, v0, v1) ->
  let r1 = B.create_registry_with_dbs 2 in
  let^? () = B.Database.set r1 ~db_index:0L ~key ~value:v0 in
  let^? () = B.Database.set r1 ~db_index:1L ~key ~value:v1 in
  let h1 = B.Registry.hash r1 in
  let r2 = B.create_registry_with_dbs 2 in
  let^? () = B.Database.set r2 ~db_index:1L ~key ~value:v1 in
  let^? () = B.Database.set r2 ~db_index:0L ~key ~value:v0 in
  let h2 = B.Registry.hash r2 in
  Bytes.equal h1 h2

(** {2 Disk-only properties} *)

[@@@warning "-16"]

(** commit/checkout roundtrip *)
let test_commit_checkout_roundtrip ?long_factor =
  QCheck2.Test.make
    ?long_factor
    ~count:10
    ~name:"disk: commit/checkout roundtrip"
    QCheck2.Gen.(pair gen_valid_key gen_small_value)
  @@ fun (key, value) ->
  let open Disk_backend in
  let repo = get_disk_repo () in
  let r = Registry.create repo in
  let^? () = Registry.resize r 1L in
  let^? () = Database.set r ~db_index:0L ~key ~value in
  let h_before = Registry.hash r in
  let commit_id = Registry.commit r in
  let r2 = Registry.checkout repo commit_id in
  let h_after = Registry.hash r2 in
  Bytes.equal h_before h_after

(** multiple commits are independent *)
let test_multiple_commits ?long_factor =
  QCheck2.Test.make
    ?long_factor
    ~count:10
    ~name:"disk: multiple commits are independent"
    QCheck2.Gen.(
      let* key = gen_valid_key in
      let* v1 = gen_small_value in
      let* v2 = gen_small_value in
      return (key, v1, v2))
  @@ fun (key, v1, v2) ->
  let open Disk_backend in
  QCheck2.assume (not (Bytes.equal v1 v2)) ;
  let repo = get_disk_repo () in
  let r = Registry.create repo in
  let^? () = Registry.resize r 1L in
  let^? () = Database.set r ~db_index:0L ~key ~value:v1 in
  let h1 = Registry.hash r in
  let commit1 = Registry.commit r in
  let^? () = Database.set r ~db_index:0L ~key ~value:v2 in
  let _commit2 = Registry.commit r in
  let r_restored = Registry.checkout repo commit1 in
  let h_restored = Registry.hash r_restored in
  Bytes.equal h1 h_restored

(** Commit/checkout snapshot: commit after a random sequence, do more ops,
    checkout — the restored state must match the hash at commit time. *)
let test_commit_checkout_snapshot ?long_factor =
  let gen =
    let open QCheck2.Gen in
    let* num_dbs = gen_small_pos ~max:3 in
    let* num_keys = gen_small_pos ~max:4 in
    let* num_ops_before = gen_small_pos ~max:15 in
    let* num_ops_after = gen_small_pos ~max:15 in
    let* keys =
      list_repeat
        num_keys
        (map
           Bytes.of_string
           (string_size (frequency [(3, 1 -- 8); (1, pure 0)])))
    in
    let* ops_before = list_repeat num_ops_before (gen_op ~num_dbs ~keys) in
    let* ops_after = list_repeat num_ops_after (gen_op ~num_dbs ~keys) in
    return (num_dbs, ops_before, ops_after)
  in
  QCheck2.Test.make
    ?long_factor
    ~count:30
    ~name:"disk: commit/checkout preserves snapshot across further mutations"
    ~print:(fun (num_dbs, ops1, ops2) ->
      Format.asprintf
        "num_dbs=%d\nbefore commit: %a\nafter commit: %a"
        num_dbs
        pp_ops
        ops1
        pp_ops
        ops2)
    gen
  @@ fun (num_dbs, ops_before, ops_after) ->
  let open Disk_backend in
  let repo = get_disk_repo () in
  let r = Registry.create repo in
  grow_registry Registry.resize r num_dbs ;
  apply_ops (module Disk_backend) r ops_before ;
  let h_at_commit = Registry.hash r in
  let commit_id = Registry.commit r in
  apply_ops (module Disk_backend) r ops_after ;
  let r2 = Registry.checkout repo commit_id in
  let h_restored = Registry.hash r2 in
  Bytes.equal h_at_commit h_restored

(** {2 Proof properties} *)

(** Deterministic check of the proof's state hashes: a proof produced from
    the NDS registry must record, as its initial state, the hash of the
    Normal-mode registry taken just before it was wrapped into prove mode,
    and, as its final state, the hash of the prove-mode registry after the
    last recorded operation. *)
let test_proof_state_hashes (module B : PROOF_LIFECYCLE_BACKEND) () =
  let key = Bytes.of_string "k" in
  (* Set up some Normal-mode state before starting the proof. *)
  let normal_r = B.create_normal_with_dbs 2 in
  let^? () =
    B.Normal.Database.set
      normal_r
      ~db_index:0L
      ~key
      ~value:(Bytes.of_string "v0")
  in
  let^? () =
    B.Normal.Database.set
      normal_r
      ~db_index:1L
      ~key
      ~value:(Bytes.of_string "v1")
  in
  (* Hash of the state right before wrapping into prove mode. *)
  let h_start = B.Normal.Registry.hash normal_r in
  let prove_r = B.Prove.start_proof normal_r in
  (* A few prove-mode operations. *)
  let^? () =
    B.Prove.Database.set prove_r ~db_index:0L ~key ~value:(Bytes.of_string "v2")
  in
  let^? () = B.Prove.Database.delete prove_r ~db_index:1L ~key in
  (* Hash of the prove-mode state after the last operation. *)
  let h_stop = B.Prove.Registry.hash prove_r in
  let proof = B.Prove.produce_proof prove_r in
  Check.(
    (Bytes.to_string (B.Prove.Proof.start_state proof) = Bytes.to_string h_start)
      string)
    ~error_msg:"proof start_state %L does not match Normal registry hash %R" ;
  Check.(
    (Bytes.to_string (B.Prove.Proof.stop_state proof) = Bytes.to_string h_stop)
      string)
    ~error_msg:"proof stop_state %L does not match Prove registry hash %R" ;
  unit

(** {2 Verify properties} *)

(** End-to-end proof verification: record a read/write step in Prove mode,
    serialise the proof, deserialise it, then replay the step in Verify mode
    against the proof.  The verify-mode registry must start at the proof's
    start state, the recorded reads must replay to the same values, and after
    replaying the write the registry must reach the proof's stop state. *)
let test_verify_replays_proof (module B : VERIFY_LIFECYCLE_BACKEND) () =
  let key_a = Bytes.of_string "a" in
  let key_b = Bytes.of_string "b" in
  let key_c = Bytes.of_string "c" in
  (* Normal-mode initial state: db0 = {a -> foo}, db1 = {b -> bar}. *)
  let normal = B.create_normal_with_dbs 2 in
  let^? () =
    B.Normal.Database.set
      normal
      ~db_index:0L
      ~key:key_a
      ~value:(Bytes.of_string "foo")
  in
  let^? () =
    B.Normal.Database.set
      normal
      ~db_index:1L
      ~key:key_b
      ~value:(Bytes.of_string "bar")
  in
  let start_hash = B.Normal.Registry.hash normal in
  (* The step: read a from db0, read b from db1, write c to db1. *)
  let prove = B.Prove.start_proof normal in
  let^? _ =
    B.Prove.Database.read prove ~db_index:0L ~key:key_a ~offset:0L ~len:3L
  in
  let^? _ =
    B.Prove.Database.read prove ~db_index:1L ~key:key_b ~offset:0L ~len:3L
  in
  let^? () =
    B.Prove.Database.set
      prove
      ~db_index:1L
      ~key:key_c
      ~value:(Bytes.of_string "baz")
  in
  let stop_hash = B.Prove.Registry.hash prove in
  let proof = B.Prove.produce_proof prove in
  (* Round-trip the proof through its byte serialisation. *)
  let proof_bytes = B.Prove.Proof.serialise proof in
  let proof =
    match B.Prove.Proof.deserialise proof_bytes with
    | Ok proof -> proof
    | Error err ->
        Test.fail
          "proof deserialisation failed: %a"
          Error_monad.pp_print_trace
          err
  in
  Check.(
    (Bytes.to_string (B.Prove.Proof.start_state proof)
    = Bytes.to_string start_hash)
      string)
    ~error_msg:"proof start_state %L does not match Normal registry hash %R" ;
  Check.(
    (Bytes.to_string (B.Prove.Proof.stop_state proof)
    = Bytes.to_string stop_hash)
      string)
    ~error_msg:"proof stop_state %L does not match Prove registry hash %R" ;
  (* Replay the step in Verify mode. *)
  let verify = B.Verify.start_verify proof in
  Check.(
    (Bytes.to_string (B.Verify.Registry.hash verify)
    = Bytes.to_string start_hash)
      string)
    ~error_msg:
      "verify registry start hash %L does not match proof start_state %R" ;
  let^? read_a =
    B.Verify.Database.read verify ~db_index:0L ~key:key_a ~offset:0L ~len:3L
  in
  Check.((Bytes.to_string read_a = "foo") string)
    ~error_msg:"verify read of a: expected foo, got %L" ;
  let^? read_b =
    B.Verify.Database.read verify ~db_index:1L ~key:key_b ~offset:0L ~len:3L
  in
  Check.((Bytes.to_string read_b = "bar") string)
    ~error_msg:"verify read of b: expected bar, got %L" ;
  let^? () =
    B.Verify.Database.set
      verify
      ~db_index:1L
      ~key:key_c
      ~value:(Bytes.of_string "baz")
  in
  Check.(
    (Bytes.to_string (B.Verify.Registry.hash verify) = Bytes.to_string stop_hash)
      string)
    ~error_msg:"verify registry stop hash %L does not match proof stop_state %R" ;
  unit

(** A proof only attests to the data it recorded.  Replaying an operation
    that touches data absent from the proof must be rejected with
    {!Nds_errors.Verification_failed}, rather than silently producing a value
    or a plain invalid-argument error. *)
let test_verify_rejects_unproven_read (module B : VERIFY_LIFECYCLE_BACKEND) () =
  let key_a = Bytes.of_string "a" in
  let key_d = Bytes.of_string "d" in
  (* db0 holds two keys; the proof below only records a read of [a], so the
     subtree covering [d] stays blinded in the proof. *)
  let normal = B.create_normal_with_dbs 1 in
  let^? () =
    B.Normal.Database.set
      normal
      ~db_index:0L
      ~key:key_a
      ~value:(Bytes.of_string "foo")
  in
  let^? () =
    B.Normal.Database.set
      normal
      ~db_index:0L
      ~key:key_d
      ~value:(Bytes.of_string "extra")
  in
  let prove = B.Prove.start_proof normal in
  let^? _ =
    B.Prove.Database.read prove ~db_index:0L ~key:key_a ~offset:0L ~len:3L
  in
  let proof = B.Prove.produce_proof prove in
  let verify = B.Verify.start_verify proof in
  (* Reading [d], whose value is absent from the proof, must diverge. *)
  let raised =
    try
      let _ =
        B.Verify.Database.read verify ~db_index:0L ~key:key_d ~offset:0L ~len:5L
      in
      false
    with Nds_errors.Verification_failed _ -> true
  in
  Check.((raised = true) bool)
    ~error_msg:
      "reading data absent from the proof should raise Verification_failed" ;
  unit

(** Populate every database of a fresh n-database Normal-mode registry with
    one distinct key/value pair. *)
let build_registry_distinct_db (type a)
    (module B : VERIFY_LIFECYCLE_BACKEND with type Normal.Registry.t = a) n =
  let normal = B.create_normal_with_dbs n in
  for i = 0 to n - 1 do
    let key = Bytes.of_string (Printf.sprintf "key-%d" i) in
    let value = Bytes.of_string (Printf.sprintf "value-%d" i) in
    let^? () =
      B.Normal.Database.set normal ~db_index:(Int64.of_int i) ~key ~value
    in
    ()
  done ;
  normal

(** A proof that records no operations leaves the whole registry blinded:
    a single blind leaf carrying its root hash.  Hashing such a fully-blinded
    registry in verify mode must reproduce the original root hash. *)
let test_verify_hashes_fully_blinded_registry
    (module B : VERIFY_LIFECYCLE_BACKEND) () =
  let normal = build_registry_distinct_db (module B) 5 in
  let start_hash = B.Normal.Registry.hash normal in
  (* Record nothing: the step touches no database, so the whole registry
     collapses to a single blind leaf carrying its root hash. *)
  let prove = B.Prove.start_proof normal in
  let proof = B.Prove.produce_proof prove in
  let proof_bytes = B.Prove.Proof.serialise proof in
  let proof =
    match B.Prove.Proof.deserialise proof_bytes with
    | Ok proof -> proof
    | Error err ->
        Test.fail
          "proof deserialisation failed: %a"
          Error_monad.pp_print_trace
          err
  in
  let verify = B.Verify.start_verify proof in
  Check.(
    (Bytes.to_string (B.Verify.Registry.hash verify)
    = Bytes.to_string start_hash)
      string)
    ~error_msg:
      "verify hash of fully-blinded registry %L does not match original root \
       hash %R" ;
  unit

(** When only one of many databases is touched, the proof reveals that one
    database and absorbs the others into blinded ancestors — a partially-blinded
    registry. *)
let test_verify_partially_blinded_registry (module B : VERIFY_LIFECYCLE_BACKEND)
    () =
  (* The choice of 16 is specifically to ensure that all databases have an
     intermediate parent hash, before the root hash of the registry - as
     the registry has MERKLE_ARITY = 4 in proofs. *)
  let num_dbs = 16 in
  let normal = build_registry_distinct_db (module B) num_dbs in
  let start_hash = B.Normal.Registry.hash normal in

  for i = 0 to num_dbs - 1 do
    let prove = B.Prove.start_proof normal in
    (* Touch a single database, leaving the other 15 blinded. *)
    let^? (_ : bytes) =
      B.Prove.Database.read
        prove
        ~db_index:(Int64.of_int i)
        ~key:(Bytes.of_string (Printf.sprintf "key-%d" i))
        ~offset:0L
        ~len:100L
    in
    let proof = B.Prove.produce_proof prove in
    let proof_bytes = B.Prove.Proof.serialise proof in
    let proof =
      match B.Prove.Proof.deserialise proof_bytes with
      | Ok proof -> proof
      | Error err ->
          Test.fail
            "proof deserialisation failed: %a"
            Error_monad.pp_print_trace
            err
    in
    let verify = B.Verify.start_verify proof in
    let^? value =
      B.Verify.Database.read
        verify
        ~db_index:(Int64.of_int i)
        ~key:(Bytes.of_string (Printf.sprintf "key-%d" i))
        ~offset:0L
        ~len:100L
    in
    Check.((Bytes.to_string value = Printf.sprintf "value-%d" i) string)
      ~error_msg:"verify read produced incorrect value %L, expected %R" ;
    Check.(
      (Bytes.to_string (B.Verify.Registry.hash verify)
      = Bytes.to_string start_hash)
        string)
      ~error_msg:
        "verify hash of partially-blinded registry %L does not match original \
         root hash %R"
  done ;
  unit

(** Verify lifecycle (property-based): from an arbitrary initial state,
    recording a single operation in prove mode yields a proof that verifies.

    For a random setup sequence (applied in Normal mode) and one further random
    operation (recorded in Prove mode), the produced proof must:
    - attest to the Normal start state and the Prove stop state
      ([start_state]/[stop_state]);
    - once round-tripped through its serialisation, drive a Verify-mode registry
      that starts at the same hash, replays that one operation to the same
      result as prove mode, and reaches the same stop hash — without diverging
      with {!Nds_errors.Verification_failed}. *)
let test_verify_lifecycle_single_op ?long_factor
    (module B : VERIFY_LIFECYCLE_BACKEND) =
  (* [BACKEND] views over each mode's registry, so the shared [apply_op]/
     [apply_ops] drivers can run a generated operation against them. *)
  let module NormalB : BACKEND with type Registry.t = B.Normal.Registry.t =
  struct
    let name = B.name

    module Registry = B.Normal.Registry
    module Database = B.Normal.Database

    let create_registry_with_dbs _ = assert false
  end in
  let module ProveB : BACKEND with type Registry.t = B.Prove.Registry.t = struct
    let name = B.name

    module Registry = B.Prove.Registry
    module Database = B.Prove.Database

    let create_registry_with_dbs _ = assert false
  end in
  let module VerifyB : BACKEND with type Registry.t = B.Verify.Registry.t =
  struct
    let name = B.name

    module Registry = B.Verify.Registry
    module Database = B.Verify.Database

    let create_registry_with_dbs _ = assert false
  end in
  let gen =
    let open QCheck2.Gen in
    let* num_dbs = gen_small_pos ~max:3 in
    let* num_keys = gen_small_pos ~max:4 in
    let* keys =
      list_repeat
        num_keys
        (map
           Bytes.of_string
           (string_size (frequency [(3, 1 -- 8); (1, pure 0)])))
    in
    let* setup = list_size (gen_small_pos ~max:20) (gen_op ~num_dbs ~keys) in
    let* op = gen_op ~num_dbs ~keys in
    return (num_dbs, setup, op)
  in
  QCheck2.Test.make
    ?long_factor
    ~name:(B.name ^ ": one prove-mode op verifies after arbitrary state")
    ~print:(fun (num_dbs, setup, op) ->
      Format.asprintf
        "num_dbs=%d@\nsetup:@\n%a@\nop: %a"
        num_dbs
        pp_ops
        setup
        pp_op
        op)
    gen
  @@ fun (num_dbs, setup, Any op) ->
  (* Arbitrary initial state, built in Normal mode. *)
  let normal = B.create_normal_with_dbs num_dbs in
  apply_ops (module NormalB) normal setup ;
  let start_hash = B.Normal.Registry.hash normal in
  (* Record exactly one operation in Prove mode. *)
  let prove = B.Prove.start_proof normal in
  let prove_res = apply_op (module ProveB) prove op in
  let stop_hash = B.Prove.Registry.hash prove in
  let proof = B.Prove.produce_proof prove in
  (* Round-trip the proof through its byte serialisation. *)
  let proof =
    match B.Prove.Proof.deserialise (B.Prove.Proof.serialise proof) with
    | Ok proof -> proof
    | Error err ->
        Test.fail
          "proof deserialisation failed: %a"
          Error_monad.pp_print_trace
          err
  in
  (* The proof attests to the Normal start state and the Prove stop state. *)
  let proof_anchors =
    Bytes.equal (B.Prove.Proof.start_state proof) start_hash
    && Bytes.equal (B.Prove.Proof.stop_state proof) stop_hash
  in
  (* Replay the single operation in Verify mode against the proof. *)
  let verify = B.Verify.start_verify proof in
  let verify_start = Bytes.equal (B.Verify.Registry.hash verify) start_hash in
  let verify_res = apply_op (module VerifyB) verify op in
  let results_match = prove_res = verify_res in
  let verify_stop = Bytes.equal (B.Verify.Registry.hash verify) stop_hash in
  proof_anchors && verify_start && results_match && verify_stop

(** {2 Model-based (stateful) properties} *)

(** Bisimulation: a random sequence of operations on the NDS must produce
    the same accept/reject decisions as the reference model (a plain
    [Map.t array]), and the final states must agree. Two systems are
    bisimilar when they accept and reject the same operations at every step
    and converge to the same observable state. *)
let test_bisimulation ?long_factor (module B : BACKEND) =
  QCheck2.Test.make
    ?long_factor
    ~name:(B.name ^ ": bisimulation on random op sequences")
    ~print:print_scenario
    (gen_scenario ~max_dbs:3 ~max_keys:5 ~max_ops:40)
  @@ fun (num_dbs, ops) ->
  let r = B.create_registry_with_dbs num_dbs in
  let model_reg = Model.create_registry_with_dbs num_dbs in
  let check_state = check_state_full model_reg (module B) r in
  List.for_all
    Fun.id
    (List.mapi
       (check_op
          ~check_state
          ~label:"Bisimulation"
          (module B)
          r
          (module Model)
          model_reg)
       ops)

(** {3 Cross-backend equivalence} *)

(** Cross-backend bisimulation: same random ops on memory and disk must
    produce identical results and per-database hashes at every step. *)
let test_cross_backend_bisimulation ?long_factor =
  QCheck2.Test.make
    ?long_factor
    ~name:"cross-backend: memory and disk agree at every step"
    ~print:print_scenario
    (gen_scenario ~max_dbs:2 ~max_keys:4 ~max_ops:20)
    (fun (num_dbs, ops) ->
      let r_mem = Memory_backend.create_registry_with_dbs num_dbs in
      let r_disk = Disk_backend.create_registry_with_dbs num_dbs in
      let check_state =
        check_state_hash
          (module Memory_backend)
          r_mem
          (module Disk_backend)
          r_disk
      in
      List.for_all
        Fun.id
        (List.mapi
           (check_op
              ~check_state
              ~label:"Cross-backend"
              (module Disk_backend)
              r_disk
              (module Memory_backend)
              r_mem)
           ops))

(** {1 Test registration} *)

let register_unit (module B : BACKEND) (name, f) =
  let body () = f (module B : BACKEND) in
  let body = if B.name = Disk_backend.name then with_disk_gc body else body in
  Test.register
    ~__FILE__
    ~title:(String.concat ": " [B.name; name])
    ~tags:["unit"; "nds"; B.name]
    body

let register_unit_disk (name, f) =
  Test.register
    ~__FILE__
    ~title:("disk: " ^ name)
    ~tags:["unit"; "nds"; "disk"]
    (with_disk_gc f)

let register_pbt ?(long = false) ?long_factor ?(extra_tags = [])
    (module B : BACKEND) f =
  let register =
    if String.starts_with ~prefix:"disk" B.name then register_pbt_with_disk_gc
    else Qcheck_tezt.register
  in
  register
    ~__FILE__
    ~seed:Random
    ~long
    ~tags:(["nds"; B.name] @ (if long then ["long"] else []) @ extra_tags)
    (f ?long_factor (module B : BACKEND))

let register_pbt_disk ?(long = false) ?long_factor f =
  register_pbt_with_disk_gc
    ~__FILE__
    ~seed:Random
    ~long
    ~tags:(["nds"; "disk"] @ if long then ["long"] else [])
    (f ?long_factor)

let register_proof_tests (module B : PROOF_LIFECYCLE_BACKEND) =
  let is_disk = String.starts_with ~prefix:"disk" B.name in
  let unit_body () = test_proof_state_hashes (module B) () in
  let unit_body = if is_disk then with_disk_gc unit_body else unit_body in
  Test.register
    ~__FILE__
    ~title:(B.name ^ ": proof has correct start/stop state hashes")
    ~tags:["unit"; "nds"; "proof"; B.name]
    unit_body

let register_verify_tests (module B : VERIFY_LIFECYCLE_BACKEND) =
  let is_disk = String.starts_with ~prefix:"disk" B.name in
  let register title body =
    let body = if is_disk then with_disk_gc body else body in
    Test.register
      ~__FILE__
      ~title:(B.name ^ ": " ^ title)
      ~tags:["unit"; "nds"; "verify"; B.name]
      body
  in
  register "verify replays a proof" @@ test_verify_replays_proof (module B) ;
  register "verify rejects a read absent from the proof"
  @@ test_verify_rejects_unproven_read (module B) ;
  register "verify hashes a fully-blinded registry"
  @@ test_verify_hashes_fully_blinded_registry (module B) ;
  register "verify works over a partially-blinded registry"
  @@ test_verify_partially_blinded_registry (module B)

(** Register the verify-lifecycle PBT for a {!VERIFY_LIFECYCLE_BACKEND}, wrapping
    the disk backend's iterations with the file-handle GC. *)
let register_verify_lifecycle_pbt ?long_factor
    (module B : VERIFY_LIFECYCLE_BACKEND) =
  let register =
    if String.starts_with ~prefix:"disk" B.name then register_pbt_with_disk_gc
    else Qcheck_tezt.register
  in
  register
    ~__FILE__
    ~seed:Random
    ~long:true
    ~tags:["nds"; "verify"; B.name; "long"]
    (test_verify_lifecycle_single_op ?long_factor (module B))

let register_with_backend (backend : (module BACKEND)) =
  (* Unit tests *)
  List.iter
    (register_unit backend)
    [
      test_fresh_size_zero;
      test_resize_by_one;
      test_resize_too_large;
      test_key_too_long;
      test_db_index_oob;
      test_registry_ops_oob;
      test_shrink_makes_oob;
      test_delete_preserves_hash;
      test_write_after_copy_clears_key;
    ] ;
  (* PBT tests *)
  List.iter
    (register_pbt backend)
    [
      test_hash_determinism;
      test_hash_sensitivity;
      test_db_isolation;
      test_registry_hash_determinism;
      test_hash_revert;
    ] ;
  List.iter
    (register_pbt ~long:true ~long_factor:5 backend)
    [test_sequence_hash_determinism; test_independent_ops_commute]

let () =
  register_with_backend (module Memory_backend) ;
  register_with_backend (module Memory_prove_backend) ;
  register_with_backend (module Disk_backend) ;
  register_with_backend (module Disk_prove_backend) ;
  register_with_backend (module Memory_verify_backend) ;
  register_with_backend (module Disk_verify_backend) ;
  register_unit_disk test_checkout_unknown_commit ;
  List.iter
    register_pbt_disk
    [
      test_commit_checkout_roundtrip;
      test_multiple_commits;
      test_commit_checkout_snapshot;
    ] ;
  (* Bisimulation *)
  register_pbt
    ~long:true
    ~long_factor:500
    (module Memory_backend)
    test_bisimulation ;
  register_pbt
    ~long:true
    ~long_factor:250
    (module Memory_prove_backend)
    test_bisimulation ;
  register_pbt
    ~long:true
    ~long_factor:10
    ~extra_tags:["slow"]
    (module Disk_backend)
    test_bisimulation ;
  register_pbt
    ~long:true
    ~long_factor:10
    ~extra_tags:["slow"]
    (module Disk_prove_backend)
    test_bisimulation ;
  register_pbt
    ~long:true
    ~long_factor:50
    (module Memory_verify_backend)
    test_bisimulation ;
  register_pbt
    ~long:true
    ~long_factor:10
    (module Disk_verify_backend)
    test_bisimulation ;
  register_pbt_disk ~long:true ~long_factor:5 test_cross_backend_bisimulation ;
  (* Proof API *)
  register_proof_tests (module Memory_proof_lifecycle) ;
  register_proof_tests (module Disk_proof_lifecycle) ;
  (* Verify API: the genuine replay-of-prior-state and divergence paths. *)
  register_verify_tests (module Memory_verify_lifecycle) ;
  register_verify_tests (module Disk_verify_lifecycle) ;
  register_verify_lifecycle_pbt ~long_factor:50 (module Memory_verify_lifecycle) ;
  register_verify_lifecycle_pbt ~long_factor:10 (module Disk_verify_lifecycle)
