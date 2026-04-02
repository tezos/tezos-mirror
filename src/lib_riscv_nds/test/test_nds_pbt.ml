(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
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
  let^? sz = B.Registry.size r in
  Check.((Int64.to_int sz = 0) int) ~error_msg:"expected size 0, got %L" ;
  unit

let test_resize_by_one =
  unit_test "resize by 1 succeeds" @@ fun (module B : BACKEND) ->
  let r = B.create_registry_with_dbs 0 in
  for i = 1 to 3 do
    let^? () = B.Registry.resize r (Int64.of_int i) in
    ()
  done ;
  let^? sz = B.Registry.size r in
  Check.((Int64.to_int sz = 3) int) ~error_msg:"expected size 3, got %L" ;
  for i = 2 downto 0 do
    let^? () = B.Registry.resize r (Int64.of_int i) in
    ()
  done ;
  let^? sz = B.Registry.size r in
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
  let key = Bytes.make 257 'x' in
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
  let^? h1 = B.Registry.hash r1 in
  let^? h2 = B.Registry.hash r2 in
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
  let^? h1 = B.Registry.hash r1 in
  let^? h2 = B.Registry.hash r2 in
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
  let^? h1 = B.Registry.hash r1 in
  let r2 = B.create_registry_with_dbs 2 in
  let^? () = B.Database.set r2 ~db_index:1L ~key ~value:v1 in
  let^? () = B.Database.set r2 ~db_index:0L ~key ~value:v0 in
  let^? h2 = B.Registry.hash r2 in
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
  let^? h_before = Registry.hash r in
  let commit_id = Registry.commit r in
  let r2 = Registry.checkout repo commit_id in
  let^? h_after = Registry.hash r2 in
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
  let^? h1 = Registry.hash r in
  let commit1 = Registry.commit r in
  let^? () = Database.set r ~db_index:0L ~key ~value:v2 in
  let _commit2 = Registry.commit r in
  let r_restored = Registry.checkout repo commit1 in
  let^? h_restored = Registry.hash r_restored in
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
  let^? h_at_commit = Registry.hash r in
  let commit_id = Registry.commit r in
  apply_ops (module Disk_backend) r ops_after ;
  let r2 = Registry.checkout repo commit_id in
  let^? h_restored = Registry.hash r2 in
  Bytes.equal h_at_commit h_restored

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

let register_pbt ?(long = false) ?long_factor (module B : BACKEND) f =
  let register =
    if B.name = Disk_backend.name then register_pbt_with_disk_gc
    else Qcheck_tezt.register
  in
  register
    ~__FILE__
    ~seed:Random
    ~long
    ~tags:(["nds"; B.name] @ if long then ["long"] else [])
    (f ?long_factor (module B : BACKEND))

let register_pbt_disk ?(long = false) ?long_factor f =
  register_pbt_with_disk_gc
    ~__FILE__
    ~seed:Random
    ~long
    ~tags:(["nds"; "disk"] @ if long then ["long"] else [])
    (f ?long_factor)

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
  register_with_backend (module Disk_backend) ;
  register_unit_disk test_checkout_unknown_commit ;
  List.iter
    register_pbt_disk
    [
      test_commit_checkout_roundtrip;
      test_multiple_commits;
      test_commit_checkout_snapshot;
    ]
