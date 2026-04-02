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
    ]

let () =
  register_with_backend (module Memory_backend) ;
  register_with_backend (module Disk_backend) ;
  register_unit_disk test_checkout_unknown_commit
