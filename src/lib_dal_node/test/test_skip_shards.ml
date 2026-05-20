(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Dal node snapshot --skip-shards option
   Invocation:   dune exec src/lib_dal_node/test/main.exe \
                  -- --file test_skip_shards.ml
   Subject:      Unit tests for the [Snapshot.Internal_for_tests.is_shard_entry] predicate
                 used to gate shard entries during snapshot import.
*)

open Filename.Infix

let () =
  Test.register
    ~__FILE__
    ~title:"snapshot: is_shard_entry recognises shard tar entries"
    ~tags:["dal"; "snapshot"; "skip_shards"]
  @@ fun () ->
  let shard_entry = Store.Stores_dirs.shard // "12345_3" in
  if not (Snapshot.Internal_for_tests.is_shard_entry shard_entry) then
    Test.fail
      "expected %s to be detected as a shard entry, got false"
      shard_entry ;
  unit

let () =
  Test.register
    ~__FILE__
    ~title:"snapshot: is_shard_entry rejects slot tar entries"
    ~tags:["dal"; "snapshot"; "skip_shards"]
  @@ fun () ->
  let slot_entry = Store.Stores_dirs.slot // "12345_3_1024" in
  if Snapshot.Internal_for_tests.is_shard_entry slot_entry then
    Test.fail
      "expected %s not to be detected as a shard entry, got true"
      slot_entry ;
  unit

let () =
  Test.register
    ~__FILE__
    ~title:"snapshot: is_shard_entry rejects skip-list tar entries"
    ~tags:["dal"; "snapshot"; "skip_shards"]
  @@ fun () ->
  let skip_list_entry = Store.Stores_dirs.skip_list_cells // "cells.bin" in
  if Snapshot.Internal_for_tests.is_shard_entry skip_list_entry then
    Test.fail
      "expected %s not to be detected as a shard entry, got true"
      skip_list_entry ;
  unit

let () =
  Test.register
    ~__FILE__
    ~title:"snapshot: is_shard_entry rejects top-level metadata entries"
    ~tags:["dal"; "snapshot"; "skip_shards"]
  @@ fun () ->
  List.iter
    (fun name ->
      if Snapshot.Internal_for_tests.is_shard_entry name then
        Test.fail
          "expected %s not to be detected as a shard entry, got true"
          name)
    ["chain_id"; "first_seen_level"; "last_processed_level"; "version"] ;
  unit

let () =
  Test.register
    ~__FILE__
    ~title:
      "snapshot: is_shard_entry rejects nested paths whose final directory is \
       not shard"
    ~tags:["dal"; "snapshot"; "skip_shards"]
  @@ fun () ->
  (* Defensive cases: the predicate must match only on the immediate parent
     directory, not on any ancestor that happens to be named "shard". *)
  let cases =
    [
      "some" // Store.Stores_dirs.shard // "extra" // "12345_3";
      Store.Stores_dirs.shard;
      "";
    ]
  in
  List.iter
    (fun name ->
      if Snapshot.Internal_for_tests.is_shard_entry name then
        Test.fail
          "expected %s not to be detected as a shard entry, got true"
          name)
    cases ;
  unit

let () =
  Test.register
    ~__FILE__
    ~title:"snapshot: is_shard_entry requires the shard filename format"
    ~tags:["dal"; "snapshot"; "skip_shards"]
  @@ fun () ->
  (* A shard file is named [<level>_<index>]; entries in the shard directory
     whose basename does not match (non-numeric, wrong arity such as a slot
     [<level>_<index>_<size>] name) must be rejected. *)
  let cases =
    [
      Store.Stores_dirs.shard // "12345_3_1024";
      Store.Stores_dirs.shard // "12345";
      Store.Stores_dirs.shard // "foo_bar";
      Store.Stores_dirs.shard // "12345_";
    ]
  in
  List.iter
    (fun name ->
      if Snapshot.Internal_for_tests.is_shard_entry name then
        Test.fail
          "expected %s not to be detected as a shard entry, got true"
          name)
    cases ;
  unit

let () =
  Test.register
    ~__FILE__
    ~title:"snapshot: is_slot_entry recognises slot tar entries"
    ~tags:["dal"; "snapshot"; "skip_shards"]
  @@ fun () ->
  let slot_entry = Store.Stores_dirs.slot // "12345_3_1024" in
  if not (Snapshot.Internal_for_tests.is_slot_entry slot_entry) then
    Test.fail "expected %s to be detected as a slot entry, got false" slot_entry ;
  unit

let () =
  Test.register
    ~__FILE__
    ~title:"snapshot: is_slot_entry requires the slot filename format"
    ~tags:["dal"; "snapshot"; "skip_shards"]
  @@ fun () ->
  (* A slot file is named [<level>_<index>_<size>]; a shard-shaped
     [<level>_<index>] name, non-numeric or wrong-arity basenames, and entries
     outside the slot directory must all be rejected. *)
  let cases =
    [
      Store.Stores_dirs.slot // "12345_3";
      Store.Stores_dirs.slot // "12345_3_1024_5";
      Store.Stores_dirs.slot // "a_b_c";
      Store.Stores_dirs.shard // "12345_3";
      "chain_id";
    ]
  in
  List.iter
    (fun name ->
      if Snapshot.Internal_for_tests.is_slot_entry name then
        Test.fail
          "expected %s not to be detected as a slot entry, got true"
          name)
    cases ;
  unit
