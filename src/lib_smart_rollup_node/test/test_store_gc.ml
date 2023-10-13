(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Smart rollup node library
    Invocation: dune exec src/lib_smart_rollup_node/test/main.exe \
                -- -f src/lib_smart_rollup_node/test/test_store_gc.ml
    Subject:    Unit tests GC for rollup node store
*)

let build_chain node_ctxt ~genesis ~length =
  let open Lwt_result_syntax in
  let* blocks = Helpers.append_dummy_l2_chain node_ctxt ~length in
  return (genesis :: blocks)

let check_raw_read name store ~gc_level (block : Sc_rollup_block.t)
    ?(should_exist = block.header.level >= gc_level) proj read key =
  let open Lwt_result_syntax in
  let+ res = read (proj store) key in
  match res with
  | None when should_exist ->
      Assert.fail_msg
        "%s has no binding for level %ld but should have been kept by the GC"
        name
        block.header.level
  | Some _ when not should_exist ->
      Assert.fail_msg
        "%s has a binding for level %ld but should have been removed by the GC"
        name
        block.header.level
  | _ -> ()

let gc_test node_ctxt ~genesis =
  let open Lwt_result_syntax in
  let length = 100 in
  let gc_level = 50l in
  let* chain = build_chain node_ctxt ~genesis ~length in
  (* Garbage collecting everything below level 50 *)
  let store = Node_context.Internal_for_tests.unsafe_get_store node_ctxt in
  let* () = Store.gc store ~level:gc_level in
  let* last_block = Helpers.append_l2_block node_ctxt ["\001I'm new"] in
  let*! () = Store.wait_gc_completion store in
  (* Checking result of GC *)
  let* () =
    List.iter_es
      (fun (block : Sc_rollup_block.t) ->
        (* Checking low-level accesses through raw store *)
        let* () =
          check_raw_read
            "l2_blocks"
            store
            ~gc_level
            block
            (fun s -> s.Store.l2_blocks)
            Store.L2_blocks.read
            block.header.block_hash
        in
        let* () =
          check_raw_read
            "messages"
            store
            ~gc_level
            block
            (fun s -> s.Store.messages)
            Store.Messages.read
            block.header.inbox_witness
        in
        let* () =
          check_raw_read
            "inboxes"
            store
            ~gc_level
            block
            (fun s -> s.Store.inboxes)
            Store.Inboxes.read
            block.header.inbox_hash
        in
        let* () =
          match block.header.commitment_hash with
          | None -> return_unit
          | Some commitment_hash ->
              check_raw_read
                "commitments"
                store
                ~gc_level
                block
                ~should_exist:(block.header.level > Int32.sub gc_level 3l)
                (* We may keep an extra commitment because we jump over commitment
                   periods (a commitment period is 3 blocks for tests). *)
                  (fun s -> s.Store.commitments)
                Store.Commitments.read
                commitment_hash
        in
        let* () =
          check_raw_read
            "levels"
            store
            ~gc_level
            block
            (fun s -> s.Store.levels_to_hashes)
            Store.Levels_to_hashes.find
            block.header.level
        in
        (* Checking access through Node_context *)
        let* store_block_by_hash =
          Node_context.find_l2_block node_ctxt block.header.block_hash
        in
        let* store_block_by_level =
          Node_context.find_l2_block_by_level node_ctxt block.header.level
        in
        (match (store_block_by_hash, store_block_by_level) with
        | None, None when block.header.level < gc_level -> ()
        | (Some _, _ | _, Some _) when block.header.level < gc_level ->
            Assert.fail_msg
              "Block %ld is available but should have been removed by the GC"
              block.header.level
        | _, None | None, _ -> assert false
        | Some store_block_by_hash, Some store_block_by_level ->
            Helpers.Assert.L2_block.equal
              ~loc:__LOC__
              ~msg:"stored_block_by_hash_ok"
              store_block_by_hash
              block ;
            Helpers.Assert.L2_block.equal
              ~loc:__LOC__
              ~msg:"stored_block_by_level_ok"
              store_block_by_level
              block) ;
        return_unit)
      (chain @ [last_block])
  in
  return_unit

let tests =
  List.map
    (fun proto ->
      Helpers.alcotest `Quick Wasm_2_0_0 proto ~boot_sector:"" gc_test)
    (Protocol_plugins.registered_protocols ())

let () =
  Alcotest_lwt.run ~__FILE__ "lib_smart_rollup_node" [("store_gc", tests)]
  |> Lwt_main.run
