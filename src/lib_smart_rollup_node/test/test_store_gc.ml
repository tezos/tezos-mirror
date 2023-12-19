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

(** This function ensures that blocks in the [chain] that are after [gc_level]
    are fully available and that information for blocks before [gc_level] has
    indeed been collected. It is meant to be called after a GC finishes. *)
let check_chain_ok ~gc_level node_ctxt store chain =
  let open Lwt_result_syntax in
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
      let* stored_block_by_hash =
        Node_context.find_l2_block node_ctxt block.header.block_hash
      in
      let* stored_block_by_level =
        Node_context.find_l2_block_by_level node_ctxt block.header.level
      in
      (match (stored_block_by_hash, stored_block_by_level) with
      | None, None when block.header.level < gc_level -> ()
      | (Some _, _ | _, Some _) when block.header.level < gc_level ->
          Assert.fail_msg
            "Block %ld is available but should have been removed by the GC"
            block.header.level
      | _, None | None, _ -> assert false
      | Some stored_block_by_hash, Some stored_block_by_level ->
          Helpers.Assert.L2_block.equal
            ~loc:__LOC__
            ~msg:"stored_block_by_hash_ok"
            stored_block_by_hash
            block ;
          Helpers.Assert.L2_block.equal
            ~loc:__LOC__
            ~msg:"stored_block_by_level_ok"
            stored_block_by_level
            block) ;
      return_unit)
    chain

(* Test that garbage collection on store performs as expected. *)
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
  let* () = check_chain_ok ~gc_level node_ctxt store (chain @ [last_block]) in
  return_unit

(* Test that garbage collection on store works correctly in the presence of
   reorganizations which happen during GC. In particular this test ensures that
   no useful data is removed by the GC in this case.

   This test creates a chain with a fork:

   --- A --- B
        `--- B' (head)

   The GC is started and a reorganization is triggered while it is running.

   --- A --- B --- C (head)
        `--- B'
*)
let gc_test_reorg node_ctxt ~genesis =
  let open Lwt_result_syntax in
  let length = 100 in
  let gc_level = 50l in
  let* chain = build_chain node_ctxt ~genesis ~length in
  let head, pred, rest =
    match List.rev chain with
    | [] | [_] -> assert false
    | head :: pred :: r -> (head, pred, List.rev r)
  in
  (* Create a fork at the same level as head *)
  let* head' =
    Helpers.add_l2_block node_ctxt ["\001Forked"] ~predecessor_l2_block:pred
  in
  (* Garbage collecting everything below level 50 *)
  let store = Node_context.Internal_for_tests.unsafe_get_store node_ctxt in
  let* () = Store.gc store ~level:gc_level in
  (* Trigger a reorganization by adding a new block on top of the alternative
     head. *)
  let* last_block =
    Helpers.add_l2_block node_ctxt ["\001Reorged"] ~predecessor_l2_block:head
  in
  let*! () = Store.wait_gc_completion store in
  (* Ensure both forked blocks are available *)
  let* reorged_block_by_hash =
    Node_context.find_l2_block node_ctxt head.header.block_hash
  in
  if reorged_block_by_hash = None then
    Assert.fail_msg "Reorged block is unavailable after GC" ;
  let* alternative_block_by_hash =
    Node_context.find_l2_block node_ctxt head'.header.block_hash
  in
  if alternative_block_by_hash = None then
    Assert.fail_msg "Alternative block after reorg is unavailable after GC" ;
  (* Checking result of GC *)
  let* () =
    check_chain_ok ~gc_level node_ctxt store (rest @ [pred; head; last_block])
  in
  return_unit

let mk_tests t =
  List.map
    (fun proto -> Helpers.alcotest `Quick Wasm_2_0_0 proto ~boot_sector:"" t)
    (Protocol_plugins.registered_protocols ())

let () =
  Alcotest_lwt.run
    ~__FILE__
    "lib_smart_rollup_node"
    [("store_gc", mk_tests gc_test); ("store_gc_reorg", mk_tests gc_test_reorg)]
  |> Lwt_main.run
