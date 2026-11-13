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
                -- -f src/lib_smart_rollup_node/test/test_store.ml
    Subject:    Unit tests for rollup node store
*)

let build_chain node_ctxt ~genesis ~length =
  let open Lwt_result_syntax in
  let* blocks = Helpers.append_dummy_l2_chain node_ctxt ~length in
  return (genesis :: blocks)

let check_raw_read name store ~gc_level (block : Sc_rollup_block.t)
    ?(should_exist = block.header.level >= gc_level) read key =
  let open Lwt_result_syntax in
  let+ res = read store key in
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
          Store.L2_blocks.find
          block.header.block_hash
      in
      let* () =
        check_raw_read
          "messages"
          store
          ~gc_level
          block
          Store.Messages.find
          block.header.inbox_witness
      in
      let* () =
        check_raw_read
          "inboxes"
          store
          ~gc_level
          block
          Store.Inboxes.find
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
              Store.Commitments.find
              commitment_hash
      in
      let* () =
        check_raw_read
          "levels"
          store
          ~gc_level
          block
          Store.L2_levels.find
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
  let gc = Store.gc store ~level:gc_level in
  let* last_block = Helpers.append_l2_block node_ctxt ["\001I'm new"] in
  let* () = gc in
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
  let gc = Store.gc store ~level:gc_level in
  (* Trigger a reorganization by adding a new block on top of the alternative
     head. *)
  let* last_block =
    Helpers.add_l2_block node_ctxt ["\001Reorged"] ~predecessor_l2_block:head
  in
  let* () = gc in
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

(* [find_last_with_commitment] ignores rows on which [l2_blocks] and
   [l2_levels] disagree, and rows above the requested level. *)
let last_with_commitment_ignores_inconsistent_rows node_ctxt ~genesis =
  let open Lwt_result_syntax in
  let* chain = build_chain node_ctxt ~genesis ~length:10 in
  let store = Node_context.Internal_for_tests.unsafe_get_store node_ctxt in
  let*? head =
    match List.rev chain with
    | head :: _ -> Ok head
    | [] -> error_with "empty chain"
  in
  let head_level = head.Sc_rollup_block.header.level in
  (* The expected level is read from the store, not assumed. *)
  let resolved () =
    let* found =
      Store.L2_blocks.find_last_with_commitment store ~level:head_level
    in
    match found with
    | Some b -> return b.Sc_rollup_block.header.level
    | None -> Assert.fail_msg "No block with a commitment was found"
  in
  let* expected = resolved () in
  let stays_within_bound () =
    let* level = resolved () in
    if Compare.Int32.(level > expected) then
      Assert.fail_msg
        "Resolved to level %ld, above the expected level %ld and the level \
         asked for %ld"
        level
        expected
        head_level
    else return_unit
  in
  let add_block ~name ~level ~recorded_at =
    let block_hash = Block_hash.hash_string [name] in
    let* () =
      Store.L2_blocks.store
        store
        {
          head with
          header =
            {
              head.header with
              block_hash;
              level;
              commitment_hash = Some (Commitment.Hash.hash_string [name]);
            };
        }
    in
    Store.L2_levels.store store recorded_at block_hash
  in
  (* Recorded at the expected level in [l2_levels], with a higher own level. *)
  let* () =
    add_block
      ~name:"disagreeing"
      ~level:(Int32.add head_level 5l)
      ~recorded_at:expected
  in
  let* () = stays_within_bound () in
  (* Agreeing levels, but above the requested level. *)
  let* () =
    add_block
      ~name:"above"
      ~level:(Int32.add head_level 9l)
      ~recorded_at:(Int32.add head_level 9l)
  in
  stays_within_bound ()

let mk_tests t =
  List.map
    (fun proto -> Helpers.alcotest `Quick Wasm_2_0_0 proto t)
    (Protocol_plugins.registered_protocols ())

let () =
  Alcotest_lwt.run
    ~__FILE__
    "lib_smart_rollup_node"
    [
      ("store_gc", mk_tests gc_test);
      ("store_gc_reorg", mk_tests gc_test_reorg);
      ( "store_last_with_commitment",
        mk_tests last_with_commitment_ignores_inconsistent_rows );
    ]
  |> Lwt_main.run
