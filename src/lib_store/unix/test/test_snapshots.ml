(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    _______

    Component: Store
    Invocation: dune exec src/lib_store/unix/test/main.exe
    Subject: Store tests ( snapshots )
*)

open Test_utils

let check_import_invariants ~test_descr ~rolling
    (previously_baked_blocks, exported_block) (imported_chain_store, head) =
  let open Lwt_result_syntax in
  protect
    ~on_error:(fun err ->
      Format.eprintf "Error while checking invariants at: %s" test_descr ;
      Lwt.return (Error err))
    (fun () ->
      (* Check that the head exists with metadata and corresponds to
           the exported block *)
      let* () =
        assert_presence_in_store ~with_metadata:true imported_chain_store [head]
      in
      let* () =
        assert_presence_in_store
          ~with_metadata:true
          imported_chain_store
          [exported_block]
      in
      Assert_lib.Crypto.equal_block
        ~msg:("imported head consistency: " ^ test_descr)
        (Store.Block.header exported_block)
        (Store.Block.header head) ;
      (* Check that we possess all the blocks wrt our descriptors *)
      let*! savepoint = Store.Chain.savepoint imported_chain_store in
      let*! checkpoint = Store.Chain.checkpoint imported_chain_store in
      let*! caboose = Store.Chain.caboose imported_chain_store in
      let expected_present, expected_absent =
        List.partition
          (fun b ->
            Compare.Int32.(Store.Block.level b <= snd checkpoint)
            && Compare.Int32.(Store.Block.level b >= snd caboose))
          previously_baked_blocks
      in
      let* () =
        assert_presence_in_store
          ~with_metadata:false
          imported_chain_store
          expected_present
      in
      let* () = assert_absence_in_store imported_chain_store expected_absent in
      (* Check that the descriptors are consistent *)
      let* expected_caboose_level =
        if rolling then
          (* In rolling: we expected to have at least the max_op_ttl
                blocks from the head *)
          let* metadata =
            Store.Block.get_block_metadata imported_chain_store head
          in
          let max_op_ttl = Store.Block.max_operations_ttl metadata in
          return Int32.(sub (Store.Block.level head) (of_int max_op_ttl))
        else return 0l
      in
      Assert.equal
        ~msg:("savepoint consistency: " ^ test_descr)
        (Store.Block.level exported_block)
        (snd savepoint) ;
      Assert.equal
        ~msg:("checkpoint consistency: " ^ test_descr)
        (snd savepoint)
        (snd checkpoint) ;
      Assert.equal
        ~msg:("caboose consistency: " ^ __LOC__)
        ~eq:Int32.equal
        ~pp:(fun ppf -> Format.fprintf ppf "%ld")
        expected_caboose_level
        (snd caboose) ;
      return_unit)

let export_import ~test_descr ~previously_baked_blocks ?exported_block_hash
    ~rolling ~export_mode (store_dir, context_dir) chain_store =
  let open Lwt_result_syntax in
  let* () = check_invariants chain_store in
  let open Filename.Infix in
  let snapshot_path = store_dir // "snapshot.full" in
  let chain_name = Distributed_db_version.Name.of_string "test" in
  let exported_block =
    match exported_block_hash with
    | None -> `Alias (`Checkpoint, 0)
    | Some hash -> `Hash (hash, 0)
  in
  let* () =
    Snapshots.export
      ~snapshot_path
      export_mode
      ~rolling
      ~block:exported_block
      ~store_dir
      ~context_dir
      ~chain_name
      ~progress_display_mode:Animation.Auto
      genesis
  in
  let dir = store_dir // "imported_store" in
  let dst_store_dir = dir // "store" in
  let dst_context_dir = dir // "context" in
  let* () =
    Snapshots.import
      ?block:exported_block_hash
      ~snapshot_path
      ~dst_store_dir
      ~dst_context_dir
      ~chain_name
      ~configured_history_mode:None
      ~user_activated_upgrades:[]
      ~user_activated_protocol_overrides:[]
      ~operation_metadata_size_limit:Unlimited
      ~progress_display_mode:Animation.Auto
      genesis
  in
  let* store' =
    Store.init
      ~store_dir:dst_store_dir
      ~context_dir:dst_context_dir
      ~allow_testchains:true
      genesis
  in
  protect
    ~on_error:(fun err ->
      let*! () = Store.close_store store' in
      Lwt.return (Error err))
    (fun () ->
      let chain_store' = Store.main_chain_store store' in
      let*! head' = Store.Chain.current_head chain_store' in
      let*! exported_block =
        match exported_block_hash with
        | Some hash ->
            Assert.equal
              ~msg:("export with given hash: " ^ test_descr)
              ~eq:Block_hash.equal
              (Store.Block.hash head')
              hash ;
            Lwt.return head'
        | None ->
            let*! checkpoint = Store.Chain.checkpoint chain_store in
            Assert.equal
              ~msg:("export checkpoint: " ^ test_descr)
              ~eq:Block_hash.equal
              (Store.Block.hash head')
              (fst checkpoint) ;
            Lwt.return head'
      in
      let history_mode = Store.Chain.history_mode chain_store' in
      assert (
        match history_mode with
        | Rolling _ when rolling -> true
        | Full _ when not rolling -> true
        | _ -> false) ;
      let* () =
        check_import_invariants
          ~test_descr
          ~rolling
          (previously_baked_blocks, exported_block)
          (chain_store', head')
      in
      return (store', chain_store', head'))

let check_baking_continuity ~test_descr ~exported_chain_store
    ~imported_chain_store =
  let open Lwt_result_syntax in
  let open Tezos_protocol_alpha.Protocol.Alpha_context in
  let*! imported_head = Store.Chain.current_head imported_chain_store in
  let* {Constants.parametric = {blocks_per_cycle; preserved_cycles; _}; _} =
    Alpha_utils.get_constants imported_chain_store imported_head
  in
  let imported_history_mode = Store.Chain.history_mode imported_chain_store in
  let imported_offset =
    match imported_history_mode with
    | History_mode.Rolling (Some {offset}) | Full (Some {offset}) -> offset
    | Rolling None | Full None -> History_mode.default_additional_cycles.offset
    | Archive -> assert false
  in
  let*! export_store_head = Store.Chain.current_head exported_chain_store in
  let level_to_reach =
    let min_nb_blocks_to_bake =
      Int32.(
        of_int
          (to_int blocks_per_cycle * (preserved_cycles + imported_offset + 2)))
    in
    Compare.Int32.(
      max
        (Store.Block.level export_store_head)
        (Int32.add (Store.Block.level imported_head) min_nb_blocks_to_bake))
  in
  (* Bake until we have enough cycles to reach our offset (and a bit more) *)
  (* Check invariants after every baking *)
  let rec loop head = function
    | 0 -> return head
    | n ->
        let* new_head = Alpha_utils.bake imported_chain_store head in
        let* () = check_invariants imported_chain_store in
        loop new_head (n - 1)
  in
  let nb_blocks_to_bake_in_import =
    Int32.(to_int (sub level_to_reach (Store.Block.level imported_head)))
  in
  let* last' = loop imported_head nb_blocks_to_bake_in_import in
  (* Also bake with the exported store so we make sure we bake the same blocks *)
  let*! exported_head = Store.Chain.current_head exported_chain_store in
  let* last =
    if Compare.Int32.(Store.Block.level export_store_head < level_to_reach) then
      let nb_blocks_to_bake_in_export =
        Int32.(
          to_int (sub level_to_reach (Store.Block.level export_store_head)))
      in
      let* _blocks, last =
        Alpha_utils.bake_n
          exported_chain_store
          nb_blocks_to_bake_in_export
          exported_head
      in
      return last
    else Store.Block.read_block_by_level exported_chain_store level_to_reach
  in
  Assert_lib.Crypto.equal_block
    ~msg:("check both head after baking: " ^ test_descr)
    (Store.Block.header last)
    (Store.Block.header last') ;
  (* Check that the checkpoint are the same *)
  let*! checkpoint = Store.Chain.checkpoint exported_chain_store in
  let*! checkpoint' = Store.Chain.checkpoint imported_chain_store in
  Assert.equal
    ~msg:("checkpoint equality: " ^ test_descr)
    ~pp:(fun ppf (hash, level) ->
      Format.fprintf ppf "%a (%ld)" Block_hash.pp hash level)
    checkpoint
    checkpoint' ;
  return_unit

let test store_path ~test_descr ?exported_block_level
    ~nb_blocks_to_bake_before_export ~rolling ~export_mode store =
  let open Lwt_result_syntax in
  let chain_store = Store.main_chain_store store in
  let*! genesis_block = Store.Chain.genesis_block chain_store in
  let* previously_baked_blocks, _current_head =
    Alpha_utils.bake_n chain_store nb_blocks_to_bake_before_export genesis_block
  in
  (* We don't have a way to lock two stores in the same process =>
     force merges to trigger by setting a new head via [bake] *)
  let consistency_check () =
    let*! checkpoint = Store.Chain.checkpoint chain_store in
    let*! savepoint = Store.Chain.savepoint chain_store in
    let*! caboose = Store.Chain.caboose chain_store in
    let export_import exported_block_hash =
      export_import
        ~test_descr
        store_path
        chain_store
        ~rolling
        ~export_mode
        ?exported_block_hash
        ~previously_baked_blocks
    in
    let expected_level =
      Option.value ~default:(snd checkpoint) exported_block_level
    in
    let open Snapshots in
    let*! block_opt =
      Store.Block.read_block_by_level_opt chain_store expected_level
    in
    let* o =
      match block_opt with
      | None -> return_some `Unknown
      | Some block ->
          if Compare.Int32.(expected_level = Store.Block.level genesis_block)
          then return_some `Genesis
          else if Compare.Int32.(expected_level = snd caboose) then
            return_some `Caboose
          else if Compare.Int32.(expected_level = snd savepoint) then
            return_some `Pruned_pred
          else if Compare.Int32.(expected_level < snd savepoint) then
            if Compare.Int32.(expected_level < snd caboose) then
              return_some `Unknown
            else return_some `Pruned
          else
            let* metadata = Store.Block.get_block_metadata chain_store block in
            let min_level =
              Compare.Int32.(
                max
                  (Store.Block.level genesis_block)
                  Int32.(
                    sub
                      (Store.Block.level block)
                      (of_int (Store.Block.max_operations_ttl metadata))))
            in
            if Compare.Int32.(min_level < snd caboose) then
              return_some `Not_enough_pred
            else (* Should not fail *)
              return_none
    in
    match o with
    | None ->
        (* Normal behavior *)
        let block = WithExceptions.Option.get ~loc:__LOC__ block_opt in
        let hash =
          Option.map (fun _ -> Store.Block.hash block) exported_block_level
        in
        let* eih = export_import hash in
        return_some eih
    | Some reason -> (
        if expected_level < snd caboose then return_none
        else
          let block = WithExceptions.Option.get ~loc:__LOC__ block_opt in
          let reason_to_string = function
            | `Pruned -> "Pruned"
            | `Pruned_pred -> "Pruned_pred"
            | `Unknown -> "Unknown"
            | `Unknown_ancestor -> "Unknown ancestor"
            | `Caboose -> "Caboose"
            | `Genesis -> "Genesis"
            | `Not_enough_pred -> "Not_enough_pred"
            | `Missing_context -> "Missing_context"
          in
          let*! r = export_import (Some (Store.Block.hash block)) in
          match r with
          | Error [Invalid_export_block {block = _; reason = reason'}]
            when reason = reason' ->
              (* Expected error *)
              return_none
          | Ok _ ->
              Assert.fail_msg
                "Unexpected success in export: expected %s error"
                (reason_to_string reason)
          | Error err ->
              Assert.fail_msg
                "Unexpected error in export. Expected error with %s - Got : %a"
                (reason_to_string reason)
                Error_monad.pp_print_trace
                err)
  in
  let* o = consistency_check () in
  match o with
  | None ->
      (* Encountered an expected error, nothing to do *)
      return_unit
  | Some (store', chain_store', _head) ->
      Lwt.finalize
        (fun () ->
          check_baking_continuity
            ~test_descr
            ~exported_chain_store:chain_store
            ~imported_chain_store:chain_store')
        (fun () ->
          (* only close store' - store will be closed by the test
                wrapper *)
          let*! _ = Store.close_store store' in
          Lwt.return_unit)

let make_tests speed genesis_parameters =
  let open Tezos_protocol_alpha.Protocol.Alpha_context in
  let {
    Parameters.constants =
      {Constants.Parametric.blocks_per_cycle; preserved_cycles; _};
    _;
  } =
    genesis_parameters
  in
  let blocks_per_cycle = Int32.to_int blocks_per_cycle in
  (* "Au paradis des louches" *)
  let nb_initial_blocks_list =
    match speed with
    | `Slow ->
        [
          preserved_cycles * blocks_per_cycle;
          ((2 * preserved_cycles) + 1) * blocks_per_cycle;
          65;
          77;
          89;
        ]
    | `Quick -> [((2 * preserved_cycles) + 1) * blocks_per_cycle; 77]
  in
  let exporter_history_modes =
    let open History_mode in
    match speed with
    | `Slow ->
        [
          Archive;
          Full (Some {offset = 0});
          Full (Some default_additional_cycles);
          Rolling (Some {offset = 0});
          Rolling (Some default_additional_cycles);
        ]
    | `Quick ->
        [
          Full (Some default_additional_cycles);
          Rolling (Some {offset = 0});
          Rolling (Some default_additional_cycles);
        ]
  in
  let export_blocks_levels nb_initial_blocks =
    match speed with
    | `Slow ->
        [
          None;
          Some Int32.(of_int (nb_initial_blocks - blocks_per_cycle));
          Some (Int32.of_int nb_initial_blocks);
        ]
    | `Quick -> [None; Some (Int32.of_int nb_initial_blocks)]
  in
  let export_mode = Snapshots.[Tar; Raw] in
  let permutations =
    List.(
      product
        (product
           nb_initial_blocks_list
           (map export_blocks_levels nb_initial_blocks_list
           |> flatten
           |> List.sort_uniq Stdlib.compare))
        (product exporter_history_modes (product [false; true] export_mode)))
    |> List.map (fun ((a, b), (c, (d, e))) -> (a, b, c, d, e))
  in
  List.filter_map
    (fun ( nb_initial_blocks,
           exported_block_level,
           history_mode,
           rolling,
           export_mode ) ->
      let test_descr =
        Format.asprintf
          "export => import with %d initial blocks from %a to %s (exported \
           block at %s) using %a format"
          nb_initial_blocks
          History_mode.pp
          history_mode
          (if rolling then "rolling" else "full")
          (match exported_block_level with
          | None -> "checkpoint"
          | Some i -> Format.sprintf "level %ld" i)
          Snapshots.pp_snapshot_format
          export_mode
      in
      match history_mode with
      | Rolling _ when rolling = false -> None
      | _ -> (
          match exported_block_level with
          | Some level
            when Compare.Int32.(Int32.of_int nb_initial_blocks <= level) ->
              None
          | None | Some _ ->
              Some
                (wrap_test
                   ~with_gc:true
                   ~keep_dir:false
                   ~history_mode
                   ~patch_context:(fun ctxt ->
                     Alpha_utils.default_patch_context ctxt)
                   ( test_descr,
                     fun store_path store ->
                       test
                         ?exported_block_level
                         ~nb_blocks_to_bake_before_export:nb_initial_blocks
                         ~rolling
                         ~test_descr
                         ~export_mode
                         store_path
                         store ))))
    permutations

let test_rolling export_mode =
  let patch_context ctxt = Alpha_utils.default_patch_context ctxt in
  let test (store_dir, context_dir) store =
    let open Lwt_result_syntax in
    let chain_store = Store.main_chain_store store in
    let*! genesis_block = Store.Chain.genesis_block chain_store in
    let nb_cycles_to_bake = 6 in
    let* _blocks, head =
      Alpha_utils.bake_until_n_cycle_end
        chain_store
        nb_cycles_to_bake
        genesis_block
    in
    (* We don't have a way to lock two stores in the same process =>
       force merges by setting a new head via [bake] *)
    let open Filename.Infix in
    let snapshot_path = store_dir // "snapshot.rolling" in
    let chain_name = Distributed_db_version.Name.of_string "test" in
    let dst_dir = store_dir // "imported_store" in
    let dst_store_dir = dst_dir // "store" in
    let dst_context_dir = dst_dir // "context" in
    let* () =
      Snapshots.export
        ~snapshot_path
        export_mode
        ~rolling:true
        ~block:(`Head 0)
        ~store_dir
        ~context_dir
        ~chain_name
        ~progress_display_mode:Animation.Auto
        genesis
    in
    let* () =
      Snapshots.import
        ~snapshot_path
        ~dst_store_dir
        ~dst_context_dir
        ~chain_name
        ~configured_history_mode:None
        ~user_activated_upgrades:[]
        ~user_activated_protocol_overrides:[]
        ~operation_metadata_size_limit:Unlimited
        ~progress_display_mode:Animation.Auto
        genesis
    in
    let* store' =
      Store.init
        ~patch_context
        ~history_mode:History_mode.default_rolling
        ~readonly:false
        ~store_dir:dst_store_dir
        ~context_dir:dst_context_dir
        ~allow_testchains:true
        genesis
    in
    let chain_store' = Store.main_chain_store store' in
    let* _head = Alpha_utils.bake_until_n_cycle_end chain_store' 4 head in
    let*! checkpoint = Store.Chain.checkpoint chain_store' in
    let* checkpoint_block =
      Store.Block.read_block chain_store' (fst checkpoint)
    in
    let* metadata =
      Store.Block.get_block_metadata chain_store' checkpoint_block
    in
    let max_op_ttl_cp =
      Int32.(
        sub (snd checkpoint) (of_int (Store.Block.max_operations_ttl metadata)))
    in
    let*! caboose = Store.Chain.caboose chain_store' in
    Assert.Int32.equal ~msg:__LOC__ max_op_ttl_cp (snd caboose) ;
    let*! () = Store.close_store store' in
    return_unit
  in
  wrap_test
    ~keep_dir:false
    ~with_gc:true
    ~history_mode:History_mode.default
    ~patch_context
    ( Format.asprintf
        "genesis consistency after rolling import (blocks per cycle = %ld) \
         using %a format"
        Alpha_utils.default_genesis_parameters.constants.blocks_per_cycle
        Snapshots.pp_snapshot_format
        export_mode,
      test )

let make_tests_rolling =
  let export_mode = Snapshots.[Tar; Raw] in
  List.map test_rolling export_mode

(* This test aims to check that the caboose and savepoint are well
   dragged when the first merge occurs, after a rolling snapshot
   import on a block which is not on a cycle's bound. Indeed, in such
   a scenario, the merge procedure may remove blocks bellow the lafl
   without cementing them. It would result in non stored caboose
   (rolling issue) and savepoint (rolling and full issue).
   In this test, we need to increase the number of blocks per cycle to
   avoid the max_op_ttl to hide this potential issue. The exported
   block must be outside the max_op_ttl of the next checkpoint. *)
let test_drag_after_import export_mode =
  let open Lwt_result_syntax in
  let constants =
    Default_parameters.
      {
        constants_test with
        blocks_per_cycle = 256l;
        cycles_per_voting_period = 1l;
        consensus_threshold = 0;
        proof_of_work_threshold = -1L;
      }
  in
  let patch_context ctxt =
    let test_parameters =
      let open Tezos_protocol_alpha_parameters in
      {
        Default_parameters.(parameters_of_constants constants) with
        bootstrap_accounts = Alpha_utils.default_accounts;
      }
    in
    Alpha_utils.patch_context
      ctxt
      ~json:(Default_parameters.json_of_parameters test_parameters)
  in
  let test (store_dir, context_dir) store =
    let chain_store = Store.main_chain_store store in
    let*! genesis_block = Store.Chain.genesis_block chain_store in
    let nb_cycles_to_bake = 2 in
    let* _blocks, head =
      Alpha_utils.bake_until_n_cycle_end
        chain_store
        nb_cycles_to_bake
        genesis_block
    in
    (* We don't have a way to lock two stores in the same process =>
        force merges by setting a new head via [bake] *)
    let open Filename.Infix in
    let snapshot_path = store_dir // "snapshot.rolling" in
    let chain_name = Distributed_db_version.Name.of_string "test" in
    let dst_dir = store_dir // "imported_store" in
    let dst_store_dir = dst_dir // "store" in
    let dst_context_dir = dst_dir // "context" in
    (* export distance is higer than the 120 max_op_tt*)
    let export_distance = 130 in
    let* export_block =
      Store.Block.read_block
        chain_store
        (Store.Block.hash head)
        ~distance:export_distance
    in
    let export_block_hash = Store.Block.hash export_block in
    let* () =
      Snapshots.export
        ~snapshot_path
        export_mode
        ~rolling:true
        ~block:(`Hash (export_block_hash, 0))
        ~store_dir
        ~context_dir
        ~chain_name
        ~progress_display_mode:Animation.Auto
        genesis
    in
    let* () =
      Snapshots.import
        ~snapshot_path
        ~dst_store_dir
        ~dst_context_dir
        ~chain_name
        ~configured_history_mode:None
        ~user_activated_upgrades:[]
        ~user_activated_protocol_overrides:[]
        ~operation_metadata_size_limit:Unlimited
        ~block:export_block_hash
        ~progress_display_mode:Animation.Auto
        genesis
    in
    let* store' =
      Store.init
        ~patch_context
        ~readonly:false
        ~store_dir:dst_store_dir
        ~context_dir:dst_context_dir
        ~allow_testchains:true
        genesis
    in
    let chain_store' = Store.main_chain_store store' in
    (* Finish to bake the current cycle. *)
    let* _, _head =
      Alpha_utils.bake_until_cycle_end chain_store' export_block
    in
    let*! savepoint_hash, savepoint_level =
      Store.Chain.savepoint chain_store'
    in
    let* savepoint = Store.Block.read_block chain_store' savepoint_hash in
    let* metadata = Store.Block.get_block_metadata chain_store' savepoint in
    let expected_caboose =
      Int32.(
        sub savepoint_level (of_int (Store.Block.max_operations_ttl metadata)))
    in
    let*! _, caboose_level = Store.Chain.caboose chain_store' in
    Assert.Int32.equal ~msg:__LOC__ caboose_level expected_caboose ;
    let block_store = Store.Unsafe.get_block_store chain_store' in
    let rec restart n head =
      if n = 0 then return head
      else
        let* _, head = Alpha_utils.bake_until_cycle_end chain_store' head in
        let*! () = Block_store.await_merging block_store in
        let*! _, caboose_level = Store.Chain.caboose chain_store' in
        let*! _, savepoint_level = Store.Chain.savepoint chain_store' in
        let* () =
          List.iter_es
            (fun level ->
              let* _sucess =
                Store.Block.read_block_by_level chain_store' level
              in
              return_unit)
            Int32.(
              List.map of_int (to_int caboose_level -- to_int savepoint_level))
        in
        restart (n - 1) head
    in
    (* With the given constants, it is required to bake 7 cycles to
       trigger the first merge. *)
    let* _h = restart 7 export_block in
    let*! () = Store.close_store store' in
    return_unit
  in
  wrap_test
    ~keep_dir:false
    ~with_gc:true
    ~history_mode:History_mode.default
    ~patch_context
    ( Format.asprintf
        "check caboose and savepoint drag after rolling import (blocks per \
         cycle = %ld) using %a format"
        constants.blocks_per_cycle
        Snapshots.pp_snapshot_format
        export_mode,
      test )

let make_tests_drag_after_import =
  let export_mode = Snapshots.[Tar; Raw] in
  List.map test_drag_after_import export_mode

(* TODO:
   export => import => export => import from full & rolling
   export equivalence
*)

let tests speed =
  let test_cases =
    let generated_tests =
      make_tests
        speed
        Tezos_protocol_alpha_parameters.Default_parameters.(
          parameters_of_constants
            {constants_sandbox with consensus_threshold = 0})
    in
    let tests_rolling = make_tests_rolling in
    let tests_drag_after_import = make_tests_drag_after_import in
    tests_rolling @ tests_drag_after_import @ generated_tests
  in
  ("snapshots", test_cases)
