(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Test_utils
open Legacy_utils

let assert_presence new_chain_store previously_baked_blocks ?savepoint ?caboose
    = function
  | History_mode.Archive ->
      assert_presence_in_store
        ~with_metadata:true
        new_chain_store
        previously_baked_blocks
  | Full _ ->
      let expected_savepoint =
        WithExceptions.Option.get ~loc:__LOC__ savepoint
      in
      let (pruned, complete) =
        List.split_n (Int32.to_int expected_savepoint) previously_baked_blocks
      in
      assert_presence_in_store ~with_metadata:false new_chain_store pruned
      >>=? fun () ->
      assert_presence_in_store ~with_metadata:true new_chain_store complete
  | Rolling _ ->
      let expected_caboose = WithExceptions.Option.get ~loc:__LOC__ caboose in
      let expected_savepoint =
        WithExceptions.Option.get ~loc:__LOC__ savepoint
      in
      let (pruned, complete) =
        let rolling_window =
          List.filter
            (fun b -> Store.Block.level b >= expected_caboose)
            previously_baked_blocks
        in
        List.split_n (Int32.to_int expected_savepoint) rolling_window
      in
      assert_presence_in_store ~with_metadata:false new_chain_store pruned
      >>=? fun () ->
      assert_presence_in_store ~with_metadata:true new_chain_store complete

let check_flags new_chain_store previously_baked_blocks history_mode =
  let last =
    List.last_opt previously_baked_blocks
    |> WithExceptions.Option.get ~loc:__LOC__
  in
  Assert.equal_history_mode
    ~msg:"history mode consistency: "
    history_mode
    (Store.Chain.history_mode new_chain_store) ;
  Store.Chain.checkpoint new_chain_store >>= fun checkpoint ->
  Store.Block.get_block_metadata new_chain_store last >>=? fun metadata ->
  let expected_checkpoint = Store.Block.last_allowed_fork_level metadata in
  Assert.equal
    ~prn:(Format.sprintf "%ld")
    ~msg:"checkpoint consistency: "
    expected_checkpoint
    (snd checkpoint) ;
  Store.Chain.savepoint new_chain_store >>= fun savepoint ->
  Store.Chain.caboose new_chain_store >>= fun caboose ->
  match history_mode with
  | History_mode.Archive ->
      Assert.equal
        ~prn:(Format.sprintf "%ld")
        ~msg:"savepoint consistency: "
        0l
        (snd savepoint) ;
      Assert.equal
        ~prn:(Format.sprintf "%ld")
        ~msg:"caboose consistency: "
        (snd savepoint)
        (snd caboose) ;
      assert_presence new_chain_store previously_baked_blocks history_mode
  | Full _ ->
      Assert.equal
        ~prn:(Format.sprintf "%ld")
        ~msg:"savepoint consistency: "
        expected_checkpoint
        (snd savepoint) ;
      Assert.equal
        ~prn:(Format.sprintf "%ld")
        ~msg:"caboose consistency: "
        0l
        (snd caboose) ;
      assert_presence
        new_chain_store
        previously_baked_blocks
        ~savepoint:(snd savepoint)
        history_mode
  | Rolling _ ->
      Assert.equal
        ~prn:(Format.sprintf "%ld")
        ~msg:"savepoint consistency: "
        expected_checkpoint
        (snd savepoint) ;
      Store.Block.get_block_metadata new_chain_store last >>=? fun metadata ->
      let max_op_ttl = Store.Block.max_operations_ttl metadata in
      let expected_caboose =
        max 0l Int32.(add (sub expected_checkpoint (of_int max_op_ttl)) 0l)
      in
      Assert.equal
        ~prn:(Format.sprintf "%ld")
        ~msg:"caboose consistency: "
        expected_caboose
        (snd caboose) ;
      assert_presence
        new_chain_store
        previously_baked_blocks
        ~caboose:expected_caboose
        ~savepoint:expected_checkpoint
        history_mode

let test_upgrade store (legacy_dir, (legacy_state : Legacy_state.t)) blocks =
  let patch_context ctxt = Alpha_utils.default_patch_context ctxt in
  let chain_store = Store.main_chain_store store in
  let genesis = Store.Chain.genesis chain_store in
  Lwt_utils_unix.create_dir legacy_dir >>= fun () ->
  let chain_name = Distributed_db_version.Name.of_string "TEZOS" in
  Legacy_state.Chain.get_exn legacy_state (Store.Chain.chain_id chain_store)
  >>= fun legacy_chain ->
  Lwt_list.map_p
    (fun block ->
      let hash = Store.Block.hash block in
      Legacy_state.Block.known legacy_chain hash >>= fun known ->
      Lwt.return (hash, known))
    blocks
  >>= fun present_blocks_in_legacy ->
  Legacy.upgrade_0_0_4 ~data_dir:legacy_dir ~patch_context ~chain_name genesis
  >>=? fun _upgrade_message ->
  let history_mode = Store.Chain.history_mode chain_store in
  let open Filename.Infix in
  Store.init
    ~patch_context
    ~history_mode
    ~readonly:false
    ~store_dir:(legacy_dir // "store")
    ~context_dir:(legacy_dir // "context")
    ~allow_testchains:true
    genesis
  >>=? fun upgraded_store ->
  Lwt.finalize
    (fun () ->
      let upgraded_chain_store = Store.main_chain_store upgraded_store in
      Lwt_list.iter_s
        (fun (hash, is_known) ->
          Store.Block.is_known upgraded_chain_store hash >>= fun is_known' ->
          Assert.equal
            ~msg:
              (Format.asprintf
                 "check %a existence after upgrade"
                 Block_hash.pp
                 hash)
            is_known
            is_known' ;
          Lwt.return_unit)
        present_blocks_in_legacy
      >>= fun () ->
      check_flags upgraded_chain_store blocks history_mode >>=? fun () ->
      Test_utils.check_invariants upgraded_chain_store >>=? fun () ->
      (* Try baking a bit after upgrading... *)
      Store.Chain.current_head upgraded_chain_store >>= fun head ->
      Alpha_utils.bake_until_n_cycle_end upgraded_chain_store 10 head
      >>=? fun _ -> return_unit)
    (fun () -> Store.close_store upgraded_store)

let test_legacy_snapshot legacy_snapshot_history_mode store
    (legacy_dir, (legacy_state : Legacy_state.t)) blocks =
  let patch_context ctxt = Alpha_utils.default_patch_context ctxt in
  let chain_store = Store.main_chain_store store in
  let genesis = Store.Chain.genesis chain_store in
  Lwt_utils_unix.create_dir legacy_dir >>= fun () ->
  let chain_name = Distributed_db_version.Name.of_string "TEZOS" in
  Legacy_state.Chain.get_exn legacy_state (Store.Chain.chain_id chain_store)
  >>= fun legacy_chain ->
  Lwt_list.map_p
    (fun block ->
      let descr = Store.Block.descriptor block in
      Legacy_state.Block.known legacy_chain (fst descr) >>= fun known ->
      Lwt.return (descr, known))
    blocks
  >>= fun present_blocks_in_legacy ->
  Legacy_chain.head legacy_chain >>= fun legacy_head ->
  let open Filename.Infix in
  let snapshot_file = legacy_dir // "legacy_snapshot" in
  let head_hash = Legacy_state.Block.hash legacy_head in
  Legacy_snapshots.export
    ~export_rolling:(legacy_snapshot_history_mode = History_mode.Legacy.Rolling)
    ~store_root:(legacy_dir // "store")
    ~context_root:(legacy_dir // "context")
    ~genesis
    snapshot_file
    head_hash
  >>=? fun () ->
  let open Filename.Infix in
  let root_dir =
    Naming.dir_path (Store.directory store) // ".." // "imported_store"
  in
  let dst_store_dir = root_dir // "store" in
  let dst_context_dir = legacy_dir // "context" in
  Snapshots.import_legacy
    ~patch_context
    ~block:head_hash
    ~dst_store_dir
    ~dst_context_dir
    ~chain_name
    ~user_activated_upgrades:[]
    ~user_activated_protocol_overrides:[]
    ~snapshot_file
    genesis
  >>=? fun () ->
  let history_mode = History_mode.convert legacy_snapshot_history_mode in
  Store.init
    ~patch_context
    ~history_mode
    ~readonly:false
    ~store_dir:dst_store_dir
    ~context_dir:dst_context_dir
    ~allow_testchains:true
    genesis
  >>=? fun imported_store ->
  let imported_chain_store = Store.main_chain_store imported_store in
  Lwt.finalize
    (fun () ->
      Lwt.catch
        (fun () ->
          Lwt_list.iter_s
            (fun ((hash, level), is_known) ->
              Store.Block.is_known imported_chain_store hash
              >>= fun is_known' ->
              if is_known && not is_known' then (
                Store.Chain.caboose imported_chain_store
                >>= fun (_, caboose_level) ->
                Assert.is_true
                  ~msg:"check block absence consistency with history mode"
                  (match history_mode with
                  | Rolling _ -> caboose_level > level
                  | _ -> false) ;
                Lwt.return_unit)
              else (
                Assert.equal
                  ~msg:
                    (Format.asprintf
                       "check %a existence after upgrade"
                       Block_hash.pp
                       hash)
                  is_known
                  is_known' ;
                Lwt.return_unit))
            present_blocks_in_legacy
          >>= fun () ->
          Test_utils.check_invariants imported_chain_store >>=? fun () ->
          (* Try baking a bit after importing... *)
          Store.Chain.current_head imported_chain_store
          >>= fun head_after_import ->
          Alpha_utils.bake_until_n_cycle_end
            imported_chain_store
            10
            head_after_import
          >>=? fun _ ->
          let highest_cemented_block =
            Cemented_block_store.get_highest_cemented_level
              (Store.Unsafe.get_block_store imported_chain_store
              |> Block_store.cemented_block_store)
          in
          match highest_cemented_block with
          | None -> return_unit
          | Some highest_cemented_level ->
              Assert.is_true
                ~msg:"is the highest cemented block above the new head"
                Compare.Int32.(
                  highest_cemented_level > Store.Block.level head_after_import) ;
              return_unit)
        (fun e ->
          Store.make_pp_store imported_store >>= fun pp ->
          Format.printf "DEBUG-IMPORTED: %a@." pp () ;
          Lwt.fail e))
    (fun () -> Store.close_store imported_store)

let test_upgrade_from_snapshot legacy_snapshot_history_mode store
    (legacy_dir, (legacy_state : Legacy_state.t)) blocks =
  let patch_context ctxt = Alpha_utils.default_patch_context ctxt in
  let chain_store = Store.main_chain_store store in
  let genesis = Store.Chain.genesis chain_store in
  Lwt_utils_unix.create_dir legacy_dir >>= fun () ->
  let chain_name = Distributed_db_version.Name.of_string "TEZOS" in
  Legacy_state.Chain.get_exn legacy_state (Store.Chain.chain_id chain_store)
  >>= fun legacy_chain ->
  Lwt_list.map_p
    (fun block ->
      let descr = Store.Block.descriptor block in
      Legacy_state.Block.known legacy_chain (fst descr) >>= fun known ->
      Lwt.return (descr, known))
    blocks
  >>= fun present_blocks_in_legacy ->
  Legacy_chain.head legacy_chain >>= fun legacy_head ->
  let open Filename.Infix in
  let snapshot_file = legacy_dir // "legacy_snapshot" in
  let head_hash = Legacy_state.Block.hash legacy_head in
  Legacy_snapshots.export
    ~export_rolling:(legacy_snapshot_history_mode = History_mode.Legacy.Rolling)
    ~store_root:(legacy_dir // "store")
    ~context_root:(legacy_dir // "context")
    ~genesis
    snapshot_file
    head_hash
  >>=? fun () ->
  let imported_root_dir =
    Naming.dir_path (Store.directory store) // ".." // ".." // "imported_store"
  in
  let imported_store_dir = imported_root_dir // "store" in
  let imported_context_dir = imported_root_dir // "context" in
  Lwt_unix.mkdir imported_root_dir 0o700 >>= fun () ->
  Legacy_snapshots.import
    ~patch_context
    ~data_dir:imported_root_dir
    ~user_activated_upgrades:[]
    ~user_activated_protocol_overrides:[]
    ~dir_cleaner:(fun _ -> Lwt.return_unit)
    ~genesis
    snapshot_file
    ~block:(Some (Block_hash.to_b58check head_hash))
  >>=? fun () ->
  Legacy_state.init
    ~patch_context
    ~store_root:imported_store_dir
    ~context_root:imported_context_dir
    genesis
  >>=? fun (state, chain_state, _, _) ->
  ( Legacy_state.Chain.checkpoint chain_state >|= fun bh ->
    Some (Block_header.hash bh, bh.shell.level) )
  >>= fun expected_checkpoint ->
  (Legacy_state.Chain.save_point chain_state >|= fun (l, bh) -> Some (bh, l))
  >>= fun expected_savepoint ->
  (Legacy_state.Chain.caboose chain_state >|= fun (l, bh) -> Some (bh, l))
  >>= fun expected_caboose ->
  Legacy_state.close state >>= fun () ->
  Legacy.upgrade_0_0_4
    ~data_dir:imported_root_dir
    ~patch_context
    ~chain_name
    genesis
  >>=? fun _upgrade_message ->
  let history_mode = Store.Chain.history_mode chain_store in
  Store.init
    ~patch_context
    ~history_mode
    ~readonly:false
    ~store_dir:imported_store_dir
    ~context_dir:imported_context_dir
    ~allow_testchains:true
    genesis
  >>=? fun upgraded_store ->
  let upgraded_chain_store = Store.main_chain_store upgraded_store in
  Lwt.finalize
    (fun () ->
      Lwt_list.iter_s
        (fun ((hash, level), is_known) ->
          Store.Block.is_known upgraded_chain_store hash >>= fun is_known' ->
          if is_known && not is_known' then (
            Store.Chain.caboose upgraded_chain_store
            >>= fun (_, caboose_level) ->
            Assert.is_true
              ~msg:"check block absence consistency with history mode"
              (match history_mode with
              | Rolling _ -> caboose_level > level
              | _ -> false) ;
            Lwt.return_unit)
          else (
            Assert.equal
              ~msg:
                (Format.asprintf
                   "check %a existence after upgrade"
                   Block_hash.pp
                   hash)
              is_known
              is_known' ;
            Lwt.return_unit))
        present_blocks_in_legacy
      >>= fun () ->
      Test_utils.check_invariants
        upgraded_chain_store
        ~expected_checkpoint
        ~expected_savepoint
        ~expected_caboose
      >>=? fun () ->
      (* Try baking a bit after importing... *)
      Store.Chain.current_head upgraded_chain_store >>= fun head ->
      Alpha_utils.bake_until_n_cycle_end upgraded_chain_store 10 head
      >>=? fun _ -> return_unit)
    (fun () -> Store.close_store upgraded_store)

(* This test aims to create a v1 snapshot (from the legacy store)
   which does not contains the block and operations metadata hash and
   check that the reconstruction procedure of the new store manages to
   restore the missing data. *)
let test_legacy_reconstruct legacy_snapshot_history_mode store
    (legacy_dir, (legacy_state : Legacy_state.t)) _blocks =
  let patch_context ctxt = Legacy_utils.patch_context ctxt in
  let chain_store = Store.main_chain_store store in
  let genesis = Store.Chain.genesis chain_store in
  Lwt_utils_unix.create_dir legacy_dir >>= fun () ->
  let chain_name = Distributed_db_version.Name.of_string "TEZOS" in
  Legacy_state.Chain.get_exn legacy_state (Store.Chain.chain_id chain_store)
  >>= fun legacy_chain ->
  Legacy_chain.head legacy_chain >>= fun legacy_head ->
  let open Filename.Infix in
  let snapshot_file = legacy_dir // "legacy_snapshot" in
  let head_hash = Legacy_state.Block.hash legacy_head in
  Legacy_snapshots.export
    ~export_rolling:(legacy_snapshot_history_mode = History_mode.Legacy.Rolling)
    ~store_root:(legacy_dir // "store")
    ~context_root:(legacy_dir // "context")
    ~genesis
    snapshot_file
    head_hash
  >>=? fun () ->
  let open Filename.Infix in
  let root_dir =
    Naming.dir_path (Store.directory store) // ".." // "imported_store"
  in
  let dst_store_dir = root_dir // "store" in
  let dst_context_dir = legacy_dir // "context" in
  Snapshots.import_legacy
    ~patch_context
    ~block:head_hash
    ~dst_store_dir
    ~dst_context_dir
    ~chain_name
    ~user_activated_upgrades:[]
    ~user_activated_protocol_overrides:[]
    ~snapshot_file
    genesis
  >>=? fun () ->
  Store.init
    ~patch_context
    ~history_mode:(History_mode.convert legacy_snapshot_history_mode)
    ~readonly:false
    ~store_dir:dst_store_dir
    ~context_dir:dst_context_dir
    ~allow_testchains:true
    genesis
  >>=? fun imported_store ->
  let imported_chain_store = Store.main_chain_store imported_store in
  (* Make sure that the imported blocks are missing the block metadata
     hash. Here, we target 2 blocks below the head as the head and its
     predecessor aims to be complete (as the head was applied thakns
     to it's complete predecessor).*)
  (Store.Block.read_block_opt imported_chain_store ~distance:2 head_hash
   >>= function
   | None ->
       Alcotest.fail "A block is unexpectidely missing from the imported store"
   | Some block ->
       if Option.is_some (Store.Block.block_metadata_hash block) then
         Alcotest.fail "Block metadata hash is available but should not."
       else (* Block metadata hash is missing, as expected *) return_unit)
  >>=? fun () ->
  Store.close_store imported_store >>= fun () ->
  Reconstruction.reconstruct
    ~patch_context
    ~store_dir:dst_store_dir
    ~context_dir:dst_context_dir
    genesis
    ~user_activated_upgrades:[]
    ~user_activated_protocol_overrides:[]
  >>=? fun () ->
  (* Restart the store, after the reconstruction, in archive mode. *)
  Store.init
    ~patch_context
    ~history_mode:History_mode.Archive
    ~readonly:false
    ~store_dir:dst_store_dir
    ~context_dir:dst_context_dir
    ~allow_testchains:true
    genesis
  >>=? fun reconstructed_store ->
  let reconstructed_chain_store = Store.main_chain_store reconstructed_store in
  (Store.Block.read_block_opt reconstructed_chain_store ~distance:2 head_hash
   >>= function
   | None ->
       Alcotest.fail "A block is unexpectidely missing from the imported store"
   | Some block ->
       if Option.is_none (Store.Block.block_metadata_hash block) then
         Alcotest.fail "Block metadata hash is missing but should not."
       else (* Block metadata hash is available, as expected *) return_unit)
  >>=? fun () -> return_unit

let make_upgrade_test_cases ?(keep_dir = false) speed :
    string Alcotest_lwt.test_case list =
  let history_modes =
    History_mode.[Legacy.Archive; Legacy.Full; Legacy.Rolling]
  in
  let nb_blocks_to_bake =
    match speed with `Slow -> 0 -- 100 | `Quick -> [8; 57; 89; 101]
  in
  let permutations = List.(product nb_blocks_to_bake history_modes) in
  List.map
    (fun (nb_blocks_to_bake, legacy_history_mode) ->
      let name =
        Format.asprintf
          "Upgrade legacy %a with %d blocks"
          History_mode.Legacy.pp
          legacy_history_mode
          nb_blocks_to_bake
      in
      let test =
        {
          name;
          speed;
          legacy_history_mode;
          nb_blocks = `Blocks nb_blocks_to_bake;
          test = test_upgrade;
        }
      in
      wrap_test_legacy ~keep_dir test)
    permutations

let make_legacy_snapshot_test_cases ~keep_dir speed =
  let history_modes =
    History_mode.[Legacy.Archive; Legacy.Full; Legacy.Rolling]
  in
  let snapshot_history_modes = History_mode.[Legacy.Full; Legacy.Rolling] in
  let nb_blocks_to_bake = [40; 57; 89; 101] in
  let permutations =
    List.(
      product nb_blocks_to_bake (product history_modes snapshot_history_modes))
    |> List.filter
         (fun (_, (legacy_history_mode, legacy_snapshot_history_mode)) ->
           if legacy_history_mode = History_mode.Legacy.Rolling then
             legacy_snapshot_history_mode = History_mode.Legacy.Rolling
           else true)
    |> List.sort_uniq compare
  in
  List.map
    (fun (nb_blocks_to_bake, (legacy_history_mode, legacy_snapshot_history_mode))
         ->
      let name =
        Format.asprintf
          "Import legacy snapshot in %a from %a with %d blocks"
          History_mode.Legacy.pp
          legacy_snapshot_history_mode
          History_mode.Legacy.pp
          legacy_history_mode
          nb_blocks_to_bake
      in
      let test =
        {
          name;
          speed;
          legacy_history_mode;
          nb_blocks = `Blocks nb_blocks_to_bake;
          test = test_legacy_snapshot legacy_snapshot_history_mode;
        }
      in
      wrap_test_legacy ~keep_dir test)
    permutations

let make_upgrade_after_snapshot_import_test_cases ?(keep_dir = false) speed :
    string Alcotest_lwt.test_case list =
  let history_modes = History_mode.[Legacy.Full; Rolling] in
  let nb_blocks_to_bake =
    match speed with `Slow -> 1 -- 100 | `Quick -> [8; 57; 89; 101]
  in
  let permutations = List.(product nb_blocks_to_bake history_modes) in
  List.map
    (fun (nb_blocks_to_bake, legacy_history_mode) ->
      let name =
        Format.asprintf
          "Upgrade legacy %a after snapshot import with %d blocks"
          History_mode.Legacy.pp
          legacy_history_mode
          nb_blocks_to_bake
      in
      let test =
        {
          name;
          speed;
          legacy_history_mode;
          nb_blocks = `Blocks nb_blocks_to_bake;
          test = test_upgrade_from_snapshot legacy_history_mode;
        }
      in
      wrap_test_legacy ~keep_dir test)
    permutations

let make_legacy_reconstruct_test_cases ?(keep_dir = false) speed :
    string Alcotest_lwt.test_case list =
  (* Make sure that we also reconstruct through both cemented and
     floating stores. *)
  let nb_blocks_to_bake = [8 * 2; 8 * 5; 8 * 8] in
  let legacy_history_mode = History_mode.Legacy.Full in
  List.map
    (fun nb_blocks ->
      let name =
        Format.asprintf
          "Storage reconstruction after a legacy snapshot import with %d blocks"
          nb_blocks
      in
      let nb_blocks = `Blocks nb_blocks in
      let test =
        {
          name;
          speed;
          legacy_history_mode;
          nb_blocks;
          test = test_legacy_reconstruct legacy_history_mode;
        }
      in
      wrap_test_legacy ~keep_dir test)
    nb_blocks_to_bake

let upgrade_tests : string Alcotest_lwt.test list =
  let speed =
    try
      let s = Sys.getenv "SLOW_TEST" in
      match String.(trim (uncapitalize_ascii s)) with
      | "true" | "1" | "yes" -> `Slow
      | _ -> `Quick
    with Not_found -> `Quick
  in
  let upgrade_cases = make_upgrade_test_cases ~keep_dir:false speed in
  let snapshots_cases = make_legacy_snapshot_test_cases ~keep_dir:false speed in
  let upgrade_snapshots_cases =
    make_upgrade_after_snapshot_import_test_cases ~keep_dir:false speed
  in
  let legacy_reconstruct_cases =
    make_legacy_reconstruct_test_cases ~keep_dir:false speed
  in
  [
    ("legacy store upgrade", upgrade_cases);
    ("legacy snapshot import", snapshots_cases);
    ("legacy store upgrade after snapshot import", upgrade_snapshots_cases);
    ("storage reconstruction after a legacy import", legacy_reconstruct_cases);
  ]

let () =
  let open Cmdliner in
  let arg =
    Arg.(
      required
      & opt (some string) None
      & info ~docv:"[LEGACY_STORE_BUILDER_PATH]" ["builder-path"])
  in
  Lwt_main.run
    ( Internal_event_unix.init () >>= fun () ->
      Alcotest_lwt.run_with_args "tezos-store-legacy" arg upgrade_tests )
