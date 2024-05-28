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

(** Testing
    _______

    Component: Store
    Invocation: dune exec src/lib_store/unix/test/main.exe -- --match construct
    Subject: Store tests ( reconstruct )
*)

open Test_utils

(* Export mode used to export snapshot. Tar is seleceted by
   default.
   FIXME: test over Raw formats as well. *)
let export_mode = Snapshots.Tar

let equal_history_mode ?loc ?msg hm1 hm2 =
  let eq = ( = ) in
  let pp = Tezos_shell_services.History_mode.pp in
  Assert.equal ?loc ?msg ~pp ~eq hm1 hm2

let check_flags descr store expected_head =
  let open Lwt_result_syntax in
  let chain_store = Store.main_chain_store store in
  let*! current_head = Store.Chain.current_head chain_store in
  Assert_lib.Crypto.equal_block
    ~msg:("head consistency: " ^ descr)
    (Store.Block.header expected_head)
    (Store.Block.header current_head) ;
  let history_mode = Store.Chain.history_mode chain_store in
  equal_history_mode
    ~msg:("history mode consistency: " ^ descr)
    history_mode
    History_mode.Archive ;
  let*! checkpoint = Store.Chain.checkpoint chain_store in
  let* metadata = Store.Block.get_block_metadata chain_store expected_head in
  let expected_checkpoint = Store.Block.last_preserved_block_level metadata in
  Assert.Int32.equal
    ~msg:("checkpoint consistency: " ^ descr)
    expected_checkpoint
    (snd checkpoint) ;
  let*! savepoint = Store.Chain.savepoint chain_store in
  Assert.equal ~msg:("savepoint consistency: " ^ descr) 0l (snd savepoint) ;
  let*! caboose = Store.Chain.caboose chain_store in
  Assert.equal
    ~msg:("caboose consistency: " ^ descr)
    (snd savepoint)
    (snd caboose) ;
  return_unit

let test_from_bootstrapped ~descr (store_dir, context_dir) store
    ~nb_blocks_to_bake ~patch_context =
  let open Lwt_result_syntax in
  let chain_store = Store.main_chain_store store in
  let genesis = Store.Chain.genesis chain_store in
  let*! genesis_block = Store.Chain.genesis_block chain_store in
  let* baked_blocks, last =
    Alpha_utils.bake_n chain_store nb_blocks_to_bake genesis_block
  in
  let*! savepoint = Store.Chain.savepoint chain_store in
  let*! () = Store.close_store store in
  let* expected_to_fail =
    Error_monad.protect
      (fun () ->
        let* () =
          Reconstruction.reconstruct
            ~patch_context
            ~store_dir
            ~context_dir
            genesis
            ~user_activated_upgrades:[]
            ~user_activated_protocol_overrides:[]
            ~operation_metadata_size_limit:Unlimited
            ~progress_display_mode:Animation.Auto
        in
        return_false)
      ~on_error:(function
        | [Reconstruction.(Reconstruction_failure Nothing_to_reconstruct)] as e
          ->
            if Compare.Int32.(snd savepoint = 0l) then
              (* It is expected as nothing was pruned *)
              return_true
            else (
              Format.printf
                "@\nTest failed:@\n%a@."
                Error_monad.pp_print_trace
                e ;
              Alcotest.fail
                "Should not fail to reconstruct (nothing_to_reconstruct \
                 raised).")
        | [Reconstruction.(Cannot_reconstruct History_mode.Archive)]
        | [Reconstruction.(Cannot_reconstruct (History_mode.Rolling _))] ->
            (* In both Archive and Rolling _ modes, the reconstruction should fail *)
            return_true
        | err ->
            Format.printf
              "@\nTest failed:@\n%a@."
              Error_monad.pp_print_trace
              err ;
            Alcotest.fail "Should not fail")
  in
  if expected_to_fail then return_unit
  else
    let* store' =
      Store.init
        ~patch_context
        ~store_dir
        ~context_dir
        ~allow_testchains:false
        genesis
    in
    let chain_store' = Store.main_chain_store store' in
    let* () = check_flags descr store' last in
    let* () =
      assert_presence_in_store ~with_metadata:true chain_store' baked_blocks
    in
    let*! () = Store.close_store store' in
    return_unit

let make_tests_bootstrapped speed patch_context =
  let history_modes =
    match speed with
    | `Slow ->
        History_mode.
          [
            Full (Some {offset = 0});
            Full (Some {offset = 3});
            Full (Some default_additional_cycles);
            Archive;
            Rolling (Some default_additional_cycles);
          ]
    | `Quick ->
        History_mode.
          [Full (Some {offset = 0}); Full (Some default_additional_cycles)]
  in
  let nb_blocks_to_bake =
    match speed with
    | `Slow -> 1 -- 100
    | `Quick -> [0; 3; 8; 21; 42; 57; 89; 92; 101]
  in
  let permutations = List.(product nb_blocks_to_bake history_modes) in
  List.map
    (fun (nb_blocks_to_bake, history_mode) ->
      let descr =
        Format.asprintf
          "Reconstructing on a bootstrapped %a node with %d blocks."
          History_mode.pp
          history_mode
          nb_blocks_to_bake
      in
      wrap_simple_store_init_test
        ~speed
        ~patch_context
        ~history_mode
        ~keep_dir:false
        ~with_gc:true
        ( descr,
          fun data_dir store ->
            test_from_bootstrapped
              ~descr
              data_dir
              store
              ~nb_blocks_to_bake
              ~patch_context ))
    permutations

let test_from_snapshot ~descr:_ (store_dir, context_dir) store
    ~nb_blocks_to_bake ~patch_context =
  let open Lwt_result_syntax in
  let chain_store = Store.main_chain_store store in
  let*! genesis_block = Store.Chain.genesis_block chain_store in
  let* baked_blocks, last =
    Alpha_utils.bake_n chain_store nb_blocks_to_bake genesis_block
  in
  let*! lpbl =
    let*! o = Store.Block.get_block_metadata_opt chain_store last in
    match o with
    | Some m -> Lwt.return (Store.Block.last_preserved_block_level m)
    | None -> assert false
  in
  let*! savepoint = Store.Chain.savepoint chain_store in
  let*! () = Store.close_store store in
  let open Filename.Infix in
  let dir = store_dir // "imported_store" in
  let dst_store_dir = dir // "store" in
  let dst_context_dir = dir // "context" in
  let* expected_to_fail =
    Error_monad.protect
      (fun () ->
        let last_hash = Store.Block.hash last in
        let snapshot_path = store_dir // "snapshot.full" in
        let chain_name = Distributed_db_version.Name.of_string "test" in
        let* () =
          Snapshots.export
            export_mode
            ~rolling:false
            ~block:(`Hash (last_hash, 0))
            ~store_dir
            ~context_dir
            ~chain_name
            ~snapshot_path
            ~progress_display_mode:Animation.Auto
            genesis
        in
        let* () =
          Snapshots.import
            ~patch_context
            ~block:last_hash
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
        let* () =
          Reconstruction.reconstruct
            ~patch_context
            ~store_dir:dst_store_dir
            ~context_dir:dst_context_dir
            genesis
            ~user_activated_upgrades:[]
            ~user_activated_protocol_overrides:[]
            ~operation_metadata_size_limit:Unlimited
            ~progress_display_mode:Animation.Auto
        in
        return_false)
      ~on_error:(function
        | [Reconstruction.(Reconstruction_failure Nothing_to_reconstruct)] as e
          ->
            if Compare.Int32.(lpbl = 0l) || snd savepoint = 0l then
              (* It is expected as nothing was pruned *)
              return_true
            else (
              Format.printf
                "@\nTest failed:@\n%a@."
                Error_monad.pp_print_trace
                e ;
              Alcotest.fail
                "Should not fail to reconstruct (nothing_to_reconstruct \
                 raised).")
        | [Reconstruction.(Cannot_reconstruct History_mode.Archive)] ->
            (* In Archive, the reconstruction should fail *)
            return_true
        | Snapshots.[Invalid_export_block {reason = `Genesis; _}] -> return_true
        | err ->
            Format.printf
              "@\nTest failed:@\n%a@."
              Error_monad.pp_print_trace
              err ;
            Alcotest.fail "Should not fail")
  in
  if expected_to_fail then return_unit
  else
    let* store' =
      Store.init
        ~store_dir:dst_store_dir
        ~context_dir:dst_context_dir
        ~allow_testchains:false
        genesis
    in
    let chain_store' = Store.main_chain_store store' in
    let* () =
      assert_presence_in_store ~with_metadata:true chain_store' baked_blocks
    in
    let*! () = Store.close_store store' in
    return_unit

let make_tests_snapshoted speed patch_context =
  let history_modes =
    match speed with
    | `Slow ->
        History_mode.
          [
            Full (Some {offset = 0});
            Full (Some {offset = 3});
            Full (Some default_additional_cycles);
            Archive;
          ]
    | `Quick ->
        History_mode.
          [Full (Some {offset = 0}); Full (Some default_additional_cycles)]
  in
  let nb_blocks_to_bake =
    match speed with
    | `Slow -> 0 -- 100
    | `Quick -> [0; 3; 8; 21; 42; 57; 89; 92; 101]
  in
  let permutations = List.(product nb_blocks_to_bake history_modes) in
  List.map
    (fun (nb_blocks_to_bake, history_mode) ->
      let descr =
        Format.asprintf
          "Reconstructing from snapshots on a %a node with %d blocks."
          History_mode.pp
          history_mode
          nb_blocks_to_bake
      in
      wrap_simple_store_init_test
        ~speed
        ~patch_context
        ~history_mode
        ~keep_dir:false
        ~with_gc:true
        ( descr,
          fun data_dir store ->
            test_from_snapshot
              ~descr
              data_dir
              store
              ~nb_blocks_to_bake
              ~patch_context ))
    permutations

let tests speed =
  let patch_context ctxt = Alpha_utils.default_patch_context ctxt in
  let test_cases_reconstruct = make_tests_bootstrapped speed patch_context in
  let test_cases_reconstruct_snapshots =
    make_tests_snapshoted speed patch_context
  in
  ("reconstruct", test_cases_reconstruct @ test_cases_reconstruct_snapshots)
