(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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
    Invocation: dune exec src/lib_store/unix/test/main.exe \
                -- --file test_block_store.ml
    Subject: Store tests ( block storing and access )
*)

module Assert = Assert
open Test_utils
open Block_store

let assert_presence_in_block_store ?(with_metadata = false) block_store blocks =
  let open Lwt_result_syntax in
  List.iter_es
    (fun b ->
      let hash = Block_repr.hash b in
      let* is_known = Block_store.mem block_store (Block (hash, 0)) in
      if not is_known then
        Alcotest.failf
          "assert_presence_in_block_store: block %a in not known"
          pp_raw_block
          b ;
      let* o =
        Block_store.read_block
          ~read_metadata:with_metadata
          block_store
          (Block (hash, 0))
      in
      match o with
      | None ->
          Alcotest.failf
            "assert_presence_in_block_store: cannot find block %a"
            pp_raw_block
            b
      | Some b' ->
          if with_metadata then (
            Assert.equal
              ~pp:Block_repr.pp_json
              ~msg:"block equality with metadata"
              b
              b' ;
            return_unit)
          else (
            Assert_lib.Crypto.equal_block
              ~msg:"block equality without metadata"
              (Block_repr.header b)
              (Block_repr.header b') ;
            return_unit))
    blocks

let assert_absence_in_block_store block_store blocks =
  let open Lwt_result_syntax in
  List.iter_es
    (fun b ->
      let hash = Block_repr.hash b in
      let* mem = Block_store.mem block_store (Block (hash, 0)) in
      match mem with
      | true ->
          Alcotest.failf
            "assert_absence_in_block_store: found unexpected block %a"
            pp_raw_block
            b
      | false -> return_unit)
    blocks

let assert_pruned_blocks_in_block_store block_store blocks =
  let open Lwt_result_syntax in
  List.iter_es
    (fun b ->
      let hash = Block_repr.hash b in
      let* is_known = Block_store.mem block_store (Block (hash, 0)) in
      if not is_known then
        Alcotest.failf
          "assert_pruned_blocks_in_block_store: block %a in not known"
          pp_raw_block
          b ;
      let* o =
        Block_store.read_block ~read_metadata:true block_store (Block (hash, 0))
      in
      match o with
      | None ->
          Alcotest.failf
            "assert_pruned_blocks_in_block_store: cannot find block %a"
            pp_raw_block
            b
      | Some {metadata = Some _; _} ->
          Alcotest.failf
            "assert_pruned_blocks_in_block_store: unpruned block %a"
            pp_raw_block
            b
      | Some ({metadata = None; _} as b') ->
          Assert_lib.Crypto.equal_block
            ~msg:"block equality without metadata"
            (Block_repr.header b)
            (Block_repr.header b') ;
          return_unit)
    blocks

let assert_cemented_bound block_store (lowest, highest) =
  let unopt_level = function
    | Some level -> level
    | None -> Assert.fail_msg "could not retrieve cemented level"
  in
  let lowest_cemented_level =
    unopt_level
      (Cemented_block_store.get_lowest_cemented_level
         (Block_store.cemented_block_store block_store))
  in
  let highest_cemented_level =
    unopt_level
      (Cemented_block_store.get_highest_cemented_level
         (Block_store.cemented_block_store block_store))
  in
  Assert.assert_true
    "is 0 the lowest cemented level?"
    Compare.Int32.(lowest_cemented_level = lowest) ;
  Assert.assert_true
    "is head's lpbl the lowest cemented level?"
    Compare.Int32.(highest_cemented_level = highest)

let test_storing_and_access_predecessors block_store =
  let open Lwt_result_syntax in
  let*! blocks, _head =
    make_raw_block_list ~kind:`Full (genesis_hash, -1l) 50
  in
  let* () =
    List.iter_es
      (fun b -> Block_store.store_block block_store b Context_hash.zero)
      blocks
  in
  let* () = assert_presence_in_block_store block_store blocks in
  let* () =
    List.iter_es
      (fun b ->
        let hash = Block_repr.hash b in
        let level = Block_repr.level b in
        List.iter_es
          (fun distance ->
            let* o =
              Block_store.get_hash block_store (Block (hash, distance))
            in
            match o with
            | None -> Alcotest.fail "expected predecessor but none found"
            | Some h ->
                Alcotest.check
                  (module Block_hash)
                  (Format.asprintf
                     "get %d-th predecessor of %a (%ld)"
                     distance
                     Block_hash.pp
                     hash
                     level)
                  h
                  (List.nth blocks (Int32.to_int level - distance)
                  |> WithExceptions.Option.get ~loc:__LOC__)
                    .hash ;
                return_unit)
          (0 -- (Int32.to_int level - 1)))
      blocks
  in
  return_unit

let make_raw_block_list_with_lpbl pred size ~lpbl =
  let open Lwt_syntax in
  let* chunk, head = make_raw_block_list ~kind:`Full pred size in
  let change_lpbl block =
    let metadata =
      WithExceptions.Option.to_exn ~none:Not_found block.Block_repr.metadata
    in
    block.Block_repr.metadata <-
      Some {metadata with last_preserved_block_level = lpbl} ;
    block
  in
  Lwt.return (List.map change_lpbl chunk, change_lpbl head)

let make_n_consecutive_cycles pred ~cycle_length ~nb_cycles =
  let open Lwt_syntax in
  assert (nb_cycles > 0) ;
  assert (cycle_length > 0) ;
  let rec loop acc pred = function
    | 0 ->
        let head =
          List.last_opt (List.hd acc |> WithExceptions.Option.get ~loc:__LOC__)
          |> WithExceptions.Option.get ~loc:__LOC__
        in
        Lwt.return (List.rev acc, head)
    | n ->
        let cycle_length =
          if Block_hash.equal (fst pred) genesis_hash then cycle_length - 1
          else cycle_length
        in
        let lpbl = max 0l (snd pred) in
        let* chunk, head =
          make_raw_block_list_with_lpbl pred cycle_length ~lpbl
        in
        loop (chunk :: acc) (Block_repr.descriptor head) (n - 1)
  in
  loop [] pred nb_cycles

let make_n_initial_consecutive_cycles block_store ~cycle_length ~nb_cycles =
  let open Lwt_syntax in
  let genesis_descr =
    Block_repr.descriptor (Block_store.genesis_block block_store)
  in
  let* cycles, head =
    make_n_consecutive_cycles genesis_descr ~cycle_length ~nb_cycles
  in
  return (cycles, head)

let test_simple_merge block_store =
  let open Lwt_result_syntax in
  let*! cycles, head =
    make_n_initial_consecutive_cycles block_store ~cycle_length:10 ~nb_cycles:2
  in
  let head_metadata =
    Block_repr.metadata head |> WithExceptions.Option.get ~loc:__LOC__
  in
  let all_blocks = List.concat cycles in
  let* () =
    List.iter_es
      (fun b -> Block_store.store_block block_store b Context_hash.zero)
      all_blocks
  in
  let* () =
    Block_store.merge_stores
      block_store
      ~on_error:(fun err ->
        Assert.fail_msg "merging failed: %a" pp_print_trace err)
      ~finalizer:(fun _ ->
        assert_presence_in_block_store block_store all_blocks)
      ~history_mode:Archive
      ~new_head:head
      ~new_head_metadata:head_metadata
      ~cementing_highwatermark:0l
  in
  let*! () = Block_store.await_merging block_store in
  assert_cemented_bound
    block_store
    (0l, Block_repr.last_preserved_block_level head_metadata) ;
  return_unit

let test_consecutive_concurrent_merges block_store =
  let open Lwt_result_syntax in
  (* Append 10 cycles of 10 blocks *)
  let*! cycles, head =
    make_n_initial_consecutive_cycles block_store ~cycle_length:10 ~nb_cycles:4
  in
  let head_metadata =
    Block_repr.metadata head |> WithExceptions.Option.get ~loc:__LOC__
  in
  let all_blocks = List.concat cycles in
  let* () =
    List.iter_es
      (fun b -> Block_store.store_block block_store b Context_hash.zero)
      all_blocks
  in
  let cycles_to_merge =
    List.fold_left
      (fun (acc, pred_cycle_lpbl) cycle ->
        let block_in_cycle =
          List.hd cycle |> WithExceptions.Option.get ~loc:__LOC__
        in
        let block_lpbl =
          Block_repr.metadata block_in_cycle
          |> WithExceptions.Option.get ~loc:__LOC__
          |> Block_repr.last_preserved_block_level
        in
        ((cycle, pred_cycle_lpbl) :: acc, block_lpbl))
      ([], 0l)
      cycles
    |> fst |> List.rev |> List.tl
    |> WithExceptions.Option.get ~loc:__LOC__
  in
  let merge_cycle (cycle, previous_cycle_lpbl) =
    let new_head =
      List.last_opt cycle |> WithExceptions.Option.get ~loc:__LOC__
    in
    let new_head_metadata =
      Block_repr.metadata new_head |> WithExceptions.Option.get ~loc:__LOC__
    in
    Block_store.merge_stores
      block_store
      ~on_error:(fun err ->
        Assert.fail_msg "merging failed: %a" pp_print_trace err)
      ~finalizer:(fun _ -> assert_presence_in_block_store block_store cycle)
      ~history_mode:Archive
      ~new_head
      ~new_head_metadata
      ~cementing_highwatermark:previous_cycle_lpbl
  in
  let threads = List.map merge_cycle cycles_to_merge in
  let*! res = Lwt.all threads in
  List.iter
    (function
      | Ok () -> ()
      | Error err -> Assert.fail_msg "merging failed: %a" pp_print_trace err)
    res ;
  let*! () = Block_store.await_merging block_store in
  let* () =
    assert_presence_in_block_store ~with_metadata:true block_store all_blocks
  in
  assert_cemented_bound
    block_store
    (0l, Block_repr.last_preserved_block_level head_metadata) ;
  return_unit

let test_ten_cycles_merge block_store =
  let open Lwt_result_syntax in
  (* Append 10 cycles *)
  let*! cycles, head =
    make_n_initial_consecutive_cycles
      block_store
      ~cycle_length:100
      ~nb_cycles:10
  in
  let all_blocks = List.concat cycles in
  let* () =
    List.iter_es
      (fun b -> Block_store.store_block block_store b Context_hash.zero)
      all_blocks
  in
  let* () =
    Block_store.merge_stores
      block_store
      ~on_error:(fun err ->
        Assert.fail_msg "merging failed: %a" pp_print_trace err)
      ~finalizer:(fun _ -> return_unit)
      ~history_mode:Archive
      ~new_head:head
      ~new_head_metadata:
        (head |> Block_repr.metadata
        |> WithExceptions.Option.to_exn ~none:Not_found)
      ~cementing_highwatermark:0l
  in
  let* () =
    assert_presence_in_block_store ~with_metadata:true block_store all_blocks
  in
  let*! () = Block_store.await_merging block_store in
  return_unit

(** Makes several branches at different level and make sure those
    created before the checkpoint are correctly GCed *)
let test_merge_with_branches block_store =
  let open Lwt_result_syntax in
  (* make an initial chain of 2 cycles of 100 blocks with each
     block's lpbl pointing to the highest block of its preceding cycle.
     i.e. 1st cycle's lpbl = 0, 2nd cycle's lpbl = 99 *)
  let*! cycles, head =
    make_n_initial_consecutive_cycles block_store ~cycle_length:100 ~nb_cycles:2
  in
  let all_blocks = List.concat cycles in
  let* () =
    List.iter_es
      (fun b -> Block_store.store_block block_store b Context_hash.zero)
      all_blocks
  in
  let branches_fork_points_to_gc = [20; 40; 60; 80; 98] in
  (* 448 => we also keep the checkpoint *)
  let* blocks_to_gc =
    List.map_es
      (fun level ->
        let fork_root =
          List.nth all_blocks (level - 1)
          |> WithExceptions.Option.get ~loc:__LOC__
        in
        let*! blocks, _head =
          make_raw_block_list_with_lpbl
            ~lpbl:0l
            (Block_repr.descriptor fork_root)
            50
        in
        (* tweek lpbl's to make them coherent *)
        List.iter
          (fun block ->
            if Compare.Int32.(Block_repr.level block > 99l) then
              block.metadata <-
                Option.map
                  (fun metadata ->
                    {metadata with Block_repr.last_preserved_block_level = 99l})
                  block.metadata)
          blocks ;
        let* () =
          List.iter_es
            (fun b -> Block_store.store_block block_store b Context_hash.zero)
            blocks
        in
        return blocks)
      branches_fork_points_to_gc
  in
  let branches_fork_points_to_keep = [120; 140; 160; 180] in
  let* blocks_to_keep =
    List.map_es
      (fun level ->
        let fork_root =
          List.nth all_blocks (level - 1)
          |> WithExceptions.Option.get ~loc:__LOC__
        in
        let*! blocks, _head =
          make_raw_block_list_with_lpbl
            ~lpbl:99l
            (Block_repr.descriptor fork_root)
            50
        in
        (* tweek lpbl's to make them coherent *)
        List.iter
          (fun block ->
            if Compare.Int32.(Block_repr.level block > 199l) then
              block.metadata <-
                Option.map
                  (fun metadata ->
                    {metadata with Block_repr.last_preserved_block_level = 199l})
                  block.metadata)
          blocks ;
        let* () =
          List.iter_es
            (fun b -> Block_store.store_block block_store b Context_hash.zero)
            blocks
        in
        return blocks)
      branches_fork_points_to_keep
  in
  (* merge 1st cycle *)
  let* () =
    Block_store.merge_stores
      block_store
      ~on_error:(fun err ->
        Assert.fail_msg "merging failed: %a" pp_print_trace err)
      ~finalizer:(fun _ -> return_unit)
      ~history_mode:Archive
      ~new_head:head
      ~new_head_metadata:
        (head |> Block_repr.metadata
        |> WithExceptions.Option.to_exn ~none:Not_found)
      ~cementing_highwatermark:0l
  in
  let*! () = Block_store.await_merging block_store in
  let* () =
    assert_presence_in_block_store
      block_store
      (all_blocks @ List.flatten blocks_to_keep)
  in
  assert_absence_in_block_store block_store (List.flatten blocks_to_gc)

let perform_n_cycles_merge ?(cycle_length = 10)
    ?(cycle_size_limit = Block_store.default_cycle_size_limit) block_store
    history_mode nb_cycles =
  let open Lwt_result_syntax in
  let*! cycles, head =
    make_n_initial_consecutive_cycles block_store ~cycle_length ~nb_cycles
  in
  let all_blocks = List.concat cycles in
  let* () =
    List.iter_es
      (fun b -> Block_store.store_block block_store b Context_hash.zero)
      all_blocks
  in
  let* () =
    assert_presence_in_block_store ~with_metadata:true block_store all_blocks
  in
  let* () =
    Block_store.merge_stores
      block_store
      ~cycle_size_limit
      ~on_error:(fun err ->
        Assert.fail_msg "merging failed: %a" pp_print_trace err)
      ~finalizer:(fun _ -> return_unit)
      ~history_mode
      ~new_head:head
      ~new_head_metadata:
        (head |> Block_repr.metadata
        |> WithExceptions.Option.to_exn ~none:Not_found)
      ~cementing_highwatermark:0l
  in
  let*! () = Block_store.await_merging block_store in
  return cycles

let test_archive_merge block_store =
  let open Lwt_result_syntax in
  let* cycles = perform_n_cycles_merge block_store Archive 10 in
  let* () =
    assert_presence_in_block_store
      ~with_metadata:true
      block_store
      (List.concat cycles)
  in
  let*! savepoint = Block_store.savepoint block_store in
  Assert.Int32.equal ~msg:"savepoint" 0l (snd savepoint) ;
  let*! caboose = Block_store.caboose block_store in
  Assert.Int32.equal ~msg:"caboose" 0l (snd caboose) ;
  return_unit

let test_full_0_merge block_store =
  let open Lwt_result_syntax in
  (* The total of blocks should be > 130 to prevent the cache from
     retaining them *)
  let cycle_length = 20 in
  let nb_cycles = 10 in
  let* cycles =
    perform_n_cycles_merge
      ~cycle_length
      block_store
      (Full (Some {offset = 0}))
      nb_cycles
  in
  let all_blocks = List.concat cycles in
  let* () =
    assert_presence_in_block_store
      ~with_metadata:false
      block_store
      (List.rev all_blocks)
    (* hack: invert the reading order to clear the cache *)
  in
  let expected_savepoint_level =
    ((nb_cycles - 1) * cycle_length) - 1 (* lpbl *) - 1
    (* lpbl max_op_ttl *)
  in
  let expected_pruned_blocks, expected_preserved_blocks =
    List.split_n
      (expected_savepoint_level - 1)
      (* the genesis block is not counted *) all_blocks
    (* First 9 cycles shouldn't have metadata except for the lpbl block
       (i.e. the last one) *)
  in
  let* () =
    assert_presence_in_block_store
      ~with_metadata:true
      block_store
      expected_preserved_blocks
  in
  let* () =
    assert_pruned_blocks_in_block_store block_store expected_pruned_blocks
  in
  let*! savepoint = Block_store.savepoint block_store in
  Assert.Int32.equal
    ~msg:"savepoint"
    (Int32.of_int expected_savepoint_level)
    (snd savepoint) ;
  let*! caboose = Block_store.caboose block_store in
  Assert.Int32.equal ~msg:"caboose" 0l (snd caboose) ;
  return_unit

let test_full_2_merge block_store =
  let open Lwt_result_syntax in
  (* The total of blocks should be > 130 to prevent the cache from
     retaining them *)
  let* cycles =
    perform_n_cycles_merge
      ~cycle_length:20
      block_store
      (Full (Some {offset = 2}))
      10
  in
  let* () =
    assert_presence_in_block_store
      ~with_metadata:false
      block_store
      (List.rev (List.concat cycles))
    (* hack: invert the reading order to clear the cache *)
  in
  let expected_preserved_blocks =
    List.concat (List.take_n 3 (List.rev cycles))
  in
  (* Last 3 cycles should have metadata *)
  let* () =
    assert_presence_in_block_store
      ~with_metadata:true
      block_store
      expected_preserved_blocks
  in
  let expected_pruned_blocks =
    List.take_n (List.length cycles - 3) cycles |> List.concat
  in
  (* First 7 cycles shouldn't have metadata *)
  let* () =
    assert_pruned_blocks_in_block_store block_store expected_pruned_blocks
  in
  let*! savepoint = Block_store.savepoint block_store in
  let expected_savepoint =
    List.nth cycles (10 - 3)
    |> WithExceptions.Option.get ~loc:__LOC__
    |> List.hd
    |> WithExceptions.Option.get ~loc:__LOC__
    |> Block_repr.level
  in
  Assert.Int32.equal ~msg:"savepoint" expected_savepoint (snd savepoint) ;
  let*! caboose = Block_store.caboose block_store in
  Assert.Int32.equal ~msg:"caboose" 0l (snd caboose) ;
  return_unit

let test_rolling_0_merge block_store =
  let open Lwt_result_syntax in
  (* The total of blocks should be > 130 to prevent the cache from
     retaining them *)
  let cycle_length = 20 in
  let nb_cycles = 10 in
  let* cycles =
    perform_n_cycles_merge
      ~cycle_length
      block_store
      (Rolling (Some {offset = 0}))
      nb_cycles
  in
  let all_blocks = List.concat cycles in
  let expected_savepoint_level =
    ((nb_cycles - 1) * cycle_length) - 1 (* lpbl *) - 1
    (* lpbl max_op_ttl *)
  in
  let expected_pruned_blocks, expected_preserved_blocks =
    List.split_n
      (expected_savepoint_level - 1)
      (* the genesis block is not counted *) all_blocks
    (* First 9 cycles shouldn't have metadata except for the lpbl
       block (i.e. the last one) *)
  in
  let* () = assert_absence_in_block_store block_store expected_pruned_blocks in
  let* () =
    assert_presence_in_block_store
      ~with_metadata:true
      block_store
      expected_preserved_blocks
  in
  let*! savepoint = Block_store.savepoint block_store in
  Assert.Int32.equal
    ~msg:"savepoint"
    (Int32.of_int expected_savepoint_level)
    (snd savepoint) ;
  let*! caboose = Block_store.caboose block_store in
  Assert.Int32.equal
    ~msg:"caboose"
    (Int32.of_int expected_savepoint_level)
    (snd caboose) ;
  return_unit

let test_rolling_2_merge block_store =
  let open Lwt_result_syntax in
  (* The total of blocks should be > 130 to prevent the cache from
     retaining them *)
  let* cycles =
    perform_n_cycles_merge
      ~cycle_length:20
      block_store
      (Rolling (Some {offset = 2}))
      10
  in
  let expected_preserved_blocks =
    List.concat (List.take_n 3 (List.rev cycles))
  in
  (* Last 3 cycles should have metadata *)
  let* () =
    assert_presence_in_block_store
      ~with_metadata:true
      block_store
      expected_preserved_blocks
  in
  let expected_pruned_blocks =
    List.take_n (List.length cycles - 3) cycles |> List.concat
  in
  (* First 7 cycles shouldn't have metadata *)
  let* () = assert_absence_in_block_store block_store expected_pruned_blocks in
  let*! savepoint = Block_store.savepoint block_store in
  let expected_savepoint =
    List.nth cycles (10 - 3)
    |> WithExceptions.Option.get ~loc:__LOC__
    |> List.hd
    |> WithExceptions.Option.get ~loc:__LOC__
    |> Block_repr.level
  in
  Assert.Int32.equal ~msg:"savepoint" expected_savepoint (snd savepoint) ;
  let*! caboose = Block_store.caboose block_store in
  Assert.Int32.equal ~msg:"caboose" expected_savepoint (snd caboose) ;
  return_unit

let test_split_cycle_merge block_store =
  let open Lwt_result_syntax in
  let* cycles =
    perform_n_cycles_merge
      ~cycle_size_limit:3l
      ~cycle_length:5
      block_store
      Archive
      5
  in
  (* All blocks w/ metadata should be present *)
  let* () =
    assert_presence_in_block_store
      ~with_metadata:true
      block_store
      (List.concat cycles)
  in
  let cemented_cycles =
    Cemented_block_store.cemented_blocks_files
      (Block_store.cemented_block_store block_store)
    |> function
    | Some a -> Array.to_list a
    | None -> []
  in
  let () =
    (* Ensure that cemented cycles are split as 2 or 3 block chunks *)
    List.iter
      (fun {Cemented_block_store.start_level; end_level; _} ->
        let diff_nb = succ Int32.(sub end_level start_level |> to_int) in
        assert (diff_nb >= 2 && diff_nb <= 3))
      cemented_cycles
  in
  return_unit

let wrap_test ?(keep_dir = true) (name, g) =
  let open Lwt_result_syntax in
  let f dir_path =
    let genesis_block =
      Block_repr.create_genesis_block
        ~genesis:Test_utils.genesis
        Context_hash.zero
    in
    let store_dir = Naming.store_dir ~dir_path in
    let chain_dir = Naming.chain_dir store_dir Chain_id.zero in
    let*! () = Lwt_utils_unix.create_dir (Naming.dir_path chain_dir) in
    let*! r =
      Block_store.create ~block_cache_limit:1 chain_dir ~genesis_block
    in
    match r with
    | Error err ->
        Format.printf
          "@\nCannot instanciate block store:@\n%a@."
          Error_monad.pp_print_trace
          err ;
        Lwt.fail Alcotest.Test_error
    | Ok block_store -> (
        let*! r =
          Lwt.finalize
            (fun () -> protect (fun () -> g block_store))
            (fun () -> Block_store.close block_store)
        in
        match r with
        | Ok () -> Lwt.return_unit
        | Error err ->
            Format.printf
              "@\nTest failed:@\n%a@."
              Error_monad.pp_print_trace
              err ;
            Lwt.fail Alcotest.Test_error)
  in
  let run _ _ =
    let prefix_dir = "tezos_block_store_test_" in
    if not keep_dir then Lwt_utils_unix.with_tempdir prefix_dir f
    else
      let base_dir = Filename.temp_file prefix_dir "" in
      Format.printf "temp dir: %s@." base_dir ;
      let*! () = Lwt_unix.unlink base_dir in
      let*! () = Lwt_unix.mkdir base_dir 0o700 in
      f base_dir
  in
  Alcotest_lwt.test_case name `Quick run

let tests : string * unit Alcotest_lwt.test_case list =
  let test_cases =
    List.map
      wrap_test
      [
        ( "block storing and access predecessors (hash only)",
          test_storing_and_access_predecessors );
        ("simple merge", test_simple_merge);
        ("consecutive & concurrent merge", test_consecutive_concurrent_merges);
        ("10 cycles merge", test_ten_cycles_merge);
        ("merge with branches", test_merge_with_branches);
        ("consecutive merge (Archive)", test_archive_merge);
        ("consecutive merge (Full + 0 cycles)", test_full_0_merge);
        ("consecutive merge (Full + 2 cycles)", test_full_2_merge);
        ("consecutive merge (Rolling + 0 cycles)", test_rolling_0_merge);
        ("consecutive merge (Rolling + 2 cycles)", test_rolling_2_merge);
        ("split cycle merge", test_split_cycle_merge);
      ]
  in
  ("block store", test_cases)

let () = Lwt_main.run (Alcotest_lwt.run ~__FILE__ "tezos-store" [tests])
