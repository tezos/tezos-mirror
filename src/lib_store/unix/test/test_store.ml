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
    Invocation: dune exec src/lib_store/unix/test/main.exe \
                 -- --file test_store.ml
    Subject: Store tests
*)

open Test_utils

let test_cycles store =
  let open Lwt_result_syntax in
  let chain_store = Store.main_chain_store store in
  let* blocks =
    List.fold_left_es
      (fun acc _ ->
        let* blocks, _head = append_cycle ~should_set_head:true chain_store in
        return (blocks @ acc))
      []
      (1 -- 10)
  in
  assert_presence_in_store chain_store blocks

let test_cases = [wrap_test ("store cycles", fun _ -> test_cycles)]

open Example_tree

(** Initialization *)

(** Chain_traversal.path *)

let rec compare_path is_eq p1 p2 =
  match (p1, p2) with
  | [], [] -> true
  | h1 :: p1, h2 :: p2 -> is_eq h1 h2 && compare_path is_eq p1 p2
  | _ -> false

let vblock tbl k =
  Nametbl.find tbl k |> WithExceptions.Option.to_exn ~none:Not_found

let pp_print_list fmt l =
  Format.fprintf
    fmt
    "@[<h>%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Block_hash.pp_short)
    (List.map Store.Block.hash l)

let test_path chain_store tbl =
  let open Lwt_syntax in
  let check_path h1 h2 p2 =
    let* o =
      Store.Chain_traversal.path
        chain_store
        ~from_block:(vblock tbl h1)
        ~to_block:(vblock tbl h2)
    in
    match o with
    | None -> Assert.fail_msg "cannot compute path %s -> %s" h1 h2
    | Some (p : Store.Block.t list) ->
        let p2 = List.map (fun b -> vblock tbl b) p2 in
        if not (compare_path Store.Block.equal p p2) then (
          Format.printf "expected:\t%a@." pp_print_list p2 ;
          Format.printf "got:\t\t%a@." pp_print_list p ;
          Assert.fail_msg "bad path %s -> %s" h1 h2) ;
        Lwt.return_unit
  in
  let* () = check_path "Genesis" "Genesis" [] in
  let* () = check_path "A1" "A1" [] in
  let* () = check_path "A2" "A6" ["A3"; "A4"; "A5"; "A6"] in
  let* () = check_path "B2" "B6" ["B3"; "B4"; "B5"; "B6"] in
  let* () = check_path "A1" "B3" ["A2"; "B1"; "B2"; "B3"] in
  return_ok_unit

(****************************************************************************)

(** Chain_traversal.common_ancestor *)

let test_ancestor chain_store tbl =
  let open Lwt_syntax in
  let check_ancestor h1 h2 expected =
    let* o =
      Store.Chain_traversal.common_ancestor
        chain_store
        (vblock tbl h1)
        (vblock tbl h2)
    in
    match o with
    | None -> Assert.fail_msg "not ancestor found"
    | Some a ->
        if
          not
            (Block_hash.equal (Store.Block.hash a) (Store.Block.hash expected))
        then Assert.fail_msg "bad ancestor %s %s" h1 h2 ;
        Lwt.return_unit
  in
  let* () = check_ancestor "Genesis" "Genesis" (vblock tbl "Genesis") in
  let* () = check_ancestor "Genesis" "A3" (vblock tbl "Genesis") in
  let* () = check_ancestor "A3" "Genesis" (vblock tbl "Genesis") in
  let* () = check_ancestor "A1" "A1" (vblock tbl "A1") in
  let* () = check_ancestor "A1" "A3" (vblock tbl "A1") in
  let* () = check_ancestor "A3" "A1" (vblock tbl "A1") in
  let* () = check_ancestor "A6" "B6" (vblock tbl "A2") in
  let* () = check_ancestor "B6" "A6" (vblock tbl "A2") in
  let* () = check_ancestor "A4" "B1" (vblock tbl "A2") in
  let* () = check_ancestor "B1" "A4" (vblock tbl "A2") in
  let* () = check_ancestor "A2" "B1" (vblock tbl "A2") in
  let* () = check_ancestor "B1" "A2" (vblock tbl "A2") in
  let* () = check_ancestor "A2" "B1" (vblock tbl "A2") in
  let* () = check_ancestor "B1" "A2" (vblock tbl "A2") in
  return_ok_unit

(****************************************************************************)

let seed =
  let receiver_id =
    P2p_peer.Id.of_string_exn (String.make P2p_peer.Id.size 'r')
  in
  let sender_id =
    P2p_peer.Id.of_string_exn (String.make P2p_peer.Id.size 's')
  in
  {Block_locator.receiver_id; sender_id}

let iter2_exn f l1 l2 =
  List.iter2 ~when_different_lengths:(Failure "iter2_exn") f l1 l2 |> function
  | Ok () -> ()
  | _ -> assert false

(** Block_locator *)

let test_locator chain_store tbl =
  let open Lwt_syntax in
  let check_locator length h1 expected =
    let* {Block_locator.history; _} =
      Store.Chain.compute_locator
        chain_store
        ~max_size:length
        (vblock tbl h1)
        seed
    in
    if Compare.List_lengths.(history <> expected) then
      Assert.fail_msg
        "Invalid locator length %s (found: %d, expected: %d)"
        h1
        (List.length history)
        (List.length expected) ;
    iter2_exn
      (fun h h2 ->
        if not (Block_hash.equal h (Store.Block.hash @@ vblock tbl h2)) then
          Assert.fail_msg "Invalid locator %s (expected: %s)" h1 h2)
      history
      expected ;
    Lwt.return_unit
  in
  let* () = check_locator 6 "A8" ["A7"; "A6"; "A5"; "A4"; "A3"; "A2"] in
  let* () =
    check_locator 8 "B8" ["B7"; "B6"; "B5"; "B4"; "B3"; "B2"; "B1"; "A2"]
  in
  let* () = check_locator 4 "B8" ["B7"; "B6"; "B5"; "B4"] in
  let* () = check_locator 0 "A5" [] in
  let* () = check_locator 100 "A5" ["A4"; "A3"; "A2"; "A1"; "Genesis"] in
  return_ok_unit

(****************************************************************************)

(** Chain.head/set_head *)

let test_head chain_store tbl =
  let open Lwt_result_syntax in
  let*! head = Store.Chain.current_head chain_store in
  let*! genesis_block = Store.Chain.genesis_block chain_store in
  if not (Store.Block.equal head genesis_block) then
    Assert.fail_msg "unexpected head" ;
  let* prev_head = Store.Chain.set_head chain_store (vblock tbl "A6") in
  if not (Store.Block.equal prev_head genesis_block) then
    Assert.fail_msg "unexpected previous head" ;
  let*! head = Store.Chain.current_head chain_store in
  if not (Store.Block.equal head (vblock tbl "A6")) then
    Assert.fail_msg "unexpected head" ;
  return_unit

(****************************************************************************)

(** Chain.mem *)

(*
    Genesis (H) - A1 - A2 - A3 - A4 - A5 - A6 - A7 - A8
                         \
                          B1 - B2 - B3 - B4 - B5 - B6 - B7 - B8
*)

let test_mem chain_store tbl =
  let open Lwt_result_syntax in
  let mem x =
    let b = vblock tbl x in
    let b_descr = Store.Block.(hash b, level b) in
    Store.Chain.is_in_chain chain_store b_descr
  in
  let test_mem x =
    let*! b = mem x in
    match b with true -> Lwt.return_unit | false -> Assert.fail_msg "mem %s" x
  in
  let test_not_mem x =
    let*! b = mem x in
    match b with
    | false -> Lwt.return_unit
    | true -> Assert.fail_msg "not (mem %s)" x
  in
  let*! () = test_not_mem "A3" in
  let*! () = test_not_mem "A6" in
  let*! () = test_not_mem "A8" in
  let*! () = test_not_mem "B1" in
  let*! () = test_not_mem "B6" in
  let*! () = test_not_mem "B8" in
  let* _ = Store.Chain.set_head chain_store (vblock tbl "A8") in
  let*! () = test_mem "A3" in
  let*! () = test_mem "A6" in
  let*! () = test_mem "A8" in
  let*! () = test_not_mem "B1" in
  let*! () = test_not_mem "B6" in
  let*! () = test_not_mem "B8" in
  let* () =
    let* prev_head = Store.Chain.set_head chain_store (vblock tbl "A6") in
    Assert.equal ~loc:__LOC__ prev_head (vblock tbl "A8") ;
    return_unit
  in
  let*! () = test_mem "A2" in
  let*! () = test_mem "A3" in
  let*! () = test_mem "A6" in
  let*! () = test_not_mem "A7" in
  let*! () = test_not_mem "A8" in
  let*! () = test_not_mem "B1" in
  let*! () = test_not_mem "B6" in
  let*! () = test_not_mem "B8" in
  let* () =
    let* prev_head = Store.Chain.set_head chain_store (vblock tbl "B6") in
    Assert.equal ~loc:__LOC__ prev_head (vblock tbl "A6") ;
    return_unit
  in
  let*! () = test_mem "A2" in
  let*! () = test_not_mem "A3" in
  let*! () = test_not_mem "A4" in
  let*! () = test_not_mem "A6" in
  let*! () = test_not_mem "A8" in
  let*! () = test_mem "B1" in
  let*! () = test_mem "B6" in
  let*! () = test_not_mem "B8" in
  let* () =
    let* prev_head = Store.Chain.set_head chain_store (vblock tbl "B8") in
    Assert.equal ~loc:__LOC__ prev_head (vblock tbl "B6") ;
    return_unit
  in
  let*! () = test_mem "A2" in
  let*! () = test_not_mem "A3" in
  let*! () = test_not_mem "A4" in
  let*! () = test_not_mem "A6" in
  let*! () = test_not_mem "A8" in
  let*! () = test_mem "B1" in
  let*! () = test_mem "B6" in
  let*! () = test_mem "B8" in
  return_unit

(****************************************************************************)

(** Chain_traversal.new_blocks *)

let test_new_blocks chain_store tbl =
  let open Lwt_syntax in
  let test head h expected_ancestor expected =
    let to_block = vblock tbl head and from_block = vblock tbl h in
    let* ancestor, blocks =
      Store.Chain_traversal.new_blocks chain_store ~from_block ~to_block
    in
    if
      not
        (Block_hash.equal
           (Store.Block.hash ancestor)
           (Store.Block.hash @@ vblock tbl expected_ancestor))
    then
      Assert.fail_msg
        "Invalid ancestor %s -> %s (expected: %s)"
        head
        h
        expected_ancestor ;
    if Compare.List_lengths.(blocks <> expected) then
      Assert.fail_msg
        "Invalid locator length %s (found: %d, expected: %d)"
        h
        (List.length blocks)
        (List.length expected) ;
    iter2_exn
      (fun h1 h2 ->
        if
          not
            (Block_hash.equal
               (Store.Block.hash h1)
               (Store.Block.hash @@ vblock tbl h2))
        then
          Assert.fail_msg "Invalid new blocks %s -> %s (expected: %s)" head h h2)
      blocks
      expected ;
    Lwt.return_unit
  in
  let* () = test "A6" "A6" "A6" [] in
  let* () = test "A8" "A6" "A6" ["A7"; "A8"] in
  let* () = test "A8" "B7" "A2" ["A3"; "A4"; "A5"; "A6"; "A7"; "A8"] in
  return_ok_unit

(** Store.Chain.checkpoint *)

(*
- Valid branch are kept after setting a checkpoint. Bad branch are cut

- Setting a checkpoint in the future does not remove anything

- Reaching a checkpoint in the future with the right block keeps that
block and remove any concurrent branch

- Reaching a checkpoint in the future with a bad block remove that block and
does not prevent a future good block from correctly being reached

- There are no bad quadratic behaviours

 *)

let test_basic_checkpoint chain_store table =
  let open Lwt_result_syntax in
  let block = vblock table "A1" in
  let* _prev_head = Store.Chain.set_head chain_store block in
  (* Setting target for A1 *)
  let* () =
    Store.Chain.set_target
      chain_store
      (Store.Block.hash block, Store.Block.level block)
  in
  let*! c_block, c_level = Store.Chain.checkpoint chain_store in
  (* Target should not be set, only the checkpoint. *)
  let* () =
    let*! o = Store.Chain.target chain_store in
    match o with
    | Some _target -> Assert.fail_msg "unexpected target"
    | None -> return_unit
  in
  if
    (not (Block_hash.equal c_block (Store.Block.hash block)))
    && Int32.equal c_level (Store.Block.level block)
  then Assert.fail_msg "unexpected checkpoint"
  else return_unit

(*
   - cp: checkpoint

  Genesis - A1 - A2 (cp) - A3 - A4 - A5
                  \
                   B1 - B2 - B3 - B4 - B5
  *)

(* Store.Chain.acceptable_block:
   will the block is compatible with the current checkpoint? *)

let test_acceptable_block chain_store table =
  let open Lwt_result_syntax in
  let block = vblock table "A2" in
  let block_hash = Store.Block.hash block in
  let level = Store.Block.level block in
  let* _prev_head = Store.Chain.set_head chain_store block in
  let* () = Store.Chain.set_target chain_store (block_hash, level) in
  (* it is accepted if the new head is greater than the checkpoint *)
  let block_1 = vblock table "A1" in
  let*! is_accepted_block =
    Store.Chain.is_acceptable_block
      chain_store
      (Store.Block.hash block_1, Store.Block.level block_1)
  in
  if not is_accepted_block then return_unit
  else Assert.fail_msg "unacceptable block was accepted"

(*
  Genesis - A1 - A2 (cp) - A3 - A4 - A5
                  \
                   B1 - B2 - B3 - B4 - B5
  *)

let test_is_valid_target chain_store table =
  let open Lwt_result_syntax in
  let block = vblock table "A2" in
  let block_hash = Store.Block.hash block in
  let level = Store.Block.level block in
  let* _prev_head = Store.Chain.set_head chain_store block in
  let* () = Store.Chain.set_target chain_store (block_hash, level) in
  (* "b3" is valid because:
     a1 - a2 (checkpoint) - b1 - b2 - b3
  *)
  return_unit

(* Setting checkpoint in the future is possible

   Storing a block at the same level with a different hash is not
   allowed.
*)

let test_future_target chain_store _ =
  let open Lwt_result_syntax in
  let*! genesis_block = Store.Chain.genesis_block chain_store in
  let genesis_descr = Store.Block.descriptor genesis_block in
  let*! bad_chain, bad_head = make_raw_block_list genesis_descr 5 in
  let*! good_chain, good_head = make_raw_block_list genesis_descr 5 in
  let* () = Store.Chain.set_target chain_store (raw_descriptor good_head) in
  let* () =
    List.iter_es
      (fun b ->
        Format.printf "storing : %a@." pp_raw_block b ;
        let* _ = store_raw_block chain_store b in
        return_unit)
      (List.rev
         (List.tl (List.rev bad_chain) |> WithExceptions.Option.get ~loc:__LOC__))
  in
  let* () =
    let*! r = store_raw_block chain_store bad_head in
    match r with
    | Error [Validation_errors.Checkpoint_error _] -> return_unit
    | Ok _ | _ -> Assert.fail_msg "incompatible head accepted"
  in
  let* () =
    List.iter_es
      (fun b ->
        let* _ = store_raw_block chain_store b in
        return_unit)
      (List.rev
         (List.tl (List.rev good_chain)
         |> WithExceptions.Option.get ~loc:__LOC__))
  in
  let* _ = store_raw_block chain_store good_head in
  return_unit

(* check if the checkpoint can be reached

   Genesis - A1 (cp) - A2 (head) - A3 - A4 - A5
                        \
                        B1 - B2 - B3 - B4 - B5
*)

let test_reach_target chain_store table =
  let open Lwt_result_syntax in
  let mem x =
    let b = vblock table x in
    Store.Chain.is_in_chain chain_store Store.Block.(hash b, level b)
  in
  let test_mem x =
    let*! b = mem x in
    match b with true -> Lwt.return_unit | false -> Assert.fail_msg "mem %s" x
  in
  let test_not_mem x =
    let*! b = mem x in
    match b with
    | false -> Lwt.return_unit
    | true -> Assert.fail_msg "not (mem %s)" x
  in
  let block = vblock table "A1" in
  let header = Store.Block.header block in
  let checkpoint_hash = Store.Block.hash block in
  let checkpoint_level = Store.Block.level block in
  let* _pred_head = Store.Chain.set_head chain_store block in
  let* () =
    Store.Chain.set_target chain_store (checkpoint_hash, checkpoint_level)
  in
  let*! c_hash, _c_level = Store.Chain.checkpoint chain_store in
  let time_now = Time.System.to_protocol (Time.System.now ()) in
  if
    Time.Protocol.compare
      (Time.Protocol.add time_now 15L)
      header.shell.timestamp
    >= 0
  then
    if
      Int32.equal header.shell.level checkpoint_level
      && not (Block_hash.equal checkpoint_hash c_hash)
    then Assert.fail_msg "checkpoint error"
    else
      let* _ = Store.Chain.set_head chain_store (vblock table "A2") in
      let*! head = Store.Chain.current_head chain_store in
      let checkpoint_reached =
        (Store.Block.header head).shell.level >= checkpoint_level
      in
      if checkpoint_reached then
        (* if reached the checkpoint, every block before the checkpoint
           must be the part of the chain *)
        if header.shell.level <= checkpoint_level then
          let*! () = test_mem "Genesis" in
          let*! () = test_mem "A1" in
          let*! () = test_mem "A2" in
          let*! () = test_not_mem "A3" in
          let*! () = test_not_mem "B1" in
          return_unit
        else Assert.fail_msg "checkpoint error"
      else Assert.fail_msg "checkpoint error"
  else Assert.fail_msg "fail future block header"

(* Check function may_update_target

   Genesis - A1 - A2 (cp) - A3 - A4 - A5
                  \
                  B1 - B2 - B3 - B4 - B5

   chain after update:

   Genesis - A1 - A2 - A3(cp) - A4 - A5
                  \
                  B1 - B2 - B3 - B4 - B5
*)

let test_not_may_update_target chain_store table =
  let open Lwt_result_syntax in
  (* set target at (2l, A2) *)
  let block_a2 = vblock table "A2" in
  let target_hash = Store.Block.hash block_a2 in
  let target_level = Store.Block.level block_a2 in
  let target = (target_hash, target_level) in
  let* _pred_head = Store.Chain.set_head chain_store block_a2 in
  let* () = Store.Chain.set_target chain_store target in
  (* set new target at (1l, A1) in the past *)
  let block_a1 = vblock table "A1" in
  let target_hash = Store.Block.hash block_a1 in
  let target_level = Store.Block.level block_a1 in
  let new_target = (target_hash, target_level) in
  Lwt.catch
    (fun () ->
      let* () = Store.Chain.set_target chain_store new_target in
      Assert.fail_msg "Unexpected target update")
    (function _ -> return_unit)

(****************************************************************************)

(** Store.Chain.block_of_identifier *)

let testable_hash =
  Alcotest.testable
    (fun fmt h -> Format.fprintf fmt "%s" (Block_hash.to_b58check h))
    Block_hash.equal

let init_block_of_identifier_test chain_store table =
  let open Lwt_result_syntax in
  let+ _ = vblock table "A8" |> Store.Chain.set_head chain_store in
  ()

let vblock_hash table name = vblock table name |> Store.Block.hash

let assert_successful_block_of_identifier
    ?(init = init_block_of_identifier_test) ~input ~expected chain_store table =
  let open Lwt_result_syntax in
  let* _ = init chain_store table in
  let+ found = Store.Chain.block_of_identifier chain_store input in
  Alcotest.check
    testable_hash
    "same block hash"
    expected
    (Store.Block.hash found)

let assert_failing_block_of_identifier ?(init = init_block_of_identifier_test)
    ~input chain_store table =
  let open Lwt_result_syntax in
  let* _ = init chain_store table in
  let*! r = Store.Chain.block_of_identifier chain_store input in
  match r with
  | Ok b ->
      Assert.fail_msg
        ~given:(Store.Block.hash b |> Block_hash.to_b58check)
        "retrieving the block did not failed as expected"
  | _ -> return_unit

let test_block_of_identifier_success_block_from_level chain_store table =
  let a5 = vblock table "A5" in
  assert_successful_block_of_identifier
    ~input:(`Level (Store.Block.level a5))
    ~expected:(Store.Block.hash a5)
    chain_store
    table

let test_block_of_identifier_success_block_from_hash chain_store table =
  let a5_hash = vblock_hash table "A5" in
  assert_successful_block_of_identifier
    ~input:(`Hash (a5_hash, 0))
    ~expected:a5_hash
    chain_store
    table

let test_block_of_identifier_success_block_from_hash_predecessor chain_store
    table =
  assert_successful_block_of_identifier
    ~input:(`Hash (vblock_hash table "A5", 2))
    ~expected:(vblock_hash table "A3")
    chain_store
    table

let test_block_of_identifier_success_block_from_hash_successor chain_store table
    =
  assert_successful_block_of_identifier
    ~input:(`Hash (vblock_hash table "A5", -2))
    ~expected:(vblock_hash table "A7")
    chain_store
    table

let test_block_of_identifier_success_caboose chain_store table =
  assert_successful_block_of_identifier
    ~input:(`Alias (`Caboose, 0))
    ~expected:(vblock_hash table "Genesis")
    chain_store
    table

let test_block_of_identifier_success_caboose_successor chain_store table =
  assert_successful_block_of_identifier
    ~input:(`Alias (`Caboose, -2))
    ~expected:(vblock_hash table "A2")
    chain_store
    table

let test_block_of_identifier_failure_caboose_predecessor chain_store table =
  assert_failing_block_of_identifier
    ~input:(`Alias (`Caboose, 2))
    chain_store
    table

let test_block_of_identifier_success_checkpoint chain_store table =
  let open Lwt_result_syntax in
  let a5 = vblock table "A5" in
  let a5_hash = Store.Block.hash a5 in
  let a5_descriptor = (a5_hash, Store.Block.level a5) in
  assert_successful_block_of_identifier
    ~init:(fun cs t ->
      let* _ = init_block_of_identifier_test cs t in
      Store.Unsafe.set_checkpoint cs a5_descriptor)
    ~input:(`Alias (`Checkpoint, 0))
    ~expected:a5_hash
    chain_store
    table

let test_block_of_identifier_success_checkpoint_predecessor chain_store table =
  let open Lwt_result_syntax in
  let a5 = vblock table "A5" in
  let a5_hash = Store.Block.hash a5 in
  let a5_descriptor = (a5_hash, Store.Block.level a5) in
  assert_successful_block_of_identifier
    ~init:(fun cs t ->
      let* _ = init_block_of_identifier_test cs t in
      Store.Unsafe.set_checkpoint cs a5_descriptor)
    ~input:(`Alias (`Checkpoint, 2))
    ~expected:(vblock_hash table "A3")
    chain_store
    table

let test_block_of_identifier_success_checkpoint_successor chain_store table =
  let open Lwt_result_syntax in
  let a5 = vblock table "A5" in
  let a5_hash = Store.Block.hash a5 in
  let a5_descriptor = (a5_hash, Store.Block.level a5) in
  assert_successful_block_of_identifier
    ~init:(fun cs t ->
      let* _ = init_block_of_identifier_test cs t in
      Store.Unsafe.set_checkpoint cs a5_descriptor)
    ~input:(`Alias (`Checkpoint, -2))
    ~expected:(vblock_hash table "A7")
    chain_store
    table

let test_block_of_identifier_failure_checkpoint_successor chain_store table =
  let open Lwt_result_syntax in
  let a5 = vblock table "A5" in
  let a5_hash = Store.Block.hash a5 in
  let a5_descriptor = (a5_hash, Store.Block.level a5) in
  assert_failing_block_of_identifier
    ~init:(fun cs t ->
      let* _ = init_block_of_identifier_test cs t in
      Store.Unsafe.set_checkpoint cs a5_descriptor)
    ~input:(`Alias (`Checkpoint, -4))
    chain_store
    table

let test_block_of_identifier_success_savepoint chain_store table =
  assert_successful_block_of_identifier
    ~input:(`Alias (`Savepoint, 0))
    ~expected:(vblock_hash table "Genesis")
    chain_store
    table

let tests =
  let test_tree_cases =
    List.map
      (wrap_test ~with_gc:false)
      [
        ("path between blocks", test_path);
        ("common ancestor", test_ancestor);
        ("block locators", test_locator);
        ("set head", test_head);
        ("blocks in chain", test_mem);
        ("new blocks", test_new_blocks);
        ("basic checkpoint", test_basic_checkpoint);
        ("is valid target", test_is_valid_target);
        ("acceptable block", test_acceptable_block);
        ("future target", test_future_target);
        ("reach target", test_reach_target);
        ("update target in node", test_not_may_update_target);
        ( "block_of_identifier should succeed to retrieve block from level",
          test_block_of_identifier_success_block_from_level );
        ( "block_of_identifier should succeed to retrieve block from hash",
          test_block_of_identifier_success_block_from_hash );
        ( "block_of_identifier should succeed to retrieve block from hash \
           predecessor",
          test_block_of_identifier_success_block_from_hash_predecessor );
        ( "block_of_identifier should succeed to retrieve block from hash \
           successor",
          test_block_of_identifier_success_block_from_hash_successor );
        ( "block_of_identifier should succeed to retrieve the caboose",
          test_block_of_identifier_success_caboose );
        ( "block_of_identifier should succeed to retrieve caboose successor",
          test_block_of_identifier_success_caboose_successor );
        ( "block_of_identifier should fail to retrieve caboose predecessor",
          test_block_of_identifier_failure_caboose_predecessor );
        ( "block_of_identifier should succeed to retrieve the checkpoint",
          test_block_of_identifier_success_checkpoint );
        ( "block_of_identifier should succeed to retrieve checkpoint predecessor",
          test_block_of_identifier_success_checkpoint_predecessor );
        ( "block_of_identifier should succeed to retrieve the checkpoint \
           successor",
          test_block_of_identifier_success_checkpoint_successor );
        ( "block_of_identifier should fail to retrieve the checkpoint \
           successor after the head",
          test_block_of_identifier_failure_checkpoint_successor );
        ( "block_of_identifier should succeed to retrieve the savepoint",
          test_block_of_identifier_success_savepoint );
      ]
  in
  ("store", test_cases @ test_tree_cases)

let () = Lwt_main.run (Alcotest_lwt.run ~__FILE__ "tezos-store" [tests])
