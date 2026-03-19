(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
    -------
    Component:  Protocol (Sapling)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_sapling.ml
    Subject:    On the privacy-preserving library Sapling
*)

open Protocol
open Alpha_context

module Raw_context_tests = struct
  open Sapling_helpers.Common

  (* This test adds to the first 100 positions in the commitments tree the
     constant value `uncommitted` for which we know the corresponding root and
     tests that the returned root is as expected. *)
  let commitments_add_uncommitted () =
    let open Lwt_result_wrap_syntax in
    let* b, _contract = Context.init1 () in
    let*@ ctx =
      Raw_context.prepare
        b.context
        ~level:b.header.shell.level
        ~predecessor_timestamp:b.header.shell.timestamp
        ~timestamp:b.header.shell.timestamp
        ~all_bakers_attest_first_level:None
    in
    let module H = Tezos_sapling.Core.Client.Hash in
    let cm = H.uncommitted ~height:0 in
    let expected_root = H.uncommitted ~height:32 in
    let*@ ctx, id =
      Lazy_storage_diff.fresh
        Lazy_storage_kind.Sapling_state
        ~temporary:false
        ctx
    in
    let*@ ctx = Sapling_storage.init ctx id ~memo_size:0 in
    let* (_ctx : Raw_context.t) =
      List.fold_left_es
        (fun ctx pos ->
          let*@ ctx, root = Sapling_storage.Commitments.get_root ctx id in
          assert (root = expected_root) ;
          let*@ ctx, _size =
            Sapling_storage.Commitments.add
              ctx
              id
              [H.to_commitment cm]
              (Int64.of_int pos)
          in
          let+@ ctx, root = Sapling_storage.Commitments.get_root ctx id in
          assert (root = expected_root) ;
          ctx)
        ctx
        (0 -- 99)
    in
    return_unit

  (* Nullifiers don't check for duplicates are it's done by verify_update,
     however committing to disk twice the same nf causes a storage error by
     trying to initialize the same key twice. *)
  let nullifier_double () =
    let open Lwt_result_wrap_syntax in
    let* b, _contract = Context.init1 () in
    let*@ ctx =
      Raw_context.prepare
        b.context
        ~level:b.header.shell.level
        ~predecessor_timestamp:b.header.shell.timestamp
        ~timestamp:b.header.shell.timestamp
        ~all_bakers_attest_first_level:None
    in
    let*@ ctx, id =
      Lazy_storage_diff.fresh
        Lazy_storage_kind.Sapling_state
        ~temporary:false
        ctx
    in
    let*@ ctx = Sapling_storage.init ctx id ~memo_size:0 in
    let nf = gen_nf () in
    let open Sapling_storage in
    let state =
      {id = Some id; diff = Sapling_storage.empty_diff; memo_size = 0}
    in
    let state = nullifiers_add state nf in
    let state = nullifiers_add state nf in
    assert (Compare.List_length_with.(state.diff.nullifiers = 2)) ;
    let*@ disk_size = Sapling_storage.Nullifiers.size ctx id in
    assert (disk_size = 0L) ;
    Sapling_storage.apply_diff ctx id state.diff |> assert_error

  (* In this test we add two lists of nullifiers to the state, one is applied to
     the context (committed to disk) and one is kept in kept in a diff (only in
     memory). We then check that nullifier_mem answers true for those two lists
     and false for a third one. *)
  let nullifier_test () =
    let open Lwt_result_wrap_syntax in
    let* b, _contract = Context.init1 () in
    let*@ ctx =
      Raw_context.prepare
        b.context
        ~level:b.header.shell.level
        ~predecessor_timestamp:b.header.shell.timestamp
        ~timestamp:b.header.shell.timestamp
        ~all_bakers_attest_first_level:None
    in
    let*@ ctx, id =
      Lazy_storage_diff.fresh
        Lazy_storage_kind.Sapling_state
        ~temporary:false
        ctx
    in
    let*@ ctx = Sapling_storage.init ctx id ~memo_size:0 in
    let nf_list_ctx =
      WithExceptions.List.init ~loc:__LOC__ 10 (fun _ -> gen_nf ())
    in
    let state =
      List.fold_left
        (fun state nf -> Sapling_storage.nullifiers_add state nf)
        {id = Some id; diff = Sapling_storage.empty_diff; memo_size = 0}
        nf_list_ctx
    in
    let*@ ctx, _ = Sapling_storage.apply_diff ctx id state.diff in
    let nf_list_diff =
      WithExceptions.List.init ~loc:__LOC__ 10 (fun _ -> gen_nf ())
    in
    let state =
      List.fold_left
        (fun state nf -> Sapling_storage.nullifiers_add state nf)
        state
        nf_list_diff
    in
    let* () =
      List.iter_ep
        (fun nf ->
          let*@ _ctx, bool = Sapling_storage.nullifiers_mem ctx state nf in
          assert bool ;
          return_unit)
        (nf_list_ctx @ nf_list_diff)
    in
    let nf_list_absent =
      WithExceptions.List.init ~loc:__LOC__ 10 (fun _ -> gen_nf ())
    in
    List.iter_ep
      (fun nf ->
        let*@ _ctx, bool = Sapling_storage.nullifiers_mem ctx state nf in
        assert (not bool) ;
        return_unit)
      nf_list_absent

  (* This test applies a diff with tuples of ciphertext, commitment. Then it
     checks the result of get_from with different indexes. *)
  let cm_cipher_test () =
    let open Lwt_result_wrap_syntax in
    Random.self_init () ;
    let memo_size = Random.int 200 in
    let* b, _contract = Context.init1 () in
    let*@ ctx =
      Raw_context.prepare
        b.context
        ~level:b.header.shell.level
        ~predecessor_timestamp:b.header.shell.timestamp
        ~timestamp:b.header.shell.timestamp
        ~all_bakers_attest_first_level:None
    in
    let*@ ctx, id =
      Lazy_storage_diff.fresh
        Lazy_storage_kind.Sapling_state
        ~temporary:false
        ctx
    in
    let*@ ctx = Sapling_storage.init ctx id ~memo_size in
    let*@ diff, ctx = Sapling_storage.state_from_id ctx id in
    let list_added =
      WithExceptions.List.init ~loc:__LOC__ 10 (fun _ ->
          gen_cm_cipher ~memo_size ())
    in
    let state = Sapling_storage.add diff list_added in
    let*@ ctx, (_ : Z.t) = Sapling_storage.apply_diff ctx id state.diff in
    let rec test_from from until expected =
      if from > until then return_unit
      else
        let*@ ctx, result = Sapling_storage.Ciphertexts.get_from ctx id from in
        let expected_cipher = List.map snd expected in
        assert (result = expected_cipher) ;
        let*@ result = Sapling_storage.Commitments.get_from ctx id from in
        let expected_cm = List.map fst expected in
        assert (result = expected_cm) ;
        test_from
          (Int64.succ from)
          until
          (WithExceptions.Option.get ~loc:__LOC__ @@ List.tl expected)
    in
    test_from 0L 9L list_added

  (* This test tests the insertion of a list vs inserting one by one.
     It does so by checking the equality of the roots. *)
  let list_insertion_test () =
    let open Lwt_result_wrap_syntax in
    Random.self_init () ;
    let memo_size = Random.int 200 in
    let* b, _contract = Context.init1 () in
    let*@ ctx =
      Raw_context.prepare
        b.context
        ~level:b.header.shell.level
        ~predecessor_timestamp:b.header.shell.timestamp
        ~timestamp:b.header.shell.timestamp
        ~all_bakers_attest_first_level:None
    in
    let*@ ctx, id_one_by_one =
      Lazy_storage_diff.fresh
        Lazy_storage_kind.Sapling_state
        ~temporary:false
        ctx
    in
    let*@ ctx = Sapling_storage.init ctx id_one_by_one ~memo_size in
    let list_to_add =
      fst @@ List.split
      @@ WithExceptions.List.init ~loc:__LOC__ 33 (fun _ ->
             gen_cm_cipher ~memo_size ())
    in
    let rec test counter ctx =
      if counter >= 32 then return_unit
      else
        (* add a single cm to the existing tree *)
        let*@ ctx, _size =
          Sapling_storage.Commitments.add
            ctx
            id_one_by_one
            [
              WithExceptions.Option.get ~loc:__LOC__
              @@ List.nth list_to_add counter;
            ]
            (Int64.of_int counter)
          (* create a new tree and add a list of cms *)
        in
        let*@ ctx, id_all_at_once =
          Lazy_storage_diff.fresh
            Lazy_storage_kind.Sapling_state
            ~temporary:false
            ctx
        in
        let*@ ctx = Sapling_storage.init ctx id_all_at_once ~memo_size in
        let*@ ctx, _size =
          Sapling_storage.Commitments.add
            ctx
            id_all_at_once
            (WithExceptions.List.init ~loc:__LOC__ (counter + 1) (fun i ->
                 WithExceptions.Option.get ~loc:__LOC__
                 @@ List.nth list_to_add i))
            0L
        in
        let*@ ctx, root_one_by_one =
          Sapling_storage.Commitments.get_root ctx id_one_by_one
        in
        let*@ ctx, root_all_at_once =
          Sapling_storage.Commitments.get_root ctx id_all_at_once
        in
        assert (root_all_at_once = root_one_by_one) ;
        test (counter + 1) ctx
    in
    test 0 ctx

  (* This test adds 10 more roots the maximum capacity, all at different
     levels, and checks that all but the first 10 are stored.
     Then it adds one in the diff and checks it is stored.
     Then it adds 10 at the same level and check that only the last one is
     stored. *)
  let root_test () =
    let open Lwt_result_wrap_syntax in
    let open Tezos_sapling.Core in
    let gen_root () =
      Data_encoding.Binary.of_bytes_exn
        Validator.Hash.encoding
        (Tezos_crypto.Hacl.Rand.gen 32)
    in
    let roots_ctx =
      WithExceptions.List.init
        ~loc:__LOC__
        (Int32.to_int Sapling_storage.Roots.size + 10)
        (fun _ -> gen_root ())
    in
    let* b, _contract = Context.init1 () in
    let*@ ctx =
      Raw_context.prepare
        b.context
        ~level:b.header.shell.level
        ~predecessor_timestamp:b.header.shell.timestamp
        ~timestamp:b.header.shell.timestamp
        ~all_bakers_attest_first_level:None
    in
    let*@ ctx, id =
      Lazy_storage_diff.fresh
        Lazy_storage_kind.Sapling_state
        ~temporary:false
        ctx
    in
    let*@ ctx = Sapling_storage.init ctx id ~memo_size:0 in
    (* Add one root per level to the context *)
    let* ctx, _ =
      List.fold_left_es
        (fun (ctx, cnt) root ->
          let*@ ctx = Sapling_storage.Roots.add ctx id root in
          (* Very low level way to "bake" a block. It would be better to use the
             helpers functions but they complicate the access to the raw_context. *)
          let+@ ctx =
            Raw_context.prepare
              ~level:(Int32.add b.header.shell.level cnt)
              ~predecessor_timestamp:b.header.shell.timestamp
              ~timestamp:b.header.shell.timestamp
              (Raw_context.recover ctx)
              ~all_bakers_attest_first_level:None
          in
          (ctx, Int32.succ cnt))
        (ctx, 0l)
        roots_ctx
    in
    (* Check mem on all the roots in the context. *)
    let state =
      Sapling_storage.
        {id = Some id; diff = Sapling_storage.empty_diff; memo_size = 0}
    in
    let* (_ : int) =
      List.fold_left_es
        (fun i root ->
          let+@ bool = Sapling_storage.root_mem ctx state root in
          assert (if i < 10 then not bool else bool) ;
          i + 1)
        0
        roots_ctx
    in
    (* Add roots w/o increasing the level *)
    let roots_same_level =
      WithExceptions.List.init ~loc:__LOC__ 10 (fun _ -> gen_root ())
    in
    let* ctx =
      List.fold_left_es
        (fun ctx root ->
          let*@ ctxt = Sapling_storage.Roots.add ctx id root in
          return ctxt)
        ctx
        roots_same_level
    in
    let* _, _ =
      List.fold_left_es
        (fun (i, ctx) root ->
          let+@ bool = Sapling_storage.root_mem ctx state root in
          assert (if i < 9 then not bool else bool) ;
          (i + 1, ctx))
        (0, ctx)
        roots_same_level
    in
    return_unit

  let test_get_memo_size () =
    let open Lwt_result_wrap_syntax in
    let* b, _contract = Context.init1 () in
    let*@ ctx =
      Raw_context.prepare
        b.context
        ~level:b.header.shell.level
        ~predecessor_timestamp:b.header.shell.timestamp
        ~timestamp:b.header.shell.timestamp
        ~all_bakers_attest_first_level:None
    in
    let*@ ctx, id =
      Lazy_storage_diff.fresh
        Lazy_storage_kind.Sapling_state
        ~temporary:false
        ctx
    in
    let*@ ctx = Sapling_storage.init ctx id ~memo_size:0 in
    let+@ memo_size = Sapling_storage.get_memo_size ctx id in
    assert (memo_size = 0)
end

module Alpha_context_tests = struct
  open Sapling_helpers.Alpha_context_helpers

  (* Create a transaction with memo_size 1, test that is validates with a newly
     created empty_state with memo_size 1 and does not with memo_size 0. *)
  let test_verify_memo () =
    let open Lwt_result_syntax in
    let* ctx = init () in
    let sk =
      Tezos_sapling.Core.Wallet.Spending_key.of_seed
        (Tezos_crypto.Hacl.Rand.gen 32)
    in
    let vt =
      let ps = Tezos_sapling.Storage.empty ~memo_size:0 in
      (* the dummy output will have memo_size 0 *)
      Tezos_sapling.Forge.forge_transaction
        ~number_dummy_outputs:1
        []
        []
        sk
        "anti-replay"
        ~bound_data:""
        ps
    in
    let* _, _ = verify_update ctx vt ~memo_size:0 |> assert_some in
    verify_update ctx vt ~memo_size:1 |> assert_none

  (* Bench the proving and validation time of shielding and transferring several
     tokens. *)
  let test_bench_phases () =
    let open Lwt_result_syntax in
    let* ctx = init () in
    let rounds = 5 in
    Log.info "\nrounds: %d\n" rounds ;
    let w = wallet_gen () in
    let cs = Tezos_sapling.Storage.empty ~memo_size:8 in
    (* one verify_update to get the id *)
    let vt = transfer w cs [] in
    let* ctx, id = verify_update ctx vt |> assert_some in
    let* cs = client_state_alpha ctx id in
    let start = Unix.gettimeofday () in
    let vts = List.map (fun _ -> transfer w cs []) (1 -- rounds) in
    let ctime_shields = Unix.gettimeofday () -. start in
    Log.info "client_shields %f\n" ctime_shields ;
    let start = Unix.gettimeofday () in
    let* ctx =
      List.fold_left_es
        (fun ctx vt ->
          let+ ctx, _id = verify_update ctx ~id vt |> assert_some in
          ctx)
        ctx
        vts
    in
    let vtime_shields = Unix.gettimeofday () -. start in
    Log.info "valdtr_shields %f\n" vtime_shields ;
    let* cs = client_state_alpha ctx id in
    let start = Unix.gettimeofday () in
    let vts = List.map (fun i -> transfer w cs [i]) (1 -- rounds) in
    let ctime_transfers = Unix.gettimeofday () -. start in
    Log.info "client_txs %f\n" ctime_transfers ;
    let start = Unix.gettimeofday () in
    let+ (_ctx : context) =
      List.fold_left_es
        (fun ctx vt ->
          let+ ctx, _id = verify_update ctx ~id vt |> assert_some in
          ctx)
        ctx
        vts
    in
    let vtime_transfers = Unix.gettimeofday () -. start in
    Log.info "valdtr_txs %f\n" vtime_transfers

  (* Same as before but for the legacy instruction. *)
  let test_bench_phases_legacy () =
    let open Lwt_result_syntax in
    let* ctx = init () in
    let rounds = 5 in
    Log.info "\nrounds: %d\n" rounds ;
    let w = wallet_gen () in
    let cs = Tezos_sapling.Storage.empty ~memo_size:8 in
    (* one verify_update to get the id *)
    let vt = transfer_legacy w cs [] in
    let* ctx, id = verify_update_legacy ctx vt |> assert_some in
    let* cs = client_state_alpha ctx id in
    let start = Unix.gettimeofday () in
    let vts = List.map (fun _ -> transfer_legacy w cs []) (1 -- rounds) in
    let ctime_shields = Unix.gettimeofday () -. start in
    Log.info "client_shields %f\n" ctime_shields ;
    let start = Unix.gettimeofday () in
    let* ctx =
      List.fold_left_es
        (fun ctx vt ->
          let+ ctx, _id = verify_update_legacy ctx ~id vt |> assert_some in
          ctx)
        ctx
        vts
    in
    let vtime_shields = Unix.gettimeofday () -. start in
    Log.info "valdtr_shields %f\n" vtime_shields ;
    let* cs = client_state_alpha ctx id in
    let start = Unix.gettimeofday () in
    let vts = List.map (fun i -> transfer_legacy w cs [i]) (1 -- rounds) in
    let ctime_transfers = Unix.gettimeofday () -. start in
    Log.info "client_txs %f\n" ctime_transfers ;
    let start = Unix.gettimeofday () in
    let+ (_ctx : context) =
      List.fold_left_es
        (fun ctx vt ->
          let+ ctx, _id = verify_update_legacy ctx ~id vt |> assert_some in
          ctx)
        ctx
        vts
    in
    let vtime_transfers = Unix.gettimeofday () -. start in
    Log.info "valdtr_txs %f\n" vtime_transfers

  (* Transfer several times the same token. *)
  let test_bench_fold_over_same_token () =
    let open Lwt_result_syntax in
    let* ctx = init () in
    let rounds = 5 in
    let w = wallet_gen () in
    let cs = Tezos_sapling.Storage.empty ~memo_size:8 in
    (* one verify_update to get the id *)
    let vt = transfer w cs [] in
    let* ctx, id = verify_update ctx vt |> assert_some in
    let rec loop cnt ctx =
      if cnt >= rounds then return_unit
      else
        (* inefficient: re-synch from scratch at each round *)
        let* cs = client_state_alpha ctx id in
        let vt = transfer w cs [cnt] in
        let* ctx, _id = verify_update ctx ~id vt |> assert_some in
        loop (cnt + 1) ctx
    in
    loop 0 ctx

  (*
    The following tests trigger all the branches of
    Sapling_validator.verify_update.
    The function performs several checks and returns None in case of failure.
    During development the function was modified to throw a different exception
    for each of its checks so to be sure that they were reached.
   *)

  (* Test that double spending the same input fails the nf check. *)
  let test_double_spend_same_input () =
    let open Lwt_result_syntax in
    let* ctx = init () in
    let w = wallet_gen () in
    let cs = Tezos_sapling.Storage.empty ~memo_size:8 in
    (* one verify_update to get the id *)
    let vt = transfer w cs [] in
    let* ctx, id = verify_update ctx vt |> assert_some in
    let* cs = client_state_alpha ctx id in
    let vt = transfer w cs [0] in
    let* _ctx, id = verify_update ctx ~id vt |> assert_some in
    let vt = transfer w cs [0; 0] in
    verify_update ctx ~id vt |> assert_none

  let test_verifyupdate_one_transaction () =
    let open Lwt_result_syntax in
    let* ctx = init () in
    let w = wallet_gen () in
    let cs = Tezos_sapling.Storage.empty ~memo_size:8 in
    let vt = transfer w cs [] in
    let* ctx, id = verify_update ctx vt |> assert_some in
    let* cs = client_state_alpha ctx id in
    let vt = transfer w cs [0] in
    (* fails sig check because of wrong balance *)
    let vt_broken =
      Tezos_sapling.Core.Validator.UTXO.
        {vt with balance = Int64.(succ vt.balance)}
    in
    let* () = verify_update ctx ~id vt_broken |> assert_none in
    (* randomize one output to fail check outputs *)
    (* don't randomize the ciphertext as it is not part of the proof *)
    let open Tezos_sapling.Core.Client.UTXO in
    let o = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd vt.outputs in
    let o_wrong_cm =
      {
        o with
        cm = randomized_byte o.cm Tezos_sapling.Core.Client.Commitment.encoding;
      }
    in
    let vt_broken =
      Tezos_sapling.Core.Validator.UTXO.{vt with outputs = [o_wrong_cm]}
    in
    let* () = verify_update ctx ~id vt_broken |> assert_none in
    (* position inside the cv *)
    let pos = Random.int 32 in
    let o_wrong_cv =
      {
        o with
        ciphertext =
          randomized_byte
            ~pos
            o.ciphertext
            Tezos_sapling.Core.Client.Ciphertext.encoding;
      }
    in
    let vt_broken =
      Tezos_sapling.Core.Validator.UTXO.{vt with outputs = [o_wrong_cv]}
    in
    verify_update ctx ~id vt_broken |> assert_none

  let test_verifyupdate_two_transactions () =
    let open Lwt_result_syntax in
    let* ctx = init () in
    let w = wallet_gen () in
    let cs = Tezos_sapling.Storage.empty ~memo_size:8 in
    (* generate the first storage *)
    let vt = transfer w cs [] in
    let* ctx, id1 = verify_update ctx vt |> assert_some in
    let* cs1 = client_state_alpha ctx id1 in
    let vt1 = transfer w cs1 [0] in
    (* generate the second storage *)
    let vt = transfer w cs [] in
    let* ctx, id2 = verify_update ctx vt |> assert_some in
    let* cs2 = client_state_alpha ctx id2 in
    let vt2 = transfer w cs2 [0] in
    (* fail root check *)
    let* () = verify_update ctx ~id:id1 vt2 |> assert_none in
    (* Swap the root so that it passes the root_mem check but fails
       the input check *)
    let vt1_broken =
      Tezos_sapling.Core.Validator.UTXO.{vt2 with root = vt1.root}
    in
    let* () = verify_update ctx ~id:id1 vt1_broken |> assert_none in
    (* fail the sig check *)
    let vt1_broken =
      Tezos_sapling.Core.Validator.UTXO.{vt1 with outputs = vt2.outputs}
    in
    verify_update ctx ~id:id1 vt1_broken |> assert_none
end

module Interpreter_tests = struct
  open Sapling_helpers.Interpreter_helpers

  let parameters_of_list transactions =
    let string = "{ " ^ String.concat " ; " transactions ^ " }" in
    Alpha_context.Script.(lazy_expr (Expr.from_string string))

  let path = project_root // Filename.dirname __FILE__

  (* In this test, we use a contract which takes a list of transactions, applies
     all of them, and assert all of them are correct. It also enforces a 1-to-1
     conversion with mutez by asking an amount to shield and asking for a pkh to
     unshield.
     We create 2 keys a and b. We originate the contract, then do two lists of
     shield for a, then transfers several outputs to b while unshielding, then
     transfer all of b inputs to a while adding dummy inputs and outputs.
     At last, we fail by making a faulty transaction. *)
  let test_shielded_tez () =
    let open Lwt_result_wrap_syntax in
    let* genesis, baker, src0, src1 = init () in
    let memo_size = 8 in
    let* dst, b1, anti_replay =
      originate_contract_hash
        (path // "contracts/sapling_contract.tz")
        "{ }"
        src0
        genesis
        baker
    in
    let wa = wallet_gen () in
    let list_transac, total =
      shield ~memo_size wa.sk 2 wa.vk (Format.sprintf "0x%s") anti_replay
    in
    let parameters = parameters_of_list list_transac in
    (* a does a list of shield transaction *)
    let* b2, _state =
      transac_and_sync ~memo_size b1 parameters total src0 dst baker
    in
    (* we shield again on another block, forging with the empty state *)
    let list_transac, total' =
      shield ~memo_size wa.sk 2 wa.vk (Format.sprintf "0x%s") anti_replay
    in
    let parameters = parameters_of_list list_transac in
    (* a does a list of shield transaction *)
    let* b3, state =
      transac_and_sync ~memo_size b2 parameters total' src0 dst baker
    in
    (* address that will receive an unshield *)
    let* balance_before_shield = Context.Contract.balance (B b3) src1 in
    (* address that will receive an unshield *)
    let wb = wallet_gen () in
    let list_addr = gen_addr 2 wb.vk in
    (* Take the first two inputs *)
    let list_forge_input =
      List.map
        (fun pos_int ->
          let pos = Int64.of_int pos_int in
          let forge_input =
            snd
              (Tezos_sapling.Forge.Input.get state pos wa.vk
              |> WithExceptions.Option.get ~loc:__LOC__)
          in
          forge_input)
        (0 -- 4)
    in
    let list_forge_output =
      List.map
        (fun addr -> Tezos_sapling.Forge.make_output addr 1L (Bytes.create 8))
        list_addr
    in
    let src1_pkh = Context.Contract.pkh src1 in
    let* incr = Incremental.begin_construction b3 in
    let alpha_ctxt = Incremental.alpha_ctxt incr in
    let*@ bound_data, _alpha_ctxt =
      Script_ir_translator.pack_data
        alpha_ctxt
        Script_typed_ir.key_hash_t
        src1_pkh
    in
    let hex_transac =
      to_hex
        (Tezos_sapling.Forge.forge_transaction
           ~number_dummy_inputs:0
           ~number_dummy_outputs:0
           list_forge_input
           list_forge_output
           wa.sk
           anti_replay
           ~bound_data:(Bytes.to_string bound_data)
           state)
        Tezos_sapling.Core.Client.UTXO.transaction_encoding
    in
    let string = Format.sprintf "{0x%s}" hex_transac in
    let parameters =
      Alpha_context.Script.(lazy_expr (Expr.from_string string))
    in
    (* a transfers to b and unshield some money to src_1 (the pkh) *)
    let* b4, state =
      transac_and_sync ~memo_size b3 parameters 0 src0 dst baker
    in
    let* balance_after_shield = Context.Contract.balance (B b4) src1 in
    let diff_due_to_shield =
      Int64.sub
        (Tez_helpers.to_mutez balance_after_shield)
        (Tez_helpers.to_mutez balance_before_shield)
    in
    (* The balance after shield is obtained from the balance before shield by
       the shield specific update. *)
    (* The inputs total [total] mutez and 2 of those are transfered in shielded tez *)
    let* () =
      Assert.equal_int ~loc:__LOC__ (Int64.to_int diff_due_to_shield) (total - 2)
    in
    let list_forge_input =
      List.map
        (fun i ->
          let pos = Int64.of_int (i + 5 + 5) in
          let forge_input =
            snd
              (Tezos_sapling.Forge.Input.get state pos wb.vk
              |> WithExceptions.Option.get ~loc:__LOC__)
          in
          forge_input)
        (0 -- 1)
    in
    let addr_a =
      snd
      @@ Tezos_sapling.Core.Client.Viewing_key.new_address
           wa.vk
           Tezos_sapling.Core.Client.Viewing_key.default_index
    in
    let output = Tezos_sapling.Forge.make_output addr_a 2L (Bytes.create 8) in
    let hex_transac =
      to_hex
        (Tezos_sapling.Forge.forge_transaction
           ~number_dummy_inputs:1
           ~number_dummy_outputs:1
           list_forge_input
           [output]
           wb.sk
           anti_replay
           ~bound_data:""
           state)
        Tezos_sapling.Core.Client.UTXO.transaction_encoding
    in
    let string = Format.sprintf "{0x%s}" hex_transac in
    let parameters =
      Alpha_context.Script.(lazy_expr (Expr.from_string string))
    in
    (* b transfers to a with dummy inputs and outputs *)
    let* b, state =
      transac_and_sync ~memo_size b4 parameters 0 src0 dst baker
    in
    (* Here we fail by doing the same transaction again*)
    let* incr = Incremental.begin_construction b in
    let fee = Tez_helpers.of_int 10 in
    let dst = Alpha_context.Contract.Originated dst in
    let* operation =
      Op.transaction ~gas_limit:Max ~fee (B b) src0 dst Tez.zero ~parameters
    in
    let* (_incr : Incremental.t) =
      Incremental.add_operation (* TODO make more precise *)
        ~expect_apply_failure:(fun _ -> return_unit)
        incr
        operation
    in
    (* Here we fail by changing the field bound_data*)
    let orginal_transac =
      Tezos_sapling.Forge.forge_transaction
        list_forge_input
        [output]
        wb.sk
        anti_replay
        ~bound_data:"right"
        state
    in
    let modified_transac =
      Tezos_sapling.Core.Validator.UTXO.
        {orginal_transac with bound_data = "wrong"}
    in
    let string =
      Format.sprintf
        "{0x%s}"
        (to_hex
           modified_transac
           Tezos_sapling.Core.Client.UTXO.transaction_encoding)
    in
    let parameters =
      Alpha_context.Script.(lazy_expr (Expr.from_string string))
    in
    let* incr = Incremental.begin_construction b in
    let fee = Tez_helpers.of_int 10 in
    let* operation =
      Op.transaction ~gas_limit:Max ~fee (B b) src0 dst Tez.zero ~parameters
    in
    let* (_incr : Incremental.t) =
      Incremental.add_operation (* TODO make more precise *)
        ~expect_apply_failure:(fun _ -> return_unit)
        incr
        operation
    in
    return_unit

  let test_push_sapling_state_should_be_forbidden () =
    let open Lwt_result_syntax in
    let* block, baker, src, _ =
      init ()
      (* Originating a contract to get a sapling_state with ID 0, used in the next contract *)
    in
    let* _, _, _ =
      originate_contract_hash
        (path // "contracts/sapling_contract.tz")
        "{ }"
        src
        block
        baker
    in
    (* Originating the next contract should fail *)
    let*! result =
      originate_contract_hash
        (path // "contracts/sapling_push_sapling_state.tz")
        "{ }"
        src
        block
        baker
    in
    match result with
    | Error
        [
          Environment.Ecoproto_error (Script_tc_errors.Ill_typed_contract _);
          Environment.Ecoproto_error
            (Script_tc_errors.Unexpected_lazy_storage _);
        ] ->
        return_unit
    | _ -> assert false

  let test_use_state_from_other_contract_and_transact () =
    let open Lwt_result_syntax in
    (*
     Attempt to use a sapling state of a contract A in a contract B
     *)
    let* block, baker, src, _ = init () (* Originating the contracts *) in
    let memo_size = 8 in
    (* let* _shielded_pool_contract_address, block, _anti_replay_shielded_pool =
       originate_contract "contracts/sapling_contract.tz" "{ }" src block baker in
    *)
    let* existing_state_contract_address, block, anti_replay_2 =
      originate_contract_hash
        (path // "contracts/sapling_use_existing_state.tz")
        "{ }"
        src
        block
        baker
    in
    (* we create one shielding transaction and transform it in Micheline to use
       it as a parameter
    *)
    let wa = wallet_gen () in
    let transactions, _total =
      shield
        ~memo_size
        wa.sk
        1
        wa.vk
        (Format.sprintf "(Pair 0x%s 0)")
        anti_replay_2
    in
    let transaction =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.hd transactions
    in
    let parameters =
      Alpha_context.Script.(lazy_expr (Expr.from_string transaction))
    in
    let*! result =
      transac_and_sync
        ~memo_size
        block
        parameters
        0
        src
        existing_state_contract_address
        baker
    in
    match result with
    | Ok _ -> Alcotest.failf "Unexpected operations success"
    | Error errs ->
        assert (
          List.exists
            (function
              | Environment.Ecoproto_error
                  (Tezos_protocol_alpha.Protocol.Script_tc_errors
                   .Unexpected_forged_value
                     _) ->
                  true
              | _ -> false)
            errs) ;
        return_unit

  (* In this test we do two transactions in one block and same two in two block.
     We check that the sate is the same expect for roots.
     The second transaction is possible only if the first one is done. *)
  let test_transac_and_block () =
    let open Lwt_result_wrap_syntax in
    let* b, baker, src, _ = init () in
    let memo_size = 8 in
    let* dst, block_start, anti_replay =
      originate_contract_hash
        (path // "contracts/sapling_contract.tz")
        "{ }"
        src
        b
        baker
    in
    let {sk; vk} = wallet_gen () in
    let hex_transac_1 = hex_shield ~memo_size {sk; vk} anti_replay in
    let string_1 = Format.sprintf "{%s}" hex_transac_1 in
    let parameters_1 =
      Alpha_context.Script.(lazy_expr (Expr.from_string string_1))
    in
    let* block_1, state =
      transac_and_sync ~memo_size block_start parameters_1 15 src dst baker
    in
    let intermediary_root = Tezos_sapling.Storage.get_root state in
    let addr =
      snd
      @@ Tezos_sapling.Core.Wallet.Viewing_key.(new_address vk default_index)
    in
    let output = Tezos_sapling.Forge.make_output addr 15L (Bytes.create 8) in
    let hex_transac_2 =
      "0x"
      ^ to_hex
          (Tezos_sapling.Forge.forge_transaction
             [
               snd
                 (Tezos_sapling.Forge.Input.get state 0L vk
                 |> WithExceptions.Option.get ~loc:__LOC__);
             ]
             [output]
             sk
             anti_replay
             ~bound_data:""
             state)
          Tezos_sapling.Core.Client.UTXO.transaction_encoding
    in
    let string_2 = Format.sprintf "{%s}" hex_transac_2 in
    let parameters_2 =
      Alpha_context.Script.(lazy_expr (Expr.from_string string_2))
    in
    let* block_1, state_1 =
      transac_and_sync ~memo_size block_1 parameters_2 0 src dst baker
    in
    let final_root = Tezos_sapling.Storage.get_root state_1 in
    let* _root, diff_1 =
      Plugin.Alpha_services.Contract.single_sapling_get_diff
        Block.rpc_ctxt
        block_1
        dst
        ~offset_commitment:0L
        ~offset_nullifier:0L
        ()
    in
    let fee = Tez_helpers.of_int 10 in
    let*? amount_tez = Tez_helpers.(one_mutez *? Int64.of_int 15) in
    let* operation1 =
      Op.transaction
        ~gas_limit:High
        ~fee
        (B block_start)
        src
        (Contract.Originated dst)
        amount_tez
        ~parameters:parameters_1
    in
    let* incr = Incremental.begin_construction block_start in
    (* We need to manually get the counter here *)
    let ctx = Incremental.alpha_ctxt incr in
    let pkh = Context.Contract.pkh src in
    let*@ counter = Alpha_context.Contract.get_counter ctx pkh in
    let* operation2 =
      Op.transaction
        ~gas_limit:High
        ~counter
        ~fee
        (B block_start)
        src
        (Contract.Originated dst)
        Tez.zero
        ~parameters:parameters_2
    in
    let* operation =
      Op.batch_operations
        ~recompute_counters:true
        ~source:src
        (I incr)
        [operation1; operation2]
    in
    let* incr = Incremental.add_operation incr operation in
    let* block_2 = Incremental.finalize_block incr in
    let* _root, diff_2 =
      Plugin.Alpha_services.Contract.single_sapling_get_diff
        Block.rpc_ctxt
        block_2
        dst
        ~offset_commitment:0L
        ~offset_nullifier:0L
        ()
    in
    (* We check that the same transactions have passed *)
    assert (diff_1 = diff_2) ;
    let is_root_in block dst root =
      let* incr = Incremental.begin_construction block in
      let ctx_2 = Incremental.alpha_ctxt incr in
      let* script =
        Plugin.Alpha_services.Contract.script Block.rpc_ctxt block dst
      in
      let ctx_without_gas_2 = Alpha_context.Gas.set_unlimited ctx_2 in
      let*@ Ex_script (Script script), ctxt =
        Script_ir_translator.parse_script
          ctx_without_gas_2
          ~elab_conf:(Script_ir_translator_config.make ~legacy:true ())
          ~allow_forged_tickets_in_storage:true
          ~allow_forged_lazy_storage_id_in_storage:true
          (Script script)
      in
      let*?@ id, _ctx_2 =
        Script_ir_translator.get_single_sapling_state
          ctxt
          script.storage_type
          script.storage
      in
      let single_id = WithExceptions.Option.get ~loc:__LOC__ id in
      let id =
        Lazy_storage_kind.Sapling_state.Id.parse_z
        @@ Alpha_context.Sapling.Id.unparse_to_z single_id
      in
      let*@ raw_ctx =
        Raw_context.prepare
          block.context
          ~level:block.header.shell.level
          ~predecessor_timestamp:block.header.shell.timestamp
          ~timestamp:block.header.shell.timestamp
          ~all_bakers_attest_first_level:None
      in
      let*@ result = Sapling_storage.Roots.mem raw_ctx id root in
      return result
    in
    (* We check that the second state did not store the root in between
         transactions. *)
    let* () = is_root_in block_2 dst intermediary_root |> assert_false in
    (* We check that the second state did store the final root. *)
    let* () = is_root_in block_2 dst final_root |> assert_true in
    (* We check that the first state did store the final root. *)
    let* () = is_root_in block_1 dst final_root |> assert_true in
    (* We check that the first state did store the root in between transactions. *)
    is_root_in block_1 dst intermediary_root |> assert_true

  (* In this test we try a contract which creates an empty sapling state on the
     fly. It then applies a list of transactions, checks they are correct and
     drops the result. We make several shields in the same list (since the state
     is drop). *)
  let test_drop () =
    let open Lwt_result_syntax in
    let* b, baker, src, _ = init () in
    let* dst, b, anti_replay =
      originate_contract_hash
        (path // "contracts/sapling_contract_drop.tz")
        "Unit"
        src
        b
        baker
    in
    let {sk; vk} = wallet_gen () in
    let list_transac, _total =
      shield ~memo_size:8 sk 4 vk (Format.sprintf "0x%s") anti_replay
    in
    let parameters = parameters_of_list list_transac in
    let dst = Contract.Originated dst in
    let* operation =
      Op.transaction
        ~gas_limit:Max
        ~fee:(Tez_helpers.of_int 10)
        (B b)
        src
        dst
        Tez.zero
        ~parameters
    in
    let* (_b : Block.t) = next_block b operation in
    return_unit

  (* We use a contrac with two states. Its parameter is two transactions and a
     bool. The two transactions are tested valid against the two states, but
     only one state according to the bool is updated.
     We do two transactions shielding to different keys in the two states.
     At each transactions both are applied but only state is updated.
     We then check that the first state is updated in the correct way. *)
  let test_double () =
    let open Lwt_result_wrap_syntax in
    let* b, baker, src, _ = init () in
    let memo_size = 8 in
    let* dst, b, anti_replay =
      originate_contract_hash
        (path // "contracts/sapling_contract_double.tz")
        "(Pair { } { })"
        src
        b
        baker
    in
    let wa = wallet_gen () in
    let hex_transac_1 = hex_shield ~memo_size wa anti_replay in
    let wb = wallet_gen () in
    let hex_transac_2 = hex_shield ~memo_size wb anti_replay in
    let str_1 =
      "(Pair True (Pair " ^ hex_transac_1 ^ " " ^ hex_transac_2 ^ "))"
    in
    let str_2 =
      "(Pair False (Pair " ^ hex_transac_2 ^ " " ^ hex_transac_1 ^ "))"
    in
    (* transac 1 is applied to state_1*)
    let parameters_1 =
      Alpha_context.Script.(lazy_expr (Expr.from_string str_1))
    in
    (* tranasc_2 is applied to state_2*)
    let parameters_2 =
      Alpha_context.Script.(lazy_expr (Expr.from_string str_2))
    in
    let fee = Tez_helpers.of_int 10 in
    let cdst = Contract.Originated dst in
    let* operation =
      Op.transaction
        ~gas_limit:Max
        ~fee
        (B b)
        src
        cdst
        Tez.zero
        ~parameters:parameters_1
    in
    let* b = next_block b operation in
    let* operation =
      Op.transaction
        ~gas_limit:Max
        ~fee
        (B b)
        src
        cdst
        Tez.zero
        ~parameters:parameters_2
    in
    let* b = next_block b operation in
    let* incr = Incremental.begin_construction b in
    let ctx = Incremental.alpha_ctxt incr in
    let ctx_without_gas = Alpha_context.Gas.set_unlimited ctx in
    let* storage =
      Plugin.Alpha_services.Contract.storage Block.rpc_ctxt b dst
    in
    let storage_lazy_expr = Alpha_context.Script.lazy_expr storage in
    let*?@ (Ty_ex_c tytype) =
      let memo_size = memo_size_of_int memo_size in
      let open Script_typed_ir in
      let state_ty = sapling_state_t ~memo_size in
      pair_t (-1) state_ty state_ty
    in
    let*@ (state_1, state_2), _ctx =
      Script_ir_translator.parse_storage
        ctx_without_gas
        ~elab_conf:(Script_ir_translator_config.make ~legacy:true ())
        ~allow_forged_tickets:true
        ~allow_forged_lazy_storage_id:true
        tytype
        ~storage:storage_lazy_expr
    in
    (*Only works when diff is empty*)
    let local_state_from_disk disk_state ctx =
      let id =
        Alpha_context.Sapling.(disk_state.id)
        |> WithExceptions.Option.get ~loc:__LOC__
      in
      let+@ diff =
        Alpha_context.Sapling.get_diff
          ctx
          id
          ~offset_commitment:0L
          ~offset_nullifier:0L
          ()
      in
      client_state_of_diff ~memo_size diff
    in
    let* state_1 = local_state_from_disk state_1 ctx in
    let+ state_2 = local_state_from_disk state_2 ctx in
    (* we check that first state contains 15 to addr_1 but not 15 to addr_2*)
    assert (Option.is_some @@ Tezos_sapling.Forge.Input.get state_1 0L wa.vk) ;
    assert (Option.is_some @@ Tezos_sapling.Forge.Input.get state_2 0L wa.vk) ;
    assert (Option.is_none @@ Tezos_sapling.Forge.Input.get state_1 0L wb.vk) ;
    assert (Option.is_none @@ Tezos_sapling.Forge.Input.get state_2 0L wb.vk)

  let test_state_as_arg () =
    let open Lwt_result_syntax in
    let* b, baker, src, _ = init () in
    let* dst, b, anti_replay =
      originate_contract_hash
        (path // "contracts/sapling_contract_state_as_arg.tz")
        "None"
        src
        b
        baker
    in
    let* dst_2, b, anti_replay_2 =
      originate_contract_hash
        (path // "contracts/sapling_contract_send.tz")
        "Unit"
        src
        b
        baker
    in
    let w = wallet_gen () in
    let hex_transac_1 = hex_shield ~memo_size:8 w anti_replay in
    let string = "Left " ^ hex_transac_1 in
    let parameters =
      Alpha_context.Script.(lazy_expr (Expr.from_string string))
    in
    let fee = Tez_helpers.of_int 10 in
    let dst = Contract.Originated dst in
    let* operation =
      Op.transaction ~gas_limit:Max ~fee (B b) src dst Tez.zero ~parameters
    in
    let* b = next_block b operation in
    let contract = "0x" ^ to_hex dst Alpha_context.Contract.encoding in
    let hex_transac_2 = hex_shield ~memo_size:8 w anti_replay_2 in
    let string = "(Pair " ^ contract ^ " " ^ hex_transac_2 ^ ")" in
    let parameters =
      Alpha_context.Script.(lazy_expr (Expr.from_string string))
    in
    let dst_2 = Contract.Originated dst_2 in
    let* operation =
      Op.transaction ~gas_limit:Max ~fee (B b) src dst_2 Tez.zero ~parameters
    in
    let* (_b : Block.t) = next_block b operation in
    return_unit
end

let tests =
  [
    Tztest.tztest
      "commitments_add_uncommitted"
      `Quick
      Raw_context_tests.commitments_add_uncommitted;
    Tztest.tztest "nullifier_double" `Quick Raw_context_tests.nullifier_double;
    Tztest.tztest "nullifier_test" `Quick Raw_context_tests.nullifier_test;
    Tztest.tztest "cm_cipher_test" `Quick Raw_context_tests.cm_cipher_test;
    Tztest.tztest
      "list_insertion_test"
      `Quick
      Raw_context_tests.list_insertion_test;
    Tztest.tztest "root" `Quick Raw_context_tests.root_test;
    Tztest.tztest "get_memo_size" `Quick Raw_context_tests.test_get_memo_size;
    Tztest.tztest "verify_memo" `Quick Alpha_context_tests.test_verify_memo;
    Tztest.tztest "bench_phases" `Slow Alpha_context_tests.test_bench_phases;
    Tztest.tztest
      "bench_phases_legacy"
      `Slow
      Alpha_context_tests.test_bench_phases_legacy;
    Tztest.tztest
      "bench_fold_over_same_token"
      `Slow
      Alpha_context_tests.test_bench_fold_over_same_token;
    Tztest.tztest
      "double_spend_same_input"
      `Quick
      Alpha_context_tests.test_double_spend_same_input;
    Tztest.tztest
      "verifyupdate_one_transaction"
      `Quick
      Alpha_context_tests.test_verifyupdate_one_transaction;
    Tztest.tztest
      "verifyupdate_two_transactions"
      `Quick
      Alpha_context_tests.test_verifyupdate_two_transactions;
    Tztest.tztest "shielded_tez" `Quick Interpreter_tests.test_shielded_tez;
    Tztest.tztest
      "use state from other contract and transact"
      `Quick
      Interpreter_tests.test_use_state_from_other_contract_and_transact;
    Tztest.tztest
      "Instruction PUSH sapling_state 0 should be forbidden"
      `Quick
      Interpreter_tests.test_push_sapling_state_should_be_forbidden;
    Tztest.tztest
      "transac_and_block"
      `Quick
      Interpreter_tests.test_transac_and_block;
    Tztest.tztest "drop" `Quick Interpreter_tests.test_drop;
    Tztest.tztest "double" `Quick Interpreter_tests.test_double;
    Tztest.tztest "state_as_arg" `Quick Interpreter_tests.test_state_as_arg;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("sapling", tests)] |> Lwt_main.run
