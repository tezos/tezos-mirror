(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Double baking evidence operation may happen when a baker
    baked two different blocks on the same level. *)

open Protocol
open Alpha_context

(****************************************************************)
(*                  Utility functions                           *)
(****************************************************************)

let get_first_different_baker baker bakers =
  return
  @@ List.find
       (fun baker' -> Signature.Public_key_hash.( <> ) baker baker')
       bakers

let get_first_different_bakers ctxt =
  Context.get_bakers ctxt
  >>=? fun bakers ->
  let baker_1 = List.hd bakers in
  get_first_different_baker baker_1 (List.tl bakers)
  >|=? fun baker_2 -> (baker_1, baker_2)

let get_first_different_endorsers ctxt =
  Context.get_endorsers ctxt
  >|=? fun endorsers ->
  let endorser_1 = (List.hd endorsers).delegate in
  let endorser_2 = (List.hd (List.tl endorsers)).delegate in
  (endorser_1, endorser_2)

(** Bake two block at the same level using the same policy (i.e. same
    baker) *)
let block_fork ?policy contracts b =
  let (contract_a, contract_b) =
    (List.hd contracts, List.hd (List.tl contracts))
  in
  Op.transaction (B b) contract_a contract_b Alpha_context.Tez.one_cent
  >>=? fun operation ->
  Block.bake ?policy ~operation b
  >>=? fun blk_a -> Block.bake ?policy b >|=? fun blk_b -> (blk_a, blk_b)

(****************************************************************)
(*                        Tests                                 *)
(****************************************************************)

(** Simple scenario where two blocks are baked by a same baker and
    exposed by a double baking evidence operation *)
let valid_double_baking_evidence () =
  Context.init 2
  >>=? fun (b, contracts) ->
  Context.get_bakers (B b)
  >>=? fun bakers ->
  let priority_0_baker = List.hd bakers in
  block_fork ~policy:(By_priority 0) contracts b
  >>=? fun (blk_a, blk_b) ->
  Op.double_baking (B blk_a) blk_a.header blk_b.header
  >>=? fun operation ->
  Block.bake ~policy:(Excluding [priority_0_baker]) ~operation blk_a
  >>=? fun blk ->
  (* Check that the frozen deposit, the fees and rewards are removed *)
  iter_s
    (fun kind ->
      let contract =
        Alpha_context.Contract.implicit_contract priority_0_baker
      in
      Assert.balance_is ~loc:__LOC__ (B blk) contract ~kind Tez.zero)
    [Deposit; Fees; Rewards]

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Check that a double baking operation fails if it exposes the same two blocks *)
let same_blocks () =
  Context.init 2
  >>=? fun (b, _contracts) ->
  Block.bake b
  >>=? fun ba ->
  Op.double_baking (B ba) ba.header ba.header
  >>=? fun operation ->
  Block.bake ~operation ba
  >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Cheating_proofs.Invalid_double_baking_evidence _ ->
          true
      | _ ->
          false)
  >>=? fun () -> return_unit

(** Check that a double baking operation exposing two blocks with
    different levels fails *)
let different_levels () =
  Context.init 2
  >>=? fun (b, contracts) ->
  block_fork ~policy:(By_priority 0) contracts b
  >>=? fun (blk_a, blk_b) ->
  Block.bake blk_b
  >>=? fun blk_b_2 ->
  Op.double_baking (B blk_a) blk_a.header blk_b_2.header
  >>=? fun operation ->
  Block.bake ~operation blk_a
  >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Cheating_proofs.Invalid_double_baking_evidence _ ->
          true
      | _ ->
          false)

(** Check that a double baking operation exposing two yet to be baked
    blocks fails *)
let too_early_double_baking_evidence () =
  Context.init 2
  >>=? fun (b, contracts) ->
  block_fork ~policy:(By_priority 0) contracts b
  >>=? fun (blk_a, blk_b) ->
  Op.double_baking (B b) blk_a.header blk_b.header
  >>=? fun operation ->
  Block.bake ~operation b
  >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Cheating_proofs.Too_early_evidence _ ->
          true
      | _ ->
          false)

(** Check that after [preserved_cycles + 1], it is not possible to
    create a double baking operation anymore *)
let too_late_double_baking_evidence () =
  Context.init 2
  >>=? fun (b, contracts) ->
  Context.get_constants (B b)
  >>=? fun Constants.{parametric = {preserved_cycles; _}; _} ->
  block_fork ~policy:(By_priority 0) contracts b
  >>=? fun (blk_a, blk_b) ->
  Block.bake_until_n_cycle_end (preserved_cycles + 1) blk_a
  >>=? fun blk ->
  Op.double_baking (B blk) blk_a.header blk_b.header
  >>=? fun operation ->
  Block.bake ~operation blk
  >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Cheating_proofs.Outdated_evidence _ ->
          true
      | _ ->
          false)

(** Check that an invalid double baking evidence that exposes two block
    baking with same level made by different bakers fails *)
let different_delegates () =
  Context.init 2
  >>=? fun (b, _) ->
  get_first_different_bakers (B b)
  >>=? fun (baker_1, baker_2) ->
  Block.bake ~policy:(By_account baker_1) b
  >>=? fun blk_a ->
  Block.bake ~policy:(By_account baker_2) b
  >>=? fun blk_b ->
  Op.double_baking (B blk_a) blk_a.header blk_b.header
  >>=? fun operation ->
  Block.bake ~operation blk_a
  >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Cheating_proofs.Inconsistent_evidence _ ->
          true
      | _ ->
          false)

let wrong_signer () =
  (* Baker_2 bakes a block but baker signs it. *)
  let header_custom_signer baker baker_2 b =
    Block.Forge.forge_header ~policy:(By_account baker_2) b
    >>=? fun header ->
    Block.Forge.set_baker baker header |> Block.Forge.sign_header
  in
  Context.init 2
  >>=? fun (b, _) ->
  get_first_different_bakers (B b)
  >>=? fun (baker_1, baker_2) ->
  Block.bake ~policy:(By_account baker_1) b
  >>=? fun blk_a ->
  header_custom_signer baker_1 baker_2 b
  >>=? fun header_b ->
  Op.double_baking (B blk_a) blk_a.header header_b
  >>=? fun operation ->
  Block.bake ~operation blk_a
  >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Baking.Invalid_block_signature _ ->
          true
      | _ ->
          false)

(** Detect when an evidence is injected twice in two different cycles *)
let double_injection_double_baking_evidence () =
  Context.init 2
  >>=? fun (blk, contracts) ->
  Context.get_bakers (B blk)
  >>=? fun bakers ->
  let baker = List.hd bakers in
  block_fork ~policy:(By_account baker) contracts blk
  >>=? fun (blk_a, blk_b) ->
  Block.bake_until_cycle_end blk_a
  >>=? fun blk ->
  Op.double_baking (B blk) blk_a.header blk_b.header
  >>=? fun evidence ->
  Block.bake ~policy:(Excluding [baker]) ~operation:evidence blk
  >>=? fun blk ->
  Block.bake_until_cycle_end blk
  >>=? fun blk ->
  Op.double_baking (B blk) blk_b.header blk_a.header
  >>=? fun evidence ->
  Block.bake ~policy:(Excluding [baker]) ~operation:evidence blk
  >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Apply.Double_injection_of_evidence ->
          true
      | _ ->
          false)

(** Previously unrequired evidence can be re-injected and slash the baker *)
let unrequired_evidence_injected () =
  Context.init 2
  >>=? fun (blk, contracts) ->
  Context.get_bakers (B blk)
  >>=? fun bakers ->
  let baker = List.hd bakers in
  block_fork ~policy:(By_account baker) contracts blk
  >>=? fun (blk_a, blk_b) ->
  Op.double_baking (B blk_a) blk_a.header blk_b.header
  >>=? fun evidence ->
  Block.bake blk_a
  >>=? fun blk ->
  block_fork ~policy:(By_account baker) contracts blk
  >>=? fun (blk_a, blk_b) ->
  Op.double_baking (B blk_a) blk_a.header blk_b.header
  >>=? fun evidence_2 ->
  Block.bake ~policy:(Excluding [baker]) ~operation:evidence blk_a
  >>=? fun blk ->
  Block.bake ~policy:(Excluding [baker]) ~operation:evidence_2 blk
  >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Apply.Unrequired_evidence ->
          true
      | _ ->
          false)
  >>=? fun () ->
  Block.bake ~policy:(By_account baker) blk
  >>=? fun blk ->
  Block.bake ~policy:(Excluding [baker]) ~operation:evidence_2 blk
  >>=? fun _blk -> return_unit

let assert_proof_exists ~loc ?(exists = true) cheat_level delegate blk =
  Context.Delegate.info (B blk) delegate
  >>=? fun delegate_info ->
  let proof_exists =
    Raw_level.LSet.mem cheat_level delegate_info.proof_levels
  in
  Assert.equal_bool ~loc proof_exists exists

(** Outdated proof is deleted from storage *)
let outdated_proof_has_been_cleaned () =
  Context.init 2
  >>=? fun (blk, contracts) ->
  Context.get_constants (B blk)
  >>=? fun Constants.{parametric = {preserved_cycles; blocks_per_cycle; _}; _} ->
  Context.get_bakers (B blk)
  >>=? fun bakers ->
  let baker = List.hd bakers in
  block_fork ~policy:(By_account baker) contracts blk
  >>=? fun (blk_a, blk_b) ->
  Context.get_level (B blk_a)
  >>=? fun cheat_level ->
  Op.double_baking (B blk_a) blk_a.header blk_b.header
  >>=? fun evidence ->
  Block.bake_until_n_cycle_end preserved_cycles blk_a
  >>=? fun blk ->
  Block.bake_n (Int32.to_int blocks_per_cycle - 2) blk
  >>=? fun blk ->
  Block.bake ~policy:(Excluding [baker]) ~operation:evidence blk
  >>=? fun blk ->
  assert_proof_exists ~loc:__LOC__ cheat_level baker blk
  >>=? fun () ->
  Block.bake blk
  >>=? fun blk ->
  Block.bake ~operation:evidence blk
  >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Cheating_proofs.Outdated_evidence _ ->
          true
      | _ ->
          false)
  >>=? fun () ->
  (* proof storage is cleaned at the end of cycles *)
  Block.bake_n (Int32.to_int blocks_per_cycle - 2) blk
  >>=? fun blk ->
  assert_proof_exists ~loc:__LOC__ cheat_level baker blk
  >>=? fun () ->
  Block.bake blk
  >>=? fun blk ->
  assert_proof_exists ~loc:__LOC__ ~exists:false cheat_level baker blk

let tests =
  [ Test.tztest
      "valid double baking evidence"
      `Quick
      valid_double_baking_evidence;
    (* Should fail*)
    Test.tztest "same blocks" `Quick same_blocks;
    Test.tztest "different levels" `Quick different_levels;
    Test.tztest
      "too early double baking evidence"
      `Quick
      too_early_double_baking_evidence;
    Test.tztest
      "too late double baking evidence"
      `Quick
      too_late_double_baking_evidence;
    Test.tztest "different delegates" `Quick different_delegates;
    Test.tztest "wrong delegate" `Quick wrong_signer;
    Test.tztest
      "reject double injection of an evidence"
      `Quick
      double_injection_double_baking_evidence;
    Test.tztest
      "inject previously unrequired evidence"
      `Quick
      unrequired_evidence_injected;
    Test.tztest
      "outdated proof has been cleaned from storage"
      `Quick
      outdated_proof_has_been_cleaned
    (* Test.tztest
     *   "evidence is valid until last block of preserved_cycle "
     *   `Quick
     *   evidence_is_valid_or_too_old *) ]
