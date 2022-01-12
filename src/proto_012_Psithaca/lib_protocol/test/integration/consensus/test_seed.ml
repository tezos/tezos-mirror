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

(** Testing
    -------
    Component:    Protocol (seed)
    Invocation:   dune exec \
                  src/proto_012_Psithaca/lib_protocol/test/integration/consensus/main.exe \
                  -- test "^seed$"
    Subject:      - seed_nonce_hash included in some blocks
                  - revelation operation of seed_nonce that should correspond
                  to each seed_nonce_hash
*)

open Protocol

(** Baking [blocks_per_commitment] blocks without a [seed_nonce_hash]
    commitment fails with [Invalid_commitment]. *)
let test_no_commitment () =
  Context.init ~consensus_threshold:0 5 >>=? fun (b, _) ->
  Context.get_constants (B b)
  >>=? fun {parametric = {blocks_per_commitment; _}; _} ->
  let blocks_per_commitment = Int32.to_int blocks_per_commitment in
  (* Bake normally until before the commitment *)
  Block.bake_n (blocks_per_commitment - 2) b >>=? fun b ->
  (* Forge a block with empty commitment and apply it *)
  Block.Forge.forge_header b >>=? fun header ->
  Block.Forge.set_seed_nonce_hash None header |> Block.Forge.sign_header
  >>=? fun header ->
  Block.apply header b >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Alpha_context.Block_header.Invalid_commitment _ -> true
      | _ -> false)

(** Choose a baker, denote it by id. In the first cycle, make id bake only once.
    Check that:
    - when id reveals the nonce too early, there's an error
    - when id reveals at the right time but the wrong value, there's an error
    - when another baker reveals correctly, it receives the tip
    - revealing twice produces an error *)
let test_revelation_early_wrong_right_twice () =
  let open Assert in
  Context.init ~consensus_threshold:0 5 >>=? fun (b, _) ->
  Context.get_constants (B b) >>=? fun csts ->
  let tip = csts.parametric.seed_nonce_revelation_tip in
  let blocks_per_commitment =
    Int32.to_int csts.parametric.blocks_per_commitment
  in
  let baking_reward_fixed_portion =
    csts.parametric.baking_reward_fixed_portion
  in
  (* get the pkh of a baker *)
  Block.get_next_baker b >>=? fun (pkh, _, _) ->
  let id = Alpha_context.Contract.implicit_contract pkh in
  let policy = Block.Excluding [pkh] in
  (* bake until commitment - 2, excluding id *)
  Block.bake_n ~policy (blocks_per_commitment - 2) b >>=? fun b ->
  Context.Contract.balance (B b) id >>=? fun bal_main ->
  (* the baker [id] will include a seed_nonce commitment *)
  Block.bake ~policy:(Block.By_account pkh) b >>=? fun b ->
  Context.get_level (B b) >>?= fun level_commitment ->
  Context.get_seed_nonce_hash (B b) >>=? fun committed_hash ->
  (* test that the baking reward is received *)
  balance_was_credited
    ~loc:__LOC__
    (B b)
    id
    bal_main
    baking_reward_fixed_portion
  >>=? fun () ->
  (* test that revealing too early produces an error *)
  Op.seed_nonce_revelation
    (B b)
    level_commitment
    (WithExceptions.Option.to_exn ~none:Not_found @@ Nonce.get committed_hash)
  |> fun operation ->
  Block.bake ~policy ~operation b >>= fun e ->
  let expected = function
    | Nonce_storage.Too_early_revelation -> true
    | _ -> false
  in
  Assert.proto_error ~loc:__LOC__ e expected >>=? fun () ->
  (* finish the cycle excluding the committing baker, id *)
  Block.bake_until_cycle_end ~policy b >>=? fun b ->
  (* test that revealing at the right time but the wrong value
     produces an error *)
  let (wrong_hash, _) = Nonce.generate () in
  Op.seed_nonce_revelation
    (B b)
    level_commitment
    (WithExceptions.Option.to_exn ~none:Not_found @@ Nonce.get wrong_hash)
  |> fun operation ->
  Block.bake ~operation b >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Nonce_storage.Inconsistent_nonce -> true
      | _ -> false)
  >>=? fun () ->
  (* reveals correctly *)
  Op.seed_nonce_revelation
    (B b)
    level_commitment
    (WithExceptions.Option.to_exn ~none:Not_found @@ Nonce.get committed_hash)
  |> fun operation ->
  Block.get_next_baker ~policy b >>=? fun (baker_pkh, _, _) ->
  let baker = Alpha_context.Contract.implicit_contract baker_pkh in
  Context.Contract.balance (B b) baker >>=? fun baker_bal ->
  Block.bake ~policy:(Block.By_account baker_pkh) ~operation b >>=? fun b ->
  (* test that the baker gets the tip reward plus the baking reward*)
  balance_was_credited
    ~loc:__LOC__
    (B b)
    baker
    baker_bal
    Test_tez.(tip +! baking_reward_fixed_portion)
  >>=? fun () ->
  (* test that revealing twice produces an error *)
  Op.seed_nonce_revelation
    (B b)
    level_commitment
    (WithExceptions.Option.to_exn ~none:Not_found @@ Nonce.get wrong_hash)
  |> fun operation ->
  Block.bake ~operation ~policy b >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Nonce_storage.Previously_revealed_nonce -> true
      | _ -> false)

(** Test that revealing too late produces an error. Note that a
    committer who doesn't reveal at cycle 1 is not punished.*)
let test_revelation_missing_and_late () =
  let open Context in
  let open Assert in
  Context.init ~consensus_threshold:0 5 >>=? fun (b, _) ->
  get_constants (B b) >>=? fun csts ->
  let blocks_per_commitment =
    Int32.to_int csts.parametric.blocks_per_commitment
  in
  (* bake until commitment *)
  Block.bake_n (blocks_per_commitment - 2) b >>=? fun b ->
  (* the next baker [id] will include a seed_nonce commitment *)
  Block.get_next_baker b >>=? fun (pkh, _, _) ->
  Block.bake b >>=? fun b ->
  Context.get_level (B b) >>?= fun level_commitment ->
  Context.get_seed_nonce_hash (B b) >>=? fun committed_hash ->
  (* finish cycle 0 excluding the committing baker [id] *)
  let policy = Block.Excluding [pkh] in
  Block.bake_until_cycle_end ~policy b >>=? fun b ->
  (* finish cycle 1 excluding the committing baker [id] *)
  Block.bake_until_cycle_end ~policy b >>=? fun b ->
  (* test that revealing too late (after cycle 1) produces an error *)
  Op.seed_nonce_revelation
    (B b)
    level_commitment
    (WithExceptions.Option.to_exn ~none:Not_found @@ Nonce.get committed_hash)
  |> fun operation ->
  Block.bake ~operation b >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Nonce_storage.Too_late_revelation -> true
      | _ -> false)

let wrap e = e >|= Environment.wrap_tzresult

(** Test that we do not distribute endorsing rewards if the nonce was
    not revealed. *)
let test_unrevealed () =
  let open Alpha_context in
  let constants =
    {
      Default_parameters.constants_test with
      endorsing_reward_per_slot = Tez.one_mutez;
      baking_reward_bonus_per_slot = Tez.zero;
      baking_reward_fixed_portion = Tez.zero;
      seed_nonce_revelation_tip = Tez.zero;
      consensus_threshold = 0;
      minimal_participation_ratio = Constants.{numerator = 0; denominator = 1};
    }
  in
  Context.init_with_constants constants 2 >>=? fun (b, accounts) ->
  let (account1, account2) =
    match accounts with a1 :: a2 :: _ -> (a1, a2) | _ -> assert false
  in
  let (_delegate1, delegate2) =
    match (Contract.is_implicit account1, Contract.is_implicit account2) with
    | (Some d, Some d') -> (d, d')
    | _ -> assert false
  in
  (* Delegate 2 will add a nonce but never reveals it *)
  Context.get_constants (B b) >>=? fun csts ->
  let blocks_per_commitment =
    Int32.to_int csts.parametric.blocks_per_commitment
  in
  let bake_and_endorse_block ?policy (pred_b, b) =
    Context.get_endorsers (B b) >>=? fun slots ->
    List.map_es
      (fun {Plugin.RPC.Validators.delegate; slots; _} ->
        Op.endorsement
          ~delegate:(delegate, slots)
          ~endorsed_block:b
          (B pred_b)
          ()
        >>=? fun op -> Operation.pack op |> return)
      slots
    >>=? fun endorsements -> Block.bake ?policy ~operations:endorsements b
  in
  (* Bake until commitment *)
  Block.bake_n (blocks_per_commitment - 2) b >>=? fun b ->
  (* Baker delegate 2 will include a seed_nonce commitment *)
  let policy = Block.By_account delegate2 in
  Block.bake_until_cycle_end ~policy b >>=? fun b ->
  Context.Delegate.info (B b) delegate2 >>=? fun info_before ->
  Block.bake ~policy b >>=? fun b' ->
  bake_and_endorse_block ~policy (b, b') >>=? fun b ->
  (* Finish cycle 1 excluding the first baker *)
  Block.bake_until_cycle_end ~policy b >>=? fun b ->
  Context.Delegate.info (B b) delegate2 >>=? fun info_after ->
  (* Assert that we did not received a reward because we didn't
     reveal the nonce. *)
  Assert.equal_tez ~loc:__LOC__ info_before.full_balance info_after.full_balance
  >>=? fun () -> return_unit

let tests =
  [
    Tztest.tztest "no commitment" `Quick test_no_commitment;
    Tztest.tztest
      "revelation_early_wrong_right_twice"
      `Quick
      test_revelation_early_wrong_right_twice;
    Tztest.tztest
      "revelation_missing_and_late"
      `Quick
      test_revelation_missing_and_late;
    Tztest.tztest "test unrevealed" `Quick test_unrevealed;
  ]
