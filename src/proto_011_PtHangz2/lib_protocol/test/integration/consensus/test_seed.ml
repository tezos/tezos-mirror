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
    Invocation:   dune exec src/proto_011_PtHangz2/lib_protocol/test/integration/consensus/main.exe -- test "^seed$"
    Subject:      - seed_nonce_hash included in some blocks
                  - revelation operation of seed_nonce that should correspond
                  to each seed_nonce_hash
*)

open Protocol
open Test_tez

(** Baking [blocks_per_commitment] blocks without a [seed_nonce_hash]
    commitment fails with [Invalid_commitment]. *)
let test_no_commitment () =
  Context.init 5 >>=? fun (b, _) ->
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
      | Apply.Invalid_commitment _ -> true
      | _ -> false)

let baking_reward ctxt (b : Block.t) =
  let priority = b.header.protocol_data.contents.priority in
  Block.get_endorsing_power b >>=? fun endorsing_power ->
  Context.get_baking_reward ctxt ~priority ~endorsing_power

(** Choose a baker, denote it by id. In the first cycle, make id bake only once.
    Check that:
    - after id bakes with a commitment the bond is frozen and the reward
      allocated
    - when id reveals the nonce too early, there's an error
    - when id reveals at the right time but the wrong value, there's an error
    - when another baker reveals correctly, it receives the tip
    - revealing twice produces an error
    - after [preserved cycles] a committer that correctly revealed
      receives back the bond and the reward. *)
let test_revelation_early_wrong_right_twice () =
  let open Assert in
  Context.init 5 >>=? fun (b, _) ->
  Context.get_constants (B b) >>=? fun csts ->
  let bond = csts.parametric.block_security_deposit in
  let tip = csts.parametric.seed_nonce_revelation_tip in
  let blocks_per_commitment =
    Int32.to_int csts.parametric.blocks_per_commitment
  in
  let preserved_cycles = csts.parametric.preserved_cycles in
  (* get the pkh of a baker *)
  Block.get_next_baker b >>=? fun (pkh, _, _) ->
  let id = Alpha_context.Contract.implicit_contract pkh in
  let policy = Block.Excluding [pkh] in
  (* bake until commitment, excluding id *)
  Block.bake_n ~policy (blocks_per_commitment - 2) b >>=? fun b ->
  Context.Contract.balance ~kind:Main (B b) id >>=? fun bal_main ->
  Context.Contract.balance ~kind:Deposit (B b) id >>=? fun bal_deposit ->
  Context.Contract.balance ~kind:Rewards (B b) id >>=? fun bal_rewards ->
  (* the baker [id] will include a seed_nonce commitment *)
  Block.bake ~policy:(Block.By_account pkh) b >>=? fun b ->
  Context.get_level (B b) >>?= fun level_commitment ->
  Context.get_seed_nonce_hash (B b) >>=? fun committed_hash ->
  baking_reward (B b) b >>=? fun reward ->
  (* test that the bond was frozen and the reward allocated *)
  balance_was_debited ~loc:__LOC__ (B b) id bal_main bond >>=? fun () ->
  balance_was_credited ~loc:__LOC__ (B b) id ~kind:Deposit bal_deposit bond
  >>=? fun () ->
  balance_was_credited ~loc:__LOC__ (B b) id ~kind:Rewards bal_rewards reward
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
  (* test that revealing at the right time but the wrong value produces an error *)
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
  Context.Contract.balance ~kind:Main (B b) baker >>=? fun baker_bal_main ->
  Context.Contract.balance ~kind:Deposit (B b) baker
  >>=? fun baker_bal_deposit ->
  Context.Contract.balance ~kind:Rewards (B b) baker
  >>=? fun baker_bal_rewards ->
  (* bake the operation in a block *)
  Block.bake ~policy ~operation b >>=? fun b ->
  baking_reward (B b) b >>=? fun baker_reward ->
  (* test that the baker gets the tip reward *)
  balance_was_debited ~loc:__LOC__ (B b) baker ~kind:Main baker_bal_main bond
  >>=? fun () ->
  balance_was_credited
    ~loc:__LOC__
    (B b)
    baker
    ~kind:Deposit
    baker_bal_deposit
    bond
  >>=? fun () ->
  Tez.( +? ) baker_reward tip >>?= fun expected_rewards ->
  balance_was_credited
    ~loc:__LOC__
    (B b)
    baker
    ~kind:Rewards
    baker_bal_rewards
    expected_rewards
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
  >>=? fun () ->
  (* bake [preserved_cycles] cycles excluding [id] *)
  List.fold_left_es
    (fun b _ -> Block.bake_until_cycle_end ~policy b)
    b
    (1 -- preserved_cycles)
  >>=? fun b ->
  (* test that [id] receives back the bond and the reward *)
  (* note that in order to have that new_bal = bal_main + reward,
     id can only bake once; this is why we exclude id from all other bake ops. *)
  balance_was_credited ~loc:__LOC__ (B b) id ~kind:Main bal_main reward
  >>=? fun () ->
  balance_is ~loc:__LOC__ (B b) id ~kind:Deposit Tez.zero >>=? fun () ->
  balance_is ~loc:__LOC__ (B b) id ~kind:Rewards Tez.zero

(** - a committer at cycle 0, which doesn't reveal at cycle 1,
      at the end of the cycle 1 looses the fees and the reward
    - revealing too late produces an error.

    The parameters allow to consider different scenarios:
    - when [with_fees] is [true] an operation is included to generate non null
      fees for the baker before it commits to a nonce hash
    - when [with_rewards] is [true] an operation is included to generate a non
      null reward for the baker before it commits to a nonce hash
    - when [double_step] is true, operations are also included after the baker
      commits to a nonce hash (depending on parameters [with_fees] and
      [with_rewards]) so that the baker's fees and reward exceed the baker's
      commitments. *)
let test_revelation_missing_and_late ~with_fees ~with_rewards ~double_step () =
  let open Context in
  let open Assert in
  Context.init 5 >>=? fun (b, accounts) ->
  get_constants (B b) >>=? fun csts ->
  let blocks_per_commitment =
    Int32.to_int csts.parametric.blocks_per_commitment
  in
  (* bake until commitment *)
  Block.bake_n (blocks_per_commitment - 2) b >>=? fun b ->
  let fee = Tez.of_int 7 in
  let (contract1, contract2) =
    match accounts with c1 :: c2 :: _ -> (c1, c2) | _ -> assert false
  in
  (if with_fees then
   (* Include a transaction so that some fees are allocated to the baker. *)
   Op.transaction
     ~gas_limit:(Alpha_context.Gas.Arith.integral_of_int_exn 3000)
     (B b)
     ~fee
     contract1
     contract2
     Tez.one
   >|=? fun op1 -> [op1]
  else return [])
  >>=? fun operations ->
  Context.get_endorser (B b) >>=? fun (endorser, slots) ->
  (if with_rewards then
   (* Include an endorsement so that some reward is allocated to the baker. *)
   Op.endorsement_with_slot ~delegate:(endorser, slots) (B b) () >|=? fun op2 ->
   operations @ [Alpha_context.Operation.pack op2]
  else return operations)
  >>=? fun operations ->
  (* the next baker [id] will include a seed_nonce commitment *)
  Block.get_next_baker ~policy:(Block.Excluding [endorser]) b
  >>=? fun (pkh, _, _) ->
  let id = Alpha_context.Contract.implicit_contract pkh in
  (* Ensure next baker has no rewards and fees yet. *)
  Context.Contract.balance ~kind:Rewards (B b) id >>=? fun bal_rewards ->
  Assert.equal_tez ~loc:__LOC__ bal_rewards Tez.zero >>=? fun () ->
  Context.Contract.balance ~kind:Fees (B b) id >>=? fun bal_fees ->
  Assert.equal_tez ~loc:__LOC__ bal_fees Tez.zero >>=? fun () ->
  (* Bake and include seed nonce commitment *)
  Block.bake ~policy:(Block.Excluding [endorser]) ~operations b >>=? fun b ->
  (* Extract committed rewards and fees and ensure they are not null. *)
  Context.Contract.balance ~kind:Fees (B b) id >>=? fun committed_fees ->
  (if with_fees then Assert.not_equal_tez ~loc:__LOC__ committed_fees Tez.zero
  else Assert.equal_tez ~loc:__LOC__ committed_fees Tez.zero)
  >>=? fun () ->
  Context.Contract.balance ~kind:Rewards (B b) id >>=? fun committed_rewards ->
  (if with_rewards then
   Assert.not_equal_tez ~loc:__LOC__ committed_rewards Tez.zero
  else Assert.equal_tez ~loc:__LOC__ committed_rewards Tez.zero)
  >>=? fun () ->
  (* Extract commitment cycle, commitment level and commitment hash. *)
  Block.current_cycle b >>=? fun commitment_cycle ->
  Context.get_level (B b) >>?= fun level_commitment ->
  Context.get_seed_nonce_hash (B b) >>=? fun committed_hash ->
  (* Add more operations so that rewards and/or fees are more than what
     has already been committed. *)
  (if double_step then
   (if with_fees then
    (* Include a transaction so that some fees are allocated to the baker. *)
    Op.transaction
      ~gas_limit:(Alpha_context.Gas.Arith.integral_of_int_exn 3000)
      (B b)
      ~fee
      contract1
      contract2
      Tez.one
    >|=? fun op1 -> [op1]
   else return [])
   >>=? fun operations ->
   Context.get_endorser (B b) >>=? fun (endorser, slots) ->
   (if with_rewards then
    (* Include an endorsement so that some reward is allocated to the baker. *)
    Op.endorsement_with_slot ~delegate:(endorser, slots) (B b) ()
    >|=? fun op2 -> operations @ [Alpha_context.Operation.pack op2]
   else return operations)
   >>=? fun operations ->
   Block.bake ~policy:(Block.By_account pkh) ~operations b
  else return b)
  >>=? fun b ->
  (* Extract balances for the cycle post baking. *)
  Context.Contract.balance ~kind:Main (B b) id >>=? fun bal_main ->
  Context.Contract.balance ~kind:Deposit (B b) id >>=? fun bal_deposit ->
  Context.Contract.balance ~kind:Rewards (B b) id >>=? fun bal_rewards ->
  Context.Contract.balance ~kind:Fees (B b) id >>=? fun bal_fees ->
  (* Check that the [double_step] parameter is effective. *)
  Assert.equal_bool
    ~loc:__LOC__
    (bal_rewards > committed_rewards)
    (with_rewards && double_step)
  >>=? fun () ->
  Assert.equal_bool
    ~loc:__LOC__
    (bal_fees > committed_fees)
    (with_fees && double_step)
  >>=? fun () ->
  (* finish cycle 0 excluding the committing baker [id] *)
  let policy = Block.Excluding [pkh] in
  Block.bake_until_cycle_end ~policy b >>=? fun b ->
  (* finish cycle 1 excluding the committing baker [id] *)
  let blocks_per_cycle = Int32.to_int csts.parametric.blocks_per_cycle in
  Block.bake_n_with_all_balance_updates ~policy blocks_per_cycle b
  >>=? fun (b, cycle_end_bupds) ->
  (* check that punishment balance updates are correct. *)
  Assert.equal_bool
    ~loc:__LOC__
    (List.mem
       ~equal:( = )
       Alpha_context.Receipt.
         ( Rewards (pkh, commitment_cycle),
           Debited committed_rewards,
           Block_application )
       cycle_end_bupds)
    with_rewards
  >>=? fun () ->
  Assert.equal_bool
    ~loc:__LOC__
    (List.mem
       ~equal:( = )
       Alpha_context.Receipt.
         ( Fees (pkh, commitment_cycle),
           Debited committed_fees,
           Block_application )
       cycle_end_bupds)
    with_fees
  >>=? fun () ->
  (* test that baker [id], which didn't reveal at cycle 1 like it was supposed to,
     at the end of the cycle 1 looses the reward and fees but not the bond *)
  balance_is ~loc:__LOC__ (B b) id ~kind:Main bal_main >>=? fun () ->
  balance_is ~loc:__LOC__ (B b) id ~kind:Deposit bal_deposit >>=? fun () ->
  balance_was_debited
    ~loc:__LOC__
    (B b)
    id
    ~kind:Rewards
    bal_rewards
    committed_rewards
  >>=? fun () ->
  balance_was_debited ~loc:__LOC__ (B b) id ~kind:Fees bal_fees committed_fees
  >>=? fun () ->
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

(** Test that the amount reported in balance updates is the actual amount burned
    when the committed amount is greater than the available amount. *)
let test_unrevealed () =
  let accounts = Account.generate_accounts 1 in
  let total_rewards = Tez.of_int 250 in
  let total_fees = Tez.of_int 550 in
  let committed_rewards = Tez.of_int 1000 in
  let committed_fees = Tez.of_int 1500 in
  let open Alpha_context in
  Block.alpha_context accounts >>=? fun ctxt ->
  match accounts with
  | [({pkh; _}, _)] ->
      (* Freeze rewards and fees for cycle 0. *)
      wrap (Delegate.freeze_rewards ctxt pkh total_rewards) >>=? fun ctxt ->
      wrap (Delegate.freeze_fees ctxt pkh total_fees) >>=? fun ctxt ->
      let unrevealed =
        Nonce.
          {
            nonce_hash = Nonce_hash.zero;
            delegate = pkh;
            rewards = committed_rewards;
            fees = committed_fees;
          }
      in
      (* Simulate an end-of-cycle event. *)
      wrap (Delegate.cycle_end ctxt (Cycle.add Cycle.root 1) [unrevealed])
      >>=? fun (_ctxt, bupds, _) ->
      (* Check that balance updates indicate what has been burned,
         i.e. all fees and rewards. *)
      let expected_bupds =
        Receipt.
          [
            (Fees (pkh, Cycle.root), Debited total_fees, Block_application);
            (Rewards (pkh, Cycle.root), Debited total_rewards, Block_application);
          ]
      in
      Assert.equal_bool ~loc:__LOC__ (bupds = expected_bupds) true
  | _ -> (* Exactly one account has been generated. *) assert false

let tests =
  [
    Tztest.tztest "no commitment" `Quick test_no_commitment;
    Tztest.tztest
      "revelation_early_wrong_right_twice"
      `Quick
      test_revelation_early_wrong_right_twice;
    Tztest.tztest
      "revelation_missing_and_late no fees and no reward (1/2)"
      `Quick
      (test_revelation_missing_and_late
         ~with_fees:false
         ~with_rewards:false
         ~double_step:false);
    Tztest.tztest
      "revelation_missing_and_late no fees and no reward (2/2)"
      `Quick
      (test_revelation_missing_and_late
         ~with_fees:false
         ~with_rewards:false
         ~double_step:true);
    Tztest.tztest
      "revelation_missing_and_late no fees and some reward (1/2)"
      `Quick
      (test_revelation_missing_and_late
         ~with_fees:false
         ~with_rewards:true
         ~double_step:false);
    Tztest.tztest
      "revelation_missing_and_late no fees and some reward (2/2)"
      `Quick
      (test_revelation_missing_and_late
         ~with_fees:false
         ~with_rewards:true
         ~double_step:true);
    Tztest.tztest
      "revelation_missing_and_late some fees and no reward (1/2)"
      `Quick
      (test_revelation_missing_and_late
         ~with_fees:true
         ~with_rewards:false
         ~double_step:false);
    Tztest.tztest
      "revelation_missing_and_late some fees and no reward (2/2)"
      `Quick
      (test_revelation_missing_and_late
         ~with_fees:true
         ~with_rewards:false
         ~double_step:true);
    Tztest.tztest
      "revelation_missing_and_late some fees and some reward (1/2)"
      `Quick
      (test_revelation_missing_and_late
         ~with_fees:true
         ~with_rewards:true
         ~double_step:false);
    Tztest.tztest
      "revelation_missing_and_late some fees and some reward (2/2)"
      `Quick
      (test_revelation_missing_and_late
         ~with_fees:true
         ~with_rewards:true
         ~double_step:true);
    Tztest.tztest "test unrevealed" `Quick test_unrevealed;
  ]
