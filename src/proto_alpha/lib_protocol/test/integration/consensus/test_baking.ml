(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
    -------
    Component:    Protocol (baking)
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_baking.ml
    Subject:      Rewards and bakers. Tests based on RPCs.
*)

open Protocol
open Alpha_context

(** Verify the level is correctly computed when the first cycle is
    passed and after baking a certain fixed number of blocks (10 for
    the moment). The result should be [blocks_per_cycle + 10] where
    [blocks_per_cycle] comes from the constants of the selected
    protocol.

    IMPROVEMENTS:
    - Randomize the number of cycle.
    - Randomize the number of accounts created at the beginning
    - Randomize the blocks per cycle.
    - Randomize the number of blocks baked after the n cycles baked
      previously. *)
let test_cycle () =
  Context.init_n ~consensus_threshold:0 5 () >>=? fun (b, _contracts) ->
  Context.get_constants (B b) >>=? fun csts ->
  let blocks_per_cycle = csts.parametric.blocks_per_cycle in
  let pp fmt x = Format.fprintf fmt "%ld" x in
  Block.bake b >>=? fun b ->
  Block.bake_until_cycle_end b >>=? fun b ->
  Context.get_level (B b) >>?= fun curr_level ->
  Assert.equal
    ~loc:__LOC__
    Int32.equal
    "not the right level"
    pp
    (Alpha_context.Raw_level.to_int32 curr_level)
    blocks_per_cycle
  >>=? fun () ->
  Context.get_level (B b) >>?= fun l ->
  Block.bake_n 10 b >>=? fun b ->
  Context.get_level (B b) >>?= fun curr_level ->
  Assert.equal
    ~loc:__LOC__
    Int32.equal
    "not the right level"
    pp
    (Alpha_context.Raw_level.to_int32 curr_level)
    (Int32.add (Alpha_context.Raw_level.to_int32 l) 10l)

(** Test baking [n] cycles in a raw works smoothly. *)
let test_bake_n_cycles n () =
  let open Block in
  let policy = By_round 0 in
  Context.init1 ~consensus_threshold:0 () >>=? fun (block, _contract) ->
  Block.bake_until_n_cycle_end ~policy n block >>=? fun (_block : block) ->
  return_unit

(** Check that, after one or two voting periods, the voting power of a baker is
   updated according to the rewards it receives for baking the blocks in the
   voting periods. Note we consider only one baker. *)
let test_voting_power_cache () =
  let open Block in
  let policy = By_round 0 in
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, _contract) ->
  Context.get_constants (B genesis) >>=? fun csts ->
  let blocks_per_voting_period =
    Int32.(
      mul
        csts.parametric.blocks_per_cycle
        csts.parametric.cycles_per_voting_period)
  in
  let blocks_per_voting_periods n =
    Int64.of_int (n * Int32.to_int blocks_per_voting_period)
  in
  Context.get_baking_reward_fixed_portion (B genesis) >>=? fun baking_reward ->
  Context.get_bakers (B genesis) >>=? fun bakers ->
  let baker = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bakers in
  Context.Delegate.full_balance (B genesis) baker >>=? fun full_balance ->
  let assert_voting_power ~loc n block =
    Context.get_voting_power (B block) baker >>=? fun voting_power ->
    Assert.equal_int64 ~loc n voting_power
  in
  (* the voting power is the full staking balance *)
  let initial_voting_power_at_genesis = Tez.to_mutez full_balance in
  assert_voting_power ~loc:__LOC__ initial_voting_power_at_genesis genesis
  >>=? fun () ->
  let rewards_after_one_voting_period =
    Test_tez.(baking_reward *! Int64.pred (blocks_per_voting_periods 1))
  in
  let expected_delta_voting_power_after_one_voting_period =
    Tez.to_mutez rewards_after_one_voting_period
  in
  Block.bake_n ~policy (Int32.to_int blocks_per_voting_period - 1) genesis
  >>=? fun block ->
  let expected_voting_power_after_one_voting_period =
    Int64.add
      initial_voting_power_at_genesis
      expected_delta_voting_power_after_one_voting_period
  in
  assert_voting_power
    ~loc:__LOC__
    expected_voting_power_after_one_voting_period
    block
  >>=? fun () ->
  let rewards_after_two_voting_periods =
    Test_tez.(baking_reward *! Int64.pred (blocks_per_voting_periods 2))
  in
  let expected_delta_voting_power_after_two_voting_periods =
    Tez.to_mutez rewards_after_two_voting_periods
  in
  Block.bake_n ~policy (Int32.to_int blocks_per_voting_period) block
  >>=? fun block ->
  let expected_voting_power_after_two_voting_periods =
    Int64.add
      initial_voting_power_at_genesis
      expected_delta_voting_power_after_two_voting_periods
  in
  assert_voting_power
    ~loc:__LOC__
    expected_voting_power_after_two_voting_periods
    block

(** test that after baking, one gets the baking reward fixed portion. *)
let test_basic_baking_reward () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (genesis, baker) ->
  Block.bake genesis >>=? fun b ->
  let baker_pkh = Context.Contract.pkh baker in
  Context.Contract.balance (B b) baker >>=? fun bal ->
  Context.Delegate.current_frozen_deposits (B b) baker_pkh
  >>=? fun frozen_deposit ->
  Context.get_baking_reward_fixed_portion (B b) >>=? fun br ->
  let open Test_tez in
  let expected_initial_balance = bal +! frozen_deposit -! br in
  Assert.equal_tez
    ~loc:__LOC__
    expected_initial_balance
    Account.default_initial_balance

let get_contract_for_pkh contracts pkh =
  let rec find_contract = function
    | [] -> assert false
    | c :: t ->
        let c_pkh = Context.Contract.pkh c in
        if Signature.Public_key_hash.equal c_pkh pkh then return c
        else find_contract t
  in
  find_contract contracts

(** Test that
    - the block producer gets the bonus for including the endorsements;
    - the payload producer gets the baking reward.

    The test checks this in two scenarios, in the first one the payload producer
    and the block producer are the same delegate, in the second one they are
    different. The first scenario is checked by first baking block [b1] and then
    block [b2] at round 0 containing a number of endorsements for [b1] and the
    checking the balance of [b2]'s baker. For the second scenario another block
    [b2'] is build on top of [b1] by a different baker, using the same payload as
    [b2].  *)
let test_rewards_block_and_payload_producer () =
  Context.init_n ~consensus_threshold:1 10 () >>=? fun (genesis, contracts) ->
  Context.get_baker (B genesis) ~round:Round.zero >>=? fun baker_b1 ->
  get_contract_for_pkh contracts baker_b1 >>=? fun baker_b1_contract ->
  Block.bake ~policy:(By_round 0) genesis >>=? fun b1 ->
  Context.get_endorsers (B b1) >>=? fun endorsers ->
  List.map_es
    (function
      | {Plugin.RPC.Validators.delegate; slots; _} -> return (delegate, slots))
    endorsers
  >>=? fun endorsers ->
  (* We let just a part of the endorsers vote; we assume here that 5 of 10
     endorsers will have together at least one slot (to pass the threshold), but
     not all slots (to make the test more interesting, otherwise we know the
     total endorsing power). *)
  let endorsers = List.take_n 5 endorsers in
  List.map_ep
    (fun (endorser, _slots) -> Op.endorsement ~delegate:endorser b1)
    endorsers
  >>=? fun endos ->
  let endorsing_power =
    List.fold_left
      (fun acc (_pkh, slots) -> acc + List.length slots)
      0
      endorsers
  in
  let fee = Tez.one in
  Op.transaction (B b1) ~fee baker_b1_contract baker_b1_contract Tez.one
  >>=? fun tx ->
  Block.bake ~policy:(By_round 0) ~operations:(endos @ [tx]) b1 >>=? fun b2 ->
  Context.get_baker (B b1) ~round:Round.zero >>=? fun baker_b2 ->
  get_contract_for_pkh contracts baker_b2 >>=? fun baker_b2_contract ->
  Context.Contract.balance (B b2) baker_b2_contract >>=? fun bal ->
  Context.Delegate.current_frozen_deposits (B b2) baker_b2
  >>=? fun frozen_deposit ->
  Context.get_baking_reward_fixed_portion (B b2) >>=? fun baking_reward ->
  Context.get_bonus_reward (B b2) ~endorsing_power >>=? fun bonus_reward ->
  (if Signature.Public_key_hash.equal baker_b2 baker_b1 then
   Context.get_baking_reward_fixed_portion (B b1)
  else return Tez.zero)
  >>=? fun reward_for_b1 ->
  (* we are in the first scenario where the payload producer is the same as the
     block producer, in our case, [baker_b2]. [baker_b2] gets the baking reward
     plus the fee for the transaction [tx]. *)
  let expected_balance =
    let open Test_tez in
    Account.default_initial_balance -! frozen_deposit +! baking_reward
    +! bonus_reward +! reward_for_b1 +! fee
  in
  Assert.equal_tez ~loc:__LOC__ bal expected_balance >>=? fun () ->
  (* Some new baker [baker_b2'] bakes b2' at the first round which does not
     correspond to a slot of [baker_b2] and it includes the PQC for [b2]. We
     check that the fixed baking reward goes to the payload producer [baker_b2],
     while the bonus goes to the the block producer (aka baker) [baker_b2']. *)
  Context.get_endorsers (B b2) >>=? fun endorsers ->
  List.map_es
    (function
      | {Plugin.RPC.Validators.delegate; slots; _} -> return (delegate, slots))
    endorsers
  >>=? fun preendorsers ->
  List.map_ep
    (fun (endorser, _slots) -> Op.preendorsement ~delegate:endorser b2)
    preendorsers
  >>=? fun preendos ->
  Context.get_baker (B b1) ~round:Round.zero >>=? fun baker_b2 ->
  Context.get_bakers (B b1) >>=? fun bakers ->
  let baker_b2' = Context.get_first_different_baker baker_b2 bakers in
  Block.bake
    ~payload_round:(Some Round.zero)
    ~locked_round:(Some Round.zero)
    ~policy:(By_account baker_b2')
    ~operations:(preendos @ endos @ [tx])
    b1
  >>=? fun b2' ->
  (* [baker_b2], as payload producer, gets the block reward and the fees *)
  Context.Contract.balance (B b2') baker_b2_contract >>=? fun bal ->
  Context.Delegate.current_frozen_deposits (B b2') baker_b2
  >>=? fun frozen_deposit ->
  let reward_for_b1 =
    if Signature.Public_key_hash.equal baker_b2 baker_b1 then baking_reward
    else Tez.zero
  in
  let expected_balance =
    let open Test_tez in
    Account.default_initial_balance +! baking_reward -! frozen_deposit
    +! reward_for_b1 +! fee
  in
  Assert.equal_tez ~loc:__LOC__ bal expected_balance >>=? fun () ->
  (* [baker_b2'] gets the bonus because he is the one who included the
     endorsements *)
  get_contract_for_pkh contracts baker_b2' >>=? fun baker_b2'_contract ->
  Context.Contract.balance (B b2') baker_b2'_contract >>=? fun bal' ->
  Context.Delegate.current_frozen_deposits (B b2') baker_b2'
  >>=? fun frozen_deposits' ->
  Context.get_baker (B genesis) ~round:Round.zero >>=? fun baker_b1 ->
  let reward_for_b1' =
    if Signature.Public_key_hash.equal baker_b2' baker_b1 then baking_reward
    else Tez.zero
  in
  let expected_balance' =
    let open Test_tez in
    Account.default_initial_balance +! bonus_reward +! reward_for_b1'
    -! frozen_deposits'
  in
  Assert.equal_tez ~loc:__LOC__ bal' expected_balance'

(** We test that:
    - a delegate that has active stake can bake;
    - a delegate that has no active stake cannot bake.
*)
let test_enough_active_stake_to_bake ~has_active_stake () =
  Context.init1 () >>=? fun (b_for_constants, _contract) ->
  Context.get_constants (B b_for_constants)
  >>=? fun Constants.{parametric = {minimal_stake; _}; _} ->
  let tpr = Tez.to_mutez minimal_stake in
  (* N.B. setting the balance has an impact on the active stake; without
     delegation, the full balance is the same as the staking balance and the
     active balance is less or equal the staking balance (see
     [Delegate_sampler.select_distribution_for_cycle]). *)
  let initial_bal1 = if has_active_stake then tpr else Int64.sub tpr 1L in
  Context.init2
    ~bootstrap_balances:[initial_bal1; tpr]
    ~consensus_threshold:0
    ()
  >>=? fun (b0, (account1, _account2)) ->
  let pkh1 = Context.Contract.pkh account1 in
  Context.get_constants (B b0)
  >>=? fun Constants.{parametric = {baking_reward_fixed_portion; _}; _} ->
  Block.bake ~policy:(By_account pkh1) b0 >>= fun b1 ->
  if has_active_stake then
    b1 >>?= fun b1 ->
    Context.Contract.balance (B b1) account1 >>=? fun bal ->
    Context.Delegate.current_frozen_deposits (B b1) pkh1
    >>=? fun frozen_deposit ->
    let expected_bal =
      Test_tez.(
        Tez.of_mutez_exn initial_bal1
        -! frozen_deposit +! baking_reward_fixed_portion)
    in
    Assert.equal_tez ~loc:__LOC__ bal expected_bal
  else
    (* pkh1 has less than minimal_stake so it will have no slots, thus it
       cannot be a proposer, thus it cannot bake. Precisely, bake fails because
       get_next_baker_by_account fails with "No slots found for pkh1" *)
    Assert.error ~loc:__LOC__ b1 (fun _ -> true)

let test_committee_sampling () =
  let test_distribution max_round distribution =
    let bootstrap_balances, bounds = List.split distribution in
    Account.generate_accounts (List.length bootstrap_balances)
    >>?= fun accounts ->
    let bootstrap_accounts =
      Account.make_bootstrap_accounts ~bootstrap_balances accounts
    in
    let consensus_committee_size = max_round in
    assert (
      (* Enforce that we are not mistakenly testing a value for committee_size
         that violates invariants of module Slot_repr. *)
      Result.is_ok
        (Slot_repr.of_int consensus_committee_size)) ;
    let constants =
      {
        Default_parameters.constants_test with
        consensus_committee_size;
        consensus_threshold = 0;
      }
    in
    let parameters =
      Default_parameters.parameters_of_constants ~bootstrap_accounts constants
    in
    Block.genesis_with_parameters parameters >>=? fun genesis ->
    Plugin.RPC.Baking_rights.get Block.rpc_ctxt ~all:true ~max_round genesis
    >|=? fun bakers ->
    let stats = Stdlib.Hashtbl.create 10 in
    Stdlib.List.iter2
      (fun acc bounds -> Stdlib.Hashtbl.add stats acc.Account.pkh (bounds, 0))
      accounts
      bounds ;
    List.iter
      (fun {Plugin.RPC.Baking_rights.delegate = pkh; _} ->
        let bounds, n = Stdlib.Hashtbl.find stats pkh in
        Stdlib.Hashtbl.replace stats pkh (bounds, n + 1))
      bakers ;
    let one_failed = ref false in

    Format.eprintf
      "@[<hov>Testing with baker distribution [%a],@ committee size %d.@]@."
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         (fun ppf (tez, _) -> Format.fprintf ppf "%Ld" tez))
      distribution
      max_round ;

    Format.eprintf
      "@[<v 2>@,%a@]@."
      (fun ppf stats ->
        Stdlib.Hashtbl.iter
          (fun pkh ((min_p, max_p), n) ->
            let failed = not (n >= min_p && n <= max_p) in
            Format.fprintf
              ppf
              "@[<h>- %a %d%a@]@,"
              Signature.Public_key_hash.pp
              pkh
              n
              (fun ppf failed ->
                if failed then
                  Format.fprintf ppf " [FAIL] should be in [%d, %d]" min_p max_p
                else Format.fprintf ppf "")
              failed ;
            one_failed := failed || !one_failed)
          stats)
      stats ;

    if !one_failed then
      Stdlib.failwith
        "The proportion of bakers marked as [FAILED] in the log output appear \
         in the wrong proportion in the committee."
    else Format.eprintf "Test succesful.@."
  in
  (* The tests below are not deterministic, but the probability that
     they fail is infinitesimal. *)
  let accounts =
    let expected_lower_bound = 6_100 and expected_upper_bound = 6_900 in
    let balance = 8_000_000_000L in
    let account = (balance, (expected_lower_bound, expected_upper_bound)) in
    Array.(make 10 account |> to_list)
  in
  test_distribution 65535 accounts >>=? fun () ->
  test_distribution
    10_000
    [
      (16_000_000_000L, (4_600, 5_400));
      (8_000_000_000L, (2_200, 2_800));
      (8_000_000_000L, (2_200, 2_800));
    ]
  >>=? fun () ->
  test_distribution
    10_000
    [(792_000_000_000L, (9_830, 9_970)); (8_000_000_000L, (40, 160))]

let tests =
  [
    Tztest.tztest "cycle" `Quick test_cycle;
    Tztest.tztest "bake_n_cycles for 12 cycles" `Quick (test_bake_n_cycles 12);
    Tztest.tztest "voting_power" `Quick test_voting_power_cache;
    Tztest.tztest
      "the fixed baking reward is given after a bake"
      `Quick
      test_basic_baking_reward;
    Tztest.tztest
      "the block producer gets the bonus while the payload producer gets the \
       baking reward "
      `Quick
      test_rewards_block_and_payload_producer;
    Tztest.tztest
      "a delegate with 8000 tez can bake"
      `Quick
      (test_enough_active_stake_to_bake ~has_active_stake:true);
    Tztest.tztest
      "a delegate with 7999 tez cannot bake"
      `Quick
      (test_enough_active_stake_to_bake ~has_active_stake:false);
    Tztest.tztest "committee sampling" `Quick test_committee_sampling;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("baking", tests)] |> Lwt_main.run
