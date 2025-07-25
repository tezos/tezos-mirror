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
    Invocation:   dune exec src/proto_023_PtSeouLo/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_baking.ml
    Subject:      Bakers and baking/voting power-related tests, based on RPCs.
                  Note that more detailed tests on baking rewards can be found
                  in lib_protocol/test/integration/test_scenario_rewards.ml
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
  let open Lwt_result_syntax in
  let* b, _contracts = Context.init_n ~consensus_threshold_size:0 5 () in
  let* csts = Context.get_constants (B b) in
  let blocks_per_cycle = csts.parametric.blocks_per_cycle in
  let pp fmt x = Format.fprintf fmt "%ld" x in
  let* b = Block.bake b in
  let* b = Block.bake_until_cycle_end b in
  let*? curr_level = Context.get_level (B b) in
  let* () =
    Assert.equal
      ~loc:__LOC__
      Int32.equal
      "not the right level"
      pp
      (Alpha_context.Raw_level.to_int32 curr_level)
      blocks_per_cycle
  in
  let*? l = Context.get_level (B b) in
  let* b = Block.bake_n 10 b in
  let*? curr_level = Context.get_level (B b) in
  Assert.equal
    ~loc:__LOC__
    Int32.equal
    "not the right level"
    pp
    (Alpha_context.Raw_level.to_int32 curr_level)
    (Int32.add (Alpha_context.Raw_level.to_int32 l) 10l)

(** Test baking [n] cycles in a raw works smoothly. *)
let test_bake_n_cycles n () =
  let open Lwt_result_syntax in
  let open Block in
  let policy = By_round 0 in
  let* block, _contract = Context.init1 ~consensus_threshold_size:0 () in
  let* (_block : block) = Block.bake_until_n_cycle_end ~policy n block in
  return_unit

let fold_es n init f =
  let open Lwt_result_syntax in
  let rec aux k accu =
    if k >= n then return accu
    else
      let* accu = f k accu in
      aux (k + 1) accu
  in
  aux 0 init

(** Test that:
    - [current_voting_power] is updated whenever the baker bakes a
      block (because of the rewards it receives).
    - [voting_power] is the snapshot of [current_voting_power] at the
      start of the current voting period. *)
let test_voting_power_cache () =
  let open Lwt_result_syntax in
  let* genesis, delegate1 = Context.init1 ~consensus_threshold_size:0 () in
  let delegate1 = Account.pkh_of_contract_exn delegate1 in
  let check_voting_power_update
      (previous_current_voting_power, previous_voting_power) block =
    let* current_voting_power =
      Context.get_current_voting_power (B block) delegate1
    in
    let* voting_power = Context.get_voting_power (B block) delegate1 in
    let* delegate1_baked_last_block =
      let* block_producer = Block.block_producer block in
      return (Signature.Public_key_hash.equal block_producer.delegate delegate1)
    in
    let* voting_period = Context.Vote.get_current_period (B block) in
    let is_last_block_of_period = Int32.equal voting_period.remaining 0l in
    Log.debug
      ~color:Log_helpers.low_debug_color
      "@[<v2>Checking updates for block at level %ld %sbaked by 'delegate1'@,\
       %a@,\
       current_voting_power: previous=%Ld new=%Ld@,\
       voting_power:         previous=%Ld new=%Ld@,\
       @]"
      block.header.shell.level
      (if delegate1_baked_last_block then "" else "not ")
      Voting_period.pp_info
      voting_period
      previous_current_voting_power
      current_voting_power
      previous_voting_power
      voting_power ;
    let* () =
      (if delegate1_baked_last_block then Assert.lt_int64
       else Assert.equal_int64)
        ~loc:__LOC__
        previous_current_voting_power
        current_voting_power
    in
    let* () =
      (* The context gets set up for a new voting period while
         applying the last block of the previous period. *)
      Assert.equal_int64
        ~loc:__LOC__
        voting_power
        (if is_last_block_of_period then current_voting_power
         else previous_voting_power)
    in
    return (current_voting_power, voting_power)
  in
  let* constants = Context.get_constants (B genesis) in
  let blocks_per_voting_period =
    Int32.(
      to_int
        (mul
           constants.parametric.blocks_per_cycle
           constants.parametric.cycles_per_voting_period))
  in
  let* full_balance = Context.Delegate.full_balance (B genesis) delegate1 in
  let initial_voting_power = Tez.to_mutez full_balance in
  let* _ =
    fold_es
      (* We bake a bit more than 3 full voting periods. *)
      ((3 * blocks_per_voting_period) + 2)
      (genesis, (initial_voting_power, initial_voting_power))
      (fun (_n : int) (block, prev_powers) ->
        let* block = Block.bake block in
        let* new_powers = check_voting_power_update prev_powers block in
        return (block, new_powers))
  in
  return_unit

(** test that after baking, one gets the baking reward fixed portion. *)
let test_basic_baking_reward () =
  let open Lwt_result_syntax in
  let* genesis, baker = Context.init1 ~consensus_threshold_size:0 () in
  let* b = Block.bake genesis in
  let baker_pkh = Context.Contract.pkh baker in
  let* bal = Context.Contract.balance (B b) baker in
  let* frozen_deposit =
    Context.Delegate.current_frozen_deposits (B b) baker_pkh
  in
  let* br = Context.get_baking_reward_fixed_portion (B b) in
  let open Tez_helpers in
  let expected_initial_balance = bal +! frozen_deposit -! br in
  Assert.equal_tez
    ~loc:__LOC__
    expected_initial_balance
    Account.default_initial_full_balance

let get_contract_for_pkh contracts pkh =
  let open Lwt_result_syntax in
  let rec find_contract = function
    | [] -> assert false
    | c :: t ->
        let c_pkh = Context.Contract.pkh c in
        if Signature.Public_key_hash.equal c_pkh pkh then return c
        else find_contract t
  in
  find_contract contracts

(** Test that
    - the block producer gets the bonus for including the attestations;
    - the payload producer gets the baking reward.

    The test checks this in two scenarios, in the first one the payload producer
    and the block producer are the same delegate, in the second one they are
    different. The first scenario is checked by first baking block [b1] and then
    block [b2] at round 0 containing a number of attestations for [b1] and the
    checking the balance of [b2]'s baker. For the second scenario another block
    [b2'] is build on top of [b1] by a different baker, using the same payload as
    [b2].  *)
let test_rewards_block_and_payload_producer () =
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init_n ~consensus_threshold_size:1 10 () in
  let* baker_b1 = Context.get_baker (B genesis) ~round:Round.zero in
  let* baker_b1_contract = get_contract_for_pkh contracts baker_b1 in
  let* b1 = Block.bake ~policy:(By_round 0) genesis in
  let* attesters = Context.get_attesters (B b1) in
  (* We let just a part of the attesters vote; we assume here that 5 of 10
     attesters will have together at least one slot (to pass the threshold), but
     not all slots (to make the test more interesting, otherwise we know the
     total attesting power). *)
  let attesters = List.take_n 5 attesters in
  let* attestations =
    List.map_ep
      (fun attester ->
        Op.attestation
          ~attesting_slot:(Op.attesting_slot_of_attester attester)
          b1)
      attesters
  in
  let attesting_power =
    List.fold_left
      (fun acc attester ->
        acc + List.length attester.Plugin.RPC.Validators.slots)
      0
      attesters
  in
  let fee = Tez.one in
  let* tx =
    Op.transaction (B b1) ~fee baker_b1_contract baker_b1_contract Tez.one
  in
  let* b2 =
    Block.bake ~policy:(By_round 0) ~operations:(attestations @ [tx]) b1
  in
  let* baker_b2 = Context.get_baker (B b1) ~round:Round.zero in
  let* baker_b2_contract = get_contract_for_pkh contracts baker_b2 in
  let* bal = Context.Contract.balance (B b2) baker_b2_contract in
  let* frozen_deposit =
    Context.Delegate.current_frozen_deposits (B b2) baker_b2
  in
  let* baking_reward = Context.get_baking_reward_fixed_portion (B b2) in
  let* bonus_reward = Context.get_bonus_reward (B b2) ~attesting_power in
  let* reward_for_b1 =
    if Signature.Public_key_hash.equal baker_b2 baker_b1 then
      Context.get_baking_reward_fixed_portion (B b1)
    else return Tez.zero
  in
  (* we are in the first scenario where the payload producer is the same as the
     block producer, in our case, [baker_b2]. [baker_b2] gets the baking reward
     plus the fee for the transaction [tx]. *)
  let expected_balance =
    let open Tez_helpers in
    Account.default_initial_full_balance -! frozen_deposit +! baking_reward
    +! bonus_reward +! reward_for_b1 +! fee
  in
  let* () = Assert.equal_tez ~loc:__LOC__ bal expected_balance in
  (* Some new baker [baker_b2'] bakes b2' at the first round which does not
     correspond to a slot of [baker_b2] and it includes the PQC for [b2]. We
     check that the fixed baking reward goes to the payload producer [baker_b2],
     while the bonus goes to the the block producer (aka baker) [baker_b2']. *)
  let* preattesters = Context.get_attesters (B b2) in
  let* preattestations =
    List.map_ep
      (fun attester ->
        Op.preattestation
          ~attesting_slot:(Op.attesting_slot_of_attester attester)
          b2)
      preattesters
  in
  let* baker_b2 = Context.get_baker (B b1) ~round:Round.zero in
  let* bakers = Context.get_bakers (B b1) in
  let baker_b2' = Context.get_first_different_baker baker_b2 bakers in
  let* b2' =
    Block.bake
      ~payload_round:Round.zero
      ~locked_round:Round.zero
      ~policy:(By_account baker_b2')
      ~operations:(preattestations @ attestations @ [tx])
      b1
  in
  (* [baker_b2], as payload producer, gets the block reward and the fees *)
  let* bal = Context.Contract.balance (B b2') baker_b2_contract in
  let* frozen_deposit =
    Context.Delegate.current_frozen_deposits (B b2') baker_b2
  in
  let reward_for_b1 =
    if Signature.Public_key_hash.equal baker_b2 baker_b1 then baking_reward
    else Tez.zero
  in
  let expected_balance =
    let open Tez_helpers in
    Account.default_initial_full_balance +! baking_reward -! frozen_deposit
    +! reward_for_b1 +! fee
  in
  let* () = Assert.equal_tez ~loc:__LOC__ bal expected_balance in
  (* [baker_b2'] gets the bonus because he is the one who included the
     attestations *)
  let* baker_b2'_contract = get_contract_for_pkh contracts baker_b2' in
  let* bal' = Context.Contract.balance (B b2') baker_b2'_contract in
  let* frozen_deposits' =
    Context.Delegate.current_frozen_deposits (B b2') baker_b2'
  in
  let* baker_b1 = Context.get_baker (B genesis) ~round:Round.zero in
  let reward_for_b1' =
    if Signature.Public_key_hash.equal baker_b2' baker_b1 then baking_reward
    else Tez.zero
  in
  let expected_balance' =
    let open Tez_helpers in
    Account.default_initial_full_balance +! bonus_reward +! reward_for_b1'
    -! frozen_deposits'
  in
  Assert.equal_tez ~loc:__LOC__ bal' expected_balance'

(** We test that:
    - a delegate that has active stake can bake;
    - a delegate that has no active stake cannot bake.
*)
let test_enough_active_stake_to_bake ~has_active_stake () =
  let open Lwt_result_syntax in
  let* b_for_constants, _contract = Context.init1 () in
  let* Constants.{parametric = {minimal_stake; _}; _} =
    Context.get_constants (B b_for_constants)
  in
  let tpr = Tez.to_mutez minimal_stake in
  (* N.B. setting the balance has an impact on the active stake; without
     delegation, the full balance is the same as the staking balance and the
     active balance is less or equal the staking balance (see
     [Delegate_sampler.select_distribution_for_cycle]). *)
  let initial_bal1 = if has_active_stake then tpr else Int64.sub tpr 1L in
  let* b0, (account1, _account2) =
    Context.init2
      ~bootstrap_balances:[initial_bal1; tpr]
      ~consensus_threshold_size:0
      ()
  in
  let pkh1 = Context.Contract.pkh account1 in
  let* baking_reward_fixed_portion =
    Context.get_baking_reward_fixed_portion (B b0)
  in
  let*! b1 = Block.bake ~policy:(By_account pkh1) b0 in
  if has_active_stake then
    let*? b1 in
    let* bal = Context.Contract.balance (B b1) account1 in
    let* frozen_deposit =
      Context.Delegate.current_frozen_deposits (B b1) pkh1
    in
    let expected_bal =
      Tez_helpers.(
        Tez.of_mutez_exn initial_bal1
        +! baking_reward_fixed_portion -! frozen_deposit)
    in
    Assert.equal_tez ~loc:__LOC__ bal expected_bal
  else
    (* pkh1 has less than minimal_stake so it will have no slots, thus it
       cannot be a proposer, thus it cannot bake. Precisely, bake fails because
       get_next_baker_by_account fails with "No slots found for pkh1" *)
    Assert.error ~loc:__LOC__ b1 (fun _ -> true)

let test_committee_sampling () =
  let open Lwt_result_syntax in
  let test_distribution max_round distribution =
    let bootstrap_balances, bounds = List.split distribution in
    let*? accounts =
      Account.generate_accounts (List.length bootstrap_balances)
    in
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
        consensus_threshold_size = 0;
      }
    in
    let parameters =
      Default_parameters.parameters_of_constants ~bootstrap_accounts constants
    in
    let* genesis = Block.genesis_with_parameters parameters in
    let+ bakers =
      Plugin.RPC.Baking_rights.get Block.rpc_ctxt ~all:true ~max_round genesis
    in
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
  let* () = test_distribution 65535 accounts in
  let* () =
    test_distribution
      10_000
      [
        (16_000_000_000L, (4_600, 5_400));
        (8_000_000_000L, (2_200, 2_800));
        (8_000_000_000L, (2_200, 2_800));
      ]
  in
  test_distribution
    10_000
    [(792_000_000_000L, (9_830, 9_970)); (8_000_000_000L, (40, 160))]

let tests =
  [
    Tztest.tztest "cycle" `Quick test_cycle;
    Tztest.tztest "bake_n_cycles for 12 cycles" `Quick (test_bake_n_cycles 12);
    Tztest.tztest "voting_power cache" `Quick test_voting_power_cache;
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
