(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Protocol (SWRR)
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                  -- --file test_swrr.ml
    Subject:      SWRR (Stake-Weighted Round Robin) baker selection
*)

open Protocol
open Alpha_context

let selected_bakers_length arr = Environment.FallbackArray.length arr

let selected_bakers_to_list arr =
  List.rev (Environment.FallbackArray.fold (fun acc pk -> pk :: acc) arr [])

let selected_delegates arr =
  List.map
    (fun (pk : Raw_context.consensus_pk) -> pk.delegate)
    (selected_bakers_to_list arr)

(* [get_selected_bakers ?cycle block] returns the result of SWRR for
   [cycle] (defaults to the current cycle of [block]), fails if there is
   none. *)
let get_selected_bakers ?cycle block =
  let open Lwt_result_syntax in
  let* stored = Plugin.RPC.swrr_selected_bakers ?cycle Block.rpc_ctxt block in
  match stored with
  | None ->
      Test.fail
        "SWRR: Selected_bakers missing in storage for cycle=%a"
        (Format.pp_print_option
           ~none:(fun fmt () -> Format.pp_print_string fmt "current")
           Cycle.pp)
        cycle
  | Some selected_bakers -> return selected_bakers

(* [count_selections delegate selection] returns the number of slots in [selection]
   assigned to [delegate]. *)
let count_selections delegate selection =
  List.fold_left
    (fun acc d -> if Implicit_account_repr.(d = delegate) then acc + 1 else acc)
    0
    selection

(* [swrr_credit_in_storage block delegate] returns the credit of [delegate].
   Returns None if the delegate is deactivated. *)
let swrr_credit_in_storage block delegate =
  let open Lwt_result_wrap_syntax in
  let* ctxt = Block.get_alpha_ctxt block in
  let raw_context = Alpha_context.Internal_for_tests.to_raw ctxt in
  let*@ credits = Storage.Stake.SWRR_credits.find raw_context in
  let credit =
    (fun credits ->
      List.find (fun (x, _) -> Implicit_account_repr.equal x delegate) credits
      |> Option.map snd)
      (Stdlib.Option.get credits)
  in
  return credit

(* Originated accounts are not used in these tests *)
let account_of_contract = function
  | Contract.Implicit delegate -> delegate
  | Contract.Originated _ -> Test.fail "SWRR: contract must be implicit"

let stake_weight_of ~stake_list delegate =
  match
    List.find
      (fun (ck, (_ : int64)) ->
        Implicit_account_repr.(ck.Consensus_key.delegate = delegate))
      stake_list
  with
  | Some (_, stake_weight) -> stake_weight
  | None ->
      Test.fail
        "SWRR: delegate %a is missing from the baking power distribution"
        Implicit_account_repr.pp
        delegate

(** Tests that the SWRR selection of a cycle holds exactly [blocks_per_cycle] slots, and
    it is only stored for the cycles for which consensus rights are known. *)
let test_swrr_sampler_correct_length () =
  let open Lwt_result_syntax in
  let* block, _bootstrap_accounts =
    Context.init3
      ~consensus_threshold_size:0
      ~swrr_new_baker_lottery_enable:true
      ()
  in
  let constants = block.constants in
  let nb_slots = constants.blocks_per_cycle in
  let* block =
    Block.bake_until_n_cycle_end (constants.consensus_rights_delay + 1) block
  in
  let check_swrr target_cycle =
    let* selected_bakers = get_selected_bakers ~cycle:target_cycle block in
    Assert.equal_int
      ~loc:__LOC__
      (Int32.to_int nb_slots)
      (selected_bakers_length selected_bakers)
  in
  let current_cycle = Block.current_cycle block in
  let first_cycle =
    Cycle.pred current_cycle |> WithExceptions.Option.get ~loc:__LOC__
  in
  let last_cycle = Cycle.add current_cycle constants.consensus_rights_delay in
  Log.info
    "SWRR: Checking selected bakers from cycle %a to %a"
    Cycle.pp
    first_cycle
    Cycle.pp
    last_cycle ;
  let* () = List.iter_es check_swrr Cycle.(first_cycle ---> last_cycle) in
  let check_empty_swrr target_cycle =
    let* stored =
      Plugin.RPC.swrr_selected_bakers ~cycle:target_cycle Block.rpc_ctxt block
    in
    match stored with
    | None -> return_unit
    | Some _ ->
        Test.fail
          "SWRR: Expected no selected_bakers in storage for target_cycle=%a"
          Cycle.pp
          target_cycle
  in
  (* [first_cycle - 1] has been cleaned up by [clear_outdated_sampling_data],
     and [last_cycle + 1] is not computed yet. *)
  let pred_first_cycle =
    Cycle.pred first_cycle |> WithExceptions.Option.get ~loc:__LOC__
  in
  let succ_last_cycle = Cycle.succ last_cycle in
  Log.info
    "SWRR: Checking empty sequence selected bakers for cycles %a and %a"
    Cycle.pp
    pred_first_cycle
    Cycle.pp
    succ_last_cycle ;
  let* () = check_empty_swrr pred_first_cycle in
  let* () = check_empty_swrr succ_last_cycle in
  return_unit

(** Tests that no slot of the SWRR selection holds a placeholder value: every slot is
    filled with an actual registered delegate. *)
let test_swrr_sampler_no_empty_value () =
  let open Lwt_result_wrap_syntax in
  let* block, _bootstrap_accounts =
    Context.init3
      ~consensus_threshold_size:0
      ~swrr_new_baker_lottery_enable:true
      ()
  in
  let constants = block.constants in
  let* block =
    Block.bake_until_n_cycle_end (constants.consensus_rights_delay + 1) block
  in
  let current_cycle = Block.current_cycle block in
  let target_cycle = Cycle.succ current_cycle in
  Log.info
    "SWRR: Checking no empty value in selected bakers for cycle %a"
    Cycle.pp
    target_cycle ;
  let* ctxt = Block.get_alpha_ctxt block in
  let raw_context = Alpha_context.Internal_for_tests.to_raw ctxt in
  let*! delegates = Storage.Delegates.elements raw_context in
  let* selected_bakers = get_selected_bakers ~cycle:target_cycle block in
  List.iter
    (fun (pk : Raw_context.consensus_pk) ->
      if Implicit_account_repr.(pk.delegate = Implicit_account_repr.zero) then
        Test.fail "SWRR: found the zero delegate in the selected bakers" ;
      if
        Signature.Public_key_hash.(
          pk.consensus_pkh = Signature.Public_key_hash.zero)
      then Test.fail "SWRR: found the zero consensus key in the selected bakers" ;
      if not (List.mem ~equal:Implicit_account_repr.equal pk.delegate delegates)
      then
        Test.fail
          "SWRR: %a is selected but is not a registered delegate"
          Implicit_account_repr.pp
          pk.delegate)
    (selected_bakers_to_list selected_bakers) ;
  return_unit

(* Bootstrap balances of the deterministic scenario below.

   A bootstrap account of balance [a] freezes
   [max minimal_stake (a / (limit_of_delegation_over_baking + 1))], that is
   [a / 20] for the balances below (see [Bootstrap_storage]), and delegates the
   remaining [19a / 20] to itself, which counts as [19a / 60] of baking power
   since [edge_of_staking_over_delegation = 3]. Its baking power is therefore
   [a / 20 + 19a / 60 = 11a / 30].

   The balances below are multiples of 60 mutez so that no rounding occurs, and
   all above [20 * minimal_stake = 120_000_000_000] mutez so that the frozen
   amount really is [a / 20]. This makes the baking powers exactly proportional
   to 5 : 4 : 3 : 2 : 1. *)
let manual_scenario_weight_unit = 44_000_000_000L

let manual_scenario_balances =
  [
    600_000_000_000L (* baking power 220_000_000_000 = 5 units *);
    480_000_000_000L (* baking power 176_000_000_000 = 4 units *);
    360_000_000_000L (* baking power 132_000_000_000 = 3 units *);
    240_000_000_000L (* baking power  88_000_000_000 = 2 units *);
    120_000_000_000L (* baking power  44_000_000_000 = 1 unit  *);
  ]

let manual_scenario_blocks_per_cycle = 20l

(* Expected SWRR selection for cycle 0, as indices in the bootstrap account
   list above, computed by hand.

   Credits all start at zero, since cycle 0 is the first cycle ever selected.
   For each slot, every baker's credit is increased by its weight, the baker
   with the highest credit is selected, and the total weight -- 15 units -- is
   subtracted from its credit. With weights 5, 4, 3, 2, 1 this gives, in units
   of [manual_scenario_weight_unit] mutez:

     slot | credits, after the increase and before the subtraction | selected
     -----+-------------------------------------------------------+---------
        0 |    5,    4,    3,    2,    1                          |    0
        1 |   -5,    8,    6,    4,    2                          |    1
        2 |    0,   -3,    9,    6,    3                          |    2
        3 |    5,    1,   -3,    8,    4                          |    3
        4 |   10,    5,    0,   -5,    5                          |    0
        5 |    0,    9,    3,   -3,    6                          |    1
        6 |    5,   -2,    6,   -1,    7                          |    4
        7 |   10,    2,    9,    1,   -7                          |    0
        8 |    0,    6,   12,    3,   -6                          |    2
        9 |    5,   10,    0,    5,   -5                          |    1
       10 |   10,   -1,    3,    7,   -4                          |    0
       11 |    0,    3,    6,    9,   -3                          |    3
       12 |    5,    7,    9,   -4,   -2                          |    2
       13 |   10,   11,   -3,   -2,   -1                          |    1
       14 |   15,    0,    0,    0,    0                          |    0

   There is no tie for the maximum credit, so the selection is easy to
   derive manually.

   After slot 14 all the credits are back to zero, so the sequence is periodic
   with period 15: slots 15 to 19 repeat slots 0 to 4. *)
let manual_scenario_expected_slots =
  [0; 1; 2; 3; 0; 1; 4; 0; 2; 1; 0; 3; 2; 1; 0]
  @
  (* the sequence is periodic with period 15 = total weight in units *)
  [0; 1; 2; 3; 0]

(** Determinism test: on a hand-crafted scenario, the SWRR selection of the
    first cycle is exactly the sequence computed by hand above. *)
let test_swrr_sampler_manual_scenario () =
  let open Lwt_result_syntax in
  let* block, bootstrap_contracts =
    Context.init_n
      5
      ~blocks_per_cycle:manual_scenario_blocks_per_cycle
      ~consensus_threshold_size:0
      ~swrr_new_baker_lottery_enable:true
      ~bootstrap_balances:manual_scenario_balances
      ()
  in
  let bakers = List.map account_of_contract bootstrap_contracts in
  (* Prepare the baking powers of the scenario: the hand computation below relies on
     them being exactly 5, 4, 3, 2 and 1 unit, and on the total being 15
     units. *)
  let* total_stake_weight, stake_list =
    Plugin.RPC.baking_power_distribution_for_current_cycle Block.rpc_ctxt block
  in
  let* () =
    List.iteri_es
      (fun i delegate ->
        Assert.equal_int64
          ~loc:__LOC__
          (stake_weight_of ~stake_list delegate)
          (Int64.mul manual_scenario_weight_unit (Int64.of_int (5 - i))))
      bakers
  in
  let* () =
    Assert.equal_int64
      ~loc:__LOC__
      total_stake_weight
      (Int64.mul manual_scenario_weight_unit 15L)
  in
  (* Cycle 0 is the first cycle selected by [Delegate_cycles.init_first_cycles],
     hence the one for which all the SWRR credits start at zero. *)
  let* selected_bakers = get_selected_bakers ~cycle:Cycle.root block in
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      (Int32.to_int manual_scenario_blocks_per_cycle)
      (selected_bakers_length selected_bakers)
  in
  let selection = selected_delegates selected_bakers in
  let bakers_array = Array.of_list bakers in
  let expected_selection =
    List.map (fun i -> bakers_array.(i)) manual_scenario_expected_slots
  in
  List.iteri
    (fun i delegate ->
      Log.info
        "SWRR: baker %a (weight %d out of 15) selected %d times out of %ld"
        Implicit_account_repr.pp_short
        delegate
        (5 - i)
        (count_selections delegate selection)
        manual_scenario_blocks_per_cycle)
    bakers ;
  Assert.assert_equal_list
    ~loc:__LOC__
    Implicit_account_repr.equal
    "SWRR selection of cycle 0 differs from the expected one"
    Implicit_account_repr.pp_short
    selection
    expected_selection

(* Bootstrap balances of the tie-breaking scenario below: all equal, so that
   every delegate ends up with the same baking power and every single slot is a
   tie between all the delegates that have not been selected yet in the current
   round of the round-robin. *)
let tie_break_nb_delegates = 5

let tie_break_balances =
  Stdlib.List.init tie_break_nb_delegates (fun _ -> 600_000_000_000L)

let tie_break_blocks_per_cycle = 20l

(* [chunks n l] cuts [l] into consecutive chunks of [n] elements. Only used on
   lists whose length is a multiple of [n], so the last chunk is full too. *)
let rec chunks n l =
  match l with
  | [] -> []
  | _ ->
      let rec take k l =
        if Compare.Int.(k <= 0) then ([], l)
        else
          match l with
          | [] -> ([], [])
          | x :: tl ->
              let taken, rest = take (k - 1) tl in
              (x :: taken, rest)
      in
      let chunk, rest = take n l in
      chunk :: chunks n rest

(** Tests which delegate is selected among several holding the same credit.
    The specific order is unspecified by the sampler. We check that fairness
    still holds. *)
let test_swrr_sampler_tie_break () =
  let open Lwt_result_syntax in
  let* block, bootstrap_contracts =
    Context.init_n
      tie_break_nb_delegates
      ~blocks_per_cycle:tie_break_blocks_per_cycle
      ~consensus_threshold_size:0
      ~swrr_new_baker_lottery_enable:true
      ~bootstrap_balances:tie_break_balances
      ()
  in
  let delegates = List.map account_of_contract bootstrap_contracts in
  (* The whole point of the scenario is that the baking powers are equal, so
     pin them rather than trusting the balances to produce them. *)
  let* total_stake_weight, stake_list =
    Plugin.RPC.baking_power_distribution_for_current_cycle Block.rpc_ctxt block
  in
  let* () =
    match delegates with
    | [] -> Test.fail "SWRR: expected %d delegates" tie_break_nb_delegates
    | first :: _ ->
        let weight = stake_weight_of ~stake_list first in
        let* () =
          List.iter_es
            (fun delegate ->
              Assert.equal_int64
                ~loc:__LOC__
                (stake_weight_of ~stake_list delegate)
                weight)
            delegates
        in
        Assert.equal_int64
          ~loc:__LOC__
          total_stake_weight
          (Int64.mul weight (Int64.of_int tie_break_nb_delegates))
  in
  (* Cycle 0 is the one whose credits all start at zero. *)
  let* selected_bakers = get_selected_bakers ~cycle:Cycle.root block in
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      (Int32.to_int tie_break_blocks_per_cycle)
      (selected_bakers_length selected_bakers)
  in
  let selection = selected_delegates selected_bakers in
  let ranges = chunks tie_break_nb_delegates selection in
  let sorted_delegates = List.sort Implicit_account_repr.compare delegates in
  (* Fairness: each aligned range holds each delegate exactly once. *)
  let* () =
    List.iteri_es
      (fun i range ->
        Assert.assert_equal_list
          ~loc:__LOC__
          Implicit_account_repr.equal
          (Format.sprintf
             "SWRR: range %d of the selection is not a permutation of the \
              delegates"
             i)
          Implicit_account_repr.pp_short
          (List.sort Implicit_account_repr.compare range)
          sorted_delegates)
      ranges
  in
  let ranks_of_selection selection =
    let rec aux seen next acc = function
      | [] -> List.rev acc
      | delegate :: tl -> (
          match List.assoc ~equal:Implicit_account_repr.equal delegate seen with
          | Some rank -> aux seen next (rank :: acc) tl
          | None -> aux ((delegate, next) :: seen) (next + 1) (next :: acc) tl)
    in
    aux [] 0 [] selection
  in
  (* A change in implementation might need to update this list to obtain the correct
     order of the delegates (fairness should still hold) *)
  let expected_ranks =
    [0; 1; 2; 3; 4] @ [4; 1; 0; 3; 2] @ [2; 1; 4; 3; 0] @ [0; 1; 2; 3; 4]
  in
  Assert.assert_equal_list
    ~loc:__LOC__
    Compare.Int.equal
    "SWRR: the tie-break order changed. This is allowed by the specification, \
     which leaves it unspecified: if the change is intended, update this test"
    Format.pp_print_int
    (ranks_of_selection selection)
    expected_ranks

(* Bootstrap balances of the variance test. They are deliberately irregular so
   that the selection counts are not round numbers. *)
let variance_balances =
  [
    4_300_050_000_000L;
    3_920_000_999_000L;
    5_629_169_000_000L;
    2_999_486_000_000L;
    1_000_000_000_000L;
    7_777_777_777_777L;
    12_345_678_901_234L;
    8_000_000_000_000L;
    2_121_212_121_212L;
    6_543_210_987_654L;
  ]

(** Fairness test: over a cycle of mainnet size, each delegate gets a number of
    round-0 slots that differs from its share of the total baking power by less
    than one slot.

    This holds because the SWRR credit of a delegate stays in [(-T, T)] where
    [T] is the total baking power: after [n] slots the credit of a delegate of
    weight [s] selected [c] times is [n * s - c * T], hence
    [|c - n * s / T| < 1]. Cycle 0 is used since it is the only cycle whose
    credits start at zero. *)
let test_swrr_sampler_check_variance () =
  let open Lwt_result_syntax in
  let* block, _bootstrap_accounts =
    Context.init_n
      10
      (* mainnet value, as the variance bound is only interesting on a
         realistically sized cycle *)
      ~blocks_per_cycle:14400l
      ~consensus_threshold_size:0
      ~swrr_new_baker_lottery_enable:true
      ~bootstrap_balances:variance_balances
      ()
  in
  let nb_slots = block.constants.blocks_per_cycle in
  let* selected_bakers = get_selected_bakers ~cycle:Cycle.root block in
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      (Int32.to_int nb_slots)
      (selected_bakers_length selected_bakers)
  in
  let selection = selected_delegates selected_bakers in
  let nb_slots = Int32.to_float nb_slots in
  let* total_stake_weight, stake_list =
    Plugin.RPC.baking_power_distribution_for_current_cycle Block.rpc_ctxt block
  in
  let total_stake_weight = Int64.to_float total_stake_weight in
  List.iter_es
    (fun ((ck : Consensus_key.t), stake_weight) ->
      let delegate = ck.delegate in
      let count = count_selections delegate selection in
      let expected =
        Int64.to_float stake_weight *. nb_slots /. total_stake_weight
      in
      let error = float_of_int count -. expected in
      Log.info
        "SWRR: Baker %a selected %d times, expected %.2f times (baking power \
         %Ld), error %.2f"
        Implicit_account_repr.pp_short
        delegate
        count
        expected
        stake_weight
        error ;
      if Compare.Float.(abs_float error <= 1.) then return_unit
      else
        Test.fail
          "SWRR: Baker %a selection count %d differs from the expected %.2f by \
           more than one slot"
          Implicit_account_repr.pp
          delegate
          count
          expected)
    stake_list

(** Credits are stored as arbitrary precision integers: a credit way above
    [Int64.max_int] neither overflows nor wraps around, and is accounted for
    exactly by the sampler. *)
let test_swrr_big_credit () =
  let open Lwt_result_wrap_syntax in
  let* block, (baker, _, _) =
    Context.init3
      ~consensus_threshold_size:0
      ~swrr_new_baker_lottery_enable:true
        (* Disable issuance so that the baking powers -- and hence the expected
           credits -- stay constant across the cycles baked below. *)
      ~issuance_weights:
        {
          base_total_issued_per_minute = Tez.zero;
          baking_reward_fixed_portion_weight = 1;
          baking_reward_bonus_weight = 0;
          attesting_reward_weight = 0;
          seed_nonce_revelation_tip_weight = 0;
          vdf_revelation_tip_weight = 0;
          dal_rewards_weight = 0;
        }
      ()
  in
  let baker = account_of_contract baker in
  (* A credit far beyond what [Int64] can hold. *)
  let injected_credit =
    Z.mul (Z.of_int64 Int64.max_int) (Z.of_int64 Int64.max_int)
  in
  let context = block.context in
  (* Get the raw context *)
  let*@ context, _balance_updates, _origination_results =
    Init_storage.prepare
      context
      ~level:0l
      ~predecessor_timestamp:Time.Protocol.epoch
      ~timestamp:Time.Protocol.epoch
  in
  (* Inject directly in the storage *)
  let*@ credits = Storage.Stake.SWRR_credits.find context in
  let credits = Stdlib.Option.get credits in
  let credits =
    List.filter
      (fun (x, _) -> not (Implicit_account_repr.equal x baker))
      credits
  in
  let credits = (baker, injected_credit) :: credits in
  let*! context = Storage.Stake.SWRR_credits.add context credits in
  (* Repackage as a usable environment context *)
  let context = Raw_context.recover context in
  let block = {block with context} in
  let constants = block.constants in
  let* block =
    Block.bake_until_n_cycle_end (constants.consensus_rights_delay + 1) block
  in
  (* The selection of the current cycle is the first one computed after the
     credit was injected. *)
  let target_cycle = Block.current_cycle block in
  let* selected_bakers = get_selected_bakers ~cycle:target_cycle block in
  let* () =
    List.iter_es
      (fun delegate ->
        if Implicit_account_repr.(delegate = baker) then return_unit
        else
          Test.fail
            "SWRR: expected %a (credit %a) to take every slot of cycle %a, but \
             %a was selected"
            Implicit_account_repr.pp
            baker
            Z.pp_print
            injected_credit
            Cycle.pp
            target_cycle
            Implicit_account_repr.pp
            delegate)
      (selected_delegates selected_bakers)
  in
  let* total_stake_weight, stake_list =
    Plugin.RPC.baking_power_distribution_for_current_cycle Block.rpc_ctxt block
  in
  let stake_weight = stake_weight_of ~stake_list baker in
  let nb_selected_slots =
    Z.of_int
      ((constants.consensus_rights_delay + 1)
      * Int32.to_int constants.blocks_per_cycle)
  in
  let expected_credit =
    Z.add
      injected_credit
      (Z.mul
         nb_selected_slots
         (Z.sub (Z.of_int64 stake_weight) (Z.of_int64 total_stake_weight)))
  in
  let* swrr_credits = Plugin.RPC.swrr_credits Block.rpc_ctxt block in
  let credit =
    match
      List.find
        (fun entry -> Implicit_account_repr.(entry.Plugin.RPC.delegate = baker))
        swrr_credits
    with
    | Some entry -> entry.Plugin.RPC.credit
    | None ->
        Test.fail "SWRR: no credit stored for %a" Implicit_account_repr.pp baker
  in
  let* () = Assert.equal_z ~loc:__LOC__ credit expected_credit in
  if Compare.Z.(credit > Z.of_int64 Int64.max_int) then return_unit
  else
    Test.fail
      "SWRR: credit %a of %a unexpectedly fits in an Int64"
      Z.pp_print
      credit
      Implicit_account_repr.pp
      baker

(** Test that for every delegate
    and every round the validators RPC hands it, the delegate stored at index
    [(cycle_position + 3 * round) mod blocks_per_cycle] of the cycle's selection
    is that same delegate.

    [blocks_per_cycle] is 16 rather than a multiple of 3 on purpose: since the
    index advances by 3 per round, a cycle length divisible by 3 would only make a
    third of the slots reachable at a given level. *)
let test_swrr_get_baker_matches_validators () =
  let open Lwt_result_syntax in
  let* block, _bootstrap_accounts =
    Context.init_n
      5
      ~blocks_per_cycle:16l
      ~consensus_threshold_size:0
      ~swrr_new_baker_lottery_enable:true
      ~bootstrap_balances:
        [
          5_000_000_000_000L;
          4_000_000_000_000L;
          3_000_000_000_000L;
          2_000_000_000_000L;
          1_000_000_000_000L;
        ]
      ()
  in
  let committee_size = block.constants.consensus_committee_size in
  let check_level block =
    let* (level : Level.t) = Plugin.RPC.current_level Block.rpc_ctxt block in
    let* selected_bakers = get_selected_bakers ~cycle:level.cycle block in
    let nb_slots = selected_bakers_length selected_bakers in
    let selection = Array.of_list (selected_delegates selected_bakers) in
    let* validators = Plugin.RPC.Validators.get Block.rpc_ctxt block in
    let* delegates =
      match validators with
      | [{Plugin.RPC.Validators.level = rpc_level; delegates; _}] ->
          let* () =
            Assert.equal_int32
              ~loc:__LOC__
              (Raw_level.to_int32 rpc_level)
              (Raw_level.to_int32 level.level)
          in
          return delegates
      | _ ->
          Test.fail
            "SWRR: expected the validators RPC to answer for exactly one level"
    in
    let* checked_rounds =
      List.fold_left_es
        (fun checked {Plugin.RPC.Validators.delegate; rounds; _} ->
          List.fold_left_es
            (fun checked round ->
              let round = Int32.to_int (Round.to_int32 round) in
              let idx =
                (Int32.to_int level.cycle_position + (3 * round)) mod nb_slots
              in
              if Implicit_account_repr.(selection.(idx) = delegate) then
                return (checked + 1)
              else
                Test.fail
                  "SWRR: at level %a round %d the validators RPC gives %a but \
                   slot %d of the selection of cycle %a holds %a"
                  Raw_level.pp
                  level.level
                  round
                  Implicit_account_repr.pp
                  delegate
                  idx
                  Cycle.pp
                  level.cycle
                  Implicit_account_repr.pp
                  selection.(idx))
            checked
            rounds)
        0
        delegates
    in
    let* () = Assert.equal_int ~loc:__LOC__ checked_rounds committee_size in
    return checked_rounds
  in
  (* Walk over more than a full cycle so that every cycle position is covered
     and the cycle boundary is crossed. *)
  let nb_levels = 20 in
  let rec walk n checked block =
    if Compare.Int.(n <= 0) then return checked
    else
      let* checked_at_level = check_level block in
      let* block = Block.bake block in
      walk (n - 1) (checked + checked_at_level) block
  in
  let* checked = walk nb_levels 0 block in
  Log.info
    "SWRR: %d (level, round) pairs checked against the selection over %d levels"
    checked
    nb_levels ;
  return_unit

(* Baking powers of the deactivation scenario, in the ratio 4 : 3 : 2. The
   delegate that gets deactivated is the first one, so it holds 4/9 of the
   baking power: it is guaranteed slots in every cycle it takes part in, and
   the other two are guaranteed enough slots to keep baking without it. *)
let deactivation_balances =
  [4_000_000_000_000L; 3_000_000_000_000L; 2_000_000_000_000L]

(* Prepares the chain by deactivating the first bootstrap delegate.
   Returns the last block before the deactivating cycle end,
   the first block after it, and the delegate. *)
let setup_deactivated_delegate () =
  let open Lwt_result_syntax in
  let* block, bootstrap_contracts =
    Context.init_n
      3
      ~blocks_per_cycle:16l
      ~consensus_threshold_size:0
      ~swrr_new_baker_lottery_enable:true
      ~bootstrap_balances:deactivation_balances
      ()
  in
  let delegate =
    match List.map account_of_contract bootstrap_contracts with
    | delegate :: _ -> delegate
    | [] -> Test.fail "SWRR: expected three bootstrap delegates"
  in
  let* info = Context.Delegate.info (B block) delegate in
  let* () = Assert.equal_bool ~loc:__LOC__ info.deactivated false in
  Log.info
    "SWRR: %a stops participating, grace period ends at cycle %a"
    Implicit_account_repr.pp_short
    delegate
    Cycle.pp
    info.grace_period ;
  let policy = Block.Excluding [delegate] in
  let* block_before = Block.bake_until_cycle ~policy info.grace_period block in
  let* info = Context.Delegate.info (B block_before) delegate in
  let* () = Assert.equal_bool ~loc:__LOC__ info.deactivated false in
  let* block_after = Block.bake_until_cycle_end ~policy block_before in
  let* info = Context.Delegate.info (B block_after) delegate in
  let* () = Assert.equal_bool ~loc:__LOC__ info.deactivated true in
  return (block_before, block_after, delegate)

(** Tests that a delegate deactivated at a cycle end has its SWRR credit reset to zero,
    and nothing brings it back over the following cycles. *)
let test_swrr_deactivated_delegate_has_zero_credit () =
  let open Lwt_result_syntax in
  let* block_before, block, delegate = setup_deactivated_delegate () in
  (* While it was active, the delegate carried a credit over from cycle to
     cycle. The balances above are chosen so that this credit is not zero,
     otherwise the reset below would prove nothing. *)
  let* credit_before = swrr_credit_in_storage block_before delegate in
  let* () =
    match credit_before with
    | Some credit when Compare.Z.(credit <> Z.zero) ->
        Log.info
          "SWRR: %a held a credit of %a before being deactivated"
          Implicit_account_repr.pp_short
          delegate
          Z.pp_print
          credit ;
        return_unit
    | Some credit ->
        Test.fail
          "SWRR: %a already had a zero credit (%a) before being deactivated, \
           the scenario no longer tests the reset"
          Implicit_account_repr.pp
          delegate
          Z.pp_print
          credit
    | None ->
        Test.fail
          "SWRR: no credit stored for the active delegate %a"
          Implicit_account_repr.pp
          delegate
  in
  let check_zero_credit block =
    let* credit = swrr_credit_in_storage block delegate in
    match credit with
    | Some _credit ->
        Test.fail
          "SWRR: the credit of the deactivated delegate %a was not removed \
           when it was deactivated."
          Implicit_account_repr.pp
          delegate
    | None -> return ()
  in
  let* () = check_zero_credit block in
  let rec check_next_cycles n block =
    if Compare.Int.(n <= 0) then return_unit
    else
      let* block =
        Block.bake_until_cycle_end ~policy:(Block.Excluding [delegate]) block
      in
      Log.info
        "SWRR: checking the credit of %a again at cycle %a"
        Implicit_account_repr.pp_short
        delegate
        Cycle.pp
        (Block.current_cycle block) ;
      let* () = check_zero_credit block in
      check_next_cycles (n - 1) block
  in
  check_next_cycles 3 block

(** Tests that a deactivated delegate disappears from the selections computed from its
    deactivation onwards, and only from those. *)
let test_swrr_deactivated_delegate_not_selected () =
  let open Lwt_result_syntax in
  let* _block_before, block, delegate = setup_deactivated_delegate () in
  let consensus_rights_delay = block.constants.consensus_rights_delay in
  let slots_in block cycle =
    let* selected_bakers = get_selected_bakers ~cycle block in
    return (count_selections delegate (selected_delegates selected_bakers))
  in
  let check_absent block cycle =
    let* slots = slots_in block cycle in
    if Compare.Int.(slots = 0) then return_unit
    else
      Test.fail
        "SWRR: the deactivated delegate %a still holds %d slots in cycle %a"
        Implicit_account_repr.pp
        delegate
        slots
        Cycle.pp
        cycle
  in
  (* The deactivation happened at the end of the previous cycle, and a cycle end
     computes the selection of the cycle [consensus_rights_delay + 1] ahead.
     So, of the cycles still in the storage, the newest one is the first that
     was selected without the delegate, and the one before it is the last that
     was selected while the delegate was still active. *)
  let current_cycle = Block.current_cycle block in
  let first_unselected_cycle = Cycle.add current_cycle consensus_rights_delay in
  let last_selected_cycle =
    Cycle.pred first_unselected_cycle |> WithExceptions.Option.get ~loc:__LOC__
  in
  let* slots = slots_in block last_selected_cycle in
  let* () =
    if Compare.Int.(slots > 0) then (
      Log.info
        "SWRR: %a still holds %d slots in cycle %a, the last selection \
         computed while it was active"
        Implicit_account_repr.pp_short
        delegate
        slots
        Cycle.pp
        last_selected_cycle ;
      return_unit)
    else
      Test.fail
        "SWRR: %a is already absent from cycle %a, which was selected while it \
         was still active"
        Implicit_account_repr.pp
        delegate
        Cycle.pp
        last_selected_cycle
  in
  let* () = check_absent block first_unselected_cycle in
  (* And it stays out of the cycles selected afterwards. *)
  let rec check_next_cycles n block =
    if Compare.Int.(n <= 0) then return_unit
    else
      let* block =
        Block.bake_until_cycle_end ~policy:(Block.Excluding [delegate]) block
      in
      let cycle =
        Cycle.add (Block.current_cycle block) consensus_rights_delay
      in
      let* () = check_absent block cycle in
      check_next_cycles (n - 1) block
  in
  check_next_cycles 3 block

(** Tests that a delegate that registers during cycle [c] first appears in the selection
    of cycle [c + consensus_rights_delay + 1], which is the first one computed
    after it acquired a stake, and in none of the earlier ones. *)
let test_swrr_new_baker_appears_at_the_right_cycle () =
  let open Lwt_result_syntax in
  let* block, (funder, _, _) =
    Context.init3
      ~consensus_threshold_size:0
      ~swrr_new_baker_lottery_enable:true
      ()
  in
  let newbie = Account.new_account () in
  (* FIXME-PA *)
  let newbie_delegate =
    Implicit_account_repr.Forbidden.of_pkh Account.(newbie.pkh)
  in
  let newbie_contract = Contract.Implicit newbie_delegate in
  (* Enough to outweigh a bootstrap account once staked, so that the new
     delegate is bound to get slots as soon as it has rights. *)
  let* funding =
    Op.transaction
      (B block)
      funder
      newbie_contract
      (Tez_helpers.of_mutez 3_000_000_000_000L)
  in
  let* block = Block.bake ~operation:funding block in
  let* self_delegation =
    Op.delegation
      ~force_reveal:true
      (B block)
      newbie_contract
      (Some newbie_delegate)
  in
  let* block = Block.bake ~operation:self_delegation block in
  let* stake =
    Adaptive_issuance_helpers.stake
      (B block)
      newbie_contract
      (Tez_helpers.of_mutez 2_500_000_000_000L)
  in
  let* block = Block.bake ~operation:stake block in
  let registration_cycle = Block.current_cycle block in
  let consensus_rights_delay = block.constants.consensus_rights_delay in
  let first_expected_cycle =
    Cycle.add registration_cycle (consensus_rights_delay + 1)
  in
  Log.info
    "SWRR: %a registered during cycle %a, expected to bake from cycle %a on"
    Implicit_account_repr.pp_short
    newbie_delegate
    Cycle.pp
    registration_cycle
    Cycle.pp
    first_expected_cycle ;
  (* Closing the registration cycle is what computes the selection of
     [first_expected_cycle]. *)
  let* block = Block.bake_until_cycle_end block in
  let slots_of cycle =
    let* selected_bakers = get_selected_bakers ~cycle block in
    let selection = selected_delegates selected_bakers in
    return (count_selections newbie_delegate selection)
  in
  let earlier_cycles =
    match Cycle.pred first_expected_cycle with
    | None -> Test.fail "SWRR: the expected cycle should not be the first one"
    | Some pred_cycle -> Cycle.(registration_cycle ---> pred_cycle)
  in
  let* () =
    List.iter_es
      (fun cycle ->
        let* slots = slots_of cycle in
        if Compare.Int.(slots = 0) then return_unit
        else
          Test.fail
            "SWRR: %a already holds %d slots in cycle %a, before its rights \
             start at cycle %a"
            Implicit_account_repr.pp
            newbie_delegate
            slots
            Cycle.pp
            cycle
            Cycle.pp
            first_expected_cycle)
      earlier_cycles
  in
  let* slots = slots_of first_expected_cycle in
  if Compare.Int.(slots > 0) then (
    Log.info
      "SWRR: %a holds %d slots in cycle %a"
      Implicit_account_repr.pp_short
      newbie_delegate
      slots
      Cycle.pp
      first_expected_cycle ;
    return_unit)
  else
    Test.fail
      "SWRR: %a holds no slot in cycle %a, its first cycle with rights"
      Implicit_account_repr.pp
      newbie_delegate
      Cycle.pp
      first_expected_cycle

let register_test ~title =
  Tezt_helpers.register_test_es
    ~__FILE__
    ~file_tags:["sampler"; "swrr"]
    ~title:("SWRR: " ^ title)

let () =
  register_test
    ~title:"SWRR sampler correct length"
    test_swrr_sampler_correct_length ;
  register_test
    ~title:"SWRR sampler no empty value"
    test_swrr_sampler_no_empty_value ;
  register_test
    ~title:"SWRR sampler manual scenario"
    test_swrr_sampler_manual_scenario ;
  register_test ~title:"SWRR sampler tie break" test_swrr_sampler_tie_break ;
  register_test
    ~title:"SWRR sampler check_variance"
    test_swrr_sampler_check_variance ;
  register_test ~title:"SWRR big credit" test_swrr_big_credit ;
  register_test
    ~title:"SWRR get_baker matches the validators RPC"
    test_swrr_get_baker_matches_validators ;
  register_test
    ~title:"SWRR deactivated delegate has a zero credit"
    test_swrr_deactivated_delegate_has_zero_credit ;
  register_test
    ~title:"SWRR deactivated delegate is not selected"
    test_swrr_deactivated_delegate_not_selected ;
  register_test
    ~title:"SWRR new baker appears at the right cycle"
    test_swrr_new_baker_appears_at_the_right_cycle
