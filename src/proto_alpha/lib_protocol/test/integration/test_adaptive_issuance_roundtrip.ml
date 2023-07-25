(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Adaptive Issuance, launch vote
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                   -- --file test_adaptive_issuance_roundtrip.ml
    Subject:      Test staking stability under Adaptive Issuance.
*)

open Adaptive_issuance_helpers

type error += Inconsistent_number_of_bootstrap_accounts

let begin_end_color = Log.Color.(BG.bright_white ++ FG.black ++ bold)

let time_color = Log.Color.FG.yellow

let action_color = Log.Color.FG.green

let event_color = Log.Color.FG.blue

type staking_parameters = {
  limit_of_staking_over_baking : int;
  baking_over_staking_edge : int;
}

let default_params =
  {
    limit_of_staking_over_baking = 1_000_000;
    baking_over_staking_edge = 1_000_000_000;
  }

let initial_bbd =
  {
    liquid = Tez.zero;
    bonds = Tez.zero;
    staked = Q.zero;
    unstaked_frozen = Tez.zero;
    unstaked_finalizable = Tez.zero;
    pool_tez = Tez.zero;
    pool_pseudo = Q.zero;
  }

type stake_value = Half | All | None | Max_tez | Amount of Tez.t

let stake_value_pp fmt value =
  let s =
    match value with
    | None -> "Zero"
    | All -> "All"
    | Half -> "Half"
    | Max_tez -> "Maximum"
    | Amount a -> Format.asprintf "%aꜩ" Tez.pp a
  in
  Format.fprintf fmt "%s" s

type account_info = {
  name : string;
  pkh : Signature.Public_key_hash.t;
  contract : Protocol.Alpha_context.Contract.t;
  delegate : string option;
  parameters : staking_parameters;
  balance : balance_breakdown;
}

type accounts_info = account_info String.Map.t

type info = {
  accounts : string list;
  accounts_info : accounts_info;
  total_supply : Tez.t;
  constants : Protocol.Alpha_context.Constants.Parametric.t;
  unstake_requests :
    (* src, amount, cycles before final *)
    (string * Tez.t * int) list;
  activate_ai : bool;
  staker : string;
  baker : string;
  last_level_rewards : Protocol.Alpha_context.Raw_level.t;
  snapshot_balances : (string * balance_breakdown) list;
}

let find_account account info =
  match String.Map.find account info.accounts_info with
  | None -> assert false
  | Some r -> r

let update_account account value info =
  let accounts_info = String.Map.add account value info.accounts_info in
  {info with accounts_info}

let get_total_staked_and_balance account info =
  let open Lwt_result_syntax in
  let pool_tez, pool_pseudo =
    match account.delegate with
    | None -> (Tez.zero, Q.one)
    | Some string ->
        let {balance = delegate_balance; _} = find_account string info in
        (delegate_balance.pool_tez, delegate_balance.pool_pseudo)
  in
  let total_staked =
    tez_of_staked account.balance.staked ~pool_tez ~pool_pseudo
  in
  let* total_balance =
    total_balance_of_breakdown account.balance ~pool_tez ~pool_pseudo
  in
  return (total_staked, total_balance)

let log_debug_balance account info =
  let open Lwt_result_syntax in
  let* total_staked, total_balance =
    get_total_staked_and_balance account info
  in
  Log.debug
    "Balance of %s:\n%aTez staked: %a\nTotal balance: %a\n"
    account.name
    balance_pp
    account.balance
    Tez.pp
    total_staked
    Tez.pp
    total_balance ;
  return_unit

let log_debug_rpc_balance account block =
  let open Lwt_result_syntax in
  let* balance, total_staked, total_balance =
    get_balance_breakdown (B block) account.contract
  in
  Log.debug
    "RPC balance of %s:\n%aTez staked: %a\nTotal balance: %a\n"
    account.name
    balance_pp
    balance
    Tez.pp
    total_staked
    Tez.pp
    total_balance ;
  return_unit

let log_debug_balance_update (account_before, info_before)
    (account_after, info_after) =
  let open Lwt_result_syntax in
  let* total_staked_bef, total_balance_bef =
    get_total_staked_and_balance account_before info_before
  in

  let* total_staked_aft, total_balance_aft =
    get_total_staked_and_balance account_after info_after
  in
  Log.debug
    "Balance update of %s:\n%aTez staked: %a -> %a\nTotal balance: %a -> %a\n"
    account_before.name
    balance_update_pp
    (account_before.balance, account_after.balance)
    Tez.pp
    total_staked_bef
    Tez.pp
    total_staked_aft
    Tez.pp
    total_balance_bef
    Tez.pp
    total_balance_aft ;
  return_unit

let update_balance ~f account info =
  let open Lwt_result_syntax in
  let* balance = f account.balance in
  let new_account = {account with balance} in
  let new_info = update_account account.name new_account info in
  let* () = log_debug_balance_update (account, info) (new_account, new_info) in
  return new_info

let update_balance_2 ~f account1 account2 info =
  let open Lwt_result_syntax in
  let* balance1, balance2 = f (account1.balance, account2.balance) in
  let new_account1 = {account1 with balance = balance1} in
  let new_account2 = {account2 with balance = balance2} in
  let new_info = update_account account1.name new_account1 info in
  let new_info = update_account account2.name new_account2 new_info in
  let* () =
    log_debug_balance_update (account1, info) (new_account1, new_info)
  in
  let* () =
    log_debug_balance_update (account2, info) (new_account2, new_info)
  in
  return new_info

(* Threaded context for the tests. Contains the block, as well as various
   informations on the state of the current test. *)
type t = Block.t * info

(* Must be applied every block if testing when ai activated and rewards != 0 *)
let apply_rewards_info current_level info =
  let open Lwt_result_syntax in
  let {last_level_rewards; total_supply; constants; _} = info in
  (* We assume one block per minute *)
  let rewards_per_block =
    constants.issuance_weights.base_total_issued_per_minute
  in
  if Tez.(rewards_per_block = zero) then return info
  else
    let delta_time =
      Protocol.Alpha_context.Raw_level.diff current_level last_level_rewards
      |> Int32.to_int
    in
    let ({parameters; _} as baker) = find_account info.baker info in
    let delta_rewards = Tez.mul_exn rewards_per_block delta_time in
    let to_liquid =
      Tez.(
        div_exn
          (mul_exn delta_rewards parameters.baking_over_staking_edge)
          1_000_000_000)
    in
    let* to_frozen = Tez.(delta_rewards - to_liquid) in
    let* info = update_balance ~f:(add_liquid_rewards to_liquid) baker info in
    let* info = update_balance ~f:(add_frozen_rewards to_frozen) baker info in
    let* total_supply = Tez.(total_supply + delta_rewards) in
    return {info with last_level_rewards = current_level; total_supply}

let check_all_balances (block, info) =
  let open Lwt_result_syntax in
  let {accounts_info; total_supply; _} = info in
  let* () =
    String.Map.iter_es
      (fun _name ({balance; delegate; contract; _} as account) ->
        let pool_tez, pool_pseudo =
          match delegate with
          | None -> (Tez.zero, Q.one)
          | Some string ->
              let {balance = delegate_balance; _} = find_account string info in
              (delegate_balance.pool_tez, delegate_balance.pool_pseudo)
        in
        let* () = log_debug_rpc_balance account block in
        assert_balance_breakdown
          ~loc:__LOC__
          (B block)
          contract
          balance
          ~pool_tez
          ~pool_pseudo)
      accounts_info
  in
  let* actual_total_supply = Context.get_total_supply (B block) in
  Assert.equal_tez ~loc:__LOC__ actual_total_supply total_supply

let apply_rewards (block, info) =
  let open Lwt_result_syntax in
  let*? current_level = Context.get_level (B block) in
  let* info = apply_rewards_info current_level info in
  let input = (block, info) in
  let* () = check_all_balances input in
  return input

type ('input, 'output) action =
  | Do (* arbitrary action *) :
      ('input -> 'output tzresult Lwt.t)
      -> ('input, 'output) action
  | Noop : ('input, 'input) action
  | Set_delegate_params : (string * staking_parameters) -> (t, t) action
  | Add_account : (* name of new account *) string -> (t, t) action
  | Reveal : string -> (t, t) action
  | Transfer :
      (* src, dest, amount *) (string * string * Tez.t)
      -> (t, t) action
  | Set_delegate : (* src, dest *) (string * string) -> (t, t) action
  | Unset_delegate : string -> (t, t) action
  | Stake : (string * stake_value) -> (t, t) action
  | Unstake : (string * stake_value) -> (t, t) action
  | Finalize_unstake : string -> (t, t) action
  | Next_block : (t, t) action
  | Next_cycle : (t, t) action
  | End_test : (t, unit) action
  | Begin_test :
      (* parametrs, list of names for delegates, activate_ai flag *)
      (Protocol.Alpha_context.Constants.Parametric.t * string list * bool)
      -> (unit, t) action

let set_staker staker : (t, t) action =
  Do (fun (block, info) -> return (block, {info with staker}))

let with_staker = "#staker#" (* Special staker/unstaker value *)

let resolve_name s (_, info) =
  if String.equal s with_staker then info.staker else s

let set_baker baker : (t, t) action =
  Do (fun (block, info) -> return (block, {info with baker}))

let apply_end_cycle_info info =
  let open Lwt_result_syntax in
  let* info, unstake_requests =
    List.fold_left_es
      (fun (info, remaining_requests) (name, amount, cd) ->
        if cd > 0 then
          return (info, (name, amount, cd - 1) :: remaining_requests)
        else
          let src = find_account name info in
          let* info = update_balance ~f:(apply_unslashable amount) src info in
          return (info, remaining_requests))
      (info, [])
      info.unstake_requests
  in
  return {info with unstake_requests}

let bake ?operation (block, info) =
  let open Lwt_result_syntax in
  Log.info
    ~color:time_color
    "Baking level %d"
    (Int32.to_int (Int32.succ Block.(block.header.shell.level))) ;
  let current_cycle = Block.current_cycle block in
  let adaptive_issuance_vote =
    if info.activate_ai then
      Protocol.Alpha_context.Per_block_votes.Per_block_vote_on
    else Per_block_vote_pass
  in
  let baker =
    try find_account info.baker info
    with _ ->
      Log.info "Invalid baker: %s not found. Aborting" info.baker ;
      assert false
  in
  let policy = Block.By_account baker.pkh in
  let* block = Block.bake ~policy ~adaptive_issuance_vote ?operation block in
  let new_current_cycle = Block.current_cycle block in
  let* input =
    if Protocol.Alpha_context.Cycle.(current_cycle = new_current_cycle) then
      return (block, info)
    else
      let* info = apply_end_cycle_info info in
      return (block, info)
  in
  apply_rewards input

let bake_until_cycle_end_slow ((init_block, _) as init_input) =
  let open Lwt_result_syntax in
  let current_cycle = Block.current_cycle init_block in
  let rec step ((old_block, _) as old_input) =
    let step_cycle = Block.current_cycle old_block in
    if Protocol.Alpha_context.Cycle.(step_cycle > current_cycle) then
      return old_input
    else
      let* new_input = bake old_input in
      step new_input
  in
  step init_input

let snapshot_balances names_list =
  Do
    (fun (block, info) ->
      let snapshot_balances =
        List.map
          (fun name -> (name, (find_account name info).balance))
          names_list
      in
      return (block, {info with snapshot_balances}))

let check_snapshot_balances =
  Do
    (fun ((_, info) as input) ->
      let open Lwt_result_syntax in
      let* () =
        List.iter_es
          (fun (name, old_balance) ->
            let new_balance = (find_account name info).balance in
            assert_balance_equal ~loc:__LOC__ old_balance new_balance)
          info.snapshot_balances
      in
      return input)

let unstake_value_to_tez src unstake_value info =
  match src.delegate with
  | None -> (
      match unstake_value with
      | Amount a -> a
      | Max_tez -> Tez.max_mutez
      | _ -> Tez.zero)
  | Some delegate_name -> (
      let delegate = find_account delegate_name info in
      let pool_tez = delegate.balance.pool_tez in
      let pool_pseudo = delegate.balance.pool_pseudo in
      match unstake_value with
      | None -> Tez.zero
      | All -> tez_of_staked src.balance.staked ~pool_tez ~pool_pseudo
      | Half ->
          tez_of_staked (Q.div_2exp src.balance.staked 1) ~pool_tez ~pool_pseudo
      | Max_tez -> Tez.max_mutez
      | Amount a -> a)

let apply_unstake_info src unstake_value info =
  let open Lwt_result_syntax in
  match src.delegate with
  | None -> return info
  | Some delegate_name ->
      let amount = unstake_value_to_tez src unstake_value info in
      if Tez.(amount = zero) then return info
      else
        let delegate = find_account delegate_name info in
        let old_unstaked = src.balance.unstaked_frozen in
        let* ({constants; _} as info) =
          if String.equal src.name delegate.name then
            update_balance ~f:(apply_self_unstake amount) src info
          else update_balance_2 ~f:(apply_unstake amount) src delegate info
        in
        let new_src = find_account src.name info in
        let cd =
          constants.preserved_cycles + constants.max_slashing_period - 1
        in
        let* actual_amount =
          Tez.(new_src.balance.unstaked_frozen - old_unstaked)
        in
        let unstake_requests =
          (src.name, actual_amount, cd) :: info.unstake_requests
        in
        return {info with unstake_requests}

let apply_stake_info src amount info =
  let open Lwt_result_syntax in
  match src.delegate with
  | None -> return info
  | Some delegate_name ->
      let delegate = find_account delegate_name info in
      if String.equal src.name delegate.name then
        update_balance ~f:(apply_self_stake amount) src info
      else update_balance_2 ~f:(apply_stake amount) src delegate info

let run_action :
    type input output. (input, output) action -> input -> output tzresult Lwt.t
    =
 fun action input ->
  let open Lwt_result_syntax in
  match action with
  | Do f -> f input
  | Noop -> return input
  | Begin_test (constants, delegates_name_list, activate_ai) ->
      Log.info ~color:begin_end_color "-- Begin test --" ;
      let bootstrap = "__bootstrap__" in
      let delegates_name_list = bootstrap :: delegates_name_list in
      (* Override threshold value if activate *)
      let constants =
        if activate_ai then (
          Log.info ~color:event_color "Setting ai threshold to 0" ;
          {
            constants with
            adaptive_issuance =
              {constants.adaptive_issuance with launch_ema_threshold = 0l};
          })
        else constants
      in
      let n = List.length delegates_name_list in
      let* block, delegates = Context.init_with_constants_n constants n in
      let*? init_level = Context.get_level (B block) in
      let init_staked = Tez.of_mutez 200_000_000_000L in
      let* accounts_info =
        List.fold_left2_es
          ~when_different_lengths:[Inconsistent_number_of_bootstrap_accounts]
          (fun accounts_info name contract ->
            let balance =
              {initial_bbd with liquid = Account.default_initial_balance}
            in
            let* balance = apply_self_stake init_staked balance in
            let pkh = Context.Contract.pkh contract in
            let account =
              {
                name;
                pkh;
                contract;
                balance;
                delegate = Some name;
                parameters = default_params;
              }
            in
            let {pool_tez; pool_pseudo; staked; _} = account.balance in
            let total_staked = tez_of_staked staked ~pool_tez ~pool_pseudo in
            let* total_balance =
              total_balance_of_breakdown account.balance ~pool_tez ~pool_pseudo
            in
            Log.debug "Initial balance for %s:\n%a" name balance_pp balance ;
            Log.debug "Initial stake: %a" Tez.pp total_staked ;
            Log.debug "Initial total balance: %a" Tez.pp total_balance ;
            return (String.Map.add name account accounts_info))
          String.Map.empty
          delegates_name_list
          delegates
      in
      let baker = bootstrap in
      let* total_supply = Context.get_total_supply (B block) in
      let info =
        {
          accounts = delegates_name_list;
          accounts_info;
          total_supply;
          constants;
          unstake_requests = [];
          activate_ai;
          staker = "";
          baker;
          last_level_rewards = init_level;
          snapshot_balances = [];
        }
      in
      let* () = check_all_balances (block, info) in
      return (block, info)
  | End_test ->
      Log.info ~color:begin_end_color "-- End test --" ;
      return_unit
  | Next_block ->
      Log.info ~color:action_color "[Next block]" ;
      bake input
  | Next_cycle ->
      Log.info ~color:action_color "[Next cycle]" ;
      let block, ({constants; activate_ai; _} as info) = input in
      if
        Tez.(constants.issuance_weights.base_total_issued_per_minute = zero)
        || not activate_ai
      then
        (* Apply rewards in info only after the while cycle ends *)
        let baker = find_account info.baker info in
        let policy = Block.By_account baker.pkh in
        let* block = Block.bake_until_cycle_end ~policy block in
        let* info = apply_end_cycle_info info in
        apply_rewards (block, info)
      else
        (* Apply rewards in info every block *)
        bake_until_cycle_end_slow input
  | Set_delegate_params (delegate_name, params) ->
      let delegate_name = resolve_name delegate_name input in
      Log.info
        ~color:action_color
        "[Set delegate parameters for \"%s\"]"
        delegate_name ;
      let block, info = input in
      let delegate = find_account delegate_name info in
      let* operation =
        set_delegate_parameters
          (B block)
          delegate.contract
          ~limit_of_staking_over_baking:params.limit_of_staking_over_baking
          ~edge_of_baking_over_staking_billionth:params.baking_over_staking_edge
      in
      let* block, info = bake ~operation (block, info) in
      let info =
        update_account delegate_name {delegate with parameters = params} info
      in
      return (block, info)
  | Add_account name ->
      let name = resolve_name name input in
      Log.info ~color:action_color "[Add account \"%s\"]" name ;
      let block, info = input in
      let new_account = Account.new_account () in
      let pkh = new_account.pkh in
      let contract = Protocol.Alpha_context.Contract.Implicit pkh in
      let account_info =
        {
          name;
          pkh;
          contract;
          delegate = None;
          parameters = default_params;
          balance = initial_bbd;
        }
      in
      let info = update_account name account_info info in
      let info = {info with accounts = name :: info.accounts} in
      return (block, info)
  | Reveal name ->
      let name = resolve_name name input in
      Log.info ~color:action_color "[Reveal \"%s\"]" name ;
      let block, info = input in
      let account = find_account name info in
      let* acc = Account.find account.pkh in
      let* operation =
        Op.revelation ~fee:Protocol.Alpha_context.Tez.zero (B block) acc.pk
      in
      bake ~operation (block, info)
  | Transfer (src_name, dst_name, amount) ->
      let src_name = resolve_name src_name input in
      let dst_name = resolve_name dst_name input in
      Log.info
        ~color:action_color
        "[Transfer \"%s\" -> \"%s\" (%aꜩ)]"
        src_name
        dst_name
        Tez.pp
        amount ;
      let block, info = input in
      let src = find_account src_name info in
      let dst = find_account dst_name info in
      let* operation =
        Op.transaction ~fee:Tez.zero (B block) src.contract dst.contract amount
      in
      let* info = update_balance_2 ~f:(apply_transfer amount) src dst info in
      let* block, info = bake ~operation (block, info) in
      return (block, info)
  | Set_delegate (src_name, delegate_name) ->
      let src_name = resolve_name src_name input in
      Log.info
        ~color:action_color
        "[Set delegate \"%s\" for \"%s\"]"
        delegate_name
        src_name ;
      let block, info = input in
      let src = find_account src_name info in
      let delegate = find_account delegate_name info in
      let* operation =
        Op.delegation ~fee:Tez.zero (B block) src.contract (Some delegate.pkh)
      in
      let* info = apply_unstake_info src Max_tez info in
      let info =
        update_account src_name {src with delegate = Some delegate_name} info
      in
      let* block, info = bake ~operation (block, info) in
      return (block, info)
  | Unset_delegate src_name ->
      let src_name = resolve_name src_name input in
      Log.info ~color:action_color "[Unset delegate of \"%s\"]" src_name ;
      let block, info = input in
      let src = find_account src_name info in
      let* operation =
        Op.delegation ~fee:Tez.zero (B block) src.contract None
      in
      let* info = apply_unstake_info src Max_tez info in
      let info = update_account src_name {src with delegate = None} info in
      let* block, info = bake ~operation (block, info) in
      return (block, info)
  | Stake (src_name, stake_value) ->
      let src_name = resolve_name src_name input in
      Log.info
        ~color:action_color
        "[Stake for \"%s\" (%a)]"
        src_name
        stake_value_pp
        stake_value ;
      let block, info = input in
      let src = find_account src_name info in
      let amount =
        match stake_value with
        | None -> Tez.zero
        | All -> src.balance.liquid
        | Half -> Tez.div_exn src.balance.liquid 2
        | Max_tez -> Tez.max_mutez
        | Amount a -> a
      in
      let* operation = stake (B block) src.contract amount in
      let* info = apply_stake_info src amount info in
      let* block, info = bake ~operation (block, info) in
      return (block, info)
  | Unstake (src_name, unstake_value) ->
      let src_name = resolve_name src_name input in

      Log.info
        ~color:action_color
        "[Unstake for \"%s\" (%a)]"
        src_name
        stake_value_pp
        unstake_value ;
      let block, info = input in
      let src = find_account src_name info in
      let amount = unstake_value_to_tez src unstake_value info in
      let* operation = unstake (B block) src.contract amount in
      let* info = apply_unstake_info src unstake_value info in
      let* block, info = bake ~operation (block, info) in
      return (block, info)
  | Finalize_unstake src_name ->
      let src_name = resolve_name src_name input in
      Log.info ~color:action_color "[Finalize_unstake for \"%s\"]" src_name ;
      let block, info = input in
      let src = find_account src_name info in
      let* operation = finalize_unstake (B block) src.contract in
      let* info = update_balance ~f:apply_finalize src info in
      let* block, info = bake ~operation (block, info) in
      return (block, info)

type ('input, 'output) scenarios =
  | Action : ('input, 'output) action -> ('input, 'output) scenarios
  | Empty : ('t, 't) scenarios
  | Concat : (('a, 'b) scenarios * ('b, 'c) scenarios) -> ('a, 'c) scenarios
  | Branch : (('a, 'b) scenarios * ('a, 'b) scenarios) -> ('a, 'b) scenarios
  | Tag : (* Name for test branch *) string -> ('t, 't) scenarios
  | Slow : (* If in scenario branch, makes the test `Slow *)
      ('t, 't) scenarios

type ('input, 'output) single_scenario =
  | End : ('t, 't) single_scenario
  | Cons :
      (('input, 't) action * ('t, 'output) single_scenario)
      -> ('input, 'output) single_scenario

let rec cat_ss :
    type a b c.
    (a, b) single_scenario -> (b, c) single_scenario -> (a, c) single_scenario =
 fun a b -> match a with End -> b | Cons (act, a') -> Cons (act, cat_ss a' b)

let combine f l1 l2 =
  List.map (fun a -> List.map (fun b -> f a b) l2) l1 |> List.flatten

let rec unfold_scenarios :
    type input output.
    (input, output) scenarios ->
    ((input, output) single_scenario * string list * bool) list = function
  | Slow -> [(End, [], true)]
  | Tag s -> [(End, [s], false)]
  | Empty -> [(End, [], false)]
  | Action a -> [(Cons (a, End), [], false)]
  | Branch (left, right) -> unfold_scenarios left @ unfold_scenarios right
  | Concat (left, right) ->
      let l = unfold_scenarios left in
      let r = unfold_scenarios right in
      combine
        (fun (sl, tl, bl) (sr, tr, br) -> (cat_ss sl sr, tl @ tr, bl || br))
        l
        r

let rec run_scenario :
    type input output.
    (input, output) single_scenario -> input -> output tzresult Lwt.t =
 fun scenario input ->
  match scenario with
  | End -> return input
  | Cons (action, next) -> run_action action input >>=? run_scenario next

let unfolded_to_test :
    (unit, unit) single_scenario * string list * bool ->
    unit Alcotest_lwt.test_case =
 fun (s, name, b) ->
  let speed = if b then `Slow else `Quick in
  let name =
    match name with
    | [] -> ""
    | [n] -> n
    | title :: tags -> title ^ ": " ^ String.concat ", " tags
  in
  Tztest.tztest name speed (run_scenario s)

let noop = Empty

let ( --> ) a b = Concat (a, Action b)

let ( ---> ) a b = Concat (a, b)

let ( |+ ) a b = Branch (a, b)

let tests_of_scenarios :
    (string * (unit, t) scenarios) list -> unit Alcotest_lwt.test_case list =
 fun scenarios ->
  List.map (fun (s, x) -> Tag s ---> x --> End_test) scenarios |> function
  | [] -> []
  | a :: t ->
      List.fold_left ( |+ ) a t |> unfold_scenarios |> List.map unfolded_to_test

(*****************************************************************************)

let rec wait_n_cycles n =
  if n <= 0 then noop
  else if n = 1 then Action Next_cycle
  else wait_n_cycles (n - 1) --> Next_cycle

let rec wait_n_blocks n =
  if n <= 0 then noop
  else if n = 1 then Action Next_block
  else wait_n_blocks (n - 1) --> Next_block

let add_account_with_funds name source amount =
  noop --> Add_account name --> Transfer (source, name, amount) --> Reveal name

let init_constants ?reward_per_block () =
  let reward_per_block = Option.value ~default:0L reward_per_block in
  let base_total_issued_per_minute = Tez.of_mutez reward_per_block in
  let default_constants = Default_parameters.constants_test in
  let issuance_weights =
    Protocol.Alpha_context.Constants.Parametric.
      {
        base_total_issued_per_minute;
        baking_reward_fixed_portion_weight = 1;
        baking_reward_bonus_weight = 0;
        attesting_reward_weight = 0;
        liquidity_baking_subsidy_weight = 0;
        seed_nonce_revelation_tip_weight = 0;
        vdf_revelation_tip_weight = 0;
      }
  in
  let minimal_block_delay = Protocol.Alpha_context.Period.one_minute in
  let cost_per_byte = Tez.zero in
  let consensus_threshold = 0 in
  {
    default_constants with
    consensus_threshold;
    issuance_weights;
    minimal_block_delay;
    cost_per_byte;
  }

let init_scenario ?reward_per_block () =
  let open Lwt_result_syntax in
  let constants = init_constants ?reward_per_block () in
  (* TODO LATER:
     if rewards != 0, info is incorrect -> reset info balances after wait *)
  let wait_ai_activation (block, info) =
    Log.info ~color:time_color "Fast forward to AI activation" ;
    let* block =
      if info.activate_ai then
        let* launch_cycle = get_launch_cycle ~loc:__LOC__ block in
        (* Bake until the activation. *)
        Block.bake_until_cycle launch_cycle block
      else assert false
    in
    Log.info ~color:event_color "AI activated" ;
    return (block, info)
  in
  (Tag "AI activated"
   --> Begin_test (constants, ["delegate"], true)
   --> Set_delegate_params ("delegate", default_params)
   --> Stake ("delegate", Amount (Tez.of_mutez 1_800_000_000_000L))
   ---> (Tag "self stake" --> set_staker "delegate"
        |+ Tag "external stake"
           ---> add_account_with_funds
                  "staker"
                  "delegate"
                  (Tez.of_mutez 2_000_000_000_000L)
           --> Set_delegate ("staker", "delegate")
           --> set_staker "staker")
   --> Do wait_ai_activation
  |+ Tag "AI disactivated, self stake"
     --> Begin_test (constants, ["delegate"], true)
     --> Set_delegate_params ("delegate", default_params)
     --> Stake ("delegate", Amount (Tez.of_mutez 1_800_000_000_000L))
     --> set_staker "delegate")
  --> Next_cycle

let simple_roundtrip =
  noop
  --> Stake (with_staker, Half)
  ---> (Tag "no wait after stake" --> Noop
       |+ Tag "wait after stake" ---> wait_n_cycles 2)
  ---> (Tag "half unstake"
        --> Unstake (with_staker, Half)
        ---> (Tag "then half unstake" ---> wait_n_cycles 2
              --> Unstake (with_staker, Half)
             |+ Tag "then unstake rest" ---> wait_n_cycles 2
                --> Unstake (with_staker, All)
             |+ Empty)
       |+ Tag "full unstake" --> Unstake (with_staker, All))
  ---> wait_n_cycles 8 --> Finalize_unstake with_staker --> Next_cycle

let tests =
  tests_of_scenarios
  @@ [
       ("Test init", init_scenario ());
       ("Test simple roundtrip", init_scenario () ---> simple_roundtrip);
     ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [("adaptive issuance roundtrip", tests)]
  |> Lwt_main.run
