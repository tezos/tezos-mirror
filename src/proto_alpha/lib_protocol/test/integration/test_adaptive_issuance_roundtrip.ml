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

let fs = Format.asprintf

(** Returns when the number of bootstrap accounts created by [Context.init_n n] is not equal to [n] *)
type error += Inconsistent_number_of_bootstrap_accounts

(** For [assert_failure], when expected error does not match the actual error. *)
type error += Unexpected_error

let default_param_wait, default_unstake_wait =
  let constants = Default_parameters.constants_test in
  let pc = constants.preserved_cycles in
  let msp = Protocol.Constants_repr.max_slashing_period in
  (pc + 1, pc + msp)

(** Contains the functions and constants relative to logging.*)
module Log_module = struct
  let begin_end_color = Log.Color.(BG.bright_white ++ FG.black ++ bold)

  let time_color = Log.Color.FG.yellow

  let action_color = Log.Color.FG.green

  let event_color = Log.Color.FG.blue

  let warning_color = Log.Color.FG.red

  let low_debug_color = Log.Color.FG.gray

  let assert_block_color = Log.Color.(BG.blue ++ FG.gray)

  let tez_color = Log.Color.FG.bright_white

  let log_debug_balance account_name account_map : unit =
    let balance, total_balance =
      balance_and_total_balance_of_account account_name account_map
    in
    Log.debug
      "Model balance of %s:\n%aTotal balance: %a\n"
      account_name
      balance_pp
      balance
      Tez.pp
      total_balance

  let log_debug_rpc_balance name contract block : unit tzresult Lwt.t =
    let open Lwt_result_syntax in
    let* balance, total_balance = get_balance_from_context (B block) contract in
    Log.debug
      "RPC balance of %s:\n%aTotal balance: %a\n"
      name
      balance_pp
      balance
      Tez.pp
      total_balance ;
    return_unit

  let log_debug_balance_update account_name old_account_map new_account_map :
      unit =
    let old_balance, old_total_balance =
      balance_and_total_balance_of_account account_name old_account_map
    in
    let new_balance, new_total_balance =
      balance_and_total_balance_of_account account_name new_account_map
    in
    Log.debug
      "Balance update of %s:\n%aTotal balance: %a -> %a\n"
      account_name
      balance_update_pp
      (old_balance, new_balance)
      Tez.pp
      old_total_balance
      Tez.pp
      new_total_balance

  (* end module Log_module *)
end

open Log_module

(** Aliases for tez values *)
type tez_quantity =
  | Half
  | All
  | All_but_one
  | Nothing
  | Max_tez
  | Amount of Tez.t

let tez_quantity_pp fmt value =
  let s =
    match value with
    | Nothing -> "Zero"
    | All -> "All"
    | All_but_one -> "All but 1µꜩ"
    | Half -> "Half"
    | Max_tez -> "Maximum"
    | Amount a -> Format.asprintf "%aꜩ" Tez.pp a
  in
  Format.fprintf fmt "%s" s

(* [all] is the amount returned when [qty = All]. If [qty = Half], returns half of that. *)
let quantity_to_tez all qty =
  match qty with
  | Nothing -> Tez.zero
  | All -> all
  | All_but_one ->
      if Tez.(equal all zero) then Tez.zero else Tez.(all -! one_mutez)
  | Half -> Test_tez.(all /! 2L)
  | Max_tez -> Tez.max_mutez
  | Amount a -> a

let default_params =
  let Protocol.Staking_parameters_repr.
        {
          limit_of_staking_over_baking_millionth;
          edge_of_baking_over_staking_billionth;
        } =
    Protocol.Staking_parameters_repr.default
  in
  {
    limit_of_staking_over_baking =
      Q.(Int32.to_int limit_of_staking_over_baking_millionth // 1_000_000);
    edge_of_baking_over_staking =
      Q.(Int32.to_int edge_of_baking_over_staking_billionth // 1_000_000_000);
  }

type double_signing_kind =
  | Double_baking
  | Double_attesting
  | Double_preattesting

type double_signing_state = {
  culprit : Signature.Public_key_hash.t;
  kind : double_signing_kind;
  evidence : Context.t -> Protocol.Alpha_context.packed_operation;
  denounced : bool;
  level : Int32.t;
}

(** Module for the [State.t] type of asserted information about the system during a test. *)
module State = struct
  (** Type of the state *)
  type t = {
    account_map : account_map;
    total_supply : Tez.t;
    constants : Protocol.Alpha_context.Constants.Parametric.t;
    param_requests : (string * staking_parameters * int) list;
    activate_ai : bool;
    baking_policy : Block.baker_policy option;
    last_level_rewards : Protocol.Alpha_context.Raw_level.t;
    snapshot_balances : (string * balance) list String.Map.t;
    saved_rate : Q.t option;
    burn_rewards : bool;
    pending_operations : Protocol.Alpha_context.packed_operation list;
    pending_slashes :
      (Signature.Public_key_hash.t * Protocol.Denunciations_repr.item) list;
    double_signings : double_signing_state list;
  }

  (** Expected number of cycles before staking parameters get applied *)
  let param_wait state = state.constants.preserved_cycles + 1

  (** Expected number of cycles before staking unstaked funds get unfrozen *)
  let unstake_wait state =
    let pc = state.constants.preserved_cycles in
    let msp = Protocol.Constants_repr.max_slashing_period in
    pc + msp

  (** From a name, returns the corresponding account *)
  let find_account (account_name : string) (state : t) : account_state =
    match String.Map.find account_name state.account_map with
    | None -> raise Not_found
    | Some r -> r

  let find_account_from_pkh (pkh : Signature.public_key_hash) (state : t) :
      string * account_state =
    String.Map.filter
      (fun _ acc -> Signature.Public_key_hash.equal pkh acc.pkh)
      state.account_map
    |> String.Map.choose
    |> function
    | None -> raise Not_found
    | Some (name, acc) -> (name, acc)

  let liquid_delegated ~name state =
    let open Result_syntax in
    String.Map.fold_e
      (fun _delegator account acc ->
        match account.delegate with
        | Some delegate when not @@ String.equal delegate name -> return acc
        | None -> return acc
        | _ -> Tez.(acc +? account.liquid))
      state.account_map
      Tez.zero

  (** Returns true iff account is a delegate *)
  let is_self_delegate (account_name : string) (state : t) : bool =
    let acc = find_account account_name state in
    match acc.delegate with
    | None -> false
    | Some del_name -> String.equal del_name account_name

  let update_map ?(log_updates = []) ~(f : account_map -> account_map)
      (state : t) : t =
    let log_updates = List.sort_uniq String.compare log_updates in
    let new_state = {state with account_map = f state.account_map} in
    List.iter
      (fun x ->
        log_debug_balance_update x state.account_map new_state.account_map)
      log_updates ;
    new_state

  let apply_burn amount src_name (state : t) : t =
    let f = apply_burn amount src_name in
    let state = update_map ~log_updates:[src_name] ~f state in
    {state with total_supply = Tez.(state.total_supply -! amount)}

  let apply_transfer amount src_name dst_name (state : t) : t =
    let f = apply_transfer amount src_name dst_name in
    update_map ~log_updates:[src_name; dst_name] ~f state

  let apply_stake amount current_cycle staker_name (state : t) : t =
    let f =
      apply_stake
        amount
        current_cycle
        state.constants.preserved_cycles
        staker_name
    in
    update_map ~log_updates:[staker_name] ~f state

  let apply_unstake cycle amount staker_name (state : t) : t =
    let f = apply_unstake cycle amount staker_name in
    update_map ~log_updates:[staker_name] ~f state

  let apply_finalize staker_name (state : t) : t =
    let f = apply_finalize staker_name in
    update_map ~log_updates:[staker_name] ~f state

  let apply_unslashable current_cycle account_name (state : t) : t =
    let unstake_wait = unstake_wait state in
    match Cycle.sub current_cycle unstake_wait with
    | None -> state
    | Some cycle ->
        let f = apply_unslashable cycle account_name in
        update_map ~log_updates:[account_name] ~f state

  let apply_unslashable_for_all current_cycle (state : t) : t =
    let unstake_wait = unstake_wait state in
    match Cycle.sub current_cycle unstake_wait with
    | None -> state
    | Some cycle ->
        let f = apply_unslashable_for_all cycle in
        (* no log *)
        update_map ~f state

  let apply_rewards ~(baker : string) block (state : t) : t tzresult Lwt.t =
    let open Lwt_result_syntax in
    let {last_level_rewards; total_supply; constants = _; _} = state in
    let*? current_level = Context.get_level (B block) in
    let current_cycle = Block.current_cycle block in
    (* We assume one block per minute *)
    let* rewards_per_block = Context.get_issuance_per_minute (B block) in
    if Tez.(rewards_per_block = zero) then return state
    else
      let delta_time =
        Protocol.Alpha_context.Raw_level.diff current_level last_level_rewards
        |> Int64.of_int32
      in
      let {parameters = _; pkh; _} = find_account baker state in
      let delta_rewards = Test_tez.(rewards_per_block *! delta_time) in
      if delta_time = 1L then
        Log.info ~color:tez_color "+%aꜩ" Tez.pp rewards_per_block
      else assert false ;
      let* to_liquid =
        portion_of_rewards_to_liquid_for_cycle
          ?policy:state.baking_policy
          (B block)
          current_cycle
          pkh
          delta_rewards
      in
      let to_frozen = Tez.(delta_rewards -! to_liquid) in
      let state = update_map ~f:(add_liquid_rewards to_liquid baker) state in
      let state = update_map ~f:(add_frozen_rewards to_frozen baker) state in
      let* total_supply = Tez.(total_supply + delta_rewards) in
      return {state with last_level_rewards = current_level; total_supply}

  (** [apply_staking_abstract_balance_updates] updates a state based on balance
      updates (found in block application metadata).
      It first collect all changes on pseudotokens, then apply them on
      accounts. *)
  let apply_staking_abstract_balance_updates balance_updates state =
    let update_staking_delegator_numerator delta account_state =
      let staking_delegator_numerator =
        Z.add delta account_state.staking_delegator_numerator
      in
      {account_state with staking_delegator_numerator}
    in
    let update_staking_delegate_denominator delta account_state =
      let staking_delegate_denominator =
        Z.add delta account_state.staking_delegate_denominator
      in
      {account_state with staking_delegate_denominator}
    in
    let add_change pkh update ~f changes =
      let delta_pt, delta_mul =
        match
          (update
            : Protocol.Alpha_context.Staking_pseudotoken.t
              Protocol.Alpha_context.Receipt.balance_update)
        with
        | Credited pt -> (pt, Z.one)
        | Debited pt -> (pt, Z.minus_one)
      in
      let delta =
        Z.mul delta_mul
        @@ Protocol.Alpha_context.Staking_pseudotoken.Internal_for_tests.to_z
             delta_pt
      in
      let f = f delta in
      Signature.Public_key_hash.Map.update
        pkh
        (function
          | None -> Some f
          | Some existing_change ->
              Some (fun account_state -> f (existing_change account_state)))
        changes
    in
    let changes =
      List.fold_left
        (fun changes balance_update ->
          let (Protocol.Alpha_context.Receipt.Balance_update_item
                (balance, update, _origin)) =
            balance_update
          in
          match balance with
          | Staking_delegator_numerator {delegator} -> (
              match delegator with
              | Originated _ -> assert false
              | Implicit pkh ->
                  add_change
                    pkh
                    update
                    changes
                    ~f:update_staking_delegator_numerator)
          | Staking_delegate_denominator {delegate} ->
              add_change
                delegate
                update
                changes
                ~f:update_staking_delegate_denominator
          | _ -> (
              match Protocol.Alpha_context.Receipt.token_of_balance balance with
              | Tez -> changes
              | Staking_pseudotoken -> assert false))
        Signature.Public_key_hash.Map.empty
        balance_updates
    in
    let update_account account_state =
      match Signature.Public_key_hash.Map.find account_state.pkh changes with
      | None -> account_state
      | Some f -> f account_state
    in
    let log_updates =
      List.map
        (fun (x, _) -> fst @@ find_account_from_pkh x state)
        (Signature.Public_key_hash.Map.bindings changes)
    in
    update_map ~log_updates ~f:(String.Map.map update_account) state

  let apply_slashing
      ( culprit,
        Protocol.Denunciations_repr.
          {rewarded; misbehaviour; misbehaviour_cycle; operation_hash} )
      current_cycle (state : t) : t * Tez.t =
    let account_map, total_burnt =
      apply_slashing
        (culprit, {rewarded; misbehaviour; misbehaviour_cycle; operation_hash})
        current_cycle
        state.constants
        state.account_map
    in
    (* TODO: add culprit's stakers *)
    let log_updates =
      List.map
        (fun x -> fst @@ find_account_from_pkh x state)
        [culprit; rewarded]
    in
    let state = update_map ~log_updates ~f:(fun _ -> account_map) state in
    (state, total_burnt)

  let apply_all_slashes_at_cycle_end current_cycle (state : t) : t =
    let state, total_burnt =
      List.fold_left
        (fun (acc_state, acc_total) x ->
          let state, burnt = apply_slashing x current_cycle acc_state in
          (state, Tez.(acc_total +! burnt)))
        (state, Tez.zero)
        state.pending_slashes
    in
    let total_supply = Tez.(state.total_supply -! total_burnt) in
    {state with pending_slashes = []; total_supply}

  (** Given an account name and new account state, updates [state] accordingly
      Preferably use other specific update functions *)
  let update_account (account_name : string) (value : account_state) (state : t)
      : t =
    let account_map = String.Map.add account_name value state.account_map in
    {state with account_map}

  let update_delegate account_name delegate_name_opt state : t =
    let account = find_account account_name state in
    update_account
      account_name
      {account with delegate = delegate_name_opt}
      state

  let add_pending_operations operations state =
    {state with pending_operations = state.pending_operations @ operations}

  let pop_pending_operations state =
    ({state with pending_operations = []}, state.pending_operations)

  let log_model_autostake name pkh old_cycle op ~optimal amount =
    Log.debug
      "Model Autostaking: at end of cycle %a, %s(%a) to reach optimal stake %a \
       %s %a"
      Cycle.pp
      old_cycle
      name
      Signature.Public_key_hash.pp
      pkh
      Tez.pp
      optimal
      op
      Tez.pp
      (Tez.of_mutez_exn amount)

  let apply_autostake ~name ~old_cycle
      ({
         pkh;
         contract = _;
         delegate;
         parameters = _;
         liquid;
         bonds = _;
         frozen_deposits;
         unstaked_frozen;
         unstaked_finalizable;
         staking_delegator_numerator = _;
         staking_delegate_denominator = _;
         frozen_rights = _;
         slashed_cycles = _;
       } :
        account_state) state =
    let open Result_syntax in
    if Some name <> delegate then (
      Log.debug
        "Model Autostaking: %s <> %s, noop@."
        name
        (Option.value ~default:"None" delegate) ;
      return state)
    else
      let* current_liquid_delegated = liquid_delegated ~name state in
      let current_frozen = Frozen_tez.total_current frozen_deposits in
      let current_unstaked_frozen_delegated =
        Unstaked_frozen.sum_current unstaked_frozen
      in
      let current_unstaked_final_delegated =
        Unstaked_finalizable.total unstaked_finalizable
      in
      let power =
        Tez.(
          current_liquid_delegated +! current_frozen
          +! current_unstaked_frozen_delegated
          +! current_unstaked_final_delegated
          |> to_mutez |> Z.of_int64)
      in
      let optimal =
        Tez.of_z
          (Z.cdiv
             power
             (Z.of_int (state.constants.limit_of_delegation_over_baking + 1)))
      in
      let autostaked =
        Int64.(sub (Tez.to_mutez optimal) (Tez.to_mutez current_frozen))
      in
      let state = apply_unslashable (Cycle.succ old_cycle) name state in
      let state = apply_finalize name state in
      (* stake or unstake *)
      let new_state =
        if autostaked > 0L then (
          log_model_autostake ~optimal name pkh old_cycle "stake" autostaked ;
          apply_stake
            Tez.(min liquid (of_mutez autostaked))
            (Cycle.succ old_cycle)
            name
            state)
        else if autostaked < 0L then (
          log_model_autostake
            ~optimal
            name
            pkh
            old_cycle
            "unstake"
            (Int64.neg autostaked) ;
          apply_unstake
            (Cycle.succ old_cycle)
            (Test_tez.of_mutez_exn Int64.(neg autostaked))
            name
            state)
        else (
          log_model_autostake
            ~optimal
            name
            pkh
            old_cycle
            "only finalize"
            autostaked ;
          state)
      in
      return new_state

  (** Applies when baking the last block of a cycle *)
  let apply_end_cycle current_cycle block state : t tzresult Lwt.t =
    let open Lwt_result_wrap_syntax in
    Log.debug ~color:time_color "Ending cycle %a" Cycle.pp current_cycle ;
    let* launch_cycle_opt =
      Context.get_adaptive_issuance_launch_cycle (B block)
    in
    (* Apply all slashes *)
    let state = apply_all_slashes_at_cycle_end current_cycle state in
    (* Sets initial frozen for future cycle *)
    let state =
      update_map
        ~f:
          (update_frozen_rights_cycle
             (Cycle.add current_cycle (state.constants.preserved_cycles + 1)))
        state
    in
    (* Apply autostaking *)
    let*?@ state =
      if not state.constants.adaptive_issuance.autostaking_enable then Ok state
      else
        match launch_cycle_opt with
        | Some launch_cycle when Cycle.(current_cycle >= launch_cycle) ->
            Ok state
        | None | Some _ ->
            String.Map.fold_e
              (fun name account state ->
                apply_autostake ~name ~old_cycle:current_cycle account state)
              state.account_map
              state
    in
    (* Apply parameter changes *)
    let state, param_requests =
      List.fold_left
        (fun (state, remaining_requests) (name, params, wait) ->
          if wait > 0 then
            (state, (name, params, wait - 1) :: remaining_requests)
          else
            let src = find_account name state in
            let state =
              update_account name {src with parameters = params} state
            in
            (state, remaining_requests))
        (state, [])
        state.param_requests
    in
    return {state with param_requests}

  (** Applies when baking the first block of a cycle.
      Technically nothing special happens, but we need to update the unslashable unstakes
      since it's done lazily *)
  let apply_new_cycle new_cycle state : t =
    apply_unslashable_for_all new_cycle state

  (* end module State *)
end

(* ======== Scenarios ======== *)

(** Usual threaded state for the tests. Contains the current block, pending operations
    and the known [State.t] *)
type t = Block.t * State.t

(** A scenario is a succession of actions. We define a branching path as a way to create multiple tests
    from the same point. This allows easy compositionality of behaviors with minimal code sharing.
    The [Tag] allows to give meaningful identifiers to the branches. It is good practice to tag each
    case in a branch (it's not necessary, but since test names must be unique, at most one branch can
    remain unnamed, and even then it can create conflicting names.)
 *)
type ('input, 'output) scenarios =
  | Action : ('input -> 'output tzresult Lwt.t) -> ('input, 'output) scenarios
  | Empty : ('t, 't) scenarios
  | Concat : (('a, 'b) scenarios * ('b, 'c) scenarios) -> ('a, 'c) scenarios
  | Branch : (('a, 'b) scenarios * ('a, 'b) scenarios) -> ('a, 'b) scenarios
  | Tag : (* Name for test branch *) string -> ('t, 't) scenarios
  | Slow : (* If in scenario branch, makes the test `Slow *)
      ('t, 't) scenarios

(** Unfolded scenario type *)
type ('input, 'output) single_scenario =
  | End_scenario : ('t, 't) single_scenario
  | Cons :
      (('input -> 't tzresult Lwt.t) * ('t, 'output) single_scenario)
      -> ('input, 'output) single_scenario

let rec cat_ss :
    type a b c.
    (a, b) single_scenario -> (b, c) single_scenario -> (a, c) single_scenario =
 fun a b ->
  match a with End_scenario -> b | Cons (act, a') -> Cons (act, cat_ss a' b)

let combine f l1 l2 =
  List.map (fun a -> List.map (fun b -> f a b) l2) l1 |> List.flatten

let rec unfold_scenarios :
    type input output.
    (input, output) scenarios ->
    ((input, output) single_scenario * string list * bool) list = function
  | Slow -> [(End_scenario, [], true)]
  | Tag s -> [(End_scenario, [s], false)]
  | Empty -> [(End_scenario, [], false)]
  | Action a -> [(Cons (a, End_scenario), [], false)]
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
  let open Lwt_result_syntax in
  fun scenario input ->
    match scenario with
    | End_scenario -> return input
    | Cons (action, next) ->
        let* result = action input in
        run_scenario next result

let unfolded_to_test :
    (unit, unit) single_scenario * string list * bool ->
    unit Alcotest_lwt.test_case =
 fun (s, name, b) ->
  let speed = if b then `Slow else `Quick in
  let name =
    match name with
    | [] -> ""
    | [n] -> n
    | title :: tags ->
        (* We chose to separate all tags with a comma, and use the head tag as a title for the test *)
        title ^ ": " ^ String.concat ", " tags
  in
  Tztest.tztest name speed (run_scenario s)

(** Useful aliases and operators *)

(* Aliases for [Empty]. Can be used as first component of a scenario instead of a tag if its not needed. *)
let noop = Empty

let no_tag = Empty

let concat :
    type a b c. (a, b) scenarios -> (b, c) scenarios -> (a, c) scenarios =
 fun a b ->
  match (a, b) with
  | Empty, Empty -> Empty
  | _, Empty -> a
  | Empty, _ -> b
  | _ -> Concat (a, b)

let branch : type a b. (a, b) scenarios -> (a, b) scenarios -> (a, b) scenarios
    =
 fun a b -> match (a, b) with Empty, Empty -> Empty | _ -> Branch (a, b)

(** Continuation connector: execute a then b *)
let ( --> ) a b = concat a b

(** Branching connector: creates two tests with different execution paths *)
let ( |+ ) a b = branch a b

(** Ends the test. Dump the state, returns [unit] *)
let end_test : ('a, unit) scenarios =
  let open Lwt_result_syntax in
  Action
    (fun _ ->
      Log.info ~color:begin_end_color "-- End test --" ;
      return_unit)

(** Transforms scenarios into Alcotest tests *)
let tests_of_scenarios :
    (string * (unit, 't) scenarios) list -> unit Alcotest_lwt.test_case list =
 fun scenarios ->
  List.map (fun (s, x) -> Tag s --> x --> end_test) scenarios |> function
  | [] -> []
  | a :: t ->
      List.fold_left ( |+ ) a t |> unfold_scenarios |> List.map unfolded_to_test

(** Arbitrary execution *)
let exec f = Action f

(** Execute a function that does not modify the block, only the state *)
let exec_state f =
  let open Lwt_result_syntax in
  Action
    (fun ((block, _state) as input) ->
      let* state = f input in
      return (block, state))

(** Execute a function that does not modify neither the block nor the state.
    Usually used for checks/asserts *)
let exec_unit f =
  let open Lwt_result_syntax in
  Action
    (fun input ->
      let* () = f input in
      return input)

(* ======== Baking ======== *)

(** After baking and applying rewards in state *)
let check_all_balances block state : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let State.{account_map; total_supply; _} = state in
  let* actual_total_supply = Context.get_total_supply (B block) in
  let*! r1 =
    String.Map.fold_s
      (fun name account acc ->
        log_debug_balance name account_map ;
        let* () = log_debug_rpc_balance name (Implicit account.pkh) block in
        let*! r =
          assert_balance_check ~loc:__LOC__ (B block) name account_map
        in
        join_errors r acc)
      account_map
      Result.return_unit
  in
  let*! r2 =
    Assert.equal
      ~loc:__LOC__
      Tez.equal
      "Total supplies do not match"
      Tez.pp
      actual_total_supply
      total_supply
  in
  join_errors r1 r2

let check_issuance_rpc block : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  (* We assume one block per minute *)
  let* rewards_per_block = Context.get_issuance_per_minute (B block) in
  let* total_supply = Context.get_total_supply (B block) in
  let* expected_issuance = Context.get_ai_expected_issuance (B block) in
  let* () =
    match expected_issuance with
    | ei :: _ ->
        (* We assume only the fixed portion is issued *)
        Assert.equal_tez
          ~loc:__LOC__
          rewards_per_block
          ei.baking_reward_fixed_portion
    | _ -> failwith "expected_issuance rpc: unexpected value"
  in
  let* yearly_rate = Context.get_ai_current_yearly_rate (B block) in
  let* yearly_rate_exact = Context.get_ai_current_yearly_rate_exact (B block) in
  let yr = float_of_string yearly_rate in
  let yre = Q.to_float yearly_rate_exact in
  (* Precision for yearly rate is 0.001 *)
  let* () =
    Assert.equal
      ~loc:__LOC__
      (fun x y -> Float.(abs (x -. y) <= 0.001))
      "Yearly rate (float)"
      Format.pp_print_float
      yr
      yre
  in
  (* Divided by 525_600 minutes per year, x100 because rpc returns a pct *)
  let issuance_from_rate =
    Tez.(
      mul_q total_supply Q.(div yearly_rate_exact ~$525_600_00)
      |> of_q ~round_up:false)
  in
  let* () =
    Assert.equal
      ~loc:__LOC__
      Tez.equal
      "Issuance"
      Tez.pp
      rewards_per_block
      issuance_from_rate
  in
  return_unit

(** Bake a block, with the given baker and the given operations. *)
let bake ?baker : t -> t tzresult Lwt.t =
 fun (block, state) ->
  let open Lwt_result_wrap_syntax in
  Log.info
    ~color:time_color
    "Baking level %d"
    (Int32.to_int (Int32.succ Block.(block.header.shell.level))) ;
  let current_cycle = Block.current_cycle block in
  let adaptive_issuance_vote =
    if state.activate_ai then
      Protocol.Alpha_context.Per_block_votes.Per_block_vote_on
    else Per_block_vote_pass
  in
  let policy =
    match baker with
    | None -> state.baking_policy
    | Some baker ->
        let {pkh; _} =
          try State.find_account baker state
          with Not_found ->
            Log.info
              ~color:warning_color
              "Invalid baker: %s not found. Aborting"
              baker ;
            assert false
        in
        Some (Block.By_account pkh)
  in
  let* baker, _, _, _ = Block.get_next_baker ?policy block in
  let baker_name, {contract = baker_contract; _} =
    State.find_account_from_pkh baker state
  in
  let* () = check_issuance_rpc block in
  let state, operations = State.pop_pending_operations state in
  let* block, state =
    let* block', metadata =
      Block.bake_with_metadata ?policy ~adaptive_issuance_vote ~operations block
    in
    let balance_updates = Block.get_balance_updates_from_metadata metadata in
    let state =
      State.apply_staking_abstract_balance_updates balance_updates state
    in
    if state.burn_rewards then
      (* Incremental mode *)
      let* i =
        Incremental.begin_construction ?policy ~adaptive_issuance_vote block
      in
      let* block_rewards = Context.get_issuance_per_minute (B block') in
      let ctxt = Incremental.alpha_ctxt i in
      let*@ context, _ =
        Protocol.Alpha_context.Token.transfer
          ctxt
          (`Contract baker_contract)
          `Burned
          block_rewards
      in
      let i = Incremental.set_alpha_ctxt i context in
      let* i = List.fold_left_es Incremental.add_operation i operations in
      let* block = Incremental.finalize_block i in
      let state = State.apply_burn block_rewards baker_name state in
      return (block, state)
    else return (block', state)
  in
  let* state = State.apply_rewards ~baker:baker_name block state in
  (* First block of a new cycle *)
  let new_current_cycle = Block.current_cycle block in
  let* state =
    if Protocol.Alpha_context.Cycle.(current_cycle = new_current_cycle) then
      return state
    else (
      Log.info
        ~color:time_color
        "Cycle %d"
        (Protocol.Alpha_context.Cycle.to_int32 new_current_cycle |> Int32.to_int) ;
      return @@ State.apply_new_cycle new_current_cycle state)
  in
  (* Dawn of a new cycle *)
  let* state =
    if not (Block.last_block_of_cycle block) then return state
    else State.apply_end_cycle current_cycle block state
  in
  let* () = check_all_balances block state in
  return (block, state)

(** Bake until a cycle is reached, using [bake] instead of [Block.bake]
    Should be slower because checks balances at the end of every block (avoidable in some cases) *)
let bake_until_next_cycle : t -> t tzresult Lwt.t =
 fun (init_block, init_state) ->
  let open Lwt_result_syntax in
  let current_cycle = Block.current_cycle init_block in
  let rec step (old_block, old_state) =
    let step_cycle = Block.current_cycle old_block in
    if Protocol.Alpha_context.Cycle.(step_cycle > current_cycle) then
      return (old_block, old_state)
    else
      let* new_block, new_state = bake (old_block, old_state) in
      step (new_block, new_state)
  in
  step (init_block, init_state)

(* ======== State updates ======== *)

(** Sets the de facto baker for all future blocks *)
let set_baker baker : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      let {pkh; _} = State.find_account baker state in
      return {state with State.baking_policy = Some (Block.By_account pkh)})

(** Exclude a list of delegates from baking *)
let exclude_bakers bakers : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      let bakers_pkh =
        List.map (fun baker -> (State.find_account baker state).pkh) bakers
      in
      return
        {state with State.baking_policy = Some (Block.Excluding bakers_pkh)})

(** Unsets the baking policy, it returns to default ([By_round 0]) *)
let unset_baking_policy : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      return {state with State.baking_policy = None})

(** Creates a snapshot of the current balances for the given account names.
    Can be used to check that balances at point A and B in the execution of a test
    are the same (either nothing happened, or a succession of actions resulted in
    getting the same values as before *)
let snapshot_balances snap_name names_list : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      Log.debug
        ~color:low_debug_color
        "Snapshoting balances as \"%s\""
        snap_name ;
      let balances =
        List.map
          (fun name -> (name, balance_of_account name state.State.account_map))
          names_list
      in
      let snapshot_balances =
        String.Map.add snap_name balances state.snapshot_balances
      in
      return {state with snapshot_balances})

(** Check balances against a previously defined snapshot *)
let check_snapshot_balances
    ?(f =
      fun ~name ~old_balance ~new_balance ->
        assert_balance_equal ~loc:__LOC__ name old_balance new_balance)
    snap_name : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_unit (fun (_block, state) ->
      Log.debug
        ~color:low_debug_color
        "Checking evolution of balances between \"%s\" and now"
        snap_name ;
      let snapshot_balances =
        String.Map.find snap_name state.State.snapshot_balances
      in
      match snapshot_balances with
      | None ->
          Log.debug
            ~color:warning_color
            "\"%s\" snapshot not found..."
            snap_name ;
          return_unit
      | Some snapshot_balances ->
          let* () =
            List.iter_es
              (fun (name, old_balance) ->
                let new_balance =
                  balance_of_account name state.State.account_map
                in
                f ~name ~old_balance ~new_balance)
              snapshot_balances
          in
          return_unit)

(** Save the current issuance rate for future use *)
let save_current_rate : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (block, state) ->
      let* rate = Context.get_ai_current_yearly_rate_exact (B block) in
      return {state with State.saved_rate = Some rate})

(** Check that [f saved_rate current_rate] is true. [f] is typically a comparison function *)
let check_rate_evolution (f : Q.t -> Q.t -> bool) : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_unit (fun (block, state) ->
      let* new_rate = Context.get_ai_current_yearly_rate_exact (B block) in
      let previous_rate = state.State.saved_rate in
      match previous_rate with
      | None -> failwith "check_rate_evolution: no rate previously saved"
      | Some previous_rate ->
          if f previous_rate new_rate then return_unit
          else
            failwith
              "check_rate_evolution: assertion failed@.previous rate: %a@.new \
               rate: %a"
              Q.pp_print
              previous_rate
              Q.pp_print
              new_rate)

(* ======== Operations ======== *)

(** Bake a single block *)
let next_block =
  exec (fun input ->
      Log.info ~color:action_color "[Next block]" ;
      bake input)

(** Bake a single block with a specific baker *)
let next_block_with_baker baker =
  exec (fun input ->
      Log.info ~color:action_color "[Next block (baker %s)]" baker ;
      bake ~baker input)

(** Bake until the end of a cycle *)
let next_cycle =
  exec (fun input ->
      Log.info ~color:action_color "[Next cycle]" ;
      bake_until_next_cycle input)

(** Executes an operation: f should return a new state and a list of operations, which are then applied *)
let exec_op f =
  let open Lwt_result_syntax in
  Action
    (fun ((block, _state) as input) ->
      let* state, ops = f input in
      let state = State.add_pending_operations ops state in
      return (block, state))
  --> next_block

(* ======== Definition of basic actions ======== *)

(** Initialize the test, given some initial parameters *)
let begin_test ~activate_ai ?(burn_rewards = false)
    (constants : Protocol.Alpha_context.Constants.Parametric.t)
    delegates_name_list : (unit, t) scenarios =
  exec (fun () ->
      let open Lwt_result_syntax in
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
              {
                constants.adaptive_issuance with
                launch_ema_threshold = 0l;
                activation_vote_enable = true;
              };
          })
        else constants
      in
      let n = List.length delegates_name_list in
      let* block, delegates = Context.init_with_constants_n constants n in
      let*? init_level = Context.get_level (B block) in
      let init_staked = Tez.of_mutez 200_000_000_000L in
      let*? account_map =
        List.fold_left2
          ~when_different_lengths:[Inconsistent_number_of_bootstrap_accounts]
          (fun account_map name contract ->
            let liquid = Tez.(Account.default_initial_balance -! init_staked) in
            let frozen_deposits = Frozen_tez.init init_staked name name in
            let frozen_rights =
              List.fold_left
                (fun map cycle -> CycleMap.add cycle init_staked map)
                CycleMap.empty
                Cycle.(root ---> add root constants.preserved_cycles)
            in
            let pkh = Context.Contract.pkh contract in
            let account =
              init_account
                ~delegate:name
                ~pkh
                ~contract
                ~parameters:default_params
                ~liquid
                ~frozen_deposits
                ~frozen_rights
                ()
            in
            let account_map = String.Map.add name account account_map in
            let balance, total_balance =
              balance_and_total_balance_of_account name account_map
            in
            Log.debug "Initial balance for %s:\n%a" name balance_pp balance ;
            Log.debug "Initial total balance: %a" Tez.pp total_balance ;
            account_map)
          String.Map.empty
          delegates_name_list
          delegates
      in
      let* total_supply = Context.get_total_supply (B block) in
      let state =
        State.
          {
            account_map;
            total_supply;
            constants;
            param_requests = [];
            activate_ai;
            baking_policy = None;
            last_level_rewards = init_level;
            snapshot_balances = String.Map.empty;
            saved_rate = None;
            burn_rewards;
            pending_operations = [];
            pending_slashes = [];
            double_signings = [];
          }
      in
      let* () = check_all_balances block state in
      return (block, state))

(** Set delegate parameters for the given delegate *)
let set_delegate_params delegate_name parameters : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      (* Simple example of action_atom definition: *)
      let delegate = State.find_account delegate_name state in
      Log.info
        ~color:action_color
        "[Set delegate parameters for \"%s\"]"
        delegate_name ;
      (* Define the operation *)
      let* operation =
        set_delegate_parameters (B block) delegate.contract ~parameters
      in
      (* Update state *)
      let wait = state.constants.preserved_cycles - 1 in
      let state =
        {
          state with
          param_requests =
            (delegate_name, parameters, wait) :: state.param_requests;
        }
      in
      (* Return both *)
      return (state, [operation]))

(** Add a new account with the given name *)
let add_account name : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      Log.info ~color:action_color "[Add account \"%s\"]" name ;
      let new_account = Account.new_account () in
      let pkh = new_account.pkh in
      let contract = Protocol.Alpha_context.Contract.Implicit pkh in
      let account_state =
        init_account ~pkh ~contract ~parameters:default_params ()
      in
      let state = State.update_account name account_state state in
      return state)

(** Reveal operation *)
let reveal name : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let account = State.find_account name state in
      Log.info ~color:action_color "[Reveal \"%s\"]" name ;
      let* acc = Account.find account.pkh in
      let* operation =
        Op.revelation ~fee:Protocol.Alpha_context.Tez.zero (B block) acc.pk
      in
      return (state, [operation]))

(** Transfer from src to dst *)
let transfer src_name dst_name amount : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let src = State.find_account src_name state in
      let dst = State.find_account dst_name state in
      let amount = quantity_to_tez src.liquid amount in
      Log.info
        ~color:action_color
        "[Transfer \"%s\" -> \"%s\" (%aꜩ)]"
        src_name
        dst_name
        Tez.pp
        amount ;
      let* operation =
        Op.transaction ~fee:Tez.zero (B block) src.contract dst.contract amount
      in
      let state = State.apply_transfer amount src_name dst_name state in
      return (state, [operation]))

(** Set delegate for src. If [delegate_name_opt = None], then unset current delegate *)
let set_delegate src_name delegate_name_opt : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let src = State.find_account src_name state in
      let delegate_pkh_opt =
        match delegate_name_opt with
        | None ->
            Log.info ~color:action_color "[Unset delegate of \"%s\"]" src_name ;
            None
        | Some delegate_name ->
            let delegate = State.find_account delegate_name state in
            Log.info
              ~color:action_color
              "[Set delegate \"%s\" for \"%s\"]"
              delegate_name
              src_name ;
            Some delegate.pkh
      in
      let cycle = Block.current_cycle block in
      let* operation =
        Op.delegation ~fee:Tez.zero (B block) src.contract delegate_pkh_opt
      in
      let balance = balance_of_account src_name state.account_map in
      let state =
        if Q.(equal balance.staked_b zero) then state
        else
          let state = State.apply_unstake cycle Tez.max_mutez src_name state in
          (* Changing delegate applies finalize if unstake happened *)
          State.apply_finalize src_name state
      in
      let state = State.update_delegate src_name delegate_name_opt state in
      return (state, [operation]))

(** Stake operation *)
let stake src_name stake_value : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let src = State.find_account src_name state in
      Log.info
        ~color:action_color
        "[Stake for \"%s\" (%a)]"
        src_name
        tez_quantity_pp
        stake_value ;
      (* Stake applies finalize *before* the stake *)
      let state = State.apply_finalize src_name state in
      let amount = quantity_to_tez src.liquid stake_value in
      let current_cycle = Block.current_cycle block in
      let* operation = stake (B block) src.contract amount in
      let state = State.apply_stake amount current_cycle src_name state in
      return (state, [operation]))

(** unstake operation *)
let unstake src_name unstake_value : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let src = State.find_account src_name state in
      Log.info
        ~color:action_color
        "[Unstake for \"%s\" (%a)]"
        src_name
        tez_quantity_pp
        unstake_value ;
      let stake_balance =
        (balance_of_account src_name state.account_map).staked_b
        |> Partial_tez.to_tez ~round_up:false
      in
      let amount = quantity_to_tez stake_balance unstake_value in
      let* operation = unstake (B block) src.contract amount in
      let cycle = Block.current_cycle block in
      let balance = balance_of_account src_name state.account_map in
      let state =
        if Q.(equal balance.staked_b zero) then state
        else
          let state = State.apply_unstake cycle amount src_name state in
          State.apply_finalize src_name state
      in
      return (state, [operation]))

(** finalize unstake operation *)
let finalize_unstake src_name : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let src = State.find_account src_name state in
      Log.info ~color:action_color "[Finalize_unstake for \"%s\"]" src_name ;
      let* operation = finalize_unstake (B block) src.contract in
      let state = State.apply_finalize src_name state in
      return (state, [operation]))

(* ======== Slashing ======== *)

let check_pending_slashings (block, state) : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let open Protocol.Denunciations_repr in
  let* denunciations_rpc = Context.get_denunciations (B block) in
  let denunciations_obj_equal
      (pkh_1, {rewarded = r1; misbehaviour = m1; misbehaviour_cycle = mc1; _})
      (pkh_2, {rewarded = r2; misbehaviour = m2; misbehaviour_cycle = mc2; _}) =
    Signature.Public_key_hash.equal pkh_1 pkh_2
    && Signature.Public_key_hash.equal r1 r2
    && Stdlib.(m1.kind = m2.kind)
    && Stdlib.(mc1 = mc2)
  in
  let compare_denunciations
      (pkh_1, {rewarded = r1; misbehaviour = m1; misbehaviour_cycle = mc1; _})
      (pkh_2, {rewarded = r2; misbehaviour = m2; misbehaviour_cycle = mc2; _}) =
    let c1 = Signature.Public_key_hash.compare pkh_1 pkh_2 in
    if c1 <> 0 then c1
    else
      let c2 = Signature.Public_key_hash.compare r1 r2 in
      if c2 <> 0 then c2
      else
        let c3 =
          match (m1.kind, m2.kind) with
          | Double_baking, Double_attesting -> -1
          | x, y when x = y -> 0
          | _ -> 1
        in
        if c3 <> 0 then c3
        else
          match (mc1, mc2) with
          | Current, Previous -> -1
          | x, y when x = y -> 0
          | _ -> 1
  in
  let denunciations_rpc = List.sort compare_denunciations denunciations_rpc in
  let denunciations_state =
    List.sort compare_denunciations state.State.pending_slashes
  in
  let denunciations_equal = List.equal denunciations_obj_equal in
  let denunciations_obj_pp fmt
      (pkh, {rewarded; misbehaviour; misbehaviour_cycle; operation_hash = _}) =
    Format.fprintf
      fmt
      "slashed: %a; rewarded: %a; kind: %s; cycle: %s@."
      Signature.Public_key_hash.pp
      pkh
      Signature.Public_key_hash.pp
      rewarded
      (match misbehaviour.kind with
      | Double_baking -> "double baking"
      | Double_attesting -> "double attesting")
      (match misbehaviour_cycle with
      | Current -> "current"
      | Previous -> "previous")
  in
  let denunciations_pp = Format.pp_print_list denunciations_obj_pp in
  let* () =
    Assert.equal
      ~loc:__LOC__
      denunciations_equal
      "Denunciations are not equal"
      denunciations_pp
      denunciations_rpc
      denunciations_state
  in
  return_unit

(** Double attestation helpers *)
let order_attestations ~correct_order op1 op2 =
  let oph1 = Protocol.Alpha_context.Operation.hash op1 in
  let oph2 = Protocol.Alpha_context.Operation.hash op2 in
  let c = Operation_hash.compare oph1 oph2 in
  if correct_order then if c < 0 then (op1, op2) else (op2, op1)
  else if c < 0 then (op2, op1)
  else (op1, op2)

let op_double_attestation ?(correct_order = true) op1 op2 ctxt =
  let e1, e2 = order_attestations ~correct_order op1 op2 in
  Op.double_attestation ctxt e1 e2

let op_double_preattestation ?(correct_order = true) op1 op2 ctxt =
  let e1, e2 = order_attestations ~correct_order op1 op2 in
  Op.double_preattestation ctxt e1 e2

let order_block_hashes ~correct_order bh1 bh2 =
  let hash1 = Protocol.Alpha_context.Block_header.hash bh1 in
  let hash2 = Protocol.Alpha_context.Block_header.hash bh2 in
  let c = Block_hash.compare hash1 hash2 in
  if correct_order then if c < 0 then (bh1, bh2) else (bh2, bh1)
  else if c < 0 then (bh2, bh1)
  else (bh1, bh2)

let op_double_baking ?(correct_order = true) bh1 bh2 ctxt =
  let bh1, bh2 = order_block_hashes ~correct_order bh1 bh2 in
  Op.double_baking ctxt bh1 bh2

let double_bake_ delegate_name (block, state) =
  let open Lwt_result_syntax in
  Log.info ~color:Log_module.event_color "Double baking with %s" delegate_name ;
  let delegate = State.find_account delegate_name state in
  let* operation =
    Adaptive_issuance_helpers.unstake (B block) delegate.contract Tez.one_mutez
  in
  let* forked_block =
    Block.bake ~policy:(By_account delegate.pkh) ~operation block
  in
  (* includes pending operations *)
  let* main_branch, state = bake ~baker:delegate_name (block, state) in
  let evidence = op_double_baking main_branch.header forked_block.header in
  let dss =
    {
      culprit = delegate.pkh;
      denounced = false;
      evidence;
      kind = Double_baking;
      level = block.header.shell.level;
    }
  in
  let state =
    {state with double_signings = dss :: state.State.double_signings}
  in
  return (main_branch, state)

(* Note: advances one block *)
let double_bake delegate_name : (t, t) scenarios =
  exec (double_bake_ delegate_name)

let double_attest_op ~op ~op_evidence ~kind delegate_name (block, state) =
  let open Lwt_result_syntax in
  Log.info
    ~color:Log_module.event_color
    "Double (pre)attesting with %s"
    delegate_name ;
  let delegate = State.find_account delegate_name state in
  let* baker, _, _, _ =
    Block.get_next_baker ?policy:state.baking_policy block
  in
  let* other_baker1, other_baker2 =
    Context.get_first_different_bakers (B block)
  in
  let other_baker =
    if not (Signature.Public_key_hash.equal baker other_baker2) then
      other_baker2
    else other_baker1
  in
  let* forked_block = Block.bake ~policy:(By_account other_baker) block in
  let* forked_block = Block.bake ?policy:state.baking_policy forked_block in
  (* includes pending operations *)
  let* block, state = bake (block, state) in
  let* main_branch, state = bake (block, state) in
  let* attestation_a = op ~delegate:delegate.pkh forked_block in
  let* attestation_b = op ~delegate:delegate.pkh main_branch in
  let evidence = op_evidence attestation_a attestation_b in
  let dss =
    {
      culprit = delegate.pkh;
      denounced = false;
      evidence;
      kind;
      level = block.header.shell.level;
    }
  in
  let state =
    {state with double_signings = dss :: state.State.double_signings}
  in
  return (main_branch, state)

let double_attest_ =
  double_attest_op
    ~op:(fun ~delegate block -> Op.raw_attestation ~delegate block)
    ~op_evidence:op_double_attestation
    ~kind:Double_attesting

(* Note: advances two blocks *)
let double_attest delegate_name : (t, t) scenarios =
  exec (double_attest_ delegate_name)

let double_preattest_ =
  double_attest_op
    ~op:(fun ~delegate block -> Op.raw_preattestation ~delegate block)
    ~op_evidence:op_double_preattestation
    ~kind:Double_preattesting

(* Note: advances two blocks *)
let double_preattest delegate_name : (t, t) scenarios =
  exec (double_preattest_ delegate_name)

let cycle_from_level blocks_per_cycle level =
  let current_cycle = Int32.div level blocks_per_cycle in
  let current_cycle = Cycle.add Cycle.root (Int32.to_int current_cycle) in
  current_cycle

let pct_from_kind (block : Block.t) = function
  | Protocol.Misbehaviour_repr.Double_baking ->
      (block.constants.percentage_of_frozen_deposits_slashed_per_double_baking
        :> int)
  | Double_attesting ->
      (block.constants
         .percentage_of_frozen_deposits_slashed_per_double_attestation
        :> int)

let get_pending_slashed_pct_for_delegate (block, state) delegate =
  let rec aux r = function
    | [] -> r
    | (culprit, {Protocol.Denunciations_repr.misbehaviour; _}) :: t ->
        if Signature.Public_key_hash.equal delegate culprit then
          let new_r = r + pct_from_kind block misbehaviour.kind in
          if new_r >= 100 then 100 else aux new_r t
        else aux r t
  in
  aux 0 state.State.pending_slashes

let update_state_denunciation (block, state)
    {culprit; denounced; evidence = _; kind; level} =
  let open Lwt_result_syntax in
  if denounced then
    (* If the double signing has already been denounced, a second denunciation should fail *)
    return (state, denounced)
  else
    let*? block_level = Context.get_level (B block) in
    let next_level =
      Protocol.Alpha_context.Raw_level.(to_int32 @@ succ block_level)
    in
    if level > next_level then
      (* The denunciation is trying to be included too early *)
      return (state, denounced)
    else
      let inclusion_cycle =
        cycle_from_level block.constants.blocks_per_cycle next_level
      in
      let ds_cycle = cycle_from_level block.constants.blocks_per_cycle level in
      if Cycle.(succ ds_cycle < inclusion_cycle) then
        (* denunciation is too late, gets refused *)
        return (state, denounced)
      else if get_pending_slashed_pct_for_delegate (block, state) culprit >= 100
      then
        (* Culprit has been slashed too much, a denunciation is not added to the list.
           TODO: is the double signing treated as included, or can it be included in the
           following cycle? *)
        return (state, denounced)
      else
        let misbehaviour_cycle =
          if Cycle.(ds_cycle = inclusion_cycle) then
            Protocol.Denunciations_repr.Current
          else if Cycle.(succ ds_cycle = inclusion_cycle) then Previous
          else assert false
        in
        let kind =
          match kind with
          | Double_baking -> Protocol.Misbehaviour_repr.Double_baking
          | Double_attesting -> Double_attesting
          | Double_preattesting -> Double_attesting
        in
        let misbehaviour =
          {
            Protocol.Misbehaviour_repr.kind;
            (* Fields level, round, and slot are unused for now. *)
            level = Protocol.Raw_level_repr.of_int32_exn level;
            round = Protocol.Round_repr.zero;
            slot = Protocol.Slot_repr.zero;
          }
        in
        (* for simplicity's sake (lol), the block producer and the payload producer are the same
           We also assume that the current state baking policy will be used for the next block *)
        let* rewarded, _, _, _ =
          Block.get_next_baker ?policy:state.baking_policy block
        in
        let culprit_name, culprit_account =
          State.find_account_from_pkh culprit state
        in
        let state =
          State.update_account
            culprit_name
            {
              culprit_account with
              slashed_cycles = inclusion_cycle :: culprit_account.slashed_cycles;
            }
            state
        in
        let new_pending_slash =
          ( culprit,
            {
              Protocol.Denunciations_repr.rewarded;
              misbehaviour;
              misbehaviour_cycle;
              operation_hash = Operation_hash.zero;
              (* unused *)
            } )
        in
        (* TODO: better log... *)
        Log.info
          ~color:Log_module.event_color
          "Including denunciation (misbehaviour cycle %a)"
          Cycle.pp
          ds_cycle ;
        let state =
          State.
            {
              state with
              pending_slashes = new_pending_slash :: state.pending_slashes;
            }
        in
        return (state, true)

let make_denunciations_ ?(filter = fun {denounced; _} -> not denounced)
    (block, state) =
  let open Lwt_result_syntax in
  let* () = check_pending_slashings (block, state) in
  let make_op state ({evidence; _} as dss) =
    if filter dss then
      let* state, denounced = update_state_denunciation (block, state) dss in
      return (Some (evidence (B block), {dss with denounced}, state))
    else return None
  in
  let rec make_op_list dss_list state r_op r_dss =
    match dss_list with
    | d :: t -> (
        let* new_op = make_op state d in
        match new_op with
        | None -> make_op_list t state r_op (d :: r_dss)
        | Some (op, p_dss, new_state) ->
            make_op_list t new_state (op :: r_op) (p_dss :: r_dss))
    | [] -> return @@ (state, List.rev r_op, List.rev r_dss)
  in
  let* state, operations, double_signings =
    make_op_list state.double_signings state [] []
  in
  let state = {state with double_signings} in
  return (state, operations)

(* Important note: do not change the baking policy behaviour once denunciations are made,
   until the operations are included in a block (by default the next block) *)
let make_denunciations ?filter () = exec_op (make_denunciations_ ?filter)

(* ======== Misc functions ========*)

let check_failure_aux ?expected_error :
    ('a -> 'b tzresult Lwt.t) -> 'a -> 'a tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun f input ->
    Log.info ~color:assert_block_color "Entering failing scenario..." ;
    let*! output = f input in
    match output with
    | Ok _ -> failwith "Unexpected success"
    | Error e -> (
        match expected_error with
        | None ->
            Log.info ~color:assert_block_color "Rollback" ;
            return input
        | Some exp_e ->
            let exp_e = exp_e input in
            if e = exp_e then (
              Log.info ~color:assert_block_color "Rollback" ;
              return input)
            else (
              Log.info
                ~color:Log.Color.FG.red
                "Unexpected error:@.%a@.Expected:@.%a@."
                (Format.pp_print_list pp)
                e
                (Format.pp_print_list pp)
                exp_e ;
              tzfail Unexpected_error))

let check_fail_and_rollback ?expected_error :
    ('a, 'b) single_scenario -> 'a -> 'a tzresult Lwt.t =
 fun sc input -> check_failure_aux ?expected_error (run_scenario sc) input

(** Useful function to test expected failures: runs the given branch until it fails,
    then rollbacks to before execution. Fails if the given branch Succeeds *)
let assert_failure ?expected_error : ('a, 'b) scenarios -> ('a, 'a) scenarios =
 fun scenarios ->
  match unfold_scenarios scenarios with
  | [] -> Empty
  | [(sc, _, _)] -> exec (check_fail_and_rollback ?expected_error sc)
  | _ ->
      exec (fun _ ->
          failwith "Error: assert_failure used with branching scenario")

(** Loop *)
let rec loop n : ('a, 'a) scenarios -> ('a, 'a) scenarios =
 fun scenario ->
  (* If branching scenarios with k branches, returns a scenario with k^n branches *)
  if n = 0 then Empty
  else if n = 1 then scenario
  else loop (n - 1) scenario --> scenario

let rec loop_action n : ('a -> 'a tzresult Lwt.t) -> ('a, 'a) scenarios =
 fun f ->
  if n = 0 then Empty
  else if n = 1 then exec f
  else loop_action (n - 1) f --> exec f

(** Check a specific balance field for a specific account is equal to a specific amount *)
let check_balance_field src_name field amount : (t, t) scenarios =
  let open Lwt_result_syntax in
  let check = Assert.equal_tez ~loc:__LOC__ amount in
  let check' a = check (Partial_tez.to_tez ~round_up:false a) in
  exec_state (fun (block, state) ->
      let src = State.find_account src_name state in
      let src_balance, src_total =
        balance_and_total_balance_of_account src_name state.account_map
      in
      let* rpc_balance, rpc_total =
        get_balance_from_context (B block) src.contract
      in
      let* () =
        match field with
        | `Liquid ->
            let* () = check rpc_balance.liquid_b in
            check src_balance.liquid_b
        | `Bonds ->
            let* () = check rpc_balance.bonds_b in
            check src_balance.bonds_b
        | `Staked ->
            let* () = check' rpc_balance.staked_b in
            check' src_balance.staked_b
        | `Unstaked_frozen_total ->
            let* () = check rpc_balance.unstaked_frozen_b in
            check src_balance.unstaked_frozen_b
        | `Unstaked_finalizable ->
            let* () = check rpc_balance.unstaked_finalizable_b in
            check src_balance.unstaked_finalizable_b
        | `Total ->
            let* () = check rpc_total in
            check src_total
      in
      return state)

(** Waiting functions *)
let rec wait_n_cycles n =
  if n <= 0 then noop
  else if n = 1 then next_cycle
  else wait_n_cycles (n - 1) --> next_cycle

let rec wait_n_blocks n =
  if n <= 0 then noop
  else if n = 1 then next_block
  else wait_n_blocks (n - 1) --> next_block

(** Wait until AI activates.
    Fails if AI is not set to be activated in the future. *)
let wait_ai_activation =
  exec (fun (block, state) ->
      let open Lwt_result_syntax in
      Log.info ~color:time_color "Fast forward to AI activation" ;
      let* output =
        if state.State.activate_ai then
          let* launch_cycle = get_launch_cycle ~loc:__LOC__ block in
          let rec bake_while (block, state) =
            let current_cycle = Block.current_cycle block in
            if Cycle.(current_cycle >= launch_cycle) then return (block, state)
            else
              let* input = bake_until_next_cycle (block, state) in
              bake_while input
          in
          bake_while (block, state)
        else assert false
      in
      Log.info ~color:event_color "AI activated" ;
      return output)

(** Create an account and give an initial balance funded by [source] *)
let add_account_with_funds name source amount =
  add_account name --> transfer source name amount --> reveal name

(* ======== Scenarios ======== *)

let test_expected_error =
  assert_failure
    ~expected_error:(fun _ -> [Exn (Failure "")])
    (exec (fun _ -> failwith ""))
  --> assert_failure
        ~expected_error:(fun _ -> [Unexpected_error])
        (assert_failure
           ~expected_error:(fun _ ->
             [Inconsistent_number_of_bootstrap_accounts])
           (exec (fun _ -> failwith "")))

let init_constants ?reward_per_block ?(deactivate_dynamic = false)
    ?blocks_per_cycle ~autostaking_enable () =
  let reward_per_block = Option.value ~default:0L reward_per_block in
  let base_total_issued_per_minute = Tez.of_mutez reward_per_block in
  let default_constants = Default_parameters.constants_test in
  (* default for tests: 12 *)
  let blocks_per_cycle =
    Option.value ~default:default_constants.blocks_per_cycle blocks_per_cycle
  in
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
  let adaptive_issuance = default_constants.adaptive_issuance in
  let adaptive_rewards_params =
    if deactivate_dynamic then
      {
        adaptive_issuance.adaptive_rewards_params with
        max_bonus =
          Protocol.Issuance_bonus_repr.max_bonus_parameter_of_Q_exn Q.zero;
      }
    else adaptive_issuance.adaptive_rewards_params
  in
  let adaptive_issuance =
    {adaptive_issuance with adaptive_rewards_params; autostaking_enable}
  in
  {
    default_constants with
    consensus_threshold;
    issuance_weights;
    minimal_block_delay;
    cost_per_byte;
    adaptive_issuance;
    blocks_per_cycle;
  }

(** Initialization of scenarios with 3 cases:
     - AI activated, staker = delegate
     - AI activated, staker != delegate
     - AI not activated (and staker = delegate)
    Any scenario that begins with this will be triplicated.
 *)
let init_scenario ?(force_ai = true) ?reward_per_block () =
  let constants =
    init_constants ?reward_per_block ~autostaking_enable:false ()
  in
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  let begin_test ~activate_ai ~self_stake =
    let name = if self_stake then "staker" else "delegate" in
    begin_test ~activate_ai constants [name]
    --> set_delegate_params name init_params
    --> set_baker "__bootstrap__"
  in
  let ai_activated =
    Tag "AI activated"
    --> (Tag "self stake" --> begin_test ~activate_ai:true ~self_stake:true
        |+ Tag "external stake"
           --> begin_test ~activate_ai:true ~self_stake:false
           --> add_account_with_funds
                 "staker"
                 "delegate"
                 (Amount (Tez.of_mutez 2_000_000_000_000L))
           --> set_delegate "staker" (Some "delegate"))
    --> wait_ai_activation
  in

  let ai_deactivated =
    Tag "AI deactivated, self stake"
    --> begin_test ~activate_ai:false ~self_stake:true
  in
  (if force_ai then ai_activated else ai_activated |+ ai_deactivated)
  --> next_block

module Roundtrip = struct
  let stake_init =
    stake "staker" Half
    --> (Tag "no wait after stake" --> Empty
        |+ Tag "wait after stake" --> wait_n_cycles 2)

  let wait_for_unfreeze_and_check wait =
    snapshot_balances "wait snap" ["staker"]
    --> wait_n_cycles (wait - 1)
    (* Balance didn't change yet, but will change next cycle *)
    --> check_snapshot_balances "wait snap"
    --> next_cycle
    --> assert_failure (check_snapshot_balances "wait snap")

  let finalize staker =
    assert_failure (check_balance_field staker `Unstaked_finalizable Tez.zero)
    --> finalize_unstake staker
    --> check_balance_field staker `Unstaked_finalizable Tez.zero

  let simple_roundtrip =
    stake_init
    --> (Tag "full unstake" --> unstake "staker" All
        |+ Tag "half unstake" --> unstake "staker" Half)
    --> wait_for_unfreeze_and_check default_unstake_wait
    --> finalize "staker" --> next_cycle

  let double_roundtrip =
    stake_init --> unstake "staker" Half
    --> (Tag "half then full unstake" --> wait_n_cycles 2
         --> unstake "staker" All
        |+ Tag "half then half unstake" --> wait_n_cycles 2
           --> unstake "staker" Half)
    --> wait_for_unfreeze_and_check (default_unstake_wait - 2)
    --> wait_for_unfreeze_and_check 2
    --> finalize "staker" --> next_cycle

  let shorter_roundtrip_for_baker =
    let constants = init_constants ~autostaking_enable:false () in
    let amount = Amount (Tez.of_mutez 333_000_000_000L) in
    let preserved_cycles = constants.preserved_cycles in
    begin_test ~activate_ai:true constants ["delegate"]
    --> next_block --> wait_ai_activation
    --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
    --> next_cycle
    --> snapshot_balances "init" ["delegate"]
    --> unstake "delegate" amount
    --> List.fold_left
          (fun acc i -> acc |+ Tag (fs "wait %i cycles" i) --> wait_n_cycles i)
          (Tag "wait 0 cycles" --> Empty)
          (Stdlib.List.init (preserved_cycles + 1) (fun i -> i + 1))
    --> stake "delegate" amount
    --> check_snapshot_balances "init"

  let status_quo_rountrip =
    let full_amount = Tez.of_mutez 10_000_000L in
    let amount_1 = Tez.of_mutez 2_999_999L in
    let amount_2 = Tez.of_mutez 7_000_001L in
    snapshot_balances "init" ["staker"]
    --> stake "staker" (Amount full_amount)
    --> next_cycle
    --> (Tag "1 unstake" --> unstake "staker" (Amount full_amount)
        |+ Tag "2 unstakes"
           --> unstake "staker" (Amount amount_1)
           --> next_cycle
           --> unstake "staker" (Amount amount_2))
    --> wait_n_cycles default_unstake_wait
    --> finalize "staker"
    --> check_snapshot_balances "init"

  let scenario_finalize =
    no_tag --> stake "staker" Half --> next_cycle --> unstake "staker" Half
    --> wait_n_cycles (default_unstake_wait + 2)
    --> assert_failure
          (check_balance_field "staker" `Unstaked_finalizable Tez.zero)
    --> (Tag "finalize with finalize" --> finalize_unstake "staker"
        |+ Tag "finalize with stake" --> stake "staker" (Amount Tez.one_mutez)
        |+ Tag "finalize with unstake"
           --> unstake "staker" (Amount Tez.one_mutez))
    --> check_balance_field "staker" `Unstaked_finalizable Tez.zero

  (* Finalize does not go through when unstake does nothing *)
  (* Todo: there might be other cases... like changing delegates *)
  let scenario_not_finalize =
    no_tag --> stake "staker" Half --> next_cycle --> unstake "staker" All
    --> wait_n_cycles (default_unstake_wait + 2)
    --> assert_failure
          (check_balance_field "staker" `Unstaked_finalizable Tez.zero)
    --> snapshot_balances "not finalize" ["staker"]
    --> (Tag "no finalize with unstake if staked = 0"
        --> unstake "staker" (Amount Tez.one_mutez))
    --> assert_failure
          (check_balance_field "staker" `Unstaked_finalizable Tez.zero)
    --> check_snapshot_balances "not finalize"

  (* TODO: there's probably more... *)
  let scenario_forbidden_operations =
    let open Lwt_result_syntax in
    let fail_if_staker_is_self_delegate staker =
      exec (fun ((_, state) as input) ->
          if State.(is_self_delegate staker state) then
            failwith "_self_delegate_exit_"
          else return input)
    in
    no_tag
    (* Staking everything works for self delegates, but not for delegated accounts *)
    --> assert_failure
          (fail_if_staker_is_self_delegate "staker" --> stake "staker" All)
    (* stake is always forbidden when amount is zero *)
    --> assert_failure (stake "staker" Nothing)
    (* One cannot stake more that one has *)
    --> assert_failure (stake "staker" Max_tez)
    (* unstake is actually authorized for amount 0, but does nothing (doesn't even finalize if possible) *)
    --> unstake "staker" Nothing

  let full_balance_in_finalizable =
    add_account_with_funds "dummy" "staker" (Amount (Tez.of_mutez 10_000_000L))
    --> stake "staker" All_but_one --> next_cycle --> unstake "staker" All
    --> wait_n_cycles (default_unstake_wait + 2)
    (* At this point, almost all the balance (but one mutez) of the stake is in finalizable *)
    (* Staking is possible, but not transfer *)
    --> assert_failure
          (transfer "staker" "dummy" (Amount (Tez.of_mutez 10_000_000L)))
    --> stake "staker" (Amount (Tez.of_mutez 10_000_000L))
    (* After the stake, transfer is possible again because the funds were finalized *)
    --> transfer "staker" "dummy" (Amount (Tez.of_mutez 10_000_000L))

  (* Stress test: what happens if someone were to stake and unstake every cycle? *)
  let odd_behavior =
    let one_cycle =
      no_tag --> stake "staker" Half --> unstake "staker" Half --> next_cycle
    in
    loop 20 one_cycle

  let change_delegate =
    let constants = init_constants ~autostaking_enable:false () in
    let init_params =
      {
        limit_of_staking_over_baking = Q.one;
        edge_of_baking_over_staking = Q.one;
      }
    in
    begin_test ~activate_ai:true constants ["delegate1"; "delegate2"]
    --> set_delegate_params "delegate1" init_params
    --> set_delegate_params "delegate2" init_params
    --> add_account_with_funds
          "staker"
          "delegate1"
          (Amount (Tez.of_mutez 2_000_000_000_000L))
    --> set_delegate "staker" (Some "delegate1")
    --> wait_ai_activation --> next_cycle --> stake "staker" Half --> next_cycle
    --> set_delegate "staker" (Some "delegate2")
    --> next_cycle
    --> assert_failure (stake "staker" Half)
    --> wait_n_cycles (default_unstake_wait + 1)
    --> stake "staker" Half

  let unset_delegate =
    let constants = init_constants ~autostaking_enable:false () in
    let init_params =
      {
        limit_of_staking_over_baking = Q.one;
        edge_of_baking_over_staking = Q.one;
      }
    in
    begin_test ~activate_ai:true constants ["delegate"]
    --> set_delegate_params "delegate" init_params
    --> add_account_with_funds
          "staker"
          "delegate"
          (Amount (Tez.of_mutez 2_000_000_000_000L))
    --> add_account_with_funds
          "dummy"
          "delegate"
          (Amount (Tez.of_mutez 2_000_000L))
    --> set_delegate "staker" (Some "delegate")
    --> wait_ai_activation --> next_cycle --> stake "staker" Half
    --> unstake "staker" All --> next_cycle --> set_delegate "staker" None
    --> next_cycle
    --> transfer "staker" "dummy" All
    (* staker has an empty liquid balance, but still has unstaked frozen tokens,
       so it doesn't get deactivated *)
    --> wait_n_cycles (default_unstake_wait + 1)
    --> finalize_unstake "staker"

  let forbid_costaking =
    let constants = init_constants ~autostaking_enable:false () in
    let init_params =
      {
        limit_of_staking_over_baking = Q.one;
        edge_of_baking_over_staking = Q.one;
      }
    in
    let no_costake_params =
      {
        limit_of_staking_over_baking = Q.zero;
        edge_of_baking_over_staking = Q.one;
      }
    in
    let amount = Amount (Tez.of_mutez 1_000_000L) in
    (* init *)
    begin_test ~activate_ai:true constants ["delegate"]
    --> set_delegate_params "delegate" init_params
    --> add_account_with_funds
          "staker"
          "delegate"
          (Amount (Tez.of_mutez 2_000_000_000_000L))
    --> set_delegate "staker" (Some "delegate")
    --> wait_ai_activation --> next_cycle
    (* try stake in normal conditions *)
    --> stake "staker" amount
    (* Change delegate parameters to forbid staking *)
    --> set_delegate_params "delegate" no_costake_params
    (* The changes are not immediate *)
    --> stake "staker" amount
    (* The parameters change is applied exactly [preserved_cycles + 1] after the request *)
    --> wait_n_cycles (default_param_wait - 1)
    (* Not yet... *)
    --> stake "staker" amount
    --> next_cycle
    (* External staking is now forbidden *)
    --> assert_failure (stake "staker" amount)
    (* Can still self-stake *)
    --> stake "delegate" amount
    (* Can still unstake *)
    --> unstake "staker" Half
    --> wait_n_cycles (default_unstake_wait + 1)
    --> finalize_unstake "staker"
    (* Can authorize stake again *)
    --> set_delegate_params "delegate" init_params
    --> wait_n_cycles (default_param_wait - 1)
    (* Not yet... *)
    --> assert_failure (stake "staker" amount)
    --> next_cycle
    (* Now possible *)
    --> stake "staker" amount

  let tests =
    tests_of_scenarios
    @@ [
         ("Test simple roundtrip", init_scenario () --> simple_roundtrip);
         ("Test double roundtrip", init_scenario () --> double_roundtrip);
         ("Test preserved balance", init_scenario () --> status_quo_rountrip);
         ("Test finalize", init_scenario () --> scenario_finalize);
         ("Test no finalize", init_scenario () --> scenario_not_finalize);
         ( "Test forbidden operations",
           init_scenario () --> scenario_forbidden_operations );
         ( "Test full balance in finalizable",
           init_scenario () --> full_balance_in_finalizable );
         ("Test stake unstake every cycle", init_scenario () --> odd_behavior);
         ("Test change delegate", change_delegate);
         ("Test unset delegate", unset_delegate);
         ("Test forbid costake", forbid_costaking);
         ("Test stake from unstake", shorter_roundtrip_for_baker);
       ]
end

module Rewards = struct
  let test_wait_with_rewards =
    let constants =
      init_constants
        ~reward_per_block:1_000_000_000L
        ~autostaking_enable:false
        ()
    in
    begin_test ~activate_ai:true constants ["delegate"]
    --> (Tag "block step" --> wait_n_blocks 200
        |+ Tag "cycle step" --> wait_n_cycles 20
        |+ Tag "wait AI activation" --> next_block --> wait_ai_activation
           --> (Tag "block step" --> wait_n_blocks 100
               |+ Tag "cycle step" --> wait_n_cycles 10))

  let test_ai_curve_activation_time =
    let constants =
      init_constants
        ~reward_per_block:1_000_000_000L
        ~deactivate_dynamic:true
        ~autostaking_enable:false
        ()
    in
    let pc = constants.preserved_cycles in
    begin_test ~activate_ai:true ~burn_rewards:true constants [""]
    --> next_block --> save_current_rate (* before AI rate *)
    --> wait_ai_activation
    (* Rate remains unchanged right after AI activation, we must wait [pc + 1] cycles *)
    --> check_rate_evolution Q.equal
    --> wait_n_cycles pc
    --> check_rate_evolution Q.equal
    --> next_cycle
    (* The new rate should be active now. With the chosen constants, it should be lower.
       We go from 1000tz per day to (at most) 5% of 4_000_000tz per year *)
    --> check_rate_evolution Q.gt

  let test_static =
    let constants =
      init_constants
        ~reward_per_block:1_000_000_000L
        ~deactivate_dynamic:true
        ~autostaking_enable:false
        ()
    in
    let rate_var_lag = constants.preserved_cycles in
    let init_params =
      {
        limit_of_staking_over_baking = Q.one;
        edge_of_baking_over_staking = Q.one;
      }
    in
    let delta = Amount (Tez.of_mutez 20_000_000_000L) in
    let cycle_stake =
      save_current_rate --> stake "delegate" delta --> next_cycle
      --> check_rate_evolution Q.gt
    in
    let cycle_unstake =
      save_current_rate --> unstake "delegate" delta --> next_cycle
      --> check_rate_evolution Q.lt
    in
    let cycle_stable =
      save_current_rate --> next_cycle --> check_rate_evolution Q.equal
    in
    begin_test ~activate_ai:true ~burn_rewards:true constants ["delegate"]
    --> set_delegate_params "delegate" init_params
    --> save_current_rate --> wait_ai_activation
    (* We stake about 50% of the total supply *)
    --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
    --> stake "__bootstrap__" (Amount (Tez.of_mutez 1_800_000_000_000L))
    --> (Tag "increase stake, decrease rate" --> next_cycle
         --> loop rate_var_lag (stake "delegate" delta --> next_cycle)
         --> loop 10 cycle_stake
        |+ Tag "decrease stake, increase rate" --> next_cycle
           --> loop rate_var_lag (unstake "delegate" delta --> next_cycle)
           --> loop 10 cycle_unstake
        |+ Tag "stable stake, stable rate" --> next_cycle
           --> wait_n_cycles rate_var_lag --> loop 10 cycle_stable
        |+ Tag "test timing" --> wait_n_cycles rate_var_lag
           --> check_rate_evolution Q.equal
           --> next_cycle --> check_rate_evolution Q.gt --> save_current_rate
           --> (Tag "increase stake" --> stake "delegate" delta
                --> wait_n_cycles rate_var_lag
                --> check_rate_evolution Q.equal
                --> next_cycle --> check_rate_evolution Q.gt
               |+ Tag "decrease stake" --> unstake "delegate" delta
                  --> wait_n_cycles rate_var_lag
                  --> check_rate_evolution Q.equal
                  --> next_cycle --> check_rate_evolution Q.lt))

  let tests =
    tests_of_scenarios
    @@ [
         ("Test wait with rewards", test_wait_with_rewards);
         ("Test ai curve activation time", test_ai_curve_activation_time);
         (* ("Test static rate", test_static); *)
       ]
end

module Autostaking = struct
  let assert_balance_evolution ~loc ~for_accounts ~part ~name ~old_balance
      ~new_balance compare =
    let open Lwt_result_syntax in
    let old_b, new_b =
      match part with
      | `liquid ->
          ( Q.of_int64 @@ Tez.to_mutez old_balance.liquid_b,
            Q.of_int64 @@ Tez.to_mutez new_balance.liquid_b )
      | `staked -> (old_balance.staked_b, new_balance.staked_b)
      | `unstaked_frozen ->
          ( Q.of_int64 @@ Tez.to_mutez old_balance.unstaked_frozen_b,
            Q.of_int64 @@ Tez.to_mutez new_balance.unstaked_frozen_b )
      | `unstaked_finalizable ->
          ( Q.of_int64 @@ Tez.to_mutez old_balance.unstaked_finalizable_b,
            Q.of_int64 @@ Tez.to_mutez new_balance.unstaked_finalizable_b )
    in
    if List.mem ~equal:String.equal name for_accounts then
      if compare new_b old_b then return_unit
      else (
        Log.debug ~color:Log_module.warning_color "Balances changes failed:@." ;
        Log.debug "@[<v 2>Old Balance@ %a@]@." balance_pp old_balance ;
        Log.debug "@[<v 2>New Balance@ %a@]@." balance_pp new_balance ;
        failwith "%s Unexpected stake evolution for %s" loc name)
    else raise Not_found

  let delegate = "delegate"

  and delegator1 = "delegator1"

  and delegator2 = "delegator2"

  let setup ~activate_ai =
    let constants = init_constants ~autostaking_enable:true () in
    begin_test ~activate_ai constants [delegate]
    --> add_account_with_funds
          delegator1
          "__bootstrap__"
          (Amount (Tez.of_mutez 2_000_000_000L))
    --> add_account_with_funds
          delegator2
          "__bootstrap__"
          (Amount (Tez.of_mutez 2_000_000_000L))
    --> next_cycle
    --> (if activate_ai then wait_ai_activation else next_cycle)
    --> snapshot_balances "before delegation" [delegate]
    --> set_delegate delegator1 (Some delegate)
    --> check_snapshot_balances "before delegation"
    --> next_cycle

  let test_autostaking =
    Tag "No Ai" --> setup ~activate_ai:false
    --> check_snapshot_balances
          ~f:
            (assert_balance_evolution
               ~loc:__LOC__
               ~for_accounts:[delegate]
               ~part:`staked
               Q.gt)
          "before delegation"
    --> snapshot_balances "before second delegation" [delegate]
    --> (Tag "increase delegation"
         --> set_delegate delegator2 (Some delegate)
         --> next_cycle
         --> check_snapshot_balances
               ~f:
                 (assert_balance_evolution
                    ~loc:__LOC__
                    ~for_accounts:[delegate]
                    ~part:`staked
                    Q.gt)
               "before second delegation"
        |+ Tag "constant delegation"
           --> snapshot_balances "after stake change" [delegate]
           --> wait_n_cycles 8
           --> check_snapshot_balances "after stake change"
        |+ Tag "decrease delegation"
           --> set_delegate delegator1 None
           --> next_cycle
           --> check_snapshot_balances
                 ~f:
                   (assert_balance_evolution
                      ~loc:__LOC__
                      ~for_accounts:[delegate]
                      ~part:`staked
                      Q.lt)
                 "before second delegation"
           --> check_snapshot_balances
                 ~f:
                   (assert_balance_evolution
                      ~loc:__LOC__
                      ~for_accounts:[delegate]
                      ~part:`unstaked_frozen
                      Q.gt)
                 "before second delegation"
           --> snapshot_balances "after unstake" [delegate]
           --> next_cycle
           --> check_snapshot_balances "after unstake"
           --> wait_n_cycles 4
           --> check_snapshot_balances
                 ~f:
                   (assert_balance_evolution
                      ~loc:__LOC__
                      ~for_accounts:[delegate]
                      ~part:`unstaked_frozen
                      Q.lt)
                 "after unstake"
           (* finalizable are auto-finalize immediately  *)
           --> check_snapshot_balances
                 ~f:
                   (assert_balance_evolution
                      ~loc:__LOC__
                      ~for_accounts:[delegate]
                      ~part:`liquid
                      Q.lt)
                 "before finalisation")
    |+ Tag "Yes AI" --> setup ~activate_ai:true
       --> check_snapshot_balances "before delegation"

  let test_overdelegation =
    (* This test assumes that all delegate accounts created in [begin_test]
       begin with 4M tz, with 5% staked *)
    let constants = init_constants ~autostaking_enable:true () in
    begin_test
      ~activate_ai:false
      constants
      ["delegate"; "faucet1"; "faucet2"; "faucet3"]
    --> add_account_with_funds
          "delegator_to_fund"
          "delegate"
          (Amount (Tez.of_mutez 3_600_000_000_000L))
    (* Delegate has 200k staked and 200k liquid *)
    --> set_delegate "delegator_to_fund" (Some "delegate")
    (* Delegate stake will not change at the end of cycle: same stake *)
    --> next_cycle
    --> check_balance_field "delegate" `Staked (Tez.of_mutez 200_000_000_000L)
    --> transfer
          "faucet1"
          "delegator_to_fund"
          (Amount (Tez.of_mutez 3_600_000_000_000L))
    (* Delegate is not overdelegated, but will need to freeze 180k *)
    --> next_cycle
    --> check_balance_field "delegate" `Staked (Tez.of_mutez 380_000_000_000L)
    --> transfer
          "faucet2"
          "delegator_to_fund"
          (Amount (Tez.of_mutez 3_600_000_000_000L))
    (* Delegate is now overdelegated, it will freeze 100% *)
    --> next_cycle
    --> check_balance_field "delegate" `Staked (Tez.of_mutez 400_000_000_000L)
    --> transfer
          "faucet3"
          "delegator_to_fund"
          (Amount (Tez.of_mutez 3_600_000_000_000L))
    (* Delegate is overmegadelegated *)
    --> next_cycle
    --> check_balance_field "delegate" `Staked (Tez.of_mutez 400_000_000_000L)

  let tests =
    tests_of_scenarios
      [
        ("Test auto-staking", test_autostaking);
        ("Test auto-staking with overdelegation", test_overdelegation);
      ]
end

module Slashing = struct
  let test_simple_slash =
    let constants = init_constants ~autostaking_enable:false () in
    let any_slash =
      Tag "double baking" --> double_bake "delegate"
      |+ Tag "double attesting" --> double_attest "delegate"
      |+ Tag "double preattesting" --> double_preattest "delegate"
    in
    begin_test
      ~activate_ai:true
      constants
      ["delegate"; "bootstrap1"; "bootstrap2"]
    --> (Tag "No AI" --> next_cycle
        |+ Tag "Yes AI" --> next_block --> wait_ai_activation)
    --> any_slash
    --> snapshot_balances "before slash" ["delegate"]
    --> ((Tag "denounce same cycle" --> make_denunciations ()
         |+ Tag "denounce next cycle" --> next_cycle --> make_denunciations ())
         --> (Empty
             |+ Tag "another slash"
                (* delegate can be forbidden in this case, so we set another baker *)
                --> set_baker "bootstrap1"
                --> any_slash --> make_denunciations ())
         --> check_snapshot_balances "before slash"
         --> exec_unit check_pending_slashings
         --> next_cycle
         --> assert_failure (check_snapshot_balances "before slash")
         --> exec_unit check_pending_slashings
         --> next_block
        |+ Tag "denounce too late" --> next_cycle --> next_cycle
           --> assert_failure
                 ~expected_error:(fun (_block, state) ->
                   let ds = state.State.double_signings in
                   let ds = match ds with [a] -> a | _ -> assert false in
                   let kind =
                     match ds.kind with
                     | Double_baking -> Protocol.Validate_errors.Anonymous.Block
                     | Double_attesting -> Attestation
                     | Double_preattesting -> Preattestation
                   in
                   let level =
                     Protocol.Alpha_context.Raw_level.of_int32_exn
                       (Int32.succ ds.level)
                   in
                   let last_cycle =
                     Cycle.add
                       (Block.current_cycle_of_level
                          ~blocks_per_cycle:
                            state.State.constants.blocks_per_cycle
                          ~current_level:ds.level)
                       Protocol.Constants_repr.max_slashing_period
                   in
                   [
                     Environment.Ecoproto_error
                       (Protocol.Validate_errors.Anonymous.Outdated_denunciation
                          {kind; level; last_cycle});
                   ])
                 (make_denunciations ())
           --> check_snapshot_balances "before slash")

  let check_is_forbidden baker = assert_failure (next_block_with_baker baker)

  let check_is_not_forbidden baker =
    let open Lwt_result_syntax in
    exec (fun ((block, state) as input) ->
        let baker = State.find_account baker state in
        let*! _ = Block.bake ~policy:(By_account baker.pkh) block in
        return input)

  let test_delegate_forbidden =
    let constants =
      init_constants ~blocks_per_cycle:30l ~autostaking_enable:false ()
    in
    begin_test
      ~activate_ai:false
      constants
      ["delegate"; "bootstrap1"; "bootstrap2"]
    --> set_baker "bootstrap1"
    --> (Tag "Many double bakes"
         --> loop_action 14 (double_bake_ "delegate")
         --> (Tag "14 double bakes are not enough to forbid a delegate"
              (*  7*14 = 98 *)
              --> make_denunciations ()
              --> check_is_not_forbidden "delegate"
             |+ Tag "15 double bakes is one too many"
                (*  7*15 = 105 > 100 *)
                --> double_bake "delegate"
                --> make_denunciations ()
                --> check_is_forbidden "delegate")
        |+ Tag "Two double attestations, in same cycle"
           --> double_attest "delegate"
           --> (Tag "very early first denounce" --> make_denunciations ()
               |+ Empty)
           --> double_attest "delegate"
           --> check_is_not_forbidden "delegate"
           --> make_denunciations ()
           (* Is forbidden the moment the denunciations are included *)
           --> check_is_forbidden "delegate"
        |+ Tag "Two double attestations, in consecutive cycles"
           --> double_attest "delegate"
           --> (Tag "early first denounce" --> make_denunciations ()
                --> next_cycle
               |+ Tag "late first denounce" --> next_cycle
                  --> make_denunciations ())
           --> double_attest "delegate"
           (* Forbidden iff the cycle of the denunciation and the previous one
              contains enough double signing events (not denunciations)
              to forbid the delegate *)
           --> (Tag "early second denounce" --> make_denunciations ()
                --> check_is_forbidden "delegate"
               |+ Tag "late second denounce" --> next_cycle
                  --> make_denunciations ()
                  --> check_is_not_forbidden "delegate")
        |+ Tag "Two double attestations, too far apart to forbid"
           --> double_attest "delegate"
           --> (Tag "early first denounce" --> make_denunciations ()
                --> next_cycle
               |+ Tag "late first denounce" --> next_cycle
                  --> make_denunciations ())
           --> next_cycle
           --> double_attest "delegate"
               (* Forbidden iff the cycle of the denunciation and the previous one
                   contains enough double signing events (not denunciations)
                  to forbid the delegate *)
           --> (Tag "early second denounce" --> make_denunciations ()
               |+ Tag "late second denounce" --> next_cycle
                  --> make_denunciations ())
           --> check_is_not_forbidden "delegate"
        |+ Tag
             "Two double attestations, in consecutive cycles, denounce out of \
              order" --> double_attest "delegate" --> next_cycle
           --> double_attest "delegate"
           --> make_denunciations
                 ~filter:(fun {level; denounced; _} ->
                   (not denounced) && level > 10l)
                 ()
           --> make_denunciations
                 ~filter:(fun {level; denounced; _} ->
                   (not denounced) && level <= 10l)
                 ()
           --> check_is_forbidden "delegate")

  let test_slash_unstake =
    let constants = init_constants ~autostaking_enable:false () in
    begin_test
      ~activate_ai:false
      constants
      ["delegate"; "bootstrap1"; "bootstrap2"]
    --> set_baker "bootstrap1" --> next_cycle --> unstake "delegate" Half
    --> next_cycle --> double_bake "delegate" --> make_denunciations ()
    --> next_cycle --> double_bake "delegate" --> make_denunciations ()
    --> wait_n_cycles 5
    --> finalize_unstake "delegate"

  let test_slash_monotonous_stake =
    let scenario ~op ~early_d =
      let constants =
        init_constants ~blocks_per_cycle:8l ~autostaking_enable:false ()
      in
      begin_test ~activate_ai:false constants ["delegate"]
      --> next_cycle
      --> loop
            6
            (op "delegate" (Amount (Tez.of_mutez 1_000_000_000L)) --> next_cycle)
      --> loop
            10
            (op "delegate" (Amount (Tez.of_mutez 1_000_000_000L))
            --> double_bake "delegate"
            -->
            if early_d then make_denunciations () --> next_cycle
            else next_cycle --> make_denunciations ())
    in
    Tag "slashes with increasing stake"
    --> (Tag "denounce early" --> scenario ~op:stake ~early_d:true
        |+ Tag "denounce late" --> scenario ~op:stake ~early_d:false)
    |+ Tag "slashes with decreasing stake"
       --> (Tag "denounce early" --> scenario ~op:unstake ~early_d:true
           |+ Tag "denounce late" --> scenario ~op:unstake ~early_d:false)

  let test_slash_timing =
    let constants =
      init_constants ~blocks_per_cycle:8l ~autostaking_enable:false ()
    in
    begin_test ~activate_ai:false constants ["delegate"]
    --> next_cycle
    --> (Tag "stake" --> stake "delegate" Half
        |+ Tag "unstake" --> unstake "delegate" Half)
    --> (Tag "with a first slash" --> double_bake "delegate"
         --> make_denunciations ()
        |+ Tag "without another slash" --> Empty)
    --> List.fold_left
          (fun acc i ->
            acc |+ Tag (string_of_int i ^ " cycles lag") --> wait_n_cycles i)
          Empty
          [3; 4; 5; 6]
    --> double_bake "delegate" --> make_denunciations () --> next_cycle

  let init_scenario_with_delegators delegate_name faucet_name delegators_list =
    let constants = init_constants ~autostaking_enable:false () in
    let rec init_delegators = function
      | [] -> Empty
      | (delegator, amount) :: t ->
          add_account_with_funds
            delegator
            faucet_name
            (Amount (Tez.of_mutez amount))
          --> set_delegate delegator (Some delegate_name)
          --> init_delegators t
    in
    let init_params =
      {
        limit_of_staking_over_baking = Q.one;
        edge_of_baking_over_staking = Q.one;
      }
    in
    begin_test ~activate_ai:true constants [delegate_name; faucet_name]
    --> set_baker faucet_name
    --> set_delegate_params "delegate" init_params
    --> init_delegators delegators_list
    --> next_block --> wait_ai_activation

  let test_many_slashes =
    let rec stake_unstake_for = function
      | [] -> Empty
      | staker :: t ->
          stake staker Half --> unstake staker Half --> stake_unstake_for t
    in
    let slash delegate = double_bake delegate --> make_denunciations () in
    Tag "double bake"
    --> (Tag "solo delegate"
        --> init_scenario_with_delegators
              "delegate"
              "faucet"
              [("delegator", 1_234_567_891L)]
        --> loop
              10
              (stake_unstake_for ["delegate"]
              --> slash "delegate" --> next_cycle))
  (* |+ Tag "delegate with one staker"
        --> init_scenario_with_delegators
              "delegate"
              "faucet"
              [("staker", 1_234_356_891L)]
        --> loop
              10
              (stake_unstake_for ["delegate"; "staker"]
              --> slash "delegate" --> next_cycle)
     |+ Tag "delegate with three stakers"
        --> init_scenario_with_delegators
              "delegate"
              "faucet"
              [
                ("staker1", 1_234_356_891L);
                ("staker2", 1_234_356_890L);
                ("staker3", 1_723_333_111L);
              ]
        --> loop
              10
              (stake_unstake_for
                 ["delegate"; "staker1"; "staker2"; "staker3"]
              --> slash "delegate" --> next_cycle))*)

  let test_no_shortcut_for_cheaters =
    let constants = init_constants ~autostaking_enable:false () in
    let amount = Amount (Tez.of_mutez 333_000_000_000L) in
    let preserved_cycles = constants.preserved_cycles in
    begin_test ~activate_ai:true constants ["delegate"]
    --> next_block --> wait_ai_activation
    --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
    --> next_cycle --> double_bake "delegate" --> make_denunciations ()
    --> next_cycle
    --> snapshot_balances "init" ["delegate"]
    --> unstake "delegate" amount
    --> (List.fold_left
           (fun acc i -> acc |+ Tag (fs "wait %i cycles" i) --> wait_n_cycles i)
           (Tag "wait 0 cycles" --> Empty)
           (Stdlib.List.init (preserved_cycles - 1) (fun i -> i + 1))
         --> stake "delegate" amount
         --> assert_failure (check_snapshot_balances "init")
        |+ Tag "wait enough cycles (preserved cycles + 1)"
           --> wait_n_cycles (preserved_cycles + 1)
           --> stake "delegate" amount
           --> check_snapshot_balances "init")

  let test_slash_correct_amount_after_stake_from_unstake =
    let constants = init_constants ~autostaking_enable:false () in
    let amount_to_unstake = Amount (Tez.of_mutez 200_000_000_000L) in
    let amount_to_restake = Amount (Tez.of_mutez 100_000_000_000L) in
    let amount_expected_in_unstake_after_slash = Tez.of_mutez 50_000_000_000L in
    let preserved_cycles = constants.preserved_cycles in
    begin_test ~activate_ai:true constants ["delegate"]
    --> next_block --> wait_ai_activation
    --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
    --> next_cycle
    --> unstake "delegate" amount_to_unstake
    --> stake "delegate" amount_to_restake
    --> List.fold_left
          (fun acc i -> acc |+ Tag (fs "wait %i cycles" i) --> wait_n_cycles i)
          (Tag "wait 0 cycles" --> Empty)
          (Stdlib.List.init (preserved_cycles - 2) (fun i -> i + 1))
    --> double_attest "delegate" --> make_denunciations () --> next_cycle
    --> check_balance_field
          "delegate"
          `Unstaked_frozen_total
          amount_expected_in_unstake_after_slash

  (* Test a non-zero request finalizes for a non-zero amount if it hasn't been slashed 100% *)
  let test_mini_slash =
    let constants = init_constants ~autostaking_enable:false () in
    (Tag "Yes AI"
     --> begin_test ~activate_ai:true constants ["delegate"; "baker"]
     --> next_block --> wait_ai_activation
    |+ Tag "No AI"
       --> begin_test ~activate_ai:false constants ["delegate"; "baker"])
    --> unstake "delegate" (Amount Tez.one_mutez)
    --> set_baker "baker" --> next_cycle
    --> ((Tag "7% slash" --> double_bake "delegate" --> make_denunciations ()
         |+ Tag "99% slash" --> next_cycle --> double_attest "delegate"
            --> loop 7 (double_bake "delegate")
            --> make_denunciations ())
        --> next_cycle
        --> check_balance_field "delegate" `Unstaked_frozen_total Tez.zero)
    --> wait_n_cycles (constants.preserved_cycles + 1)

  let test_slash_rounding =
    let constants = init_constants ~autostaking_enable:false () in
    begin_test ~activate_ai:true constants ["delegate"; "baker"]
    --> set_baker "baker" --> next_block --> wait_ai_activation
    --> unstake "delegate" (Amount (Tez.of_mutez 2L))
    --> next_cycle --> double_bake "delegate" --> double_bake "delegate"
    --> make_denunciations () --> wait_n_cycles 7
    --> finalize_unstake "delegate"

  (* TODO #6645: reactivate tests *)
  let tests =
    tests_of_scenarios
    @@ [
         ("Test simple slashing", test_simple_slash);
         ("Test slashed is forbidden", test_delegate_forbidden);
         ("Test slash with unstake", test_slash_unstake);
         ("Test slashes with simple varying stake", test_slash_monotonous_stake);
         ( "Test multiple slashes with multiple stakes/unstakes",
           test_many_slashes );
         ("Test slash timing", test_slash_timing);
         ( "Test stake from unstake deactivated when slashed",
           test_no_shortcut_for_cheaters );
         ( "Test stake from unstake reduce initial amount",
           test_slash_correct_amount_after_stake_from_unstake );
         ("Test unstake 1 mutez then slash", test_mini_slash);
         ("Test slash rounding", test_slash_rounding);
       ]
end

let tests =
  let open Lwt_result_syntax in
  (tests_of_scenarios
  @@ [
       ("Test expected error in assert failure", test_expected_error);
       ("Test init", init_scenario () --> Action (fun _ -> return_unit));
     ])
  @ Roundtrip.tests @ Rewards.tests @ Autostaking.tests @ Slashing.tests

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [("adaptive issuance roundtrip", tests)]
  |> Lwt_main.run
