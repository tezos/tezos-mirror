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

(** Returns when the number of bootstrap accounts created by [Context.init_n n] is not equal to [n] *)
type error += Inconsistent_number_of_bootstrap_accounts

let default_param_cd, default_unstake_cd =
  let constants = Default_parameters.constants_test in
  let pc = constants.preserved_cycles in
  let msp = constants.max_slashing_period in
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

(** Double attestation helpers *)
let order_attestations ~correct_order op1 op2 =
  let oph1 = Protocol.Alpha_context.Operation.hash op1 in
  let oph2 = Protocol.Alpha_context.Operation.hash op2 in
  let c = Operation_hash.compare oph1 oph2 in
  if correct_order then if c < 0 then (op1, op2) else (op2, op1)
  else if c < 0 then (op2, op1)
  else (op1, op2)

let double_attestation ctxt ?(correct_order = true) op1 op2 =
  let e1, e2 = order_attestations ~correct_order op1 op2 in
  Op.double_attestation ctxt e1 e2

let double_preattestation ctxt ?(correct_order = true) op1 op2 =
  let e1, e2 = order_attestations ~correct_order op1 op2 in
  Op.double_preattestation ctxt e1 e2

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
  | Half -> Tez.div_exn all 2
  | Max_tez -> Tez.max_mutez
  | Amount a -> a

let default_params =
  let Protocol.Staking_parameters_repr.
        {
          limit_of_staking_over_baking_millionth;
          edge_of_baking_over_staking_billionth;
        } =
    Protocol.Staking_parameters_repr.default  in
  {
    limit_of_staking_over_baking =
      Int32.to_int limit_of_staking_over_baking_millionth;
    edge_of_baking_over_staking =
      Int32.to_int edge_of_baking_over_staking_billionth;
  }

(** Module for the [State.t] type of asserted information about the system during a test. *)
module State = struct
  (** Type of the state *)
  type t = {
    account_names : string list;
    account_map : account_map;
    total_supply : Tez.t;
    constants : Protocol.Alpha_context.Constants.Parametric.t;
    param_requests : (string * staking_parameters * int) list;
    activate_ai : bool;
    baker : string;
    last_level_rewards : Protocol.Alpha_context.Raw_level.t;
    snapshot_balances : (string * balance) list String.Map.t;
    pending_operations : Protocol.Alpha_context.packed_operation list;
  }

  (** Expected number of cycles before staking parameters get applied *)
  let param_cd state = state.constants.preserved_cycles + 1

  (** Expected number of cycles before staking unstaked funds get unfrozen *)
  let unstake_cd state =
    let pc = state.constants.preserved_cycles in
    let msp = state.constants.max_slashing_period in
    pc + msp

  (** From a name, returns the corresponding account *)
  let find_account (account_name : string) (state : t) : account_state =
    match String.Map.find account_name state.account_map with
    | None -> raise Not_found
    | Some r -> r

  (** Returns true iff account is a delegate *)
  let is_self_delegate (account_name : string) (state : t) : bool =
    let acc = find_account account_name state in
    match acc.delegate with
    | None -> false
    | Some d ->
        let del = find_account d state in
        String.equal del.name acc.name

  let update_map ?(log_updates = []) ~(f : account_map -> account_map)
      (state : t) : t =
    let log_updates = List.sort_uniq String.compare log_updates in
    let new_state = {state with account_map = f state.account_map} in
    List.iter
      (fun x ->
        log_debug_balance_update x state.account_map new_state.account_map)
      log_updates ;
    new_state

  let apply_transfer amount src_name dst_name (state : t) : t =
    let f = apply_transfer amount src_name dst_name in
    update_map ~log_updates:[src_name; dst_name] ~f state

  let apply_stake amount staker_name (state : t) : t =
    let f = apply_stake amount staker_name in
    update_map ~log_updates:[staker_name] ~f state

  let apply_unstake cycle amount staker_name (state : t) : t =
    let f = apply_unstake cycle amount staker_name in
    update_map ~log_updates:[staker_name] ~f state

  let apply_finalize staker_name (state : t) : t =
    let f = apply_finalize staker_name in
    update_map ~log_updates:[staker_name] ~f state

  let apply_unslashable cycle (state : t) : t =
    let f = apply_unslashable cycle in
    (* no log *)
    update_map ~f state

  let apply_rewards block (state : t) : t tzresult Lwt.t =
    let open Lwt_result_syntax in
    let {last_level_rewards; total_supply; constants = _; _} = state in
    let*? current_level = Context.get_level (B block) in
    (* We assume one block per minute *)
    let* rewards_per_block = Context.get_issuance_per_minute (B block) in
    if Tez.(rewards_per_block = zero) then return state
    else
      let delta_time =
        Protocol.Alpha_context.Raw_level.diff current_level last_level_rewards
        |> Int32.to_int
      in
      let ({parameters; _} as baker) = find_account state.baker state in
      let delta_rewards = Tez.mul_exn rewards_per_block delta_time in
      if delta_time = 1 then
        Log.info ~color:tez_color "+%aꜩ" Tez.pp rewards_per_block
      else if delta_time > 1 then
        Log.info
          ~color:tez_color
          "+%aꜩ (over %d blocks, %aꜩ per block)"
          Tez.pp
          delta_rewards
          delta_time
          Tez.pp
          rewards_per_block
      else assert false ;
      let to_liquid =
        Tez.mul_q
          delta_rewards
          (Q.of_ints parameters.edge_of_baking_over_staking 1_000_000_000)
      in
      let to_liquid = Partial_tez.to_tez ~round_up:true to_liquid in
      let to_frozen = Tez.(delta_rewards -! to_liquid) in
      let state =
        update_map ~f:(add_liquid_rewards to_liquid baker.name) state
      in
      let state =
        update_map ~f:(add_frozen_rewards to_frozen baker.name) state
      in
      let* total_supply = Tez.(total_supply + delta_rewards) in
      return {state with last_level_rewards = current_level; total_supply}

  (* TODO *)
  let apply_slashing _pct _delegate_name (state : t) : t = state

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

  (** When reaching a new cycle: apply unstakes and parameters changes.
    We expect these changes after applying the last block of a cycle *)
  let apply_end_cycle new_cycle state : t =
    let unstake_cd = unstake_cd state in
    (* Prepare finalizable unstakes *)
    let state =
      match Cycle.sub new_cycle unstake_cd with
      | None -> state
      | Some cycle -> apply_unslashable cycle state    in
    (* Apply parameter changes *)
    let state, param_requests =
      List.fold_left
        (fun (state, remaining_requests) (name, params, cd) ->
          if cd > 0 then (state, (name, params, cd - 1) :: remaining_requests)
          else
            let src = find_account name state in
            let state =
              update_account name {src with parameters = params} state
            in
            (state, remaining_requests))
        (state, [])
        state.param_requests    in
    (* Refresh initial amount of frozen deposits at cycle end *)
    let state =
      update_map
        ~f:
          (String.Map.map (fun x ->
               {
                 x with
                 frozen_deposits =
                   Frozen_tez.refresh_at_new_cycle x.frozen_deposits;
               }))
        state    in
    {state with param_requests}

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
    remain unnamed, and even then it can create conflictng names.)
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
 fun scenario input ->
  match scenario with
  | End_scenario -> return input
  | Cons (action, next) -> action input >>=? run_scenario next

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
        title ^ ": " ^ String.concat ", " tags  in
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
  Action
    (fun _ ->
      Log.info ~color:begin_end_color "-- End test --" ;
      return_unit)

(** Transforms scenarios into Alcotest tests *)
let tests_of_scenarios :
    (string * (unit, t) scenarios) list -> unit Alcotest_lwt.test_case list =
 fun scenarios ->
  List.map (fun (s, x) -> Tag s --> x --> end_test) scenarios |> function
  | [] -> []
  | a :: t ->
      List.fold_left ( |+ ) a t |> unfold_scenarios |> List.map unfolded_to_test

(** Arbitrary execution *)
let exec f = Action f

(** Execute a function that does not modify the block, only the state *)
let exec_state f =  let open Lwt_result_syntax in
  Action
    (fun ((block, _state) as input) ->
      let* state = f input in
      return (block, state))

(* ======== Baking ======== *)

(** After baking and applying rewards in state *)
let check_all_balances block state : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let State.{account_map; total_supply; _} = state in
  let* () =
    String.Map.iter_es
      (fun _name account ->
        log_debug_balance account.name account_map ;
        assert_balance_check ~loc:__LOC__ (B block) account.name account_map)
      account_map
  in
  let* actual_total_supply = Context.get_total_supply (B block) in
  Assert.equal_tez ~loc:__LOC__ actual_total_supply total_supply

(** Apply rewards in state + check *)
let apply_rewards block state : State.t tzresult Lwt.t =  let open Lwt_result_syntax in
  let* state = State.apply_rewards block state in
  let* () = check_all_balances block state in
  return state

(** Bake a block, with the given baker and the given operations. *)
let bake ?baker : t -> t tzresult Lwt.t =
 fun (block, state) ->
  let open Lwt_result_syntax in
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
  let baker =
    match baker with
    | None -> (
        try State.find_account state.baker state
        with Not_found ->
          Log.info
            ~color:warning_color
            "Invalid baker: %s not found. Aborting"
            state.baker ;
          assert false)
    | Some baker -> baker
  in
  let policy = Block.By_account baker.pkh in
  let state, operations = State.pop_pending_operations state in
  let* block = Block.bake ~policy ~adaptive_issuance_vote ~operations block in
  (* TODO: mistake ? They apply before we reach the new cycle... *)
  let new_current_cycle = Block.current_cycle block in
  let state =
    if Protocol.Alpha_context.Cycle.(current_cycle = new_current_cycle) then
      state
    else (
      Log.info
        ~color:time_color
        "Cycle %d"
        (Protocol.Alpha_context.Cycle.to_int32 new_current_cycle |> Int32.to_int) ;
      State.apply_end_cycle new_current_cycle state)
  in
  let* state = apply_rewards block state in
  return (block, state)

(** Bake until the end of a cycle, using [bake] instead of [Block.bake]
    Should be slower because checks balances at the end of every block (avoidable in some cases) *)
let bake_until_cycle_end_slow : t -> t tzresult Lwt.t =
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
  exec_state (fun (_block, state) -> return {state with State.baker})

(** Creates a snapshot of the current balances for the given account names.
    Can be used to check that balances at point A and B in the execution of a test
    are the same (either nothing happened, or a succession of actions resulted in
    getting the same values as before *)
let snapshot_balances snap_name names_list : (t, t) scenarios =
  exec_state (fun (_block, state) ->
      Log.debug
        ~color:low_debug_color
        "Snapshoting balances as \"%s\""
        snap_name ;
      let balances =        List.map
          (fun name -> (name, balance_of_account name state.State.account_map))
          names_list      in      let snapshot_balances =
        String.Map.add snap_name balances state.snapshot_balances      in
      return {state with snapshot_balances})

(** Check balances against a previously defined snapshot *)
let check_snapshot_balances snap_name : (t, t) scenarios =  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      Log.debug
        ~color:low_debug_color
        "Checking equality of balances between \"%s\" and now"
        snap_name ;      let snapshot_balances =
        String.Map.find snap_name state.State.snapshot_balances      in
      match snapshot_balances with
      | None ->
          Log.debug
            ~color:warning_color
            "\"%s\" snapshot not found..."
            snap_name ;
          return state
      | Some snapshot_balances ->
          let* () =
            List.iter_es
              (fun (name, old_balance) ->
                let new_balance =
                  balance_of_account name state.State.account_map
                in
                assert_balance_equal ~loc:__LOC__ old_balance new_balance)
              snapshot_balances
          in
          return state)

(* ======== Operations ======== *)

(** Bake a single block *)
let next_block =
  exec (fun input ->
      Log.info ~color:action_color "[Next block]" ;
      bake input)

(** Bake until the end of a cycle *)
let next_cycle =
  exec (fun input ->
      let open Lwt_result_syntax in
      Log.info ~color:action_color "[Next cycle]" ;
      let block, (State.{constants; activate_ai; _} as state) = input in
      if
        Tez.(constants.issuance_weights.base_total_issued_per_minute = zero)
        || not activate_ai
      then
        (* Apply rewards in state only after the while cycle ends *)
        let new_cycle = Cycle.succ (Block.current_cycle block) in
        let baker = State.find_account state.baker state in
        let policy = Block.By_account baker.pkh in
        let* block, state =
          if state.pending_operations = [] then return (block, state)
          else bake input
        in
        let* block = Block.bake_until_cycle new_cycle ~policy block in
        (* TODO: other way around ?? *)
        let state = State.apply_end_cycle new_cycle state in
        let* state = apply_rewards block state in
        return (block, state)
      else
        (* Apply rewards in state every block *)
        bake_until_cycle_end_slow input)

(** Executes an operation: f should return a new state and a list of operations, which are then applied *)
let exec_op f =  let open Lwt_result_syntax in
  Action
    (fun ((block, _state) as input) ->
      let* state, ops = f input in
      let state = State.add_pending_operations ops state in
      return (block, state))
  --> next_block

(* ======== Definition of basic actions ======== *)

(** Initialize the test, given some initial parameters *)
let begin_test ~activate_ai
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
              {constants.adaptive_issuance with launch_ema_threshold = 0l};
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
            let frozen_deposits = Frozen_tez.init init_staked name in
            let pkh = Context.Contract.pkh contract in
            let account =
              init_account
                ~name
                ~delegate:name
                ~pkh
                ~contract
                ~parameters:default_params
                ~liquid
                ~frozen_deposits
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
      let baker = bootstrap in
      let* total_supply = Context.get_total_supply (B block) in
      let state =
        State.
          {
            account_names = delegates_name_list;
            account_map;
            total_supply;
            constants;
            param_requests = [];
            activate_ai;
            baker;
            last_level_rewards = init_level;
            snapshot_balances = String.Map.empty;
            pending_operations = [];
          }
      in
      let* () = check_all_balances block state in
      return (block, state))

(** Set delegate parameters for the given delegate *)
let set_delegate_params delegate_name params : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      (* Simple example of action_atom definition: *)
      let delegate = State.find_account delegate_name state in
      Log.info
        ~color:action_color
        "[Set delegate parameters for \"%s\"]"
        delegate.name ;
      (* Define the operation *)
      let* operation =
        set_delegate_parameters
          (B block)
          delegate.contract
          ~limit_of_staking_over_baking:params.limit_of_staking_over_baking
          ~edge_of_baking_over_staking_billionth:
            params.edge_of_baking_over_staking
      in
      (* Update state *)
      let cd = state.constants.preserved_cycles - 1 in
      let state =
        {
          state with
          param_requests = (delegate_name, params, cd) :: state.param_requests;
        }
      in
      (* Return both *)
      return (state, [operation]))

(** Add a new account with the given name *)
let add_account name : (t, t) scenarios =
  exec_state (fun (_block, state) ->
      Log.info ~color:action_color "[Add account \"%s\"]" name ;
      let new_account = Account.new_account () in
      let pkh = new_account.pkh in
      let contract = Protocol.Alpha_context.Contract.Implicit pkh in
      let account_state =
        init_account ~name ~pkh ~contract ~parameters:default_params ()
      in
      let state = State.update_account name account_state state in
      let state = {state with account_names = name :: state.account_names} in
      return state)

(** Reveal operation *)
let reveal name : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let account = State.find_account name state in
      Log.info ~color:action_color "[Reveal \"%s\"]" account.name ;
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
        src.name
        dst.name
        Tez.pp
        amount ;
      let* operation =
        Op.transaction ~fee:Tez.zero (B block) src.contract dst.contract amount
      in
      let state = State.apply_transfer amount src.name dst.name state in
      return (state, [operation]))

(** Set delegate for src. If [delegate_name_opt = None], then unset current delegate *)
let set_delegate src_name delegate_name_opt : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let src = State.find_account src_name state in
      let delegate_pkh_opt =
        match delegate_name_opt with
        | None ->
            Log.info ~color:action_color "[Unset delegate of \"%s\"]" src.name ;
            None
        | Some delegate_name ->
            let delegate = State.find_account delegate_name state in
            Log.info
              ~color:action_color
              "[Set delegate \"%s\" for \"%s\"]"
              delegate.name
              src.name ;
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
        src.name
        tez_quantity_pp
        stake_value ;
      (* Stake applies finalize *before* the stake *)
      let state = State.apply_finalize src_name state in
      let amount = quantity_to_tez src.liquid stake_value in
      let* operation = stake (B block) src.contract amount in
      let state = State.apply_stake amount src_name state in
      return (state, [operation]))

(** unstake operation *)
let unstake src_name unstake_value : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let src = State.find_account src_name state in
      Log.info
        ~color:action_color
        "[Unstake for \"%s\" (%a)]"
        src.name
        tez_quantity_pp
        unstake_value ;
      let stake_balance =
        (balance_of_account src_name state.account_map).staked_b
        |> Partial_tez.to_tez
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
      Log.info ~color:action_color "[Finalize_unstake for \"%s\"]" src.name ;
      let* operation = finalize_unstake (B block) src.contract in
      let state = State.apply_finalize src_name state in
      return (state, [operation]))


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
