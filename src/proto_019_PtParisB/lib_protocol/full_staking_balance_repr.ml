(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module is responsible for the construction, observation and encoding of
    full staking balances that are maintained to be used at cycle end to compute
    staking rights.

    The module will handle a lazy migration starting at protocol P that adds two
    new fields to the balance, the minimal delegated balance over the cycle and
    the last level at which it has been modified.
    As there is no trivial default value for Level_repr, the
    level_of_min_delegated is optional but the module must preserve the
    invariant that if a min_delegated_in_cycle has been stored, a level is
    stored with it.
*)

type t = {
  own_frozen : Tez_repr.t;
  staked_frozen : Tez_repr.t;
  delegated : Tez_repr.t;
  min_delegated_in_cycle : Tez_repr.t;
  level_of_min_delegated : Level_repr.t option;
}

let cycle_of_min_delegated (level_of_min_delegated : Level_repr.t option) =
  match level_of_min_delegated with
  | None -> Cycle_repr.root
  | Some l -> l.cycle

let init ~own_frozen ~staked_frozen ~delegated ~current_level =
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle = delegated;
    level_of_min_delegated = Some current_level;
  }

let encoding =
  let open Data_encoding in
  (* This encoding is backward-compatible with the encoding used in Oxford, so
     as to avoid a stitching in P. It will act as a lazy migration.
     The case in which [added_in_p] is [None] happen only for pre-existing
     values in the storage.
     For them, using [(delegated, None)] and using Cycle_repr.root when no level
     is set will behave correctly. *)
  let added_in_p =
    obj2
      (req "min_delegated_in_cycle" Tez_repr.encoding)
      (req "level_of_min_delegated" (option Level_repr.encoding))
  in
  conv
    (fun {
           own_frozen;
           staked_frozen;
           delegated;
           min_delegated_in_cycle;
           level_of_min_delegated;
         } ->
      ( own_frozen,
        staked_frozen,
        delegated,
        Some (min_delegated_in_cycle, level_of_min_delegated) ))
    (fun (own_frozen, staked_frozen, delegated, added_in_p_opt) ->
      let min_delegated_in_cycle, level_of_min_delegated =
        added_in_p_opt |> Option.value ~default:(delegated, None)
      in
      {
        own_frozen;
        staked_frozen;
        delegated;
        min_delegated_in_cycle;
        level_of_min_delegated;
      })
    (obj4
       (req "own_frozen" Tez_repr.encoding)
       (req "staked_frozen" Tez_repr.encoding)
       (req "delegated" Tez_repr.encoding)
       (varopt "min_delegated_in_cycle_and_level" added_in_p))

let voting_weight
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle = _;
      level_of_min_delegated = _;
    } =
  let open Result_syntax in
  let* frozen = Tez_repr.(own_frozen +? staked_frozen) in
  let+ all = Tez_repr.(frozen +? delegated) in
  Tez_repr.to_mutez all

let apply_slashing ~percentage
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle;
      level_of_min_delegated;
    } =
  let remaining_percentage = Percentage.neg percentage in
  let own_frozen =
    Tez_repr.mul_percentage ~rounding:`Down own_frozen remaining_percentage
  in
  let staked_frozen =
    Tez_repr.mul_percentage ~rounding:`Down staked_frozen remaining_percentage
  in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    level_of_min_delegated;
  }

let own_frozen
    {
      own_frozen;
      staked_frozen = _;
      delegated = _;
      min_delegated_in_cycle = _;
      level_of_min_delegated = _;
    } =
  own_frozen

let staked_frozen
    {
      own_frozen = _;
      staked_frozen;
      delegated = _;
      min_delegated_in_cycle = _;
      level_of_min_delegated = _;
    } =
  staked_frozen

let total_frozen
    {
      own_frozen;
      staked_frozen;
      delegated = _;
      min_delegated_in_cycle = _;
      level_of_min_delegated = _;
    } =
  Tez_repr.(own_frozen +? staked_frozen)

let current_delegated
    {
      own_frozen = _;
      staked_frozen = _;
      delegated;
      min_delegated_in_cycle = _;
      level_of_min_delegated = _;
    } =
  delegated

(* The minimum over the cycle is either:
     - the current delegated value if it didn't change during the cycle, i.e.
       [cycle_of_min_delegated] is not the current cycle;
     - or the stored [min_delegated_in_cycle] otherwise.
*)
let min_delegated_in_cycle ~current_cycle
    {
      own_frozen = _;
      staked_frozen = _;
      delegated;
      min_delegated_in_cycle;
      level_of_min_delegated;
    } =
  let cycle_of_min_delegated = cycle_of_min_delegated level_of_min_delegated in
  if Cycle_repr.(cycle_of_min_delegated < current_cycle) then delegated
  else (
    assert (Cycle_repr.(cycle_of_min_delegated = current_cycle)) ;
    min_delegated_in_cycle)

let current_total
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle = _;
      level_of_min_delegated = _;
    } =
  let open Result_syntax in
  let* total_frozen = Tez_repr.(own_frozen +? staked_frozen) in
  Tez_repr.(total_frozen +? delegated)

let allowed_staked_frozen ~adaptive_issuance_global_limit_of_staking_over_baking
    ~delegate_limit_of_staking_over_baking_millionth
    {
      own_frozen;
      staked_frozen;
      delegated = _;
      min_delegated_in_cycle = _;
      level_of_min_delegated = _;
    } =
  let global_limit_of_staking_over_baking_millionth =
    Int64.(
      mul
        1_000_000L
        (of_int adaptive_issuance_global_limit_of_staking_over_baking))
  in
  let limit_of_staking_over_baking_millionth =
    Compare.Int64.min
      global_limit_of_staking_over_baking_millionth
      (Int64.of_int32 delegate_limit_of_staking_over_baking_millionth)
  in
  match
    Tez_repr.mul_ratio
      ~rounding:`Down
      own_frozen
      ~num:limit_of_staking_over_baking_millionth
      ~den:1_000_000L
  with
  | Ok max_allowed_staked_frozen ->
      Tez_repr.min staked_frozen max_allowed_staked_frozen
  | Error _max_allowed_staked_frozen_overflows -> staked_frozen

let own_ratio ~adaptive_issuance_global_limit_of_staking_over_baking
    ~delegate_limit_of_staking_over_baking_millionth
    ({
       own_frozen;
       staked_frozen = _;
       delegated = _;
       min_delegated_in_cycle = _;
       level_of_min_delegated = _;
     } as t) =
  if Tez_repr.(own_frozen = zero) then (0L, 1L)
  else
    let allowed_staked_frozen =
      allowed_staked_frozen
        ~adaptive_issuance_global_limit_of_staking_over_baking
        ~delegate_limit_of_staking_over_baking_millionth
        t
    in
    if Tez_repr.(allowed_staked_frozen = zero) then (1L, 1L)
    else
      let own_frozen = Tez_repr.to_mutez own_frozen in
      let allowed_staked_frozen = Tez_repr.to_mutez allowed_staked_frozen in
      (own_frozen, Int64.add own_frozen allowed_staked_frozen)

let has_minimal_frozen_stake ~minimal_frozen_stake full_staking_balance =
  let own_frozen = own_frozen full_staking_balance in
  Tez_repr.(own_frozen >= minimal_frozen_stake)

(* The set of delegates to consider [Active_delegates_with_minimal_stake] is an
   over-approximation of participating delegates. It is maintained by
   {!Stake_storage}.
   To avoid having to do any maintenance at cycle end, we have to rely on values
   that do not change when crossing cycle boundaries: the current amount works,
   the minimal in a given cycle wouldn't. *)
let has_minimal_stake_to_be_considered ~minimal_stake full_staking_balance =
  match current_total full_staking_balance with
  | Error _total_overflows ->
      true
      (* If the total overflows, we are definitely over the minimal stake. *)
  | Ok staking_balance -> Tez_repr.(staking_balance >= minimal_stake)

let remove_delegated ~(current_level : Level_repr.t) ~amount
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle = old_min_delegated_in_cycle;
      level_of_min_delegated;
    } =
  let open Result_syntax in
  let+ delegated = Tez_repr.(delegated -? amount) in
  let cycle_of_min_delegated = cycle_of_min_delegated level_of_min_delegated in
  let current_cycle = current_level.cycle in
  let min_delegated_in_cycle, level_of_min_delegated =
    if Cycle_repr.(cycle_of_min_delegated < current_cycle) then
      (* after decrease *) (delegated, Some current_level)
    else (
      assert (Cycle_repr.(cycle_of_min_delegated = current_cycle)) ;
      let minimum = Tez_repr.min delegated old_min_delegated_in_cycle in
      ( minimum,
        if Tez_repr.(minimum = old_min_delegated_in_cycle) then
          level_of_min_delegated
        else Some current_level ))
  in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    level_of_min_delegated;
  }

let remove_own_frozen ~amount
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle;
      level_of_min_delegated;
    } =
  let open Result_syntax in
  let+ own_frozen = Tez_repr.(own_frozen -? amount) in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    level_of_min_delegated;
  }

let remove_staked_frozen ~amount
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle;
      level_of_min_delegated;
    } =
  let open Result_syntax in
  let+ staked_frozen = Tez_repr.(staked_frozen -? amount) in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    level_of_min_delegated;
  }

let add_delegated ~(current_level : Level_repr.t) ~amount
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle = old_min_delegated_in_cycle;
      level_of_min_delegated;
    } =
  let open Result_syntax in
  let cycle_of_min_delegated = cycle_of_min_delegated level_of_min_delegated in
  let current_cycle = current_level.cycle in
  let min_delegated_in_cycle, level_of_min_delegated =
    if Cycle_repr.(cycle_of_min_delegated < current_cycle) then
      (* before increase *) (delegated, Some current_level)
    else (
      assert (Cycle_repr.(cycle_of_min_delegated = current_cycle)) ;
      (old_min_delegated_in_cycle, level_of_min_delegated))
  in
  let+ delegated = Tez_repr.(delegated +? amount) in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    level_of_min_delegated;
  }

let add_own_frozen ~amount
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle;
      level_of_min_delegated;
    } =
  let open Result_syntax in
  let+ own_frozen = Tez_repr.(own_frozen +? amount) in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    level_of_min_delegated;
  }

let add_staked_frozen ~amount
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle;
      level_of_min_delegated;
    } =
  let open Result_syntax in
  let+ staked_frozen = Tez_repr.(staked_frozen +? amount) in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    level_of_min_delegated;
  }

module Internal_for_tests_and_RPCs = struct
  let min_delegated_in_cycle
      {
        own_frozen = _;
        staked_frozen = _;
        delegated = _;
        min_delegated_in_cycle;
        level_of_min_delegated = _;
      } =
    min_delegated_in_cycle

  let level_of_min_delegated
      {
        own_frozen = _;
        staked_frozen = _;
        delegated = _;
        min_delegated_in_cycle = _;
        level_of_min_delegated;
      } =
    level_of_min_delegated
end
