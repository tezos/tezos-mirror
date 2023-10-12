(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  own_frozen : Tez_repr.t;
  staked_frozen : Tez_repr.t;
  delegated : Tez_repr.t;
  min_delegated_in_cycle : Tez_repr.t;
  cycle_of_min_delegated : Cycle_repr.t;
}

let init ~own_frozen ~staked_frozen ~delegated =
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle = delegated;
    cycle_of_min_delegated = Cycle_repr.root;
  }

let encoding =
  let open Data_encoding in
  (* This encoding is backward-compatible with the encoding used in Oxford, so
     as to avoid a stitching in P. It will act as a lazy migration.
     The case in which [added_in_p] is [None] happen only for pre-existing
     values in the storage. For them, using [(delegated, Cycle_repr.root)]
     should behave correctly. *)
  let added_in_p =
    obj2
      (req "min_delegated_in_cycle" Tez_repr.encoding)
      (req "cycle_of_min_delegated" Cycle_repr.encoding)
  in
  conv
    (fun {
           own_frozen;
           staked_frozen;
           delegated;
           min_delegated_in_cycle;
           cycle_of_min_delegated;
         } ->
      ( own_frozen,
        staked_frozen,
        delegated,
        Some (min_delegated_in_cycle, cycle_of_min_delegated) ))
    (fun (own_frozen, staked_frozen, delegated, added_in_p_opt) ->
      let min_delegated_in_cycle, cycle_of_min_delegated =
        added_in_p_opt |> Option.value ~default:(delegated, Cycle_repr.root)
      in
      {
        own_frozen;
        staked_frozen;
        delegated;
        min_delegated_in_cycle;
        cycle_of_min_delegated;
      })
    (obj4
       (req "own_frozen" Tez_repr.encoding)
       (req "staked_frozen" Tez_repr.encoding)
       (req "delegated" Tez_repr.encoding)
       (varopt "min_delegated_in_cycle_and_cycle" added_in_p))

let voting_weight
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle = _;
      cycle_of_min_delegated = _;
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
      cycle_of_min_delegated;
    } =
  let remaining_percentage = Int_percentage.neg percentage in
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
    cycle_of_min_delegated;
  }

let own_frozen
    {
      own_frozen;
      staked_frozen = _;
      delegated = _;
      min_delegated_in_cycle = _;
      cycle_of_min_delegated = _;
    } =
  own_frozen

let staked_frozen
    {
      own_frozen = _;
      staked_frozen;
      delegated = _;
      min_delegated_in_cycle = _;
      cycle_of_min_delegated = _;
    } =
  staked_frozen

let total_frozen
    {
      own_frozen;
      staked_frozen;
      delegated = _;
      min_delegated_in_cycle = _;
      cycle_of_min_delegated = _;
    } =
  Tez_repr.(own_frozen +? staked_frozen)

let delegated
    {
      own_frozen = _;
      staked_frozen = _;
      delegated;
      min_delegated_in_cycle = _;
      cycle_of_min_delegated = _;
    } =
  delegated

let total
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle = _;
      cycle_of_min_delegated = _;
    } =
  let open Result_syntax in
  let* total_frozen = Tez_repr.(own_frozen +? staked_frozen) in
  Tez_repr.(total_frozen +? delegated)

let own_ratio
    {
      own_frozen;
      staked_frozen;
      delegated = _;
      min_delegated_in_cycle = _;
      cycle_of_min_delegated = _;
    } =
  if Tez_repr.(staked_frozen = zero) then (1L, 1L)
  else if Tez_repr.(own_frozen = zero) then (0L, 1L)
  else
    let own_frozen = Tez_repr.to_mutez own_frozen in
    let staked_frozen = Tez_repr.to_mutez staked_frozen in
    (own_frozen, Int64.add own_frozen staked_frozen)

let has_minimal_frozen_stake ~minimal_frozen_stake full_staking_balance =
  let own_frozen = own_frozen full_staking_balance in
  Tez_repr.(own_frozen >= minimal_frozen_stake)

let has_minimal_stake_to_be_considered ~minimal_stake full_staking_balance =
  match total full_staking_balance with
  | Error _total_overflows ->
      true
      (* If the total overflows, we are definitely over the minimal stake. *)
  | Ok staking_balance -> Tez_repr.(staking_balance >= minimal_stake)

let remove_delegated ~amount
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle;
      cycle_of_min_delegated;
    } =
  let open Result_syntax in
  let+ delegated = Tez_repr.(delegated -? amount) in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    cycle_of_min_delegated;
  }

let remove_own_frozen ~amount
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle;
      cycle_of_min_delegated;
    } =
  let open Result_syntax in
  let+ own_frozen = Tez_repr.(own_frozen -? amount) in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    cycle_of_min_delegated;
  }

let remove_staked_frozen ~amount
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle;
      cycle_of_min_delegated;
    } =
  let open Result_syntax in
  let+ staked_frozen = Tez_repr.(staked_frozen -? amount) in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    cycle_of_min_delegated;
  }

let add_delegated ~amount
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle;
      cycle_of_min_delegated;
    } =
  let open Result_syntax in
  let+ delegated = Tez_repr.(delegated +? amount) in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    cycle_of_min_delegated;
  }

let add_own_frozen ~amount
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle;
      cycle_of_min_delegated;
    } =
  let open Result_syntax in
  let+ own_frozen = Tez_repr.(own_frozen +? amount) in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    cycle_of_min_delegated;
  }

let add_staked_frozen ~amount
    {
      own_frozen;
      staked_frozen;
      delegated;
      min_delegated_in_cycle;
      cycle_of_min_delegated;
    } =
  let open Result_syntax in
  let+ staked_frozen = Tez_repr.(staked_frozen +? amount) in
  {
    own_frozen;
    staked_frozen;
    delegated;
    min_delegated_in_cycle;
    cycle_of_min_delegated;
  }
