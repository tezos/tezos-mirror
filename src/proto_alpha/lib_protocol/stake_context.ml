(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Stake_repr

let staking_weight ctxt {frozen; delegated} =
  let frozen = Tez_repr.to_mutez frozen in
  let delegated = Tez_repr.to_mutez delegated in
  let staking_over_delegation_edge =
    Constants_storage.adaptive_inflation_staking_over_delegation_edge ctxt
  in
  if Constants_storage.adaptive_inflation_enable ctxt then
    Int64.(add frozen (div delegated (of_int staking_over_delegation_edge)))
  else Int64.add frozen delegated

let compare ctxt s1 s2 =
  Int64.compare (staking_weight ctxt s1) (staking_weight ctxt s2)

let voting_weight ctxt {Stake_repr.Full.own_frozen; costaked_frozen; delegated}
    =
  let open Result_syntax in
  let+ frozen = Tez_repr.(own_frozen +? costaked_frozen) in
  staking_weight ctxt (Stake_repr.make ~frozen ~delegated)

let apply_limits ctxt staking_parameters
    {Stake_repr.Full.own_frozen; costaked_frozen; delegated} =
  let open Result_syntax in
  let delegation_over_baking_limit =
    Int64.of_int (Constants_storage.delegation_over_baking_limit ctxt)
  in
  let staking_over_baking_global_limit_millionth =
    Int64.(
      mul
        1_000_000L
        (of_int
           (Constants_storage
            .adaptive_inflation_staking_over_baking_global_limit
              ctxt)))
  in
  let {Staking_parameters_repr.staking_over_baking_limit_millionth; _} =
    staking_parameters
  in
  let staking_over_baking_limit_millionth =
    let delegate_staking_over_baking_limit_millionth =
      Int64.of_int32 staking_over_baking_limit_millionth
    in
    Compare.Int64.min
      staking_over_baking_global_limit_millionth
      delegate_staking_over_baking_limit_millionth
  in
  let allowed_costaked_frozen =
    match
      Tez_repr.mul_ratio
        own_frozen
        ~num:staking_over_baking_limit_millionth
        ~den:1_000_000L
    with
    | Ok max_allowed_costaked_frozen ->
        Tez_repr.min costaked_frozen max_allowed_costaked_frozen
    | Error _max_allowed_costaked_frozen_overflows -> costaked_frozen
  in
  (* Overcostaked tez count as delegated. *)
  let* overcostaked = Tez_repr.(costaked_frozen -? allowed_costaked_frozen) in
  let* delegated = Tez_repr.(delegated +? overcostaked) in
  (* Overdelegated tez don't count. *)
  let delegated =
    match Tez_repr.(own_frozen *? delegation_over_baking_limit) with
    | Ok max_allowed_delegated -> Tez_repr.min max_allowed_delegated delegated
    | Error _max_allowed_delegated_overflows -> delegated
  in
  let+ frozen = Tez_repr.(own_frozen +? allowed_costaked_frozen) in
  Stake_repr.make ~frozen ~delegated

let baking_weight ctxt staking_parameters f =
  let open Result_syntax in
  let+ s = apply_limits ctxt staking_parameters f in
  staking_weight ctxt s
