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

open Alpha_context

type expected_rewards = {
  baking_reward_fixed_portion : Tez.t;
  baking_reward_bonus_per_slot : Tez.t;
  endorsing_reward_per_slot : Tez.t;
  liquidity_baking_subsidy : Tez.t;
  seed_nonce_revelation_tip : Tez.t;
  vdf_revelation_tip : Tez.t;
}

let expected_rewards_encoding : expected_rewards Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           baking_reward_fixed_portion;
           baking_reward_bonus_per_slot;
           endorsing_reward_per_slot;
           liquidity_baking_subsidy;
           seed_nonce_revelation_tip;
           vdf_revelation_tip;
         } ->
      ( baking_reward_fixed_portion,
        baking_reward_bonus_per_slot,
        endorsing_reward_per_slot,
        liquidity_baking_subsidy,
        seed_nonce_revelation_tip,
        vdf_revelation_tip ))
    (fun ( baking_reward_fixed_portion,
           baking_reward_bonus_per_slot,
           endorsing_reward_per_slot,
           liquidity_baking_subsidy,
           seed_nonce_revelation_tip,
           vdf_revelation_tip ) ->
      {
        baking_reward_fixed_portion;
        baking_reward_bonus_per_slot;
        endorsing_reward_per_slot;
        liquidity_baking_subsidy;
        seed_nonce_revelation_tip;
        vdf_revelation_tip;
      })
    (obj6
       (req "baking_reward_fixed_portion" Tez.encoding)
       (req "baking_reward_bonus_per_slot" Tez.encoding)
       (req "endorsing_reward_per_slot" Tez.encoding)
       (req "liquidity_baking_subsidy" Tez.encoding)
       (req "seed_nonce_revelation_tip" Tez.encoding)
       (req "vdf_revelation_tip" Tez.encoding))

module S = struct
  open Data_encoding

  let q_encoding =
    conv
      (fun Q.{num; den} -> (num, den))
      (fun (num, den) -> Q.make num den)
      (obj2 (req "numerator" n) (req "denominator" n))

  let context_path = RPC_path.(open_root / "context")

  let path = RPC_path.(context_path / "inflation")

  let total_supply =
    RPC_service.get_service
      ~description:"Returns the total supply (in mutez) available on the chain"
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(context_path / "total_supply")

  let total_frozen_stake =
    RPC_service.get_service
      ~description:"Returns the total stake (in mutez) frozen on the chain"
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(context_path / "total_frozen_stake")

  let current_yearly_rate =
    RPC_service.get_service
      ~description:
        "Returns the current expected maximum yearly inflation rate (in %)"
      ~query:RPC_query.empty
      ~output:(string Plain)
      RPC_path.(path / "current_yearly_rate")

  let current_yearly_rate_exact =
    RPC_service.get_service
      ~description:
        "Returns the current expected maximum yearly inflation rate (exact \
         quotient)"
      ~query:RPC_query.empty
      ~output:q_encoding
      RPC_path.(path / "current_yearly_rate_exact")

  let current_rewards_per_minute =
    RPC_service.get_service
      ~description:
        "Returns the current expected maximum rewards per minute (in mutez)"
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "rewards_per_minute")

  let launch_cycle =
    RPC_service.get_service
      ~description:
        "Returns the cycle at which the launch of the Adaptive Inflation \
         feature is set to happen. A result of None means that the feature is \
         not yet set to launch."
      ~query:RPC_query.empty
      ~output:(Data_encoding.option Cycle.encoding)
      RPC_path.(context_path / "adaptive_inflation_launch_cycle")

  let expected_rewards =
    RPC_service.get_service
      ~description:"Returns the expected rewards for the provided block"
      ~query:RPC_query.empty
      ~output:expected_rewards_encoding
      RPC_path.(path / "expected_rewards")
end

let q_to_float_string q =
  let offset = 1000 in
  let unit = Z.div q.Q.num q.den in
  let q = Q.(sub q (unit /// Z.one)) in
  let q = Q.(mul q (offset // 1)) in
  let dec = Z.div q.num q.den in
  let padded_dec_string = Format.asprintf "%03d" (Z.to_int dec) in
  Format.asprintf "%a.%s" Z.pp_print unit padded_dec_string

let current_rewards_per_minute ctxt =
  let open Lwt_result_syntax in
  let base_total_rewards_per_minute =
    (Constants.reward_weights ctxt).base_total_rewards_per_minute
  in
  let q_base_total_rewards_per_minute =
    Tez.to_mutez base_total_rewards_per_minute |> Q.of_int64
  in
  let cycle = (Level.current ctxt).cycle in
  let* f = Delegate.Rewards.For_RPC.get_reward_coeff ctxt ~cycle in
  let f = Q.mul f q_base_total_rewards_per_minute (* rewards per minute *) in
  return f

(* Does the reverse operations of [compute_coeff] in [adaptive_inflation_storage.ml] *)
let current_yearly_rate_value ~formatter ctxt =
  let open Lwt_result_syntax in
  let q_min_per_year = Q.of_int 525600 in
  let* total_supply = Contract.get_total_supply ctxt in
  let q_total_supply = Tez.to_mutez total_supply |> Q.of_int64 in
  let* f = current_rewards_per_minute ctxt in
  let f = Q.div f q_total_supply (* inflation per minute *) in
  let f = Q.mul f q_min_per_year (* inflation per year *) in
  (* transform into a string *)
  let f = Q.(mul f (100 // 1)) in
  return (formatter f)

let collect_expected_rewards ~ctxt =
  let open Alpha_context.Delegate.Rewards in
  {
    baking_reward_fixed_portion = baking_reward_fixed_portion ctxt;
    baking_reward_bonus_per_slot = baking_reward_bonus_per_slot ctxt;
    endorsing_reward_per_slot = endorsing_reward_per_slot ctxt;
    liquidity_baking_subsidy = liquidity_baking_subsidy ctxt;
    seed_nonce_revelation_tip = seed_nonce_revelation_tip ctxt;
    vdf_revelation_tip = vdf_revelation_tip ctxt;
  }

let register () =
  let open Services_registration in
  let open Lwt_result_syntax in
  register0 ~chunked:false S.total_supply (fun ctxt () () ->
      Contract.get_total_supply ctxt) ;
  register0 ~chunked:false S.total_frozen_stake (fun ctxt () () ->
      let cycle = (Level.current ctxt).cycle in
      Stake_distribution.get_total_frozen_stake ctxt cycle) ;
  register0 ~chunked:false S.current_yearly_rate (fun ctxt () () ->
      current_yearly_rate_value ~formatter:q_to_float_string ctxt) ;
  register0 ~chunked:false S.current_yearly_rate_exact (fun ctxt () () ->
      current_yearly_rate_value ~formatter:(fun x -> x) ctxt) ;
  register0 ~chunked:false S.current_rewards_per_minute (fun ctxt () () ->
      let* f = current_rewards_per_minute ctxt in
      return (Tez.of_mutez_exn (Q.to_int64 f))) ;
  register0 ~chunked:false S.launch_cycle (fun ctxt () () ->
      Adaptive_inflation.launch_cycle ctxt) ;
  register0 ~chunked:false S.expected_rewards (fun ctxt () () ->
      return @@ collect_expected_rewards ~ctxt)

let total_supply ctxt block =
  RPC_context.make_call0 S.total_supply ctxt block () ()

let total_frozen_stake ctxt block =
  RPC_context.make_call0 S.total_frozen_stake ctxt block () ()

let current_yearly_rate ctxt block =
  RPC_context.make_call0 S.current_yearly_rate ctxt block () ()

let current_yearly_rate_exact ctxt block =
  RPC_context.make_call0 S.current_yearly_rate_exact ctxt block () ()

let current_rewards_per_minute ctxt block =
  RPC_context.make_call0 S.current_rewards_per_minute ctxt block () ()

let launch_cycle ctxt block =
  RPC_context.make_call0 S.launch_cycle ctxt block () ()

let expected_rewards ctxt block =
  RPC_context.make_call0 S.expected_rewards ctxt block () ()
