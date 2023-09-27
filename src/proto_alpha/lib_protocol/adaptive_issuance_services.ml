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
  cycle : Cycle.t;
  baking_reward_fixed_portion : Tez.t;
  baking_reward_bonus_per_slot : Tez.t;
  attesting_reward_per_slot : Tez.t;
  liquidity_baking_subsidy : Tez.t;
  seed_nonce_revelation_tip : Tez.t;
  vdf_revelation_tip : Tez.t;
}

let expected_rewards_encoding : expected_rewards Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           cycle;
           baking_reward_fixed_portion;
           baking_reward_bonus_per_slot;
           attesting_reward_per_slot;
           liquidity_baking_subsidy;
           seed_nonce_revelation_tip;
           vdf_revelation_tip;
         } ->
      ( cycle,
        baking_reward_fixed_portion,
        baking_reward_bonus_per_slot,
        attesting_reward_per_slot,
        liquidity_baking_subsidy,
        seed_nonce_revelation_tip,
        vdf_revelation_tip ))
    (fun ( cycle,
           baking_reward_fixed_portion,
           baking_reward_bonus_per_slot,
           attesting_reward_per_slot,
           liquidity_baking_subsidy,
           seed_nonce_revelation_tip,
           vdf_revelation_tip ) ->
      {
        cycle;
        baking_reward_fixed_portion;
        baking_reward_bonus_per_slot;
        attesting_reward_per_slot;
        liquidity_baking_subsidy;
        seed_nonce_revelation_tip;
        vdf_revelation_tip;
      })
    (obj7
       (req "cycle" Cycle.encoding)
       (req "baking_reward_fixed_portion" Tez.encoding)
       (req "baking_reward_bonus_per_slot" Tez.encoding)
       (req "attesting_reward_per_slot" Tez.encoding)
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

  let path = RPC_path.(context_path / "issuance")

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
        "Returns the current expected maximum yearly issuance rate (in %)"
      ~query:RPC_query.empty
      ~output:(string Plain)
      RPC_path.(path / "current_yearly_rate")

  let current_yearly_rate_exact =
    RPC_service.get_service
      ~description:
        "Returns the current expected maximum yearly issuance rate (exact \
         quotient)"
      ~query:RPC_query.empty
      ~output:q_encoding
      RPC_path.(path / "current_yearly_rate_exact")

  let current_yearly_rate_details =
    RPC_service.get_service
      ~description:
        "Returns the static and dynamic parts of the current expected maximum \
         yearly issuance rate."
      ~query:RPC_query.empty
      ~output:(obj2 (req "static" q_encoding) (req "dynamic" q_encoding))
      RPC_path.(path / "current_yearly_rate_details")

  let current_issuance_per_minute =
    RPC_service.get_service
      ~description:
        "Returns the current expected maximum issuance per minute (in mutez)"
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "issuance_per_minute")

  let launch_cycle =
    RPC_service.get_service
      ~description:
        "Returns the cycle at which the launch of the Adaptive Issuance \
         feature is set to happen. A result of None means that the feature is \
         not yet set to launch."
      ~query:RPC_query.empty
      ~output:(Data_encoding.option Cycle.encoding)
      RPC_path.(context_path / "adaptive_issuance_launch_cycle")

  let expected_issuance =
    RPC_service.get_service
      ~description:
        "Returns the expected issued tez for the provided block and the next \
         five cycles"
      ~query:RPC_query.empty
      ~output:(Data_encoding.list expected_rewards_encoding)
      RPC_path.(path / "expected_issuance")
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
  let base_total_issued_per_minute =
    (Constants.issuance_weights ctxt).base_total_issued_per_minute
  in
  let q_base_total_issued_per_minute =
    Tez.to_mutez base_total_issued_per_minute |> Q.of_int64
  in
  let cycle = (Level.current ctxt).cycle in
  let* f = Delegate.Rewards.For_RPC.get_reward_coeff ctxt ~cycle in
  let f = Q.mul f q_base_total_issued_per_minute (* rewards per minute *) in
  return f

(* Does the reverse operations of [compute_coeff] in [adaptive_issuance_storage.ml] *)
let current_yearly_rate_value ~formatter ctxt =
  let open Lwt_result_syntax in
  let q_min_per_year = Q.of_int 525600 in
  let* total_supply = Contract.get_total_supply ctxt in
  let q_total_supply = Tez.to_mutez total_supply |> Q.of_int64 in
  let* f = current_rewards_per_minute ctxt in
  let f = Q.div f q_total_supply (* issuance rate per minute *) in
  let f = Q.mul f q_min_per_year (* issuance rate per year *) in
  (* transform into a string *)
  let f = Q.(mul f (100 // 1)) in
  return (formatter f)

let collect_expected_rewards ~ctxt =
  let open Lwt_result_syntax in
  let open Delegate.Rewards in
  let ctxt_cycle = (Level.current ctxt).cycle in
  let csts = (Constants.all ctxt).parametric in
  let reward_of_cycle cycle =
    if Cycle.(cycle = ctxt_cycle) then
      return
        {
          cycle;
          baking_reward_fixed_portion = baking_reward_fixed_portion ctxt;
          baking_reward_bonus_per_slot = baking_reward_bonus_per_slot ctxt;
          attesting_reward_per_slot = attesting_reward_per_slot ctxt;
          liquidity_baking_subsidy = liquidity_baking_subsidy ctxt;
          seed_nonce_revelation_tip = seed_nonce_revelation_tip ctxt;
          vdf_revelation_tip = vdf_revelation_tip ctxt;
        }
    else
      (* This coeff is correct only when applied to Cycle lesser than
         [preserved_cycles] after the current context, otherwise the coeff will
         not be set and thus we get the default values. *)
      let open Delegate.Rewards.For_RPC in
      let* coeff = get_reward_coeff ctxt ~cycle in
      return
        {
          cycle;
          baking_reward_fixed_portion =
            reward_from_constants
              ~coeff
              csts
              ~reward_kind:Baking_reward_fixed_portion;
          baking_reward_bonus_per_slot =
            reward_from_constants
              ~coeff
              csts
              ~reward_kind:Baking_reward_bonus_per_slot;
          attesting_reward_per_slot =
            reward_from_constants
              ~coeff
              csts
              ~reward_kind:Attesting_reward_per_slot;
          liquidity_baking_subsidy =
            reward_from_constants
              ~coeff
              csts
              ~reward_kind:Liquidity_baking_subsidy;
          seed_nonce_revelation_tip =
            reward_from_constants
              ~coeff
              csts
              ~reward_kind:Seed_nonce_revelation_tip;
          vdf_revelation_tip =
            reward_from_constants ~coeff csts ~reward_kind:Vdf_revelation_tip;
        }
  in
  let queried_cycles =
    Cycle.(ctxt_cycle ---> add ctxt_cycle csts.preserved_cycles)
  in
  List.map_es reward_of_cycle queried_cycles

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
  register0 ~chunked:false S.current_yearly_rate_details (fun ctxt () () ->
      let* total = current_yearly_rate_value ~formatter:(fun x -> x) ctxt in
      let cycle = Some (Level.current ctxt).cycle in
      let* bonus = Delegate.Rewards.For_RPC.get_reward_bonus ctxt ~cycle in
      let dynamic = (bonus :> Q.t) in
      let static = Q.(total - dynamic) in
      return (static, dynamic)) ;
  register0 ~chunked:false S.current_issuance_per_minute (fun ctxt () () ->
      let* f = current_rewards_per_minute ctxt in
      return (Tez.of_mutez_exn (Q.to_int64 f))) ;
  register0 ~chunked:false S.launch_cycle (fun ctxt () () ->
      Adaptive_issuance.launch_cycle ctxt) ;
  register0 ~chunked:false S.expected_issuance (fun ctxt () () ->
      collect_expected_rewards ~ctxt)

let total_supply ctxt block =
  RPC_context.make_call0 S.total_supply ctxt block () ()

let total_frozen_stake ctxt block =
  RPC_context.make_call0 S.total_frozen_stake ctxt block () ()

let current_yearly_rate ctxt block =
  RPC_context.make_call0 S.current_yearly_rate ctxt block () ()

let current_yearly_rate_exact ctxt block =
  RPC_context.make_call0 S.current_yearly_rate_exact ctxt block () ()

let current_yearly_rate_details ctxt block =
  RPC_context.make_call0 S.current_yearly_rate_details ctxt block () ()

let current_issuance_per_minute ctxt block =
  RPC_context.make_call0 S.current_issuance_per_minute ctxt block () ()

let launch_cycle ctxt block =
  RPC_context.make_call0 S.launch_cycle ctxt block () ()

let expected_issuance ctxt block =
  RPC_context.make_call0 S.expected_issuance ctxt block () ()
