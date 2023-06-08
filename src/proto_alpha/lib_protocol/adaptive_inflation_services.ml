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

module S = struct
  let context_path = RPC_path.(open_root / "context")

  let _path = RPC_path.(context_path / "inflation")

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

let register () =
  let open Services_registration in
  register0 ~chunked:false S.total_supply (fun ctxt () () ->
      Contract.get_total_supply ctxt) ;
  register0 ~chunked:false S.total_frozen_stake (fun ctxt () () ->
      let cycle = (Level.current ctxt).cycle in
      Stake_distribution.get_total_frozen_stake ctxt cycle)

let total_supply ctxt block =
  RPC_context.make_call0 S.total_supply ctxt block () ()

let total_frozen_stake ctxt block =
  RPC_context.make_call0 S.total_frozen_stake ctxt block () ()
