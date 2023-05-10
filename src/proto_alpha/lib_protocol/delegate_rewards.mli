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

(* These functions return the amount of tez rewarded for each action *)

val baking_reward_fixed_portion : Raw_context.t -> Tez_repr.t

val baking_reward_bonus_per_slot : Raw_context.t -> Tez_repr.t

val endorsing_reward_per_slot : Raw_context.t -> Tez_repr.t

val liquidity_baking_subsidy : Raw_context.t -> Tez_repr.t

val seed_nonce_revelation_tip : Raw_context.t -> Tez_repr.t

val vdf_revelation_tip : Raw_context.t -> Tez_repr.t

(* For testing purposes *)
module Internal_for_tests : sig
  type reward_kind =
    | Baking_reward_fixed_portion
    | Baking_reward_bonus_per_slot
    | Endorsing_reward_per_slot
    | Liquidity_baking_subsidy
    | Seed_nonce_revelation_tip
    | Vdf_revelation_tip

  val reward_from_constants :
    csts:Constants_parametric_repr.t -> reward_kind:reward_kind -> Tez_repr.t
end
