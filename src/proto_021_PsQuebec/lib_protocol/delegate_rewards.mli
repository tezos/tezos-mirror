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

val baking_reward_fixed_portion : Raw_context.t -> Tez_repr.t tzresult

val baking_reward_bonus_per_slot : Raw_context.t -> Tez_repr.t tzresult

val attesting_reward_per_slot : Raw_context.t -> Tez_repr.t tzresult

val liquidity_baking_subsidy : Raw_context.t -> Tez_repr.t tzresult

val seed_nonce_revelation_tip : Raw_context.t -> Tez_repr.t tzresult

val vdf_revelation_tip : Raw_context.t -> Tez_repr.t tzresult

module For_RPC : sig
  type reward_kind =
    | Baking_reward_fixed_portion
    | Baking_reward_bonus_per_slot
    | Attesting_reward_per_slot
    | Seed_nonce_revelation_tip
    | Vdf_revelation_tip

  (** [reward_from_constants ~coeff csts ~reward_kind] returns the amount of
      rewards in {!Tez_repr.t} for the given [reward_kind], according to the
      given parameters in [csts]. The (optional) value [coeff] is a
      multiplicative factor applied to the rewards (default = 1).
      It verifies [reward_from_constants ~coeff csts ~reward_kind =
      coeff * reward_from_constants csts ~reward_kind].*)
  val reward_from_constants :
    ?coeff:Q.t ->
    Constants_parametric_repr.t ->
    reward_kind:reward_kind ->
    Tez_repr.t tzresult

  val liquidity_baking_subsidy_from_constants :
    Constants_parametric_repr.t -> Tez_repr.t tzresult
end
