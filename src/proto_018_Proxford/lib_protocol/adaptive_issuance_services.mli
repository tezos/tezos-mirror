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

val expected_rewards_encoding : expected_rewards Data_encoding.t

val total_supply : 'a #RPC_context.simple -> 'a -> Tez.t shell_tzresult Lwt.t

val total_frozen_stake :
  'a #RPC_context.simple -> 'a -> Tez.t shell_tzresult Lwt.t

val current_yearly_rate :
  'a #RPC_context.simple -> 'a -> string shell_tzresult Lwt.t

val current_yearly_rate_exact :
  'a #RPC_context.simple -> 'a -> Q.t shell_tzresult Lwt.t

val current_yearly_rate_details :
  'a #RPC_context.simple -> 'a -> (Q.t * Q.t) shell_tzresult Lwt.t

val current_issuance_per_minute :
  'a #RPC_context.simple -> 'a -> Tez.t shell_tzresult Lwt.t

val launch_cycle :
  'a #RPC_context.simple -> 'a -> Cycle.t option shell_tzresult Lwt.t

(** Returns the list of expected issued tez for the current cycle and for the next
    [preserved_cycles] cycles. *)
val expected_issuance :
  'a #RPC_context.simple -> 'a -> expected_rewards list shell_tzresult Lwt.t

val register : unit -> unit
