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

(** [load_reward_coeff ctxt] loads the current cycle's reward coeff from the
    storage into the context *)
val load_reward_coeff : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** [update_stored_rewards_at_cycle_end ctxt ~new_cycle] updates
    {!Storage.Reward_coeff} with a new coefficient that will be applied
    [preserved_cycles] cycles after the given [new_cycle]. This new coefficient
    depends on the current {!Storage.Total_supply}, and the total active stake
    for when this coefficient is computed.

    This function also removes obsolete values from {!Storage.Reward_coeff},
    and stores the current cycle's coefficient in the context for faster
    access. *)
val update_stored_rewards_at_cycle_end :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t tzresult Lwt.t

(** [init_ema ctxt] adds into the context an adaptive inflation vote EMA at 0 *)
val init_ema : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** [activate ctxt ~cycle] adds into the context the cycle at which
    the adaptive inflation feature gets activated. If this function is
    never called, then the context does not contain the cycle for the
    feature activation, which implies the feature is inactive.

    In practice, it means that you may call
    [Storage.Adaptive_inflation.Activation.find ctxt] to get the value
    of the cycle at which the feature is activated, and if that call
    returns [None], then it means the feature has not been
    voted to be activated (yet). *)
val activate :
  Raw_context.t -> cycle:Cycle_repr.t -> Raw_context.t tzresult Lwt.t

module For_RPC : sig
  (** [get_reward_coeff ctxt cycle] reads the reward coeff for the given cycle
      from the storage.
      Returns [Q.one] if the given cycle is not between [current_cycle] and
      [current_cycle + preserved_cycles].
      If adaptive inflation has not been activated, or has been activated and the
      given cycle is less than [preserved_cycles] after the activation cycle,
      then this function returns [Q.one].
      Used only for RPCs. To get the actual rewards, use [Delegate_rewards]. *)
  val get_reward_coeff :
    Raw_context.t -> cycle:Cycle_repr.t -> Q.t tzresult Lwt.t
end
