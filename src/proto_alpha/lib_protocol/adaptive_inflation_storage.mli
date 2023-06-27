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

(** [set_adaptive_inflation_enable ctxt] sets the feature flag in the
   in-memory part of the context if the adaptive inflation feature has
   already launched. This means that the activation vote resulted in
   an approbation from the stakeholders and this happened sufficiently
   long ago. *)
val set_adaptive_inflation_enable :
  Raw_context.t -> Raw_context.t tzresult Lwt.t

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

(** [init ctxt] adds into the context an adaptive inflation vote EMA
    at 0, and and adaptive inflation launch cycle at None. *)
val init : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** [update_ema ctxt ~vote] returns the new context with the new EMA *)
val update_ema :
  Raw_context.t ->
  vote:Toggle_votes_repr.toggle_vote ->
  (Raw_context.t
  * Cycle_repr.t option
  * Toggle_votes_repr.Adaptive_inflation_launch_EMA.t)
  tzresult
  Lwt.t

(** [launch_cycle ctxt] reads from the context the cycle at which
    the adaptive inflation feature is set to activate.

    If this function returns [None], then it means the feature has not been
    voted to be activated (yet). *)
val launch_cycle : Raw_context.t -> Cycle_repr.t option tzresult Lwt.t

module For_RPC : sig
  (** [get_reward_coeff ctxt cycle] reads the reward coeff for the given cycle
      from the storage.

      Fails if the given cycle is not between [current_cycle] and
      [current_cycle + preserved_cycles].

      If adaptive inflation has not been activated,
      then this function returns [Q.one].
      Used only for RPCs. To get the actual rewards, use [Delegate_rewards]. *)
  val get_reward_coeff :
    Raw_context.t -> cycle:Cycle_repr.t -> Q.t tzresult Lwt.t
end
