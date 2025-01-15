(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* /!\ This file must not be removed when protocol Q gets frozen. *)

open Protocol
open Protocol.Alpha_context

let ghostnet_id = Chain_id.of_b58check_exn "NetXnHfVqm9iesp"

let ghostnet_fix_level = Raw_level.of_int32_exn 10_062_848l

(* The issue is that on ghostnet, consensus_rights_delay was still 3
   in Paris, then gets lowered to 2 in Quebec without properly
   stitching the context. This issue doesn't affect mainnet, where
   consensus_rights_delay was already 2 in Paris and is still 2 in
   Quebec.

   Denote n the first cycle of Q on ghostnet. At the end of cycle n,
   we compute the rights for cycle n + 1 + consensus_rights_delay = n
   + 3. But at the end of cycle n-1, the Paris protocol has already
   computed the rights for this cycle since its consensus_rights_delay
   was higher. And for instance in lib_protocol/stake_storage.ml we
   call Storage.Stake.Selected_distribution_for_cycle.init on a cycle
   that already has an entry in the storage, which fails.

   To fix this, we overwrite finalize_application in
   plugin_register.ml to call this function, which clears the
   problematic tables during level 10_062_848 which is the last level
   of cycle n.

   /!\ This function must always leave [application_state] unchanged
   on mainnet. *)
let fix_ghostnet_state (application_state : Apply.application_state) =
  let open Lwt_syntax in
  if
    Chain_id.(application_state.chain_id = ghostnet_id)
    && Raw_level.(
         (Level.current application_state.ctxt).level = ghostnet_fix_level)
  then
    let (ctxt : Raw_context.t) = Obj.magic (application_state.ctxt : context) in
    let current_cycle = (Raw_context.current_level ctxt).cycle in
    let cycle_to_clear = Cycle_repr.add current_cycle 3 in
    let* ctxt =
      Storage.Stake.Selected_distribution_for_cycle.remove ctxt cycle_to_clear
    in
    let* ctxt = Storage.Stake.Total_active_stake.remove ctxt cycle_to_clear in
    let* ctxt = Storage.Delegate_sampler_state.remove ctxt cycle_to_clear in
    let (ctxt : context) = Obj.magic (ctxt : Raw_context.t) in
    return {application_state with ctxt}
  else return application_state
