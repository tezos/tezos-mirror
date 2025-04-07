(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* /!\ This file must not be removed when protocol Q gets frozen. *)

open Protocol
open Protocol.Alpha_context

(* The issue is that on networks with consensus_rights_delay greater than 2 in
   Paris, the consensus_rights_delay gets lowered to 2 in Quebec without
   properly stitching the context. This issue doesn't affect mainnet, where
   consensus_rights_delay was already 2 in Paris and is still 2 in Quebec.

   Denote n the first cycle of Q on any non-mainnet network. At the end of cycle
   n, we compute the rights for cycle n + 1 + consensus_rights_delay = n + 3.
   But at the end of cycle n-1, the Paris protocol has already computed the
   rights for this cycle since its consensus_rights_delay was higher. And for
   instance in lib_protocol/stake_storage.ml we call
   Storage.Stake.Selected_distribution_for_cycle.init on a cycle that already
   has an entry in the storage, which fails.

   To fix this, we overwrite finalize_application in plugin_register.ml to call
   the following function, which, at the last level of cycle n, clears all the
   problematic tables.

   /!\ This function must always leave [application_state] unchanged on
   mainnet. *)
let fix_ghostnet_state (application_state : Apply.application_state) =
  let open Lwt_syntax in
  if Chain_id.(application_state.chain_id <> Constants_repr.mainnet_id) then
    let* first_quebec_level =
      First_level_of_protocol.get application_state.ctxt
    in
    match first_quebec_level with
    | Error _ -> return application_state
    | Ok lvl ->
        let raw_first_quebec_level_repr =
          Raw_level.Internal_for_tests.to_repr Raw_level.(succ lvl)
        in
        let (ctxt : Raw_context.t) =
          Alpha_context.Internal_for_tests.to_raw
            (application_state.ctxt : context)
        in
        let cycle_eras = Raw_context.cycle_eras ctxt in
        let first_quebec_level =
          Level_repr.level_from_raw ~cycle_eras raw_first_quebec_level_repr
        in
        let first_quebec_cycle = first_quebec_level.cycle in
        let current_level = Raw_context.current_level ctxt in
        let current_cycle = current_level.cycle in
        if
          Cycle_repr.(first_quebec_cycle = current_cycle)
          && Level_repr.last_of_cycle ~cycle_eras current_level
        then
          (* Cleans up all distribution for cycles
             n+3..n+Paris.consensus_rights_delay *)
          let rec clear ctxt n =
            let cycle_to_clear = Cycle_repr.add current_cycle n in
            let* b =
              Storage.Stake.Selected_distribution_for_cycle.mem
                ctxt
                cycle_to_clear
            in
            if b then
              let* ctxt =
                Storage.Stake.Selected_distribution_for_cycle.remove
                  ctxt
                  cycle_to_clear
              in
              let* ctxt =
                Storage.Stake.Total_active_stake.remove ctxt cycle_to_clear
              in
              let* ctxt =
                Storage.Delegate_sampler_state.remove ctxt cycle_to_clear
              in
              clear ctxt (n + 1)
            else return ctxt
          in
          let* ctxt = clear ctxt 3 in
          let (ctxt : context) = Obj.magic (ctxt : Raw_context.t) in
          return {application_state with ctxt}
        else return application_state
  else return application_state
