(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let current ctxt = (Raw_context.current_level ctxt).cycle

(* The context needs to keep data on [current_cycle -
   (max_slashing_period - 1)], because potential misbehaviours from
   that cycle are slashed at the end of [current_cycle], and the
   slashing depends on baking rights at the misbehaviour level. Older
   cycles than that are no longer needed. *)
let oldest_cycle_with_sampling_data ctxt =
  match Cycle_repr.sub (current ctxt) Constants_repr.slashing_delay with
  | None -> (* The context has data on all past cycles. *) Cycle_repr.root
  | Some cycle -> cycle

(* At the dawn of [new_cycle], sampling data can be cleared on cycles
   that are strictly older than [new_cycle - (max_slashing_period -
   1)]. In practice, we only need to clear cycle [new_cycle -
   max_slashing_period], because prior cycles have already been clean
   up earlier. *)
let cycle_to_clear_of_sampling_data ~new_cycle =
  Cycle_repr.sub new_cycle (Constants_repr.slashing_delay + 1)

let greatest_unstake_finalizable_cycle ctxt =
  Cycle_repr.sub
    (current ctxt)
    (Constants_storage.unstake_finalization_delay ctxt + 1)
