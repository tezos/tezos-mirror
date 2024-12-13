(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let current ctxt = (Raw_context.current_level ctxt).cycle

let greatest_unstake_finalizable_cycle ctxt =
  Cycle_repr.sub
    (current ctxt)
    (Constants_storage.unstake_finalization_delay ctxt + 1)
