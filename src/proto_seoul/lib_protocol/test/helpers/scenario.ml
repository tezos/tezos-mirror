(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module centralises all modules related to [Scenario] writing and
    execution. Most scenario tests would use most if not all of them, so
    they only need to [open Scenario]. *)

include Scenario_base
include Scenario_op
include Scenario_dsl
include Scenario_begin
include Scenario_constants
include Scenario_bake
