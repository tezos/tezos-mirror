(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let check_all_bakers_attest_at_level ctxt level =
  match Constants_storage.all_bakers_attest_activation_level ctxt with
  | None -> false
  | Some act_level -> Raw_level_repr.(level.Level_repr.level >= act_level)

let consensus_threshold ctxt _level =
  Constants_storage.consensus_threshold_size ctxt

let consensus_committee ctxt _level =
  Constants_storage.consensus_committee_size ctxt
