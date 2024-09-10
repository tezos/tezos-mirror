(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Data

(** Generic type, used by both report creation and theshold definition *)
type ('level, 'round, 'tm) report0 = {
  level : 'level;
  timestamp : 'tm;
  round : 'round;
}

type report = (int32, int32, Time.Protocol.t) report0

type thresholds = (unit, int32 option, int32 option) report0

(** [block_report level block]
    Compute report for a given block
*)
let block_report (level : int32) (block : Block.t) : report =
  let round = block.round in
  let timestamp = block.timestamp in
  {level; timestamp; round}
