(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Data

(** Generic type, used by both report creation and theshold definition *)
type ('level, 'round, 'tm, 'delay) report0 = {
  level : 'level;
  timestamp : 'tm;
  round : 'round;
  validation_delay : 'delay;
  application_delay : 'delay;
}

type report = (int32, int32, Time.Protocol.t, float option) report0

type thresholds = (unit, int32 option, int32 option, float option) report0

(** [block_report level block]
    Compute report for a given block
*)
let block_report (level : int32) (block : Block.t) : report =
  let round = block.round in
  let open Data in
  let timestamp = block.timestamp in
  let timestamp_f = Int64.to_float (Time.Protocol.to_seconds block.timestamp) in
  let min getter =
    match List.filter_map getter block.reception_times with
    | [] -> None
    | hd :: tl ->
        List.fold_left Time.System.min hd tl |> Ptime.to_float_s |> Option.some
  in
  let validation_time = min (fun r -> r.Block.validation_time) in
  let application_time = min (fun r -> r.Block.application_time) in
  let delay_with timestamp = Option.map (fun t -> Float.sub t timestamp) in
  let validation_delay = delay_with timestamp_f validation_time in
  let application_delay =
    Option.bind validation_time (fun t -> delay_with t application_time)
  in
  {level; timestamp; round; validation_delay; application_delay}
