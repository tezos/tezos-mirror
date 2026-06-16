(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let begin_end_color = Log.Color.(BG.bright_white ++ FG.black ++ bold)

let time_color = Log.Color.FG.yellow

let action_color = Log.Color.FG.green

let event_color = Log.Color.FG.blue

let warning_color = Log.Color.FG.red

let low_debug_color = Log.Color.FG.gray

let batch_color = Log.Color.(BG.green ++ FG.black)

let assert_block_color = Log.Color.(BG.blue ++ FG.gray)

let tez_color = Log.Color.FG.bright_white

let check_color = Log.Color.FG.gray
