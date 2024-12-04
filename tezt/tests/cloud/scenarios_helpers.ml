(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let toplog (fmt : ('a, Format.formatter, unit, unit) format4) : 'a =
  Log.info ~prefix:"TOP" ~color:Log.Color.FG.green fmt
