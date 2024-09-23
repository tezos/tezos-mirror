(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let environment_profiler = Profiler.unplugged ()

module Environment_profiler = (val Profiler.wrap environment_profiler)

let context_ops_profiler = Profiler.unplugged ()

module Context_ops_profiler = (val Profiler.wrap context_ops_profiler)
