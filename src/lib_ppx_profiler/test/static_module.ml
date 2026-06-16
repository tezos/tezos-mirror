(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Test file for PPX profiler transformation with static modules.

   The PPX transforms: expr [@profiler.record_s {profiler_module = Module} "label"]
   Into: Module.record_s ~cpu:None VERBOSITY ("label", []) @@ fun () -> expr

   This file tests static module paths like Profiler and My_module.Profiler. *)

type verbosity = Notice | Info | Debug

module Profiler = struct
  type nonrec verbosity = verbosity = Notice | Info | Debug

  let record_s ~cpu:_ _verbosity (_name, _metadata) f = f ()
end

module My_module = struct
  module Profiler = Profiler
end

(* Test: Default profiler (no profiler_module) uses Profiler *)
let test_default () =
  Lwt.return_unit [@profiler.record_s {verbosity = Notice} "default"]

(* Test: Static simple module *)
let test_static_simple () =
  Lwt.return_unit
  [@profiler.record_s {verbosity = Notice; profiler_module = Profiler} "simple"]

(* Test: Static qualified path *)
let test_static_qualified () =
  Lwt.return_unit
  [@profiler.record_s
    {verbosity = Info; profiler_module = My_module.Profiler} "qualified"]
