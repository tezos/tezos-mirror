(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Test file for PPX profiler transformation with first-class modules.

   The PPX transforms: expr [@profiler.record_s {profiler_module = fcm} "label"]
   Into: let (module Profiler__) = fcm in
         Profiler__.record_s ~cpu:None VERBOSITY ("label", []) @@ fun () -> expr

   This file tests first-class module expressions like variables, field accesses,
   and module value accesses. *)

type verbosity = Notice | Info | Debug

module type PROFILER = sig
  type nonrec verbosity = verbosity = Notice | Info | Debug

  val record_s :
    cpu:bool option ->
    verbosity ->
    string * 'a list ->
    (unit -> 'b Lwt.t) ->
    'b Lwt.t
end

module Profiler_impl : PROFILER = struct
  type nonrec verbosity = verbosity = Notice | Info | Debug

  let record_s ~cpu:_ _verbosity (_name, _metadata) f = f ()
end

(* First-class module in a variable *)
let profiler : (module PROFILER) = (module Profiler_impl)

(* Module holding a first-class module *)
module Holder = struct
  let profiler : (module PROFILER) = (module Profiler_impl)
end

(* Record with a profiler field *)
type state = {profiler : (module PROFILER)}

let state = {profiler = (module Profiler_impl)}

(* Nested record *)
type inner = {profiler : (module PROFILER)}

type outer = {inner : inner}

let outer = {inner = {profiler = (module Profiler_impl)}}

(* Test: FCM variable *)
let test_fcm_variable () =
  Lwt.return_unit
  [@profiler.record_s
    {verbosity = Notice; profiler_module = profiler} "variable"]

(* Test: FCM module value access *)
let test_fcm_module_value () =
  Lwt.return_unit
  [@profiler.record_s
    {verbosity = Notice; profiler_module = Holder.profiler} "module_value"]

(* Test: FCM field access *)
let test_fcm_field () =
  Lwt.return_unit
  [@profiler.record_s
    {verbosity = Notice; profiler_module = state.profiler} "field"]

(* Test: FCM nested field access *)
let test_fcm_nested () =
  Lwt.return_unit
  [@profiler.record_s
    {verbosity = Notice; profiler_module = outer.inner.profiler} "nested"]
