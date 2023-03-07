(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Defines a type [scenario] and its constructors, how to run it on the PVM *)

open Pvm_instance

type 'a run_state

type 'a action = 'a run_state -> 'a run_state Lwt.t

(** step in a scenario, associates an action to a label *)
type scenario_step

(** all informations needed to run a benchmark scenario *)
type scenario

(** [make_scenario_step step_name action] creates a scenario_step
      (1 action) *)
val make_scenario_step : string -> Wasm.tree action -> scenario_step

(** [make_scenario scenario_name kernel_path actions] creates a scenario with
      - a [scenario_name]
      - the kernel stored at [kernel_path]
      - a list of [actions] *)
val make_scenario : string -> string -> scenario_step list -> scenario

(** [ignore_scenario scenario] returns a scenario that will be ignored during a
     run. Can be used to reproduce part of run, ignoring some parts without
      having to delete anything. *)
val ignore_scenario : scenario -> scenario

(** action corresponding to a top level call of PVM, in slow mode,
    including reboots if necessary. *)
val exec_slow :
  reveal_builtins:Tezos_scoru_wasm.Builtins.reveals -> Wasm.tree action

(** action corresponding to a top level call of PVM, using fast execution,
    including reboots if necessary. *)
val exec_fast :
  reveal_builtins:Tezos_scoru_wasm.Builtins.reveals -> Wasm.tree action

(** [load_messages level messages] returns the action corresponding to
      adding a list of [messages] in the inbox at a given [level]. *)
val load_messages : int32 -> Exec.message list -> Wasm.tree action

(** [apply_step step tree] evaluate a scenario [step] on a given [tree],
    returning the collected benchmark data and the new state as a tree. *)
val apply_step :
  ?verbose:bool ->
  ?totals:bool ->
  ?irmin:bool ->
  scenario_step ->
  Wasm.tree ->
  (Data.benchmark * Wasm.tree) Lwt.t

(** [run_scenarios filename benches] Execute a list of scenario with options:
      - verbose: print info during execution
      - totals: adds summary data point for each step
      - irmin: adds data point for decoding / encoding the state
      and output the result in a csv format in a file with the provided name *)
val run_scenarios :
  ?verbose:bool ->
  ?totals:bool ->
  ?irmin:bool ->
  string ->
  scenario list ->
  unit Lwt.t
