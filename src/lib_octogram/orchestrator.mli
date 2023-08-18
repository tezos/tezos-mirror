(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** [run_recipe r] unrolls the recipe [r], that is it starts its agents,
    orchestrates its jobs, then clean-up. *)
val run_recipe : keep_alive:bool -> Recipe.t -> unit Lwt.t

(** {1 Developers API} *)

(** For developers interested in program Octogram scenarios though OCaml rather
    than the intended YAML recipe file. *)

val initialize_agents :
  octogram_binary:Remote_agent.octogram_binary ->
  state:Orchestrator_state.t ->
  Recipe.agent list ->
  unit Lwt.t

val terminate_agents : state:Orchestrator_state.t -> unit Lwt.t

val run_job :
  state:Orchestrator_state.t ->
  agent:Remote_agent.t ->
  re:Jingoo.Jg_types.tvalue ->
  string Job.t ->
  unit Lwt.t

val run_stage : state:Orchestrator_state.t -> Stage.t -> unit Lwt.t
