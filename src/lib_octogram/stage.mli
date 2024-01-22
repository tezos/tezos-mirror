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

(** A stage defines an ordered set of jobs to be executed on a set of agents
    known to the orchestrator. *)

type t = {
  name : string;
  with_agents : string list;
      (** A list of regular expression used to select the subset of agents
          expected to run the jobs. *)
  run_agents : Execution_params.mode;
      (** Decide if the jobs of the stage has to be executed by each agent
          sequentially (there is at most one agent running jobs at a time) or
          concurrently (all agents start executing their job at the same time). *)
  run_jobs : Execution_params.mode;
      (** Decide if the job constituting a stage has to be run concurrently or
      sequentially by a given agent.  *)
  ask_confirmation : bool;
      (** Decide whether or not the user has to request the start of the
          execution. *)
  jobs : string Job.t list;
}

val encoding : t Data_encoding.t
