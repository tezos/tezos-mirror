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

type t

val initial_state : ?prometheus:Remote_prometheus.t -> Global_variables.t -> t

val get_agent : t -> Agent_name.t -> Remote_agent.t

val iter_agents :
  Execution_params.mode -> t -> (Remote_agent.t -> unit Lwt.t) -> unit Lwt.t

val record_agent : t -> Remote_agent.t -> unit

val forget_agent : t -> Agent_name.t -> unit

val get_global_variables : t -> Global_variables.t

(** [with_global_variables state f] uses the blocking function [f] to update
    the global variables of [state]. This is the only way to modify the global
    variables, as a mitigation for the highly asynchronous nature of the
    orchestrator.

    That is, you get the freshest version possible of the global variables
    before you update it, compare to using a [get]/[set] pattern. *)
val with_global_variables :
  t -> (Global_variables.t -> Global_variables.t) -> unit

val uri_resolver : t -> Uri_resolver.t

val record_metrics_source : t -> Agent_name.t -> string -> string -> int -> unit

val with_prometheus : default:'a -> t -> (Remote_prometheus.t -> 'a) -> 'a
