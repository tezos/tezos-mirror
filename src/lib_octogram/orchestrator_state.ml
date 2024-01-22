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

module SMap = Agent_name.Dictionary

type t = {
  prometheus : Remote_prometheus.t option;
  mutable agents : Remote_agent.t SMap.t;
  mutable vars : Global_variables.t;
}

let initial_state ?prometheus vars = {prometheus; agents = SMap.empty; vars}

let get_agent state name = SMap.find name state.agents

let iter_agent_p state f =
  Lwt_seq.iter_p
    (fun (_, agent) -> f agent)
    (SMap.to_seq state.agents |> Lwt_seq.of_seq)

let iter_agent_s state f =
  Lwt_seq.iter_s
    (fun (_, agent) -> f agent)
    (SMap.to_seq state.agents |> Lwt_seq.of_seq)

let iter_agents mode =
  match mode with
  | Execution_params.Concurrent -> iter_agent_p
  | Sequential -> iter_agent_s

let record_agent state agent =
  state.agents <- SMap.add (Remote_agent.name agent) agent state.agents

let forget_agent state name = state.agents <- SMap.remove name state.agents

let get_global_variables state = state.vars

let with_global_variables state k = state.vars <- k state.vars

let uri_resolver state ?suffix node_kind service_kind
    (agent_name : Agent_name.t) node_name =
  let agent = SMap.find agent_name state.agents in
  let ip =
    Remote_agent.get_service_info node_kind service_kind agent node_name
  in
  let runner = Remote_agent.runner agent in
  (Runner.address (Some runner), ip, suffix)

let with_prometheus ~default state k =
  match state.prometheus with
  | Some prometheus -> k prometheus
  | None -> default

let record_metrics_source state (agent_name : Agent_name.t) node_name address
    port =
  with_prometheus ~default:() state @@ fun prometheus ->
  Background.register
  @@ Remote_prometheus.record_metrics_source
       prometheus
       {
         job_name = sf "%s://%s" (agent_name :> string) node_name;
         target = sf "%s:%d" address port;
       }
