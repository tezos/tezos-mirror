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

type agent = {
  name : Agent_name.t;
  address : string;
  user : string;
  port : int;
  identity : string;
}

type t = {
  agents : agent list;
  prometheus_agent : agent option;
  vars : Global_variables.t;
  stages : Stage.t list;
  octogram_binary : Remote_agent.octogram_binary;
}

let agent_encoding =
  Data_encoding.(
    conv
      (fun {name; address; user; port; identity} ->
        (name, address, user, port, identity))
      (fun (name, address, user, port, identity) ->
        {name; address; user; port; identity})
      (obj5
         (req "name" Agent_name.encoding)
         (req "address" string)
         (req "user" string)
         (req "port" int31)
         (req "identity" string)))

let encoding =
  Data_encoding.(
    delayed @@ fun () ->
    conv
      (fun {agents; prometheus_agent; vars; stages; octogram_binary} ->
        (agents, prometheus_agent, vars, stages, octogram_binary))
      (fun (agents, prometheus_agent, vars, stages, octogram_binary) ->
        {agents; prometheus_agent; vars; stages; octogram_binary})
      (obj5
         (dft "agents" (list agent_encoding) [])
         (opt "prometheus_agent" agent_encoding)
         Global_variables.(dft "vars" encoding empty)
         (req "stages" (list Stage.encoding))
         (dft
            "octogram_binary"
            Remote_agent.octogram_binary_encoding
            (Push {local_path = "./octogram"}))))

let runner_of_agent agent =
  Runner.create
    ~address:agent.address
    ~ssh_user:agent.user
    ~ssh_port:agent.port
    ~ssh_id:agent.identity
    ()
