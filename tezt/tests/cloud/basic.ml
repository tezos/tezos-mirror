(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let simple () =
  Cloud.register
    ~vms:[Configuration.make (); Configuration.make ()]
    ~__FILE__
    ~tags:["simple"; "health"; Tag.cloud]
    ~title:"Simple health check to check local configuration"
  @@ fun t ->
  let agents = Cloud.agents t in
  let agent0 = List.nth agents 0 in
  let agent1 = List.nth agents (1 mod List.length agents) in
  let* output =
    Process.spawn
      ~name:"agent0"
      ~runner:(Agent.runner agent0)
      "echo"
      ["Hello world"]
    |> Process.check_and_read_stdout
  in
  Log.info "%s from agent 0" (String.trim output) ;
  let* output =
    Process.spawn
      ~name:"agent1"
      ~runner:(Agent.runner agent1)
      "echo"
      ["Hello world"]
    |> Process.check_and_read_stdout
  in
  Log.info "%s from agent 1" (String.trim output) ;
  unit

let run_vm () =
  Cloud.register
    ~vms:[Configuration.make ()]
    ~__FILE__
    ~tags:["run"; "vm"; Tag.cloud]
    ~title:"Run a new VM"
  @@ fun t ->
  let agents = Cloud.agents t in
  let agent = List.nth agents 0 in
  let* _ =
    Agent.host_run_command agent "echo" ["Hello world"]
    |> Process.check_and_read_stdout
  in
  Log.info "VM is ready" ;
  let cmd =
    match Agent.cmd_wrapper agent with
    | None -> assert false
    | Some {cmd; args} -> cmd :: args
  in
  Log.report
    "You can connect onto the VM via the following command:@.%s"
    (String.concat " " cmd) ;
  unit

let register () =
  simple () ;
  run_vm ()
