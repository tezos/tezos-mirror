(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let simple () =
  Cloud.register
    ~vms:(fun () ->
      return [Agent.Configuration.make (); Agent.Configuration.make ()])
    ~__FILE__
    ~tags:["simple"; "health"]
    ~title:"Simple health check to check local configuration"
  @@ fun t ->
  let agents = Cloud.agents t in
  let agent0 = List.nth agents 0 in
  let agent1 = List.nth agents (1 mod List.length agents) in
  let* output =
    Process.spawn
      ~name:(Agent.name agent0)
      ?runner:(Agent.runner agent0)
      "echo"
      ["Hello world"]
    |> Process.check_and_read_stdout
  in
  Log.info "%s from agent 0" (String.trim output) ;
  let* output =
    Process.spawn
      ~name:(Agent.name agent1)
      ?runner:(Agent.runner agent1)
      "echo"
      ["Hello world"]
    |> Process.check_and_read_stdout
  in
  Log.info "%s from agent 1" (String.trim output) ;
  unit

let run_vm () =
  Cloud.register
    ~vms:(fun () -> return [Agent.Configuration.make ()])
    ~__FILE__
    ~tags:["run"; "vm"]
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

let run_detached () =
  Cloud.register
    ~vms:(fun () -> return [Agent.Configuration.make ()])
    ~__FILE__
    ~tags:["run"; "detach"]
    ~title:"Run a command and detach in a vm"
  @@ fun t ->
  let agents = Cloud.agents t in
  let agent = List.nth agents 0 in
  Log.info "Run a command and detach. You should not wait" ;
  let* _ =
    Agent.docker_run_command ~detach:true agent "sleep" ["10"] |> Process.wait
  in
  Log.info "OK" ;
  Log.info "Run a command without detaching. You should wait 10sec" ;
  let* _ = Agent.docker_run_command agent "sleep" ["10"] |> Process.wait in
  Log.info "OK" ;
  unit

let register () =
  simple () ;
  run_vm () ;
  run_detached ()
