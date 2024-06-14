(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let simple () =
  Cloud.register
    ~vms:[Cloud.default_vm_configuration; Cloud.default_vm_configuration]
    ~__FILE__
    ~tags:["simple"; "health"; Tag.cloud]
    ~title:"Simple health check to check local configuration"
  @@ fun t ->
  let agents = Cloud.agents t in
  let agent0 = List.nth agents 0 in
  let agent1 = List.nth agents 1 in
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

let register () = simple ()