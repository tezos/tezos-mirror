(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  mutable name : string;
  vm_name : string;
  cmd_wrapper : Gcloud.cmd_wrapper option;
  point : string * int;
  runner : Runner.t;
  next_available_port : unit -> int;
  configuration : Configuration.t;
}

let make ?cmd_wrapper ~ssh_id ~point:((address, ssh_port) as point)
    ~configuration ~next_available_port ~name () =
  let ssh_user = "root" in
  let runner = Runner.create ~ssh_user ~ssh_id ~ssh_port ~address () in
  {
    point;
    runner;
    name;
    vm_name = name;
    next_available_port;
    configuration;
    cmd_wrapper;
  }

let name {name; _} = name

let point {point; _} = point

let cmd_wrapper agent = agent.cmd_wrapper

let set_name agent name = agent.name <- name

let path_of agent binary = agent.configuration.binaries_path // binary

let copy agent ~source ~destination =
  let runner = agent.runner in
  let identity =
    Option.fold ~none:[] ~some:(fun i -> ["-i"; i]) runner.ssh_id
  in
  let port =
    Option.fold
      ~none:[]
      ~some:(fun p -> ["-P"; Format.sprintf "%d" p])
      runner.ssh_port
  in
  let* exists =
    let process = Process.spawn ~runner "ls" [destination] in
    let* status = process |> Process.wait in
    match status with WEXITED 0 -> Lwt.return_true | _ -> Lwt.return_false
  in
  if exists then Lwt.return_unit
  else
    let destination =
      Format.sprintf
        "%s%s:%s"
        (Option.fold
           ~none:""
           ~some:(fun u -> Format.sprintf "%s@" u)
           runner.ssh_user)
        runner.address
        destination
    in
    let* () =
      Process.run "scp" (["-O"] @ identity @ port @ [source] @ [destination])
    in
    Lwt.return_unit

let copy =
  (* We memoize the copy so that it is done at most once per destination per
     scenario. This optimisation ease the writing of scenario so that copy can
     always be called before using the file copied. *)
  let already_copied = Hashtbl.create 11 in
  fun agent ~source ->
    let destination = path_of agent source in
    match Hashtbl.find_opt already_copied (agent, destination) with
    | Some promise -> promise
    | None ->
        let p =
          let* () =
            Process.spawn
              ~runner:agent.runner
              "mkdir"
              ["-p"; Filename.dirname destination]
            |> Process.check
          in
          let* () = copy agent ~source ~destination in
          Lwt.return destination
        in
        Hashtbl.replace already_copied (agent, destination) p ;
        p

let next_available_port t = t.next_available_port ()

let runner {runner; _} = runner

let configuration {configuration; _} = configuration
