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

let docker_image_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"gcp"
        Json_only
        string
        (function Env.Gcp {alias} -> Some alias | _ -> None)
        (fun alias -> Gcp {alias});
      case
        ~title:"octez_latest_release"
        Json_only
        (constant "octez-latest-release")
        (function Env.Octez_latest_release -> Some () | _ -> None)
        (fun () -> Octez_latest_release);
    ]

let configuration_encoding =
  let open Data_encoding in
  let open Configuration in
  conv
    (fun {machine_type; binaries_path; docker_image; max_run_duration = _} ->
      (machine_type, binaries_path, docker_image))
    (fun (machine_type, binaries_path, docker_image) ->
      Configuration.make ~machine_type ~binaries_path ~docker_image ())
    (obj3
       (req "machine_type" Data_encoding.string)
       (req "binaries_path" Data_encoding.string)
       (req "docker_image" docker_image_encoding))

let cmd_wrapper_encoding =
  let open Data_encoding in
  conv
    (fun Gcloud.{cmd; args} -> (cmd, args))
    (fun (cmd, args) -> {cmd; args})
    (obj2
       (req "cmd" Data_encoding.string)
       (req "args" (Data_encoding.list Data_encoding.string)))

let encoding =
  let open Data_encoding in
  conv
    (fun {
           name;
           vm_name;
           cmd_wrapper;
           point;
           runner;
           next_available_port;
           configuration;
         } ->
      let ssh_id = runner.Runner.ssh_id |> Option.get in
      ( name,
        vm_name,
        cmd_wrapper,
        point,
        ssh_id,
        next_available_port (),
        configuration ))
    (fun ( name,
           vm_name,
           cmd_wrapper,
           point,
           ssh_id,
           next_available_port,
           configuration ) ->
      let ssh_port = snd point in
      let address = fst point in
      let runner =
        Runner.create ~ssh_user:"root" ~ssh_id ~ssh_port ~address ()
      in
      let next_available_port =
        let current_port = ref (next_available_port - 1) in
        fun () ->
          incr current_port ;
          !current_port
      in
      {
        name;
        vm_name;
        cmd_wrapper;
        point;
        runner;
        next_available_port;
        configuration;
      })
    (obj7
       (req "name" Data_encoding.string)
       (req "vm_name" Data_encoding.string)
       (req "cmd_wrapper" (Data_encoding.option cmd_wrapper_encoding))
       (req
          "point"
          (Data_encoding.tup2 Data_encoding.string Data_encoding.int31))
       (req "ssh_id" Data_encoding.string)
       (req "next_available_port" Data_encoding.int31)
       (req "configuration" configuration_encoding))

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
