(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Configuration = struct
  include Types.Agent_configuration

  let pp fmt x =
    Format.fprintf
      fmt
      "%a"
      Data_encoding.Json.pp
      (Data_encoding.Json.construct encoding x)

  let uri_of_docker_image docker_image =
    match (docker_image, Env.mode) with
    | ( Types.Agent_configuration.Gcp {alias},
        ( `Local_orchestrator_remote_agents | `Remote_orchestrator_remote_agents
        | `Remote_orchestrator_local_agents | `Ssh_host _ ) ) ->
        let* registry_uri = Env.registry_uri () in
        Lwt.return (Format.asprintf "%s/%s" registry_uri alias)
    | Gcp {alias}, `Local_orchestrator_local_agents -> Lwt.return alias
    | ( Octez_release _,
        ( `Local_orchestrator_remote_agents | `Remote_orchestrator_remote_agents
        | `Remote_orchestrator_local_agents | `Ssh_host _ ) ) ->
        let* registry_uri = Env.registry_uri () in
        Lwt.return (Format.asprintf "%s/octez" registry_uri)
    | Octez_release _, `Local_orchestrator_local_agents -> Lwt.return "octez"

  let registry_uri_of_docker_image docker_image =
    match (docker_image, Env.mode) with
    | ( Types.Agent_configuration.Gcp {alias = _},
        ( `Local_orchestrator_remote_agents | `Remote_orchestrator_remote_agents
        | `Remote_orchestrator_local_agents | `Ssh_host _ ) ) ->
        let* registry_uri = Env.registry_uri () in
        return (Some registry_uri)
    | Gcp {alias = _}, `Local_orchestrator_local_agents -> Lwt.return None
    | ( Octez_release _,
        ( `Local_orchestrator_remote_agents | `Remote_orchestrator_remote_agents
        | `Remote_orchestrator_local_agents | `Ssh_host _ ) ) ->
        let* registry_uri = Env.registry_uri () in
        Lwt.return (Some registry_uri)
    | Octez_release _, `Local_orchestrator_local_agents -> return None

  let docker_image_name docker_image =
    match (docker_image, Env.mode) with
    | Types.Agent_configuration.Gcp {alias}, _ -> alias
    | Octez_release _, _ -> "octez"

  let gen_name =
    let cpt = ref (-1) in
    fun () ->
      incr cpt ;
      Format.asprintf "agent-%03d" !cpt

  let make ?os ?binaries_path ?max_run_duration ?machine_type ?disk_type
      ?disk_size_gb ?docker_image ?dockerbuild_args ?(name = gen_name ()) () =
    let os = Option.value ~default:Cli.os os in
    let binaries_path = Option.value ~default:Cli.binaries_path binaries_path in
    let max_run_duration =
      let default =
        if Cli.no_max_run_duration then None else Some Cli.max_run_duration
      in
      Option.value ~default max_run_duration
    in
    let machine_type = Option.value ~default:Cli.machine_type machine_type in
    let disk_type =
      match disk_type with Some d -> Some d | None -> Cli.disk_type
    in
    let disk_size_gb =
      match disk_size_gb with Some s -> Some s | None -> Cli.disk_size_gb
    in
    let docker_image =
      Option.value ~default:(Gcp {alias = Env.dockerfile_alias}) docker_image
    in
    make
      ~os
      ~binaries_path
      ?max_run_duration
      ?disk_type
      ?disk_size_gb
      ?dockerbuild_args
      ~machine_type
      ~docker_image
      ~name
      ()
end

type t = {
  vm_name : string option;
  zone : string option;
  point : (string * int) option;
  runner : Runner.t option;
  next_available_port : unit -> int;
  configuration : Configuration.t;
  process_monitor : Process_monitor.t option;
  service_manager : Service_manager.t option;
  artifacts_dir : string option;
  mutable on_shutdown : (unit -> unit Lwt.t) list;
}

let ssh_id () = Env.ssh_private_key_filename ()

(* Encodings *)

let encoding =
  let open Data_encoding in
  conv
    (fun {
           vm_name;
           zone;
           point;
           runner = _;
           next_available_port;
           configuration;
           process_monitor;
           service_manager = _;
           artifacts_dir;
           on_shutdown = _;
         }
       ->
      ( vm_name,
        zone,
        point,
        next_available_port (),
        configuration,
        process_monitor,
        artifacts_dir ))
    (fun ( vm_name,
           zone,
           point,
           next_available_port,
           configuration,
           process_monitor,
           artifacts_dir )
       ->
      let next_available_port =
        let current_port = ref (next_available_port - 1) in
        fun () ->
          incr current_port ;
          !current_port
      in
      let runner =
        match point with
        | None -> None
        | Some (address, ssh_port) ->
            let ssh_id = ssh_id () in
            (* The host check is very not convenient at all for this setting. This makes
               tezt cloud sensible to man in the middle attacks. There are other options
               like doing the check the first time only etc... But they all fail at some
               point. I think the issue is that:

                - GCP may reuse IP addresses
                - The docker image generates new key anytime it is generated

                I don't have a good proposition that keeps a nice UX and is secure at the moment.
            *)
            Runner.create
              ~options:Ssh.ssh_options
              ~ssh_user:"root"
              ~ssh_id
              ~ssh_port
              ~address
              ()
            |> Option.some
      in
      {
        vm_name;
        zone;
        point;
        runner;
        next_available_port;
        configuration;
        process_monitor;
        service_manager = None;
        artifacts_dir;
        on_shutdown =
          [] (* As of now, this encoding is only used when reattaching *);
      })
    (obj7
       (req "vm_name" (option string))
       (req "zone" (option string))
       (req "point" (option (tup2 string int31)))
       (req "next_available_port" int31)
       (req "configuration" Configuration.encoding)
       (opt "process_monitor" Process_monitor.encoding)
       (opt "artifacts_dir" string))

(* Getters *)

let name {configuration = {name; _}; _} = name

let vm_name {vm_name; _} = vm_name

let point {point; _} = point

let next_available_port t = t.next_available_port ()

let runner {runner; _} = runner

let configuration {configuration; _} = configuration

let artifacts_dir {artifacts_dir; _} = artifacts_dir

let make ?zone ?ssh_id ?point ~configuration ~next_available_port ~vm_name
    ~process_monitor ~artifacts_dir () =
  let ssh_user = "root" in
  let runner =
    match (point, ssh_id) with
    | None, None -> None
    | Some _, None | None, Some _ ->
        Test.fail "Agent.make was not initialized correctly"
    | Some (address, ssh_port), Some ssh_id ->
        Runner.create
          ~options:Ssh.ssh_options
          ~ssh_user
          ~ssh_id
          ~ssh_port
          ~address
          ()
        |> Option.some
  in
  {
    point;
    runner;
    vm_name;
    next_available_port;
    configuration;
    zone;
    process_monitor;
    service_manager = Service_manager.init () |> Option.some;
    artifacts_dir;
    on_shutdown = [];
  }

let cmd_wrapper {zone; vm_name; _} =
  match (zone, vm_name) with
  | None, None -> None
  | Some zone, Some vm_name ->
      let ssh_private_key_filename =
        if Env.mode = `Remote_orchestrator_local_agents then
          Env.ssh_private_key_filename ~home:"$HOME" ()
        else Env.ssh_private_key_filename ()
      in
      Some (Gcloud.cmd_wrapper ~zone ~vm_name ~ssh_private_key_filename)
  | _ ->
      Test.fail
        "Inconsistent agent setup, only one of zone and vm_name has been \
         declared."

let path_of agent binary = agent.configuration.vm.binaries_path // binary

let process_monitor agent = agent.process_monitor

let service_manager t = t.service_manager

let temp_execution_path () =
  (* This assumes that Tezt.Temp.file always returns the same result for the
     same process. *)
  Temp.dir ""

let register_shutdown_callback t callback =
  t.on_shutdown <- callback :: t.on_shutdown

let host_run_command agent cmd args =
  match cmd_wrapper agent with
  | None -> Process.spawn cmd args
  | Some cmd_wrapper ->
      Process.spawn cmd_wrapper.Gcloud.cmd (cmd_wrapper.args @ [cmd] @ args)

let docker_run_command ?name agent ?(detach = false) cmd args =
  (* This function allows to run a command and detach it from the terminal
      session and parent process. This allows to run a command in background
      without the session (and processes group) being killed by ssh on
      disconnection. It uses the [setsid -f] to detach the session.
     Automatically log stdout and stderr of the command in tezt temporary dir *)
  let run_detached ?runner cmd args =
    let whole_cmd = String.concat " " (cmd :: args) in
    let cmd = "sh" in
    let args =
      "-c"
      :: [
           "setsid -f " ^ whole_cmd ^ " > "
           ^ Temp.file ?runner (cmd ^ ".log")
           ^ " 2>&1";
         ]
    in
    (cmd, args)
  in
  match agent.runner with
  | None ->
      let cmd, args = if detach then run_detached cmd args else (cmd, args) in
      Process.spawn ?name cmd args
  | Some runner ->
      let cmd, args =
        if detach then run_detached ~runner cmd args else (cmd, args)
      in
      Process.spawn ?name ~runner cmd args

let scp agent ~is_directory ~source ~destination
    (direction : [< `ToRunner | `FromRunner]) =
  let runner_path runner path =
    Format.sprintf
      "%s%s:%s"
      (Option.fold
         ~none:""
         ~some:(fun u -> Format.sprintf "%s@" u)
         runner.Runner.ssh_user)
      runner.address
      path
  in
  match agent.runner with
  | None ->
      if source <> destination then Process.run "cp" [source; destination]
      else Lwt.return_unit
  | Some runner ->
      let source, destination =
        match direction with
        | `ToRunner -> (source, runner_path runner destination)
        | `FromRunner -> (runner_path runner source, destination)
      in
      let identity =
        Option.fold ~none:[] ~some:(fun i -> ["-i"; i]) runner.Runner.ssh_id
      in
      let port =
        Option.fold
          ~none:[]
          ~some:(fun p -> ["-P"; Format.sprintf "%d" p])
          runner.Runner.ssh_port
      in
      let* () =
        Process.run
          "scp"
          ((if is_directory then ["-r"] else [])
          @ Ssh.scp_options @ identity @ port @ [source] @ [destination])
      in
      Lwt.return_unit

let copy agent ~consistency_check ~is_directory ~source ~destination =
  let* exists =
    let process = docker_run_command agent "ls" [destination] in
    let* status = process |> Process.wait in
    match status with
    | WEXITED 0 ->
        let hash_of_md5_output output =
          String.trim output |> String.split_on_char ' ' |> List.hd
        in
        (* If the file already exists on the remote machine, we compare the
           hashes to check whether they are the same. *)
        Lwt.catch
          (fun () ->
            let* destination_hash =
              let* output =
                docker_run_command agent "md5sum" [destination]
                |> Process.check_and_read_stdout
              in
              (* md5sum output is: '<hash> <file>'. We only take the hash. *)
              Lwt.return (hash_of_md5_output output)
            in
            let process = Process.spawn "md5sum" [source] in
            let* status = process |> Process.wait in
            match status with
            | WEXITED 0 ->
                let* output = Process.check_and_read_stdout process in
                let source_hash = hash_of_md5_output output in
                let is_consistent = destination_hash = source_hash in
                if consistency_check then Lwt.return is_consistent
                else if is_consistent then Lwt.return_true
                else (
                  Log.warn
                    "The file %s is already on agent %s but has a different \
                     hash."
                    source
                    (name agent) ;
                  Lwt.return_true)
            | _ -> Lwt.return_true)
          (fun _ -> Lwt.return_false)
    | _ -> Lwt.return_false
  in
  if exists then Lwt.return_unit
  else scp agent ~is_directory ~source ~destination `ToRunner

(** [is_binary file] checks using the `file` Linux command if the [file] is
    binary. The command returns an output of the form `file: <file_type> ...`;
    the file is binary if <file_type> is "ELF". *)
let is_binary file =
  (* With '-L' it dereferences symbolic links. Useful if the file is
     actually a symbolic link to a binary file. *)
  let* output = Process.run_and_read_stdout "file" ["-L"; file] in
  (* output is of the form "file: type_of_file ..." *)
  String.split_on_char ' ' output
  |> List.tl |> List.hd |> String.trim
  |> String.starts_with ~prefix:"ELF"
  |> Lwt.return

let copy =
  (* We memoize the copy so that it is done at most once per destination per
     scenario. This optimisation eases the writing of scenario so that copy can
     always be called before using the file copied. *)
  let already_copied = Hashtbl.create 11 in
  fun ?consistency_check
      ?(refresh = false)
      ?(is_directory = false)
      ?destination
      agent
      ~source
    ->
    let consistency_check =
      Option.value consistency_check ~default:Env.check_file_consistency
    in
    let destination =
      Option.value ~default:(path_of agent source) destination
    in
    match Hashtbl.find_opt already_copied (agent, destination) with
    | Some promise when not refresh -> promise
    | Some _ | None ->
        (* Octez images uses alpine, so we can't copy binaries from our local setup. *)
        let* is_binary_file = is_binary source in
        let octez_release_image =
          match agent.configuration.vm.docker_image with
          | Octez_release _ -> true
          | Gcp _ -> false
        in
        if is_binary_file && octez_release_image then
          Lwt.return (path_of agent source)
        else
          let p =
            let* () =
              docker_run_command
                agent
                "mkdir"
                ["-p"; Filename.dirname destination]
              |> Process.check
            in
            let* () =
              copy agent ~consistency_check ~is_directory ~source ~destination
            in
            Lwt.return destination
          in
          Hashtbl.replace already_copied (agent, destination) p ;
          p
