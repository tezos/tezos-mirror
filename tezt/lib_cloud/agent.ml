(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  (* The name initially is the same as [vm_name] and can be changed dynamically by the scenario. *)
  mutable name : string;
  vm_name : string;
  zone : string option;
  point : string * int;
  runner : Runner.t;
  next_available_port : unit -> int;
  configuration : Configuration.t;
}

let ssh_id () =
  if Env.mode = `Orchestrator then Env.ssh_private_key_filename ~home:"/root" ()
  else Env.ssh_private_key_filename ()

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
    (fun {machine_type; binaries_path; docker_image; max_run_duration = _; os} ->
      (machine_type, binaries_path, docker_image, os))
    (fun (machine_type, binaries_path, docker_image, os) ->
      Configuration.make ~os ~machine_type ~binaries_path ~docker_image ())
    (obj4
       (req "machine_type" Data_encoding.string)
       (req "binaries_path" Data_encoding.string)
       (req "docker_image" docker_image_encoding)
       (req "os" Data_encoding.string))

let encoding =
  let open Data_encoding in
  conv
    (fun {
           name;
           vm_name;
           zone;
           point;
           runner = _;
           next_available_port;
           configuration;
         } ->
      (name, vm_name, zone, point, next_available_port (), configuration))
    (fun (name, vm_name, zone, point, next_available_port, configuration) ->
      let ssh_port = snd point in
      let address = fst point in
      let ssh_id = ssh_id () in
      let runner =
        Runner.create ~ssh_user:"root" ~ssh_id ~ssh_port ~address ()
      in
      let next_available_port =
        let current_port = ref (next_available_port - 1) in
        fun () ->
          incr current_port ;
          !current_port
      in
      {name; vm_name; zone; point; runner; next_available_port; configuration})
    (obj6
       (req "name" Data_encoding.string)
       (req "vm_name" Data_encoding.string)
       (req "zone" (Data_encoding.option Data_encoding.string))
       (req
          "point"
          (Data_encoding.tup2 Data_encoding.string Data_encoding.int31))
       (req "next_available_port" Data_encoding.int31)
       (req "configuration" configuration_encoding))

let make ?zone ~ssh_id ~point:((address, ssh_port) as point) ~configuration
    ~next_available_port ~name () =
  let ssh_user = "root" in
  let runner = Runner.create ~ssh_user ~ssh_id ~ssh_port ~address () in
  {
    point;
    runner;
    name;
    vm_name = name;
    next_available_port;
    configuration;
    zone;
  }

let name {name; _} = name

let vm_name {vm_name; _} = vm_name

let point {point; _} = point

let cmd_wrapper {zone; vm_name; _} =
  match zone with
  | None -> None
  | Some zone ->
      let ssh_private_key_filename =
        if Env.mode = `Orchestrator then
          Env.ssh_private_key_filename ~home:"/root" ()
        else Env.ssh_private_key_filename ()
      in
      Some (Gcloud.cmd_wrapper ~zone ~vm_name ~ssh_private_key_filename)

let set_name agent name = agent.name <- name

let path_of agent binary = agent.configuration.binaries_path // binary

let host_run_command agent cmd args =
  match cmd_wrapper agent with
  | None -> Process.spawn cmd args
  | Some cmd_wrapper ->
      Process.spawn cmd_wrapper.Gcloud.cmd (cmd_wrapper.args @ [cmd] @ args)

let docker_run_command agent cmd args =
  let cmd, args =
    Runner.wrap_with_ssh agent.runner (Runner.Shell.cmd [] cmd args)
  in
  (* The host check is very not convenient at all for this setting. This makes
     tezt cloud sensible to man in the middle attacks. There are other options
     like doing the check the first time only etc... But they all fail at some
     point. I think the issue is that:

      - GCP may reuse IP addresses
      - The docker image generates new key anytime it is generated

      I don't have a good proposition that keeps a nice UX and is secure at the moment.
  *)
  Process.spawn cmd (["-o"; "StrictHostKeyChecking=no"] @ args)

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
    let process = docker_run_command agent "ls" [destination] in
    let* status = process |> Process.wait in
    match status with
    | WEXITED 0 ->
        let hash_of_md5_output output =
          String.trim output |> String.split_on_char ' ' |> List.hd
        in
        (* If the file already exists on the remote machine, we compare the
           hashes to be sure they are the same. *)
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
                Lwt.return (destination_hash = source_hash)
            | _ -> Lwt.return_true)
          (fun _ -> Lwt.return_false)
    | _ -> Lwt.return_false
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
      (* FIXME: I forgot why we enforce [-0]. *)
      Process.run
        "scp"
        (["-O"]
        @ ["-o"; "StrictHostKeyChecking=no"]
        @ identity @ port @ [source] @ [destination])
    in
    Lwt.return_unit

let is_binary file =
  let* output = Process.run_and_read_stdout "file" [file] in
  String.split_on_char ' ' output
  |> List.tl |> List.hd |> String.trim
  |> String.starts_with ~prefix:"ELF"
  |> Lwt.return

let copy =
  (* We memoize the copy so that it is done at most once per destination per
     scenario. This optimisation ease the writing of scenario so that copy can
     always be called before using the file copied. *)
  let already_copied = Hashtbl.create 11 in
  fun ?destination agent ~source ->
    Log.info "COPY %s" source ;
    let destination =
      Option.value ~default:(path_of agent source) destination
    in
    match Hashtbl.find_opt already_copied (agent, destination) with
    | Some promise -> promise
    | None ->
        (* Octez_latest_release image uses alpine, we can't copy binaries from our local setup. *)
        let* is_binary_file = is_binary source in
        if
          is_binary_file
          && agent.configuration.docker_image = Octez_latest_release
        then Lwt.return (path_of agent source)
        else
          let p =
            let* () =
              docker_run_command
                agent
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

let runner {runner; _} = Some runner

let configuration {configuration; _} = configuration
