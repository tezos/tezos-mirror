(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let run ?runner ?cmd_wrapper ?(interface = "0.0.0.0") () =
  let* is_already_running =
    let process =
      Env.run_command
        ?runner
        ?cmd_wrapper
        "docker"
        ["ps"; "--format"; "{{.Names}}"]
    in
    let* status = Process.wait process in
    match status with
    | Unix.WEXITED 0 ->
        let* images_name = Process.check_and_read_stdout process in
        String.split_on_char '\n' images_name
        |> List.exists (fun s -> s = "netdata")
        |> Lwt.return
    | _ ->
        Test.fail
          "Unexpected failure: Could not list docker images on the remote \
           machine"
  in
  if is_already_running then Lwt.return_unit
  else
    let cmd = "docker" in
    (* Those arguments are the ones given by netdata on their online
       documentation. We could decide to put it in the eDSL module of docker. *)
    let* bind_volume =
      if interface <> "0.0.0.0" then
        (* Write a minimal Netdata config that overrides the web server
           bind address. This is mounted into the container to restrict
           Netdata to localhost when auth is enabled. *)
        let conf_dir = Path.tmp_dir // "netdata" in
        let conf_file = conf_dir // "netdata.conf" in
        let conf_content = sf "[web]\n    bind to = %s:19999\n" interface in
        let* () =
          Process.spawn ?runner "mkdir" ["-p"; conf_dir] |> Process.check
        in
        let* () =
          Process.spawn
            ?runner
            "sh"
            [
              "-c";
              sf
                "printf '%%s' %s > %s"
                (Filename.quote conf_content)
                (Filename.quote conf_file);
            ]
          |> Process.check
        in
        Lwt.return ["-v"; sf "%s:/etc/netdata/netdata.conf:ro" conf_file]
      else Lwt.return []
    in
    (* IMPORTANT: [bind_volume] is appended after [base_args] below so that
       the individual file bind-mount to /etc/netdata/netdata.conf overrides
       the file inside the netdataconfig named volume. Changing this order
       would cause the custom config to be shadowed. *)
    let base_args =
      [
        "run";
        "-d";
        "--rm";
        "--name=netdata";
        "--pid=host";
        "--network=host";
        "-v";
        "netdataconfig:/etc/netdata";
        "-v";
        "netdatalib:/var/lib/netdata";
        "-v";
        "netdatacache:/var/cache/netdata";
        "-v";
        "/etc/passwd:/host/etc/passwd:ro";
        "-v";
        "/etc/group:/host/etc/group:ro";
        "-v";
        "/etc/localtime:/etc/localtime:ro";
        "-v";
        "/proc:/host/proc:ro";
        "-v";
        "/sys:/host/sys:ro";
        "-v";
        "/etc/os-release:/host/etc/os-release:ro";
        "-v";
        "/var/log:/host/var/log:ro";
        "-v";
        "/var/run/docker.sock:/var/run/docker.sock:ro";
        "--cap-add";
        "SYS_PTRACE";
        "--cap-add";
        "SYS_ADMIN";
        "--security-opt";
        "apparmor=unconfined";
      ]
      @ bind_volume @ ["netdata/netdata"]
    in
    let* _ =
      Env.run_command ?runner ?cmd_wrapper cmd base_args |> Process.check
    in
    Lwt.return_unit
