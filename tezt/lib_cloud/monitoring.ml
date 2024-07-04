(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let run ?cmd_wrapper () =
  let* is_already_running =
    let process =
      Env.run_command ?cmd_wrapper "docker" ["ps"; "--format"; "{{.Names}}"]
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
    let args =
      "run -d --rm --name=netdata   --pid=host   --network=host   -v \
       netdataconfig:/etc/netdata   -v netdatalib:/var/lib/netdata   -v \
       netdatacache:/var/cache/netdata   -v /etc/passwd:/host/etc/passwd:ro   \
       -v /etc/group:/host/etc/group:ro   -v \
       /etc/localtime:/etc/localtime:ro   -v /proc:/host/proc:ro   -v \
       /sys:/host/sys:ro   -v /etc/os-release:/host/etc/os-release:ro   -v \
       /var/log:/host/var/log:ro   -v \
       /var/run/docker.sock:/var/run/docker.sock:ro --cap-add SYS_PTRACE   \
       --cap-add SYS_ADMIN   --security-opt apparmor=unconfined   \
       netdata/netdata" |> String.split_on_char ' '
      |> List.filter (fun s -> s <> "")
    in
    let* _ = Env.run_command ?cmd_wrapper cmd args |> Process.check in
    Lwt.return_unit
