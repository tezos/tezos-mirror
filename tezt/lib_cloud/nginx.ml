(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let container_name = "nginx-auth"

let config_dir = Path.tmp_dir // "nginx"

let config_file = config_dir // "default.conf"

let htpasswd_file = config_dir // "htpasswd"

type service = {listen_port : int; proxy_target : string}

type t = {mutable services : service list}

let server_block {listen_port; proxy_target} =
  sf
    {|server {
    listen 0.0.0.0:%d;
    auth_basic "tezt-cloud";
    auth_basic_user_file /etc/nginx/.htpasswd;

    location / {
        proxy_pass %s;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
    }
}|}
    listen_port
    proxy_target

let generate_config services =
  List.map server_block services |> String.concat "\n\n"

let write_config services =
  let contents = generate_config services in
  with_open_out config_file (fun oc -> output_string oc contents)

let generate_htpasswd ~username ~password =
  let process, stdin =
    Process.spawn_with_stdin "openssl" ["passwd"; "-apr1"; "-stdin"]
  in
  let* () = Lwt_io.write_line stdin password in
  let* () = Lwt_io.close stdin in
  let* output = Process.check_and_read_stdout process in
  let hash = String.trim output in
  let contents = sf "%s:%s\n" username hash in
  with_open_out htpasswd_file (fun oc -> output_string oc contents) ;
  (* nginx:alpine docker runs workers as user nginx (not root), so the htpasswd
     file has to be world-readable. *)
  Unix.chmod htpasswd_file 0o644 ;
  Lwt.return_unit

let run ~username ~password ~services () =
  let* () = Process.run "mkdir" ["-p"; config_dir] in
  let* () = generate_htpasswd ~username ~password in
  write_config services ;
  let* () =
    Process.run
      "docker"
      [
        "run";
        "-d";
        "--rm";
        "--name";
        container_name;
        "--network";
        "host";
        "-v";
        sf "%s:/etc/nginx/conf.d:ro" config_dir;
        "-v";
        sf "%s:/etc/nginx/.htpasswd:ro" htpasswd_file;
        "nginx:alpine";
      ]
  in
  (* Health check: expect 401 Unauthorized on the first service port *)
  let* () =
    match services with
    | [] -> Lwt.return_unit
    | first :: _ ->
        let is_ready output = String.trim output = "401" in
        let run () =
          Process.spawn
            "curl"
            [
              "-s";
              "-o";
              "/dev/null";
              "-w";
              "%{http_code}";
              sf "http://localhost:%d/" first.listen_port;
            ]
        in
        let* _ = Env.wait_process ~is_ready ~run () in
        Lwt.return_unit
  in
  Lwt.return {services}

let add_service t service =
  t.services <- service :: t.services ;
  write_config t.services ;
  let* () =
    Process.run "docker" ["exec"; container_name; "nginx"; "-s"; "reload"]
  in
  Lwt.return_unit

let shutdown _t =
  let* () = Docker.kill container_name |> Process.check in
  Lwt.return_unit
