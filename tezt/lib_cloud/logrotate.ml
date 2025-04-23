(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let config name = Format.asprintf "/etc/logrotate.d/%s" name

let write_config ~name ~target_file ~max_rotations agent =
  (* Write file locally *)
  let source = Temp.file (Format.asprintf "%s.logrotate" name) in
  (* Choose a reasonable 300 rotations of 200MB max file
     Files after 300 rotations are deleted
     The rotate is triggered daily or if they are greater than 200MB *)
  Base.with_open_out source (fun oc ->
      output_string
        oc
        (Format.asprintf
           "%s {@\n%s"
           target_file
           {|
# Rotates logs every day
  daily
# Rotates the log when it reaches 200 megabytes, even if not time for rotation
  maxsize 200M
# Skips rotation if the log file is empty
  notifempty
# Doesn't produce an error if the log file is missing
  missingok
# Compresses rotated logs using gzip by default
  compress
# Copies the log file then truncates the original instead of moving it
  copytruncate
# Runs rotation commands with root user and group permissions  daily
  su root root
# Maximum number of rotations before removing the oldests.
|}
        ^ Format.asprintf "  rotate %d\n}\n" max_rotations)) ;
  Log.info "Teztcloud.Logrotate: written configuration for %s" target_file ;
  (* Upload the generated config in the destination /etc/logrotate.d/$name *)
  let destination = config name in
  Log.info
    "Teztcloud.Logrotate: uploading %s to %s:%s"
    target_file
    (Agent.name agent)
    destination ;
  let* _ = Agent.copy agent ~source ~destination in
  Lwt.return_unit

let run ~name agent =
  let name = config name in
  (* Run logrotate as root in the docker container *)
  Log.info "Teztcloud.Logrotate: executing task %s" name ;
  Agent.docker_run_command agent "logrotate" [name] |> Process.check
