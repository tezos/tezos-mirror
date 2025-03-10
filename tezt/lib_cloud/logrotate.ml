(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let config name = Format.asprintf "/etc/logrotate.d/%s" name

let write_config ~name ~target_file agent =
  (* Write file locally *)
  let source = Temp.file (Format.asprintf "%s.logrotate" (Agent.name agent)) in
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
# Runs rotation commands with root user and group permissions
  su root root
# Maximum number of rotations before removing the oldests
  rotate 300
}
|})) ;
  (* Upload the generated config in the destination /etc/logrotate.d/$name *)
  let destination = config name in
  let* _ = Agent.copy agent ~source ~destination in
  Lwt.return_unit

let run ~name agent =
  let name = config name in
  (* Run logrotate as root in the docker container *)
  Agent.docker_run_command agent "logrotate" [name] |> Process.check
