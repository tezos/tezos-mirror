(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let config name = Format.asprintf "/etc/logrotate.d/%s" name

let write_config ~name ?pidfile ?max_rotations ?max_size ~target_file agent =
  let open Jingoo in
  let template =
    {|{{ target_file }} {
# Rotates logs every day
  daily
{% if has_max_size %}
# Rotates the log when it reaches {{ max_size }} kbytes, even if not time for rotation
  maxsize {{ max_size }}k
{% endif %}
# Skips rotation if the log file is empty
  notifempty
# Doesn't produce an error if the log file is missing
  missingok
# Compresses rotated logs using gzip by default
  compress
# Runs rotation commands with root user and group permissions  daily
  su root root
{% if has_max_rotation %}
# Maximum number of rotations before removing the oldests.
  rotate {{ max_rotation }}
{% endif %}
# Send a signal to process in order to reload it
  postrotate
      {% if has_pidfile -%}
      if [ -f {{ pidfile }} ]; then
          # Send SIGHUP to pid in order to force reopen its logfile
          kill -HUP $(cat {{ pidfile }})
      fi
      {%- else -%}
      # No pidfile is provided, send to *ALL* processes whose name is "{{ name }}".
      pkill -HUP {{ name }}
      {%- endif %}
  endscript
}
|}
  in
  Log.info "Teztcloud.Logrotate: applying template" ;
  let content =
    Jg_template.from_string
      template
      ~models:
        [
          ("target_file", Jg_types.Tstr target_file);
          ("name", Jg_types.Tstr name);
          ("has_max_rotation", Jg_types.Tbool (Option.is_some max_rotations));
          ("max_rotation", Jg_types.Tint (Option.value ~default:0 max_rotations));
          ("has_max_size", Jg_types.Tbool (Option.is_some max_size));
          ("max_size", Jg_types.Tint (Option.value ~default:0 max_size));
          ("has_pidfile", Jg_types.Tbool (Option.is_some pidfile));
          ("pidfile", Jg_types.Tstr (Option.value ~default:"" pidfile));
        ]
  in
  Log.info "Teztcloud.Logrotate: applying template: done" ;
  (* Write file locally *)
  let source = Temp.file (Format.asprintf "%s.logrotate" name) in
  (* Output file locally, to upload it *)
  Base.with_open_out source (fun oc -> output_string oc content) ;
  Log.info "Teztcloud.Logrotate: written configuration for %s" target_file ;
  (* Upload the generated config in the destination /etc/logrotate.d/$name *)
  let destination = config name in
  Log.info
    "Teztcloud.Logrotate: uploading %s to %s:%s"
    source
    (Agent.name agent)
    destination ;
  let* _ = Agent.copy agent ~source ~destination in
  Lwt.return_unit

let run ~name agent =
  let name = config name in
  (* Run logrotate as root in the docker container *)
  Log.info "Teztcloud.Logrotate: executing task %s" name ;
  Agent.docker_run_command agent "logrotate" [name] |> Process.check
