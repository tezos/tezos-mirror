(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Scenarios_helpers

(** This module allows to configure NginX as a reverse proxy in front
    of a collection of DAL nodes. This allows to balance the load on
    the DAL nodes based on which DAL slot index is used while
    complying with the interface of the rollup node, which expects a
    single DAL node endpoint.

    The NginX reverse-proxy configuration is added as an NginX site
    (in directory /etc/nginx/sites-available/ which is symlinked in
    /etc/nginx/sites-enabled/). This module makes no assumption about
    the existence of other NginX sites and should be invisible to them
    except that:

    - it disables the "default" NginX site (the file
      /etc/nginx/sites-enabled/default is removed if it exists),
    - it overrides the "reverse_proxy" NgninX site if there is some.
  *)

(* An NginX node is made of directives inside potentially nested context blocks.
*)
type node = Directive of string | Context_block of string * node list

type config = node list

let rec pp_node out = function
  | Directive s -> Format.fprintf out "%s;" s
  | Context_block (head, l) ->
      Format.fprintf out "@[<v 2>%s {@;%a@]@;}" head pp_config l

and pp_config config =
  Format.pp_print_list
    ~pp_sep:(fun out () -> Format.fprintf out "@;")
    pp_node
    config

let init ~agent ~site config =
  (* A NGINX reverse proxy which balances load between producer DAL
     nodes based on the requested slot index. *)
  let runner = Agent.runner agent in
  let () = toplog "Launching reverse proxy" in
  let () = toplog "Generating nginx reverse proxy config" in
  let config_filename = Tezt.Temp.file (sf "nginx_config_%s" site) in
  let out_chan = Stdlib.open_out config_filename in
  let config_ppf = Format.formatter_of_out_channel out_chan in
  Format.fprintf config_ppf "@[<v 0>%a@]@." pp_config config ;
  close_out out_chan ;

  (* Upload and delete the configuration file. *)
  let* (_ : string) =
    Agent.copy
      ~destination:(Format.sprintf "/etc/nginx/sites-available/%s" site)
      ~source:config_filename
      agent
  in
  let* () = Process.spawn "rm" [config_filename] |> Process.check in

  (* Disable the default configuration *)
  let* () =
    Process.spawn ?runner "rm" ["-f"; "/etc/nginx/sites-enabled/default"]
    |> Process.check
  in

  (* Enable the reverse proxy configuration *)
  let* () =
    Process.spawn
      ?runner
      "ln"
      [
        "-s";
        Format.sprintf "/etc/nginx/sites-available/%s" site;
        Format.sprintf "/etc/nginx/sites-enabled/%s" site;
      ]
    |> Process.check
  in

  (* Check the NginX configuration *)
  let* () = Process.spawn ?runner "nginx" ["-t"] |> Process.check in

  (* Start the NginX service *)
  (* If the service can be stopped (i.e. the command doesn't fail), we
       probably are in a SysV system. Nginx will run as a service natively.
       Otherwise, we need to start it manually. *)
  let* service_cmd =
    Process.spawn ?runner "service" ["nginx"; "stop"] |> Process.wait
  in
  match Process.validate_status service_cmd with
  | Ok () -> Process.spawn ?runner "service" ["nginx"; "start"] |> Process.check
  | Error _ ->
      (* Runs the process as a daemon, and don't bind the process, otherwise
         Tezt will wait for it to finish.
        *)
      let _process = Process.spawn ?runner "nginx" ["-g"; "daemon on;"] in
      unit

(* Returns an NginX configuration corresponding to:
server {
  listen 0.0.0.0:<port>;
  server_name <server_name>;

  location <location> {
    proxy_pass <proxy_pass>;
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
  }
}
*)
let make_simple_config ~server_name ~port ~location ~proxy_pass =
  [
    Context_block
      ( "server",
        [
          Directive (sf "listen 0.0.0.0:%d" port);
          Directive (sf "server_name %s" server_name);
          Context_block
            ( sf "location %s" location,
              [
                Directive (sf "proxy_pass %s" proxy_pass);
                Directive "proxy_set_header Host $host";
                Directive "proxy_set_header X-Real-IP $remote_addr";
              ] );
        ] );
  ]

let init_simple agent ~site ~server_name ~port ~location ~proxy_pass =
  let config = make_simple_config ~server_name ~port ~location ~proxy_pass in
  init ~agent ~site config

(* Returns an NginX configuration node corresponding to:
server {
  listen 0.0.0.0:<port> ssl;
  server_name <server_name>;

  ssl_certificate <certificate>;
  ssl_certificate_key <certificate_key>;

  location <location> {
    proxy_pass <proxy_pass>;
    proxy_http_version 1.1;
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_buffering off;
    proxy_request_buffering off;
    proxy_cache off;
    proxy_read_timeout 3600;
    proxy_send_timeout 3600;
  }
}
The
    proxy_http_version 1.1;
    proxy_buffering off;
    proxy_request_buffering off;
    proxy_cache off;
    proxy_read_timeout 3600;
    proxy_send_timeout 3600;
part enables streamed RPCs.
*)
let simple_ssl_node ~server_name ~port ~location ~proxy_pass ~certificate
    ~certificate_key =
  [
    Context_block
      ( "server",
        [
          Directive (sf "listen 0.0.0.0:%d ssl" port);
          Directive (sf "server_name %s" server_name);
          Directive (sf "ssl_certificate %s" certificate);
          Directive (sf "ssl_certificate_key %s" certificate_key);
          Context_block
            ( sf "location %s" location,
              [
                Directive (sf "proxy_pass %s" proxy_pass);
                Directive "proxy_http_version 1.1";
                Directive "proxy_set_header Host $host";
                Directive "proxy_set_header X-Real-IP $remote_addr";
                Directive "proxy_buffering off";
                Directive "proxy_request_buffering off";
                Directive "proxy_cache off";
                Directive "proxy_read_timeout 3600";
                Directive "proxy_send_timeout 3600";
              ] );
        ] );
  ]
