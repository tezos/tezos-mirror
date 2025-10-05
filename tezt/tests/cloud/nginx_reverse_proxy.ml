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

(* An NginX configuration file is made of directives inside
   potentially nested context blocks. *)
type config = Directive of string | Context_block of string * config list

let rec pp_config out = function
  | Directive s -> Format.fprintf out "%s;" s
  | Context_block (head, l) ->
      Format.fprintf
        out
        "@[<v 2>%s {@;%a@]@;}"
        head
        (Format.pp_print_list
           ~pp_sep:(fun out () -> Format.fprintf out "@;")
           pp_config)
        l

let init_reverse_proxy ~agent ~site config =
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
