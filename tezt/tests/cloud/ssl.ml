(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** This module provides a basic interface to generate SSL certificates using
    certbot, for instance to enable HTTPS. *)

(* Typical SSL generation creates several files. Among which, one has the
   certificate, and another the certificate key. These two are needed in order to
   set up HTTPS through NginX. *)
type t = {certificate : string; key : string}

(* Generate a SSL certificate for [endpoint].
   [endpoint] must be a name, not an IP. This means you need to know or decide
   the name of the machine beforehand, i.e. the one you'll configure in DNS. *)
let generate agent endpoint =
  let* () =
    Agent.docker_run_command
      ~name:"certbot"
      agent
      "certbot"
      [
        "certonly";
        "--standalone";
        "--non-interactive";
        "--agree-tos";
        "-d";
        endpoint;
      ]
    |> Process.check
  in
  (* Default output files. *)
  let certificate = sf "/etc/letsencrypt/live/%s/fullchain.pem" endpoint in
  let key = sf "/etc/letsencrypt/live/%s/privkey.pem" endpoint in
  return {certificate; key}
