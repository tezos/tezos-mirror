(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [run ?runner ?cmd_wrapper ?interface ()] runs netdata on the VM.

    When [interface] is provided (e.g. ["127.0.0.1"]), Netdata's web
    server is bound to that address instead of the default ["0.0.0.0"].
    This is used to restrict direct access when auth is enabled and
    traffic goes through the nginx reverse proxy. *)
val run :
  ?runner:Runner.t ->
  ?cmd_wrapper:Gcloud.cmd_wrapper ->
  ?interface:string ->
  unit ->
  unit Lwt.t
