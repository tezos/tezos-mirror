(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [run ?cmd_wrapper ()] runs netdata on the VM. *)
val run :
  ?runner:Runner.t -> ?cmd_wrapper:Gcloud.cmd_wrapper -> unit -> unit Lwt.t
