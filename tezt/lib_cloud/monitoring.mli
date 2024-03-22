(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [run ~run_command] runs netdata on the VM. [run_command] is a callback to
  run a command on the VM. *)
val run :
  run_command:(string -> string list -> (Process.t, string) Runnable.t) ->
  unit Lwt.t
