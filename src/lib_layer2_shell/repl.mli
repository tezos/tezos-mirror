(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [loop cmds state term] starts a new read-eval-print loop allowing
    users to interactively execute the commands in [cmds] on top of the
    provided [state].. *)
val loop : 'state Commands.command trace -> 'state -> LTerm.t -> unit Lwt.t
