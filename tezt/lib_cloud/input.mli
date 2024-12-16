(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** This module should be the only one that reads on [stdin]. *)

(** [next ()] returns the next line on stdin or none if stdin is closed. *)
val next : unit -> string option Lwt.t
