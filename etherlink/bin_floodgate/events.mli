(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [is_ready ()] advertises that Floodgate has started and is ready to spam. *)
val is_ready : unit -> unit Lwt.t
