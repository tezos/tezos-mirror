(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [generate_key ()] generates an ssh key based on the [Env.tezt_cloud]
     variable environment. *)
val generate_key : unit -> unit Lwt.t
