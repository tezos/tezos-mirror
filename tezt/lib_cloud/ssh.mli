(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [generate_key ()] generates an ssh key based on the [Env.tezt_cloud]
     variable environment. *)
val generate_key : unit -> unit Lwt.t

(** [ssh_public_key()] returns the ssh public key associated to the generate_key
    It calls [generate_key] if it does not exist *)
val public_key : unit -> string Lwt.t
