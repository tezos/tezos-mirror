(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [generate_key ()] generates an ssh key based on the [Env.tezt_cloud]
     variable environment. *)
val generate_key : unit -> unit Lwt.t

(** [ssh_public_key()] returns the ssh public key associated to the generate_key
    It calls [generate_key] if it does not exist *)
val public_key : unit -> string Lwt.t

(* Default options required to properly run through ssh. *)
val ssh_options : string list

(* Default options required to properly run scp command. As scp command's syntax
   is close to the ssh one, we reuse the [ssh_options]. This might be breaking
   if incompatible ssh options are used. *)
val scp_options : string list
