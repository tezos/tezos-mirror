(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* This interface defines how tezt tests will interact with cast client.
   Cast is an ethereum client to interact with the node provided by Foundry
   https://book.getfoundry.sh/cast *)

(** [version ()] returns the version of cast. *)
val version : unit -> string Lwt.t
