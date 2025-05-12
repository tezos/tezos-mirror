(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [start db rpc] starts an RPC server with the given database and configuration
    and returns a function that can be called to stop the server *)
val start : Db.t -> Config.rpc -> (unit -> unit Lwt.t) Lwt.t
