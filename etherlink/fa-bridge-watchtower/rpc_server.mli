(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [start db config rpc] starts an RPC server with the given database and
    configuration.

    @param db The SQLite database connection
    @param config The global application configuration
    @param rpc The RPC server configuration (address and port)
    @return A function that takes a websocket client and registers it with the
      RPC server.  This function can be called when a new websocket connection
      is established.
*)
val start :
  Sqlite.t -> Config.t -> Config.rpc -> (Websocket_client.t -> unit) Lwt.t
