(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** [run ?disable_logging ?disable_shard_validation ~data_dir ~configuration_override ()]
    starts a DAL node with the given data directory and function to generate an initial
    configuration.

    This function performs the following steps:

    - Loads the configuration from the provided data directory, or uses a
    default configuration if none is found. The configuration is then
    potentially overridden by [configuration_override];

    - Fetches the DAL configuration from the L1 node;

    - Creates and starts a Gossipsub worker for handling pub/sub messaging;

    - Initializes/opens the store for persisting node data;

    - Starts a crawler for the L1 node;

    - Initializes the node context, containing differents stores, caches and
    configurations;

    - Starts the RPC server to handle incoming RPC requests;

    - Connects the Gossipsub worker with the P2P layer and to the crawler;

    - Can disable the shard validation using [?disable_shard_validation].
*)
val run :
  ?disable_logging:bool ->
  ?disable_shard_validation:bool ->
  data_dir:string ->
  configuration_override:(Configuration_file.t -> Configuration_file.t) ->
  unit ->
  unit tzresult Lwt.t
