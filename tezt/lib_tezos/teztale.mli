(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** A user is either an archiver or an admin *)
type user = {login : string; password : string}

type interface = {address : string; port : int}

module Server : sig
  (** See parameters of {!run} function for details. *)
  type conf = {
    name : string;
    interface : interface;
    users : user list;
    admin : user;
    public_directory : string option;
  }

  (** Expose paths of the files involved in the test:
      - Server configuration file
      - SQLite database
  *)
  type filenames = {conf_filename : string; db_filename : string}

  type t = {process : Process.t; filenames : filenames; conf : conf}

  (** [run ?runner ?path ?name ?address ?port ?users ?admin ?public_directory ()]

      Spawn a teztale server with some given parameters:
      - runner: runner used to spawn the process
      - path: path of the teztale server executable
      - name: name the the server used in logs
      - address: where teztale server will be listening on (default is [127.0.0.1])
      - port: port associated with [address]
      - users: list of archivers allowed to feed the server. You can add
        users once the server is started using the {!add_user} function.
      - admin: credential used for adming tasks such as adding an allowed archiver
        default is [admin:password]
      - public_directory: TODO
  *)
  val run :
    ?runner:Runner.t ->
    ?path:string ->
    ?name:string ->
    ?address:string ->
    ?port:int ->
    ?users:user list ->
    ?admin:user ->
    ?public_directory:string ->
    unit ->
    t Lwt.t

  (** Wait until teztale server is listening to its defined interface *)
  val wait_for_readiness : t -> unit Lwt.t

  (** [add_user server user]
      Add an archiver (its credentials) allowed to feed the database.
  *)
  val add_user :
    ?runner:Runner.t ->
    t ->
    ?public_address:string ->
    user ->
    (unit, exn) Result.t Lwt.t
end

module Archiver : sig
  (** See parameters of {!run} function for details. *)
  type conf = {name : string; user : user; feed : interface list}

  type t = {process : Process.t; conf : conf}

  (** [run ?runner ?path ?name ~node_port user feed]

      Spawn a teztale archiver with some given parameters:
      - runner: runner used to spawn the process
      - path: path of the teztale server executable
      - name: name the the server used in logs
      - node_port: port used for the octez node RPCs
      - user: login and password used by the archiver. Make sure that
        login and password used have been allowed in the server. See
        [?users] argument of {!Server.run} or {!Server.add_user}.
      - feed: list of interfaces for teztale servers to feed. If the archiver
        is feeding multiple servers, every server must accept login info defined
        by [user] parameter.
  *)
  val run :
    ?runner:Runner.t ->
    ?path:string ->
    ?name:string ->
    node_port:int ->
    user ->
    interface list ->
    t Lwt.t
end
