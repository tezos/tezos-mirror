(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type user = {login : string; password : string}

type interface = {address : string; port : int}

module Server : sig
  type conf = {
    name : string;
    interface : interface;
    users : user list;
    admin : user;
  }

  type filenames = {conf_filename : string; db_filename : string}

  type t = {process : Process.t; filenames : filenames; conf : conf}

  val run :
    ?runner:Runner.t ->
    ?path:string ->
    ?name:string ->
    ?address:string ->
    ?port:int ->
    ?users:user list ->
    ?admin:user ->
    unit ->
    t Lwt.t

  val add_user : t -> user -> (unit, exn) Result.t Lwt.t
end

module Archiver : sig
  type conf = {name : string; user : user; feed : interface list}

  type t = {process : Process.t; conf : conf}

  val run :
    ?runner:Runner.t ->
    ?path:string ->
    ?name:string ->
    node_port:int ->
    user ->
    interface list ->
    t Lwt.t
end
