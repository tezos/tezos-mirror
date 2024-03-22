(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Wrapper module for docker commands. By default, [tag] is ["latest"]. *)

(** [build] is an alias for [docker build]. *)
val build :
  ?tag:string ->
  ?dockerfile:string ->
  args:(string * string) list ->
  unit ->
  (Process.t, unit) runnable

(** [tag] is an alias for [docker tag]. *)
val tag : ?tag:string -> string -> (Process.t, unit) runnable

(** [push] is an alias for [docker push]. *)
val push : ?tag:string -> string -> (Process.t, unit) runnable

(** [pull] is an alias for [docker pull]. *)
val pull : ?tag:string -> string -> (Process.t, unit) runnable

(** [run] is an alias for [docker run]. *)
val run :
  ?rm:bool ->
  ?name:string ->
  ?network:string ->
  ?publish_ports:string * string * string * string ->
  string ->
  string list ->
  (Process.t, unit) runnable

(** [kill] is an alias for [docker kill]. *)
val kill : string -> (Process.t, unit) runnable

(** [rm] is an alias for [docker rm]. *)
val rm : string -> (Process.t, unit) runnable

(** [cp] is an alias for [docker cp]. *)
val cp :
  string ->
  kind:[`From_host | `To_host] ->
  source:string ->
  destination:string ->
  (Process.t, unit) runnable
