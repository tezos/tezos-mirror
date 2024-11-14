(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Wrapper module for docker commands. By default, [tag] is ["latest"]. *)

(** [build] is an alias for [docker build]. *)
val build :
  ?image_name:string ->
  ?alias:string ->
  ?tag:string ->
  ?dockerfile:string ->
  args:(string * string) list ->
  unit ->
  Process.t

(** [tag] is an alias for [docker tag]. *)
val tag :
  ?image_name:string ->
  ?alias:string ->
  ?tag:string ->
  registry_uri:string ->
  unit ->
  Process.t

(** [push] is an alias for [docker push]. *)
val push :
  ?image_name:string ->
  ?alias:string ->
  ?tag:string ->
  registry_uri:string ->
  unit ->
  Process.t

(** [pull] is an alias for [docker pull]. *)
val pull :
  ?image_name:string ->
  ?alias:string ->
  ?tag:string ->
  registry_uri:string ->
  unit ->
  Process.t

(** [run] is an alias for [docker run]. *)
val run :
  ?rm:bool ->
  ?name:string ->
  ?network:string ->
  ?publish_ports:string * string * string * string ->
  string ->
  string list ->
  Process.t

(** [kill] is an alias for [docker kill]. *)
val kill : string -> Process.t

(** [rm] is an alias for [docker rm]. *)
val rm : string -> Process.t

(** [cp] is an alias for [docker cp]. *)
val cp :
  string ->
  kind:[`From_host | `To_host] ->
  source:string ->
  destination:string ->
  Process.t
