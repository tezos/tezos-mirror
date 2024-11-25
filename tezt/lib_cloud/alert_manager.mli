(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Prometheus alert. *)
type alert

(** [alert name ~expr ~severity ?for_ ?description ?summary] creates
    an alert with:
   - [name]: alert identifier.
   - [expr]: promQL expression that triggers the alert.
   - [severity]: alert level (either [`Critical], [`Warning], [`Info]
     or [`None]).
   - [for_]: optional duration before firing.
   - [description]: pptional detailed explanation.
   - [summary]: optional brief overview. *)
val alert :
  expr:string ->
  severity:[`Critical | `Info | `None | `Warning] ->
  ?for_:string ->
  ?description:string ->
  ?summary:string ->
  string ->
  alert

(** [name_of_alert alert] returns the name associated with the
    [alert]. *)
val name_of_alert : alert -> string

(** Jingoo alert template. *)
val alert_template : alert -> Jingoo.Jg_types.tvalue

type t

val run : configuration_files:string list -> t Lwt.t

val shutdown : t -> unit Lwt.t
