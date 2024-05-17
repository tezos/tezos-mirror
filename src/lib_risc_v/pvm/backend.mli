(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type reveals

type write_debug = string -> unit Lwt.t

type input_info

type state = Storage.tree

val compute_step_many :
  ?reveal_builtins:reveals ->
  ?write_debug:write_debug ->
  ?stop_at_snapshot:bool ->
  max_steps:int64 ->
  state ->
  (state * int64) Lwt.t

val compute_step : state -> state Lwt.t

val compute_step_with_debug : ?write_debug:write_debug -> state -> state Lwt.t

val get_tick : state -> Z.t Lwt.t

type status

val get_status : state -> status Lwt.t

val string_of_status : status -> string
