(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type reveals

type write_debug = string -> unit Lwt.t

type input_info

type state = Storage.State.t

type status = Octez_riscv_api.status

type reveal_data = Octez_riscv_api.reveal_data

type input = Octez_riscv_api.input

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

val get_status : state -> status Lwt.t

val get_message_counter : state -> int64 Lwt.t

val string_of_status : status -> string

val install_boot_sector : state -> string -> state Lwt.t

val get_current_level : state -> int32 option Lwt.t

val state_hash : state -> bytes

val set_input : state -> input -> state Lwt.t
