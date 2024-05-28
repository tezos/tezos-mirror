(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Api = Octez_riscv_api

type reveals = unit

type write_debug = string -> unit Lwt.t

type input_info

type state = Storage.State.t

type status = Api.status

let compute_step_many ?reveal_builtins:_ ?write_debug:_ ?stop_at_snapshot:_
    ~max_steps state =
  Lwt.return (Api.octez_riscv_compute_step_many max_steps state)

let compute_step state = Lwt.return (Api.octez_riscv_compute_step state)

let compute_step_with_debug ?write_debug:_ state =
  Lwt.return (Api.octez_riscv_compute_step state)

let get_tick state = Lwt.return (Z.of_int64 (Api.octez_riscv_get_tick state))

let get_status state = Lwt.return (Api.octez_riscv_get_status state)

let string_of_status status = Api.octez_riscv_string_of_status status
