(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type reveals = unit

type write_debug = string -> unit Lwt.t

type input_info

type state = Storage.State.t

let compute_step_many ?reveal_builtins:_ ?write_debug:_ ?stop_at_snapshot:_
    ~max_steps:_ state =
  Lwt.return (state, 0L)

let compute_step = Lwt.return

let compute_step_with_debug ?write_debug:_ = Lwt.return

let get_tick _state = Lwt.return (Z.of_int 0)

type status = Riscv_dummy_status

let get_status _state = Lwt.return Riscv_dummy_status

let string_of_status _status = "riscv_dummy_status"
