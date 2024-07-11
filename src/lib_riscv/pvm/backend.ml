(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Api = Octez_riscv_api

type reveals = unit

type write_debug = string -> unit Lwt.t

type input_info

type state = Storage.State.t

type status = Api.status

let pvm_hooks = Api.octez_riscv_default_pvm_hooks ()

let compute_step_many ?reveal_builtins:_ ?write_debug:_ ?stop_at_snapshot:_
    ~max_steps state =
  Lwt.return (Api.octez_riscv_compute_step_many max_steps state pvm_hooks)

let compute_step state =
  Lwt.return (Api.octez_riscv_compute_step state pvm_hooks)

let compute_step_with_debug ?write_debug:_ state =
  Lwt.return (Api.octez_riscv_compute_step state pvm_hooks)

let get_tick state = Lwt.return (Z.of_int64 (Api.octez_riscv_get_tick state))

let get_status state = Lwt.return (Api.octez_riscv_get_status state)

let get_message_counter state =
  Lwt.return (Api.octez_riscv_get_message_counter state)

let string_of_status status = Api.octez_riscv_string_of_status status

let install_boot_sector state boot_sector =
  Lwt.return
    (Api.octez_riscv_install_boot_sector state (Bytes.of_string boot_sector))

let get_current_level state = Lwt.return (Api.octez_riscv_get_level state)

let state_hash state = Api.octez_riscv_state_hash state

let set_input state level message_counter payload =
  Lwt.return
    (Api.octez_riscv_set_input_message
       state
       level
       message_counter
       (Bytes.of_string payload))

let set_metadata state address origination_level =
  Lwt.return (Api.octez_riscv_set_metadata state address origination_level)
