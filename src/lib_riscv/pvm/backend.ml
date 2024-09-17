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

type reveal_data = Api.reveal_data

type input = Api.input

(* The kernel debug logging function (`string -> unit Lwt.t`) passed by the node
 * to [compute_step] and [compute_step_many] cannot be passed directly
 * to the Rust backend, which expects a `u8 -> ()` function and cannot run Lwt
 * computations. We also don't want to call it on every character output by the
 * kernel debug log
 * (TODO: https://linear.app/tezos/issue/RV-154/use-line-buffering-for-kernel-output)
 * We instead pass a closure which accumulates the log in an
 * extensible buffer and, once the Rust function returns, pass the buffer
 * to the kernel debug logging function. *)
let with_hooks printer f =
  let open Lwt_syntax in
  let debug_log = Buffer.create 1024 in
  let res = f (fun c -> Buffer.add_char debug_log (Char.chr c)) in
  let* () = printer (Buffer.contents debug_log) in
  return res

let compute_step_many ?reveal_builtins:_ ?write_debug ?stop_at_snapshot:_
    ~max_steps state =
  match write_debug with
  | None -> Lwt.return (Api.octez_riscv_compute_step_many max_steps state)
  | Some printer ->
      with_hooks
        printer
        (Api.octez_riscv_compute_step_many_with_debug max_steps state)

let compute_step state = Lwt.return (Api.octez_riscv_compute_step state)

let compute_step_with_debug ?write_debug state =
  match write_debug with
  | None -> Lwt.return (Api.octez_riscv_compute_step state)
  | Some printer ->
      with_hooks printer (Api.octez_riscv_compute_step_with_debug state)

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

let set_input state input = Lwt.return (Api.octez_riscv_set_input state input)
