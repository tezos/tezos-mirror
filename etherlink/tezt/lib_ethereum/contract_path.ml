(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let exchanger_path () =
  Base.(project_root // "etherlink/kernel_evm/l1_bridge/exchanger.tz")

let bridge_path () =
  Base.(project_root // "etherlink/kernel_evm/l1_bridge/evm_bridge.tz")

let admin_path () =
  Base.(project_root // "etherlink/kernel_evm/l1_bridge/admin.tz")

let withdrawal_abi_path () =
  Base.(project_root // "etherlink/kernel_evm/l1_bridge/withdrawal.abi")

let delayed_path () =
  Base.(
    project_root
    // "etherlink/kernel_evm/l1_bridge/delayed_transaction_bridge.tz")
