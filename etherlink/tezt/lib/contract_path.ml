(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let exchanger_path () =
  Base.(project_root // "etherlink/tezos_contracts/exchanger.tz")

let bridge_path () =
  Base.(project_root // "etherlink/tezos_contracts/evm_bridge.tz")

let admin_path () = Base.(project_root // "etherlink/tezos_contracts/admin.tz")

let withdrawal_abi_path () =
  Base.(project_root // "etherlink/tezos_contracts/withdrawal.abi")

let fa_withdrawal_abi_path () =
  Base.(project_root // "etherlink/tezos_contracts/fa_withdrawal.abi")

let delayed_path () =
  Base.(
    project_root
    // "etherlink/tezos_contracts/chunked_delayed_transaction_bridge.tz")

let ticket_router_tester_path () =
  Base.(
    project_root
    // "etherlink/tezos_contracts/fa_bridge/ticket_router_tester.tz")
