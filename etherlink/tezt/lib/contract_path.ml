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

let fast_withdrawal_path () =
  Base.(project_root // "etherlink/tezos_contracts/fast_withdrawal_mockup.tz")

let fast_withdrawal_abi_path () =
  Base.(project_root // "etherlink/tezos_contracts/fast_withdrawal.abi")

let fa_withdrawal_abi_path () =
  Base.(project_root // "etherlink/tezos_contracts/fa_withdrawal.abi")

let service_provider_path () =
  Base.(project_root // "etherlink/tezos_contracts/service_provider.tz")

let delayed_path ~kernel:_ =
  (* The path to the delayed transaction bridge depends on the version
     of the kernel. The versions which don't support chunking must use
     "delayed_transaction_bridge.tz", the versions which do support
     chunking must use "chunked_delayed_transaction_bridge.tz" *)
  let contract_basename = "chunked_delayed_transaction_bridge" in
  Base.(project_root // sf "etherlink/tezos_contracts/%s.tz" contract_basename)

let ticket_router_tester_path () =
  Base.(
    project_root
    // "etherlink/tezos_contracts/fa_bridge/ticket_router_tester.tz")

let fa_deposit_path () =
  Base.(project_root // "etherlink/tezos_contracts/fa_deposit.tz")
