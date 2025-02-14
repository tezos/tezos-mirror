(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let supported_networks = Configuration.[Mainnet; Testnet]

let network_name = function
  | Configuration.Mainnet -> "Mainnet"
  | Testnet -> "Testnet"

let rollup_address network =
  Tezos_crypto.Hashed.Smart_rollup_address.of_b58check_exn
  @@
  match network with
  | Configuration.Mainnet -> "sr1Ghq66tYK9y3r8CC1Tf8i8m5nxh8nTvZEf"
  | Testnet -> "sr18wx6ezkeRjt1SZSeZ2UQzQN3Uc3YLMLqg"

let latest_snapshot_url = function
  | Configuration.Mainnet ->
      "https://snapshotter-sandbox.nomadic-labs.eu/etherlink-mainnet/evm-snapshot-sr1Ghq66tYK9y-latest.gz"
  | Testnet ->
      "https://snapshotter-sandbox.nomadic-labs.eu/etherlink-ghostnet/evm-snapshot-sr18wx6ezkeRj-latest.gz"
