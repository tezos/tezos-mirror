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

let network_of_address addr =
  match Tezos_crypto.Hashed.Smart_rollup_address.to_b58check addr with
  | "sr1Ghq66tYK9y3r8CC1Tf8i8m5nxh8nTvZEf" -> Some Configuration.Mainnet
  | "sr18wx6ezkeRjt1SZSeZ2UQzQN3Uc3YLMLqg" -> Some Testnet
  | _ -> None

type kernel = Bifrost | Calypso | Calypso2 | Dionysus

let kernel_from_string = function
  | "bifrost" -> Some Bifrost
  | "calypso" -> Some Calypso
  | "calypso2" -> Some Calypso2
  | "dionysus" -> Some Dionysus
  | _ -> None

let root_hash_from_kernel = function
  | Bifrost ->
      `Hex "00fda6968ec17ed11dee02dc91d15606e6f02c8d7e00d8baeaee24fc0188898261"
  | Calypso ->
      `Hex "00224058a50dbf4c0b5f6d5e4ee672cd63d0911959b335e587b4112a7eea7b2323"
  | Calypso2 ->
      `Hex "008ce0e105f0f1446d78430badcc83aa5672c66bf0bc4fb51962cb765c80e8a60e"
  | Dionysus ->
      `Hex "0008105ea6fb0e4331d7bbc93f0e8843ae91eeb235741054cb2b345ac2d19b9ec9"
