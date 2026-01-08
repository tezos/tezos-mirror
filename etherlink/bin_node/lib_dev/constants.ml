(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

let supported_networks = Configuration.[Mainnet; Testnet; Shadownet]

let network_name = function
  | Configuration.Mainnet -> "Mainnet"
  | Testnet -> "Testnet"
  | Shadownet -> "Shadownet"

let rollup_address network =
  Tezos_crypto.Hashed.Smart_rollup_address.of_b58check_exn
  @@
  match network with
  | Configuration.Mainnet -> "sr1Ghq66tYK9y3r8CC1Tf8i8m5nxh8nTvZEf"
  | Testnet -> "sr18wx6ezkeRjt1SZSeZ2UQzQN3Uc3YLMLqg"
  | Shadownet -> "sr19fMYrr5C4qqvQqQrDSjtP31GcrWjodzvg"

let network_of_address addr =
  match Tezos_crypto.Hashed.Smart_rollup_address.to_b58check addr with
  | "sr1Ghq66tYK9y3r8CC1Tf8i8m5nxh8nTvZEf" -> Some Configuration.Mainnet
  | "sr18wx6ezkeRjt1SZSeZ2UQzQN3Uc3YLMLqg" -> Some Testnet
  | "sr19fMYrr5C4qqvQqQrDSjtP31GcrWjodzvg" -> Some Shadownet
  | _ -> None

type kernel =
  | Bifrost
  | Calypso
  | Calypso2
  | Dionysus
  | DionysusR1
  | Ebisu
  | Farfadet

let kernel_from_string = function
  | "bifrost" -> Some Bifrost
  | "calypso" -> Some Calypso
  | "calypso2" -> Some Calypso2
  | "dionysus" -> Some Dionysus
  | "dionysus-r1" -> Some DionysusR1
  | "ebisu" -> Some Ebisu
  | "farfadet" -> Some Farfadet
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
  | DionysusR1 ->
      `Hex "0001010d789e7cccc25c785cf73a658574ed0995ef36b8416a46ab0ddc6b058b39"
  | Ebisu ->
      `Hex "00fea18ffecd0563f942b8b4c67911302754d7e505b5b5672ff03cb927b79ba830"
  | Farfadet ->
      `Hex "0079e0f348b608ce486c9e5e1fdf84b650019922bf3383b562522c2c8f60a098da"
