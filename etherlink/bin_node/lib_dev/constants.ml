(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025-2026 Functori <contact@functori.com>                   *)
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
  | Mainnet_beta
  | Mainnet_gamma
  | Bifrost
  | Calypso
  | Calypso2
  | Dionysus
  | DionysusR1
  | Ebisu
  | Farfadet
  | FarfadetR1
  | FarfadetR2
  | Latest

let kernel_from_string = function
  | "mainnet-beta" -> Some Mainnet_beta
  | "mainnet-gamma" -> Some Mainnet_gamma
  | "bifrost" -> Some Bifrost
  | "calypso" -> Some Calypso
  | "calypso2" -> Some Calypso2
  | "dionysus" -> Some Dionysus
  | "dionysus-r1" -> Some DionysusR1
  | "ebisu" -> Some Ebisu
  | "farfadet" -> Some Farfadet
  | "farfadet-r1" -> Some FarfadetR1
  | "farfadet-r2" -> Some FarfadetR2
  | "latest" -> Some Latest
  | _ -> None

let kernel_to_ordinal = function
  | Mainnet_beta -> 0
  | Mainnet_gamma -> 1
  | Bifrost -> 2
  | Calypso -> 3
  | Calypso2 -> 4
  | Dionysus -> 5
  | DionysusR1 -> 6
  | Ebisu -> 7
  | Farfadet -> 8
  | FarfadetR1 -> 9
  | FarfadetR2 -> 10
  | Latest -> Int.max_int

let compare_kernel a b = Int.compare (kernel_to_ordinal a) (kernel_to_ordinal b)

let kernel_is_older a ~than:b = compare_kernel a b < 0

let kernel_is_newer a ~than:b = compare_kernel a b > 0

let root_hash_from_released_kernel = function
  | Mainnet_beta ->
      Some
        (`Hex
           "007dda075160255c2671224d7517ca377945ec29a1d1b060c4c3658f0e59b7abb2")
  | Mainnet_gamma ->
      Some
        (`Hex
           "00e58c94bd5a793b09e8b127a1fcf4958c72ca6f86078a78760b41415fb3a7801d")
  | Bifrost ->
      Some
        (`Hex
           "00fda6968ec17ed11dee02dc91d15606e6f02c8d7e00d8baeaee24fc0188898261")
  | Calypso ->
      Some
        (`Hex
           "00224058a50dbf4c0b5f6d5e4ee672cd63d0911959b335e587b4112a7eea7b2323")
  | Calypso2 ->
      Some
        (`Hex
           "008ce0e105f0f1446d78430badcc83aa5672c66bf0bc4fb51962cb765c80e8a60e")
  | Dionysus ->
      Some
        (`Hex
           "0008105ea6fb0e4331d7bbc93f0e8843ae91eeb235741054cb2b345ac2d19b9ec9")
  | DionysusR1 ->
      Some
        (`Hex
           "0001010d789e7cccc25c785cf73a658574ed0995ef36b8416a46ab0ddc6b058b39")
  | Ebisu ->
      Some
        (`Hex
           "00fea18ffecd0563f942b8b4c67911302754d7e505b5b5672ff03cb927b79ba830")
  | Farfadet ->
      Some
        (`Hex
           "0079e0f348b608ce486c9e5e1fdf84b650019922bf3383b562522c2c8f60a098da")
  | FarfadetR1 ->
      Some
        (`Hex
           "0056aea7f98b2bc4d18edb450b2f098f6e95e5356f30a1fac2b50080f3e482bad1")
  | FarfadetR2 ->
      Some
        (`Hex
           "0031c5d2a4b555d151cc521a42500f61db12898cd4f7178aac42541b0c81a82e74")
  | Latest -> None
