(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025-2026 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

let supported_networks = Configuration.[Mainnet; Testnet; Shadownet; Previewnet]

let network_name = function
  | Configuration.Mainnet -> "Mainnet"
  | Testnet -> "Testnet"
  | Shadownet -> "Shadownet"
  | Previewnet -> "Previewnet"

let rollup_address network =
  Tezos_crypto.Hashed.Smart_rollup_address.of_b58check_exn
  @@
  match network with
  | Configuration.Mainnet -> "sr1Ghq66tYK9y3r8CC1Tf8i8m5nxh8nTvZEf"
  | Testnet -> "sr18wx6ezkeRjt1SZSeZ2UQzQN3Uc3YLMLqg"
  | Shadownet -> "sr19fMYrr5C4qqvQqQrDSjtP31GcrWjodzvg"
  | Previewnet -> "sr1TCYofXUuJjmQvZ26XE4YAwXdfetQfZ6rR"

let network_of_address addr =
  match Tezos_crypto.Hashed.Smart_rollup_address.to_b58check addr with
  | "sr1Ghq66tYK9y3r8CC1Tf8i8m5nxh8nTvZEf" -> Some Configuration.Mainnet
  | "sr18wx6ezkeRjt1SZSeZ2UQzQN3Uc3YLMLqg" -> Some Testnet
  | "sr19fMYrr5C4qqvQqQrDSjtP31GcrWjodzvg" -> Some Shadownet
  | "sr1TCYofXUuJjmQvZ26XE4YAwXdfetQfZ6rR" -> Some Previewnet
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
  | FarfadetR3
  | FarfadetR4
  | FarfadetR5
  | FarfadetR6
  | Previewnet02
  | Previewnet04
  | Previewnet05
  | Previewnet06
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
  | "farfadet-r3" -> Some FarfadetR3
  | "farfadet-r4" -> Some FarfadetR4
  | "farfadet-r5" -> Some FarfadetR5
  | "farfadet-r6" -> Some FarfadetR6
  | "previewnet-0.2" -> Some Previewnet02
  | "previewnet-0.4" -> Some Previewnet04
  | "previewnet-0.5" -> Some Previewnet05
  | "previewnet-0.6" -> Some Previewnet06
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
  | FarfadetR3 -> 11
  | FarfadetR4 -> 12
  | FarfadetR5 -> 13
  | FarfadetR6 -> 14
  | Previewnet02 -> 15
  | Previewnet04 -> 16
  | Previewnet05 -> 17
  | Previewnet06 -> 18
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
           "00a932181ea0b3446ec1d509c33680a473f133bd1aa92d144d2011fe9fd1e2787f")
  | FarfadetR3 ->
      Some
        (`Hex
           "0005d2c53f57df68b2027ecf592169cf8ce0ee7b3a6ecc215d58e42733c6eed131")
  | FarfadetR4 ->
      Some
        (`Hex
           "00625d22abf10a520cae5489b7e19df70219a150d336ee6dc0a8eb4c21eca43c1b")
  | FarfadetR5 ->
      Some
        (`Hex
           "007c73209bc68c2e0099e105b92ef4c674387532afbf5d51b7f1043472f9d65e9b")
  | FarfadetR6 ->
      Some
        (`Hex
           "0083d8142e9c5f2a35ead6eb31d6344f3803f90eacb03ccfb6c482df353f85908a")
  | Previewnet02 | Previewnet04 | Previewnet05 | Previewnet06 | Latest -> None
