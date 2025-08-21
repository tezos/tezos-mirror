(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(* [share_is_trap pkh share ~traps_fraction] checks that
     `hash(pkh . share) < traps_fraction * 2^hash_size`,
   where the dot denotes concatenation and |v| the length of the bitstring v. *)
let share_is_trap =
  let hash_size_bits = 8 * Tezos_crypto.Blake2B.size in
  let two_to_hash_size = Z.(shift_left one hash_size_bits) in
  fun pkh share ~traps_fraction ->
    let open Error_monad.Result_syntax in
    let* pkh_bytes =
      Data_encoding.Binary.to_bytes
        Tezos_crypto.Signature.Public_key_hash.encoding
        pkh
    in
    let+ share_bytes =
      Data_encoding.Binary.to_bytes Cryptobox.share_encoding share
    in
    let hash =
      Tezos_crypto.Blake2B.(hash_bytes [pkh_bytes; share_bytes] |> to_string)
      |> Z.of_bits
    in
    let threshold =
      two_to_hash_size |> Q.of_bigint |> Q.mul traps_fraction |> Q.to_bigint
    in
    Z.leq hash threshold
