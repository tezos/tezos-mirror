(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

module Dal_share_hash = struct
  module H =
    Tezos_crypto.Blake2B.Make
      (Tezos_crypto.Base58)
      (struct
        let name = "pkh_and_dal_share_hash"

        let title = "A hash of a pkh and a DAL share"

        let b58check_prefix = "\077\167\043" (* shh(53) for "share hash" *)

        let size = Some 32
      end)

  include H
end

let two_to_hash_size = Z.(shift_left one Dal_share_hash.size)

(* This function checks that `hash(delegate . share) < trap_rate * 2^|hash|`,
   where the dot denotes concatenation and |v| the length of the bitstring v. *)
let share_is_trap delegate share ~(traps_fraction : Q.t) =
  let open Error_monad.Result_syntax in
  let* pkh_bytes =
    Data_encoding.Binary.to_bytes
      Tezos_crypto.Signature.Public_key_hash.encoding
      delegate
  in
  let+ share_bytes =
    Data_encoding.Binary.to_bytes Cryptobox.share_encoding share
  in
  let hash =
    Dal_share_hash.(hash_bytes [pkh_bytes; share_bytes] |> to_bytes)
    |> Bytes.to_string |> Z.of_bits
  in
  let threshold =
    two_to_hash_size |> Q.of_bigint |> Q.mul traps_fraction |> Q.to_bigint
  in
  Z.leq hash threshold
