(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** [share_is_trap pkh share ~traps_fraction] determines whether the given
    [share] is classified as a "trap" for the delegate identified by [pkh],
    based on the fraction [traps_fraction] of shards that should be traps.

    The function computes the hash of the concatenation of [pkh] and [share],
    denoted as `hash(pkh . share)`, where the dot represents concatenation.  It
    then checks if this hash value is less than `traps_fraction * 2^n`, where
    `n` is the (fixed) bit size of the hash.

    The function returns:

    - [Ok true] if the share is considered a trap for the given [pkh].

    - [Ok false] if the share is not a trap.

    - [Error write_error] if there is an issue encoding [pkh] or [share].

    This function assumes [traps_fraction] is valid (i.e., a rational number
    within [0, 1]).
*)
val share_is_trap :
  Tezos_crypto.Signature.Public_key_hash.t ->
  Cryptobox.share ->
  traps_fraction:Q.t ->
  (bool, Data_encoding.Binary.write_error) result
