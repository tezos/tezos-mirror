(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Bitset = Tezos_base.Bitset

type t = {next_nonce : Z.t; bitset : Bitset.t}

let create ~next_nonce = {next_nonce; bitset = Bitset.empty}

let offset ~nonce1 ~nonce2 =
  let open Result_syntax in
  if Z.gt nonce2 nonce1 then
    error_with
      "Internal: invalid nonce diff. nonce2 %a must be inferior or equal to \
       nonce1 %a."
      Z.pp_print
      nonce2
      Z.pp_print
      nonce1
  else
    let offset = Z.(nonce1 - nonce2) in
    if Z.fits_int offset then return (Z.to_int offset)
    else
      error_with
        "Internal: invalid nonce offset, it's too large to fit in an integer."

let add {next_nonce; bitset} ~nonce =
  let open Result_syntax in
  if Z.lt nonce next_nonce then return {next_nonce; bitset}
  else
    let* offset_position = offset ~nonce1:nonce ~nonce2:next_nonce in
    let* bitset = Bitset.add bitset offset_position in
    return {next_nonce; bitset}

let add_many {next_nonce; bitset} ~nonce ~length =
  let open Result_syntax in
  if Z.lt nonce next_nonce then return {next_nonce; bitset}
  else
    let* offset_position = offset ~nonce1:nonce ~nonce2:next_nonce in
    let* bitset = Bitset.add_many bitset offset_position length in
    return {next_nonce; bitset}

let remove {next_nonce; bitset} ~nonce =
  let open Result_syntax in
  if Z.lt nonce next_nonce then
    (* no need to remove a nonce that can't exist in the bitset *)
    return {next_nonce; bitset}
  else
    let* offset_position = offset ~nonce1:nonce ~nonce2:next_nonce in
    let* bitset = Bitset.remove bitset offset_position in
    return {next_nonce; bitset}

let remove_many {next_nonce; bitset} ~nonce ~length =
  let open Result_syntax in
  if Z.lt nonce next_nonce then
    (* we don't remove the nonces if they can't all exist in the bitset *)
    return {next_nonce; bitset}
  else
    let* offset_position = offset ~nonce1:nonce ~nonce2:next_nonce in
    let* bitset = Bitset.remove_many bitset offset_position length in
    return {next_nonce; bitset}

let shift {next_nonce; bitset} ~nonce =
  let open Result_syntax in
  let* offset = offset ~nonce1:nonce ~nonce2:next_nonce in
  let* bitset = Bitset.shift_right bitset ~offset in
  return {next_nonce = nonce; bitset}

let is_empty {bitset; _} = Bitset.is_empty bitset

let next_gap {next_nonce; bitset} =
  let offset_position = Z.(trailing_zeros @@ lognot @@ Bitset.to_z bitset) in
  Z.(next_nonce + of_int offset_position)

let shift_then_next_gap bitset_nonce ~shift_nonce =
  let open Result_syntax in
  let* bitset_nonce = shift bitset_nonce ~nonce:shift_nonce in
  return @@ next_gap bitset_nonce
