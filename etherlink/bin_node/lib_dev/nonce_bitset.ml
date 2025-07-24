(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [Nonce_bitset] registers known nonces from transactions that went
    through the tx_queue from a specific sender address. With this
    structure it's easy to do bookkeeping of address' nonce without
    going through all the transactions of the queue.

    The invariants are that for any nonce_bitset [nb]:

    - When creating [nb], [nb.next_nonce] is the next valid nonce for
    the state.

    - When adding a nonce [n] to [nb], [n] must be superior or equal
    to [nb.next_nonce]. This is enforced by the validation in
    {!Validate.is_tx_valid}.

    - [nb.next_nonce] can only increase over time. This is enforced by
    [shift] and [offset].
 *)
module Bitset = Tezos_base.Bitset

(** [t] allows to register for a given address all nonces that are
      currently used by transaction in the tx_queue. *)
type t = {
  next_nonce : Z.t;
      (** [next_nonce] is the base value for any position found in
            {!field:bitset}. It’s set to be the next expected nonce
            for a given address, which is the nonce found in the
            backend. *)
  bitset : Bitset.t;
}

(** [create ~next_nonce] creates a {!t} struct with empty [bitset]. *)
let create ~next_nonce = {next_nonce; bitset = Bitset.empty}

(** [offset ~nonce1 ~nonce2] computes the difference between
      [nonce1] and [nonce2].

      Fails if [nonce2 > nonce1] or if the difference between the two is
      more than {!Int.max_int}. *)
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

(** [add bitset_nonce ~nonce] adds the nonce [nonce] to [bitset_nonce]. *)
let add {next_nonce; bitset} ~nonce =
  let open Result_syntax in
  let* offset_position = offset ~nonce1:nonce ~nonce2:next_nonce in
  let* bitset = Bitset.add bitset offset_position in
  return {next_nonce; bitset}

(** [remove bitset_nonce ~nonce] removes the nonce [nonce] from
      [bitset_nonce].

      If [nonce] is strictly inferior to [bitset_nonce.next_nonce] then
      it's a no-op because nonce can't exist in the bitset. *)
let remove {next_nonce; bitset} ~nonce =
  let open Result_syntax in
  if Z.lt nonce next_nonce then
    (* no need to remove a nonce that can't exist in the bitset *)
    return {next_nonce; bitset}
  else
    let* offset_position = offset ~nonce1:nonce ~nonce2:next_nonce in
    let* bitset = Bitset.remove bitset offset_position in
    return {next_nonce; bitset}

(** [shift bitset_nonce ~nonce] shifts the bitset of [bitset_nonce]
      so the next_nonce is now [nonce]. Shifting the bitset means
      that nonces that are inferior to [nonce] are dropped.

      Fails if [nonce] is strictly inferior to
      [bitset_nonce.next_nonce]. *)
let shift {next_nonce; bitset} ~nonce =
  let open Result_syntax in
  let* offset = offset ~nonce1:nonce ~nonce2:next_nonce in
  let* bitset = Bitset.shift_right bitset ~offset in
  return {next_nonce = nonce; bitset}

(** [is_empty bitset_nonce] checks if the bitset is empty, i.e. no
      position is at 1. *)
let is_empty {bitset; _} = Bitset.is_empty bitset

(** [next_gap bitset_nonce] returns the next available nonce. *)
let next_gap {next_nonce; bitset} =
  let offset_position = Z.(trailing_zeros @@ lognot @@ Bitset.to_z bitset) in
  Z.(next_nonce + of_int offset_position)

(** [shift_then_next_gap bitset_nonce ~shift_nonce] calls {!shift
      ~nonce:shift_nonce} then {!next_gap bitset_nonce}. *)
let shift_then_next_gap bitset_nonce ~shift_nonce =
  let open Result_syntax in
  let* bitset_nonce = shift bitset_nonce ~nonce:shift_nonce in
  return @@ next_gap bitset_nonce
