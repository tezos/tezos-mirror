(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Issuance bonus.

   The issuance bonus is a rationnal but is stored as fixed point integer
   to limit the serialized size as with Q we would have no control on the size
   each component (numerator, denominator).

    It is expected to always be between 0 and the protocol's parametric
    constant max_bonus.
    The int64 encoding of the bonus is made such that the approximation
    resulting of this encoding is negligeable when the bonus is used in a
    context where the total supply of the network is in the order of magnitude
    of 2^50 mutez (10^15 mutez)
*)
type t = private Q.t

val zero : t

val encoding : t Data_encoding.t

(** Getting a bonus out of rationnal.
    It will fail if the decoding doesn't provide a value that is valid wrt
    protocol's parametric constants
  *)
val of_Q :
  constants:Constants_parametric_repr.adaptive_rewards_params ->
  Q.t ->
  t tzresult

val migrate_max_bonus_from_O_to_P : int64 -> Q.t
