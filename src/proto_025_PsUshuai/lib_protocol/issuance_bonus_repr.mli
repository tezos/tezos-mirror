(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Issuance bonus.

   The issuance bonus is a rational but is stored as fixed point integer
   to limit the serialized size as with Q we would have no control on the size
   each component (numerator, denominator).

    It is expected to always be between 0 and the protocol's parametric
    constant [max_bonus].
    The int64 encoding of the bonus is made such that the approximation
    resulting of this encoding is negligible when the bonus is used in a
    context where the total supply of the network is in the order of magnitude
    of 2^50 mutez (10^15 mutez)
*)

(** An issuance bonus is a rational between zero and some [max_bonus]. *)
type t = private Q.t

(** A [max_bonus] is a value between zero and one. *)
type max_bonus = private t

val zero : t

val encoding : t Data_encoding.t

val max_bonus_encoding : max_bonus Data_encoding.t

type error += Out_of_bounds_bonus

(** Getting a bonus out of rational.
    It will fail if the decoding doesn't provide a value that is valid wrt
    protocol's parametric constants
  *)
val of_Q : max_bonus:max_bonus -> Q.t -> t tzresult

(** Use only to define the [max_bonus] parameter from [Default_parameters]. *)
val max_bonus_parameter_of_Q_exn : Q.t -> max_bonus
