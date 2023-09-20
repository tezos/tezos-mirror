(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Adding and removing stake can be done from/toward a delegate, one
   of its staker, or both the delegate and all its stakers at
   once. We need to distinguish these cases to enforce the staking
   over baking limit. *)
type staker =
  | Single of Contract_repr.t * Signature.public_key_hash
      (** A single staker, either the delegate itself or one of its
       staker. *)
  | Shared of Signature.public_key_hash
      (** The delegate and all its stakers simultaneously. *)

val staker_encoding : staker Data_encoding.t

val compare_staker : staker -> staker -> int

val staker_delegate : staker -> Signature.public_key_hash
