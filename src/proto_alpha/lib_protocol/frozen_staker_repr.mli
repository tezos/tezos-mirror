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
type t = private
  | Baker of Signature.public_key_hash  (** The baker itself. *)
  | Single of {staker : Contract_repr.t; delegate : Signature.public_key_hash}
      (** A single staker, cannot be the delegate. *)
  | Shared of {delegate : Signature.public_key_hash}
      (** The delegate and all its stakers simultaneously. *)

val baker : Signature.public_key_hash -> t

val single : staker:Contract_repr.t -> delegate:Signature.public_key_hash -> t

val shared : delegate:Signature.public_key_hash -> t

val encoding : t Data_encoding.t

val compare : t -> t -> int
