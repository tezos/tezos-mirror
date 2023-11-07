(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Adding and removing stake can be done from/toward a delegate, one
   of its staker, or all its stakers at
   once. We need to distinguish these cases to enforce the staking
   over baking limit. *)
type t = private
  | Baker of Signature.public_key_hash  (** The baker itself. *)
  | Single_staker of {
      staker : Contract_repr.t;
      delegate : Signature.public_key_hash;
    }  (** A single staker, cannot be the delegate. *)
  | Shared_between_stakers of {delegate : Signature.public_key_hash}
      (** The delegate's stakers simultaneously (delegate excluded). *)

val baker : Signature.public_key_hash -> t

val single_staker :
  staker:Contract_repr.t -> delegate:Signature.public_key_hash -> t

val shared_between_stakers : delegate:Signature.public_key_hash -> t

val encoding : t Data_encoding.t

val compare : t -> t -> int
