(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@tezos.com>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Functions for sampling (pk,pkh,sk) triplets.

    This module provides an implementation for finite pools of random
    (pk,pkh,sk) triplets. The pools grows to the target [size] as the user
    samples from it. When the target size is reached, sampling is performed
    in a round-robin fashion.
 *)

module type Param_S = sig
  type algo

  (** Maximal size of the key pool. *)
  val size : int

  (** Algorithm to use for triplet generation. *)
  val algo : [`Algo of algo | `Default]
end

module type P_Finite_key_pool_S = sig
  type public_key_hash

  type public_key

  type secret_key

  (** Sample a public key from the pool. *)
  val pk : public_key Base_samplers.sampler

  (** Sample a public key hash from the pool. *)
  val pkh : public_key_hash Base_samplers.sampler

  (** Sample a secret key from the pool. *)
  val sk : secret_key Base_samplers.sampler

  (** Sample a (pkh, pk, sk) triplet from the pool. *)
  val all : (public_key_hash * public_key * secret_key) Base_samplers.sampler
end

module V0 : sig
  module type Finite_key_pool_S =
    P_Finite_key_pool_S
      with type public_key_hash := Tezos_crypto.Signature.V0.Public_key_hash.t
       and type public_key := Tezos_crypto.Signature.V0.Public_key.t
       and type secret_key := Tezos_crypto.Signature.V0.Secret_key.t

  (** Create a finite key pool. *)
  module Make_finite_key_pool
      (Arg : Param_S with type algo := Tezos_crypto.Signature.V0.algo) :
    Finite_key_pool_S
end

module V1 : sig
  module type Finite_key_pool_S =
    P_Finite_key_pool_S
      with type public_key_hash := Tezos_crypto.Signature.V1.Public_key_hash.t
       and type public_key := Tezos_crypto.Signature.V1.Public_key.t
       and type secret_key := Tezos_crypto.Signature.V1.Secret_key.t

  (** Create a finite key pool. *)
  module Make_finite_key_pool
      (Arg : Param_S with type algo := Tezos_crypto.Signature.V1.algo) :
    Finite_key_pool_S
end

module V2 : sig
  module type Finite_key_pool_S =
    P_Finite_key_pool_S
      with type public_key_hash := Tezos_crypto.Signature.V2.Public_key_hash.t
       and type public_key := Tezos_crypto.Signature.V2.Public_key.t
       and type secret_key := Tezos_crypto.Signature.V2.Secret_key.t

  (** Create a finite key pool. *)
  module Make_finite_key_pool
      (Arg : Param_S with type algo := Tezos_crypto.Signature.V2.algo) :
    Finite_key_pool_S
end

module V3 : sig
  module type Finite_key_pool_S =
    P_Finite_key_pool_S
      with type public_key_hash := Tezos_crypto.Signature.V3.Public_key_hash.t
       and type public_key := Tezos_crypto.Signature.V3.Public_key.t
       and type secret_key := Tezos_crypto.Signature.V3.Secret_key.t

  (** Create a finite key pool. *)
  module Make_finite_key_pool
      (Arg : Param_S with type algo := Tezos_crypto.Signature.V3.algo) :
    Finite_key_pool_S
end

module V_latest : sig
  module type Finite_key_pool_S =
    P_Finite_key_pool_S
      with type public_key_hash :=
        Tezos_crypto.Signature.V_latest.Public_key_hash.t
       and type public_key := Tezos_crypto.Signature.V_latest.Public_key.t
       and type secret_key := Tezos_crypto.Signature.V_latest.Secret_key.t

  (** Create a finite key pool. *)
  module Make_finite_key_pool
      (Arg : Param_S with type algo := Tezos_crypto.Signature.V_latest.algo) :
    Finite_key_pool_S
end

include module type of V_latest
