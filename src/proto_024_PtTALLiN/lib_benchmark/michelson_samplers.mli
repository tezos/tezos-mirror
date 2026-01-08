(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Sampling various Michelson values. *)

exception SamplingError of string

open Protocol
open Base_samplers

(** This module exposes a functor implementing various samplers for Michelson.
    These allow to sample:
    - types and comparable types (given a target size),
    - values and comparable values of a given Michelson type (given some more
      parameters fixed at functor instantiation time)
    - stacks

    Note that some kind of values might not be supported. At the time of writing,
    the value sampler doesn't handle the following types:
    - Sapling transaction and states
    - Timelock chests and chest keys
    - Operations
    - Lambdas (ie code)

    For the latter, consider using the samplers in {!Michelson_mcmc_samplers}.
*)

(** Parameters for the Michelson samplers. *)
type parameters = {
  base_parameters : Michelson_samplers_base.parameters;
  list_size : Base_samplers.range;
      (** The range of the size, measured in number of elements, in which lists must be sampled.*)
  set_size : Base_samplers.range;
      (** The range of the size, measured in number of elements, in which sets must be sampled.*)
  map_size : Base_samplers.range;
      (** The range of the size, measured in number of bindings, in which maps must be sampled.*)
}

(** Encoding for sampler prameters. *)
val parameters_encoding : parameters Data_encoding.t

type type_name =
  [ `TUnit
  | `TInt
  | `TNat
  | `TSignature
  | `TString
  | `TBytes
  | `TMutez
  | `TKey_hash
  | `TKey
  | `TTimestamp
  | `TAddress
  | `TBool
  | `TPair
  | `TOr
  | `TLambda
  | `TOption
  | `TList
  | `TSet
  | `TMap
  | `TBig_map
  | `TContract
  | `TSapling_transaction
  | `TSapling_transaction_deprecated
  | `TSapling_state
  | `TOperation
  | `TChain_id
  | `TBls12_381_g1
  | `TBls12_381_g2
  | `TBls12_381_fr
  | `TTicket ]

(** The module type produced by the [Make] functor. *)
module type S = sig
  (** Basic Michelson samplers, re-exported for convenience by the functor. *)
  module Michelson_base : Michelson_samplers_base.S

  (** Samplers for random Michelson types. *)
  module Random_type : sig
    (** [m_type ~size ?blacklist] samples a type containing exactly
        [size] constructors. The [blacklist] is a predicate which can
        be used to discard some unwanted cases. *)
    val m_type :
      size:int ->
      ?blacklist:(type_name -> bool) ->
      unit ->
      Script_typed_ir.ex_ty sampler

    (** [m_comparable_type ~size] samples a comparable type containing
        exactly [size] constructors. *)
    val m_comparable_type :
      size:int -> Script_ir_translator.ex_comparable_ty sampler
  end

  (** Samplers for random Michelson values. Restrictions apply on the
      supported types as listed at the beginning of this file. *)
  module Random_value : sig
    (** Sample a value given its type. *)
    val value : ('a, _) Script_typed_ir.ty -> 'a sampler

    (** Sample a comparable value given its type. *)
    val comparable : 'a Script_typed_ir.comparable_ty -> 'a sampler

    (** Sample a stack given its type. *)
    val stack : ('a, 'b) Script_typed_ir.stack_ty -> ('a * 'b) sampler
  end
end

(** Instantiate a module of type {!S}. *)
module Make : functor
  (P : sig
     val parameters : parameters
   end)
  (Crypto_samplers : Crypto_samplers.Finite_key_pool_S)
  -> S

module Internal_for_tests : sig
  type type_name =
    [ `TAddress
    | `TBig_map
    | `TBls12_381_fr
    | `TBls12_381_g1
    | `TBls12_381_g2
    | `TBool
    | `TBytes
    | `TChain_id
    | `TContract
    | `TInt
    | `TKey
    | `TKey_hash
    | `TLambda
    | `TList
    | `TMap
    | `TMutez
    | `TNat
    | `TOperation
    | `TOption
    | `TPair
    | `TSapling_state
    | `TSapling_transaction
    | `TSapling_transaction_deprecated
    | `TSet
    | `TSignature
    | `TString
    | `TTicket
    | `TTimestamp
    | `TOr
    | `TUnit ]
end
