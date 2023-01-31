(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Samplers for basic Michelson values (not including pairs, ors, tickets, big maps, etc) *)

open Protocol
open Base_samplers

(** Parameters for basic samplers *)
type parameters = {
  int_size : Base_samplers.range;
      (** The range of the size, measured in bytes, in which big integers must be sampled.*)
  string_size : Base_samplers.range;
      (** The range of the size, measured in bytes, in which strings must be sampled.*)
  bytes_size : Base_samplers.range;
      (** The range of the size, measured in bytes, in which [bytes] must be sampled.*)
}

(** Encoding for [parameters] *)
val parameters_encoding : parameters Data_encoding.t

(** A module of type [S] packs samplers used to construct basic Michelson values. *)
module type S = sig
  val int : Script_int.z Script_int.num sampler

  val nat : Script_int.n Script_int.num sampler

  val signature : Tezos_crypto.Signature.t sampler

  val string : Script_string.t sampler

  val bytes : bytes sampler

  val tez : Alpha_context.Tez.tez sampler

  val timestamp : Script_timestamp.t sampler
end

(** The [Make] functor instantiates a module of type [S], where the
    samplers satisfy the given parameters. *)
module Make : functor
  (P : sig
     val parameters : parameters
   end)
  -> S
