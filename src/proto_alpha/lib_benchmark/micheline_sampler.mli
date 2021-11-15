(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <team@marigold.dev>                           *)
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

(** Micheline sampling. *)

(** A [width_function] specifies the distribution of node degree as a function
    of [depth]. A [width_function] {e must} be supported by the nonnegative
    integers.

    Note that picking a [width_function] which doesn't converge fast enough to
    the singular distribution on 0 could yield very large terms. *)
type width_function = depth:int -> int Base_samplers.sampler

(** [reasonable_width_function] is a width function which works well
    empirically. *)
val reasonable_width_function : width_function

(** [Base_samplers] specifies samplers for leaves, primitives and annotations. *)
module type Base_samplers = sig
  (** The type of primitives. *)
  type prim

  val sample_prim : prim Base_samplers.sampler

  val sample_annots : string list Base_samplers.sampler

  val sample_string : string Base_samplers.sampler

  val sample_bytes : Bytes.t Base_samplers.sampler

  val sample_z : Z.t Base_samplers.sampler

  val width_function : width_function
end

(** Applying the [Make] functor below yields a module with the following
    type. *)
module type S = sig
  type prim

  (** [sample w] is a Micheline sampler for the prescribed primitive
      type. The sampler uses the provided width function [w]. *)
  val sample : (int, prim) Micheline.node Base_samplers.sampler
end

(** [Make] instantiates a micheline sampler. *)
module Make (P : Base_samplers) : S with type prim = P.prim
