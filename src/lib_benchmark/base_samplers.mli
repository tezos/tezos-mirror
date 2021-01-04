(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@tezos.com>                       *)
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

(** The type of samplers. Samplers should be deterministic in their input.
    Note that a value of type [Random.State.t] is mutable and can be updated
    destructively at each call of the sampler. *)
type 'a sampler = Random.State.t -> 'a

(** Range (inclusive) *)
type range = {min : int; max : int}

val range_encoding : range Data_encoding.t

(** [sample_in_interval ~range] creates a sampler in the specified interval. *)
val sample_in_interval : range:range -> int sampler

(** Samples a boolean uniformly at random *)
val uniform_bool : bool sampler

(** Samples the specified number of bits uniformly at random. *)
val uniform_partial_byte : nbits:int -> char sampler

(** Samples a string of length [nbytes] uniformly at random. *)
val uniform_string : nbytes:int -> string sampler

(** Samples bytes of length [nbytes] uniformly at random. *)
val uniform_bytes : nbytes:int -> bytes sampler

(** Samples a non-negative big integer stored on [nbytes] bytes,
    uniformly at random (modulo trailing zeroes) *)
val uniform_nat : nbytes:int -> Z.t sampler

(** Samples a big integer stored on [nbytes] bytes, uniformly at
    random (modulo trailing zeroes) *)
val uniform_int : nbytes:int -> Z.t sampler

(** Samples a size in bytes uniformly in [range] and then
    samples a uniform non-negative big integer of this size.  *)
val nat : range:range -> Z.t sampler

(** Samples a size in bytes uniformly in [range] and then
    samples a uniform big integer of this size.  *)
val int : range:range -> Z.t sampler

(** Samples a readable character. *)
val uniform_readable_ascii : char sampler

(** Samples a readable string with length sampled uniformly in [range]. *)
val readable_ascii_string : range:range -> string sampler

(** Samples a string with length sampled uniformly in [range]. *)
val string : range:range -> string sampler

(** Samples bytes with length sampled uniformly in [range]. *)
val bytes : range:range -> bytes sampler

(** Sampling of "adversarial" values in the sense that they exhibit the
    worst-case performance of the usual comparison functions.

    These samplers generate pairs of a common prefix that has a size
    uniformly sampled in [range], and a list of [n] distinct (with high-probability)
    elements sharing this common prefix.
 *)
module Adversarial : sig
  val integers : range:range -> n:int -> (Z.t * Z.t list) sampler

  val strings : range:range -> n:int -> (string * string list) sampler

  val bytes : range:range -> n:int -> (bytes * bytes list) sampler
end
