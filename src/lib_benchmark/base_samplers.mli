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

(** [sample_in_interval ~range] creates a sampler in the specified interval.
    @raise Invalid_argument if [range.max < range.min].
 *)
val sample_in_interval : range:range -> int sampler

(** [sample_float_in_interval ~min ~max] creates a sampler in the
    specified interval.
    @raise Invalid_argument if [max <= min].
*)
val sample_float_in_interval : min:float -> max:float -> float sampler

(** Samples a boolean uniformly at random *)
val uniform_bool : bool sampler

(** Sample a byte uniformly at random *)
val uniform_byte : char sampler

(** Samples the specified number of bits uniformly at random.
    The sampled bits are the [nbits] least significant ones in the
    returned char.
    @raise Invalid_argument if [nbits < 1 || nbits > 8].
 *)
val uniform_partial_byte : nbits:int -> char sampler

(** Samples a string of length [nbytes] uniformly at random.
    @raise Invalid_argument if [nbytes < 0].
 *)
val uniform_string : nbytes:int -> string sampler

(** Samples bytes of length [nbytes] uniformly at random.
    @raise Invalid_argument if [nbytes < 0].
 *)
val uniform_bytes : nbytes:int -> bytes sampler

(** Samples a non-negative big integer stored on [nbytes] bytes,
    uniformly at random (modulo trailing zeroes).
    @raise Invalid_argument if [nbytes < 0].
 *)
val uniform_nat : nbytes:int -> Z.t sampler

(** Samples a big integer stored on [nbytes] bytes, uniformly at
    random (modulo trailing zeroes).
    @raise Invalid_argument if [nbytes < 0].
 *)
val uniform_int : nbytes:int -> Z.t sampler

(** Samples a size in bytes uniformly in [size] and then
    samples a uniform non-negative big integer of this size.
    @raise Invalid_argument if [size.max < size.min]
    or if [size.min < 0].
 *)
val nat : size:range -> Z.t sampler

(** Samples a size in bytes uniformly in [size] and then
    samples a uniform big integer of this size.
    @raise Invalid_argument if [size.max < size.min]
    or if [size.min < 0].
 *)
val int : size:range -> Z.t sampler

(** Samples a readable character. *)
val uniform_readable_ascii : char sampler

(** Samples a readable string with length sampled uniformly in [size].
    @raise Invalid_argument if [size.max < size.min]
    or if [range.min < 0].
 *)
val readable_ascii_string : size:range -> string sampler

(** Samples a string with length sampled uniformly in [size].
    @raise Invalid_argument if [size.max < size.min]
    or if [size.min < 0].
 *)
val string : size:range -> string sampler

(** Samples bytes with length sampled uniformly in [size].
    @raise Invalid_argument if [size.max < size.min]
    or if [size.min < 0].
 *)
val bytes : size:range -> bytes sampler

(** Sampling of "adversarial" values in the sense that they exhibit the
    worst-case performance of the usual comparison functions.

    These samplers generate pairs of a common prefix that has a size
    uniformly sampled in [prefix_size], and a list of [card] distinct (with high-probability)
    elements sharing this common prefix.

    @raise Invalid_argument if [prefix_size.max < prefix_size.min]
    or if [prefix_size.min < 0] or of [card <= 0].
 *)
module Adversarial : sig
  val integers : prefix_size:range -> card:int -> (Z.t * Z.t list) sampler

  val strings : prefix_size:range -> card:int -> (string * string list) sampler

  val bytes : prefix_size:range -> card:int -> (bytes * bytes list) sampler
end
