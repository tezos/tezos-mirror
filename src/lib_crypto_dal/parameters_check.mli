(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Number of bytes fitting in a Scalar.t. Since scalars are integer modulo
   r~2^255, we restrict ourselves to 248-bit integers (31 bytes). *)
val scalar_bytes_amount : int

(** The page size is a power of two and thus not a multiple of [scalar_bytes_amount],
   hence the + 1 to account for the remainder of the division. *)
val page_length : page_size:int -> int

(** for a given [size] (in bytes), return the length of the corresponding
   domain *)
val domain_length : size:int -> int

(** [slot_as_polynomial_length ~slot_size ~page_size] returns the length of the
   polynomial of maximal degree representing a slot of size [slot_size] with
   [slot_size / page_size] pages (page_size must divides slot_size). The
   returned length thus depends on the number of pages. *)
val slot_as_polynomial_length : slot_size:int -> page_size:int -> int

(** Returns [max_polynomial_length], [erasure_encoded_polynomial_length] &
   [shard_length] for the given parameters *)
val compute_lengths :
  redundancy_factor:int ->
  slot_size:int ->
  page_size:int ->
  number_of_shards:int ->
  int * int * int

(** Fails if and only if the given parameters are not suitable for the DAL. *)
val ensure_validity_without_srs :
  slot_size:int ->
  page_size:int ->
  redundancy_factor:int ->
  number_of_shards:int ->
  (unit, [> `Fail of string]) result
