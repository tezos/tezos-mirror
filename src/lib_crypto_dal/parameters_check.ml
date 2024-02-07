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

open Error_monad
open Kzg.Bls

(* Number of bytes fitting in a Scalar.t. Since scalars are integer modulo
   r~2^255, we restrict ourselves to 248-bit integers (31 bytes). *)
let scalar_bytes_amount = Scalar.size_in_bytes - 1

(* The page size is a power of two and thus not a multiple of [scalar_bytes_amount],
   hence the + 1 to account for the remainder of the division. *)
let page_length ~page_size = Int.div page_size scalar_bytes_amount + 1

(* for a given [size] (in bytes), return the length of the corresponding
   domain *)
let domain_length ~size =
  let length = page_length ~page_size:size in
  let length_domain, _, _ = Kzg.Utils.FFT.select_fft_domain length in
  length_domain

(* [slot_as_polynomial_length ~slot_size ~page_size] returns the length of the
   polynomial of maximal degree representing a slot of size [slot_size] with
   [slot_size / page_size] pages (page_size must divides slot_size). The
   returned length thus depends on the number of pages. *)
let slot_as_polynomial_length ~slot_size ~page_size =
  let page_length_domain = domain_length ~size:page_size in
  slot_size / page_size * page_length_domain

let compute_lengths ~redundancy_factor ~slot_size ~page_size ~number_of_shards =
  let max_polynomial_length = slot_as_polynomial_length ~slot_size ~page_size in
  let erasure_encoded_polynomial_length =
    redundancy_factor * max_polynomial_length
  in
  let shard_length = erasure_encoded_polynomial_length / number_of_shards in
  (max_polynomial_length, erasure_encoded_polynomial_length, shard_length)
