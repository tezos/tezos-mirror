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

let ensure_validity_without_srs ~slot_size ~page_size ~redundancy_factor
    ~number_of_shards =
  let open Result_syntax in
  let assert_result condition error_message =
    if not condition then fail (`Fail (error_message ())) else return_unit
  in
  let* () =
    assert_result (number_of_shards > 0) (fun () ->
        Format.asprintf
          "The number of shards must be a strictly positive integer. Given: %d"
          number_of_shards)
  in
  let max_polynomial_length, erasure_encoded_polynomial_length, shard_length =
    compute_lengths ~redundancy_factor ~slot_size ~page_size ~number_of_shards
  in
  let* () =
    assert_result
      (Kzg.Utils.is_power_of_two slot_size)
      (* According to the specification the length of a slot are in MiB *)
      (fun () ->
        Format.asprintf
          "Slot size is expected to be a power of 2. Given: %d"
          slot_size)
  in
  let* () =
    assert_result
      (Kzg.Utils.is_power_of_two page_size)
      (* According to the specification the lengths of a page are in MiB *)
      (fun () ->
        Format.asprintf
          "Page size is expected to be a power of 2. Given: %d"
          page_size)
  in
  let* () =
    assert_result
      (Kzg.Utils.is_power_of_two redundancy_factor && redundancy_factor >= 2)
      (* The redundancy factor should be a power of 2 so that n is a power of 2
         for proper FFT sizing. The variable [polynomial_length] is assumed to be a power of 2
         as the output of [slot_as_polynomial_length]. *)
        (fun () ->
        Format.asprintf
          "Redundancy factor is expected to be a power of 2 and greater than \
           2. Given: %d"
          redundancy_factor)
  in
  (* At this point [erasure_encoded_polynomial_length] is a power of 2, and [erasure_encoded_polynomial_length > max_polynomial_length]. *)
  let* () =
    assert_result
      (page_size >= 32 && page_size < slot_size)
      (* The size of a page must be greater than 31 bytes (32 > 31 is the next
         power of two), the size in bytes of a scalar element, and strictly less
         than the slot size. *)
        (fun () ->
        Format.asprintf
          "Page size is expected to be greater than '32' and strictly less \
           than the slot_size '%d'. Got: %d"
          slot_size
          page_size)
  in
  let max_two_adicity_log = 32 in
  let two_adicity_log =
    snd Z.(remove (of_int erasure_encoded_polynomial_length) (of_int 2))
  in
  let* () =
    assert_result
      (two_adicity_log <= max_two_adicity_log)
      (* The 2-adicity of [erasure_encoded_polynomial_length] must be at most 2^32,
         the size of the biggest subgroup of 2^i roots of unity in the multiplicative group of Fr,
         because the FFTs operate on such groups. *)
      (fun () ->
        Format.asprintf
          "Slot size (%d) and/or redundancy factor (%d) is/are too high: \
           expected 2-adicity of erasure_encoded_polynomial_length (%d) to be \
           at most 2^%d, got: 2^%d"
          slot_size
          redundancy_factor
          erasure_encoded_polynomial_length
          max_two_adicity_log
          two_adicity_log)
  in
  let* () =
    assert_result
      (erasure_encoded_polynomial_length mod number_of_shards == 0
      && number_of_shards < erasure_encoded_polynomial_length)
      (* The number of shards must divide n, so [number_of_shards <= erasure_encoded_polynomial_length].
         Moreover, the inequality is strict because if [number_of_shards = erasure_encoded_polynomial_length],
         the domains for the FFT contain only one element and we cannot build
         FFT domains with only one element. Given that [erasure_encoded_polynomial_length] is a power of two,
         it follows that the maximum number of shards is [erasure_encoded_polynomial_length/2]. *)
        (fun () ->
        Format.asprintf
          "The number of shards must divide, and not be equal to %d. For the \
           given parameter, the maximum number of shards is %d. Got: %d."
          erasure_encoded_polynomial_length
          (erasure_encoded_polynomial_length / 2)
          number_of_shards)
  in
  let* () =
    assert_result
      (shard_length < max_polynomial_length)
      (* Since shard_length = n / number_of_shards, we obtain
         (all quantities are positive integers):
         shard_length < k
         => n / number_of_shards < k
         => n / k < number_of_shards
         => redundancy_factor < number_of_shards
         since number_of_shards is a power of 2 the minimum value for
         number_of_shards is 2 * redundancy_factor. *)
      (fun () ->
        Format.asprintf
          "For the given parameters, the minimum number of shards is %d. Got \
           %d."
          (redundancy_factor * 2)
          number_of_shards)
  in
  let domain_length = 2 * max_polynomial_length / shard_length in
  let* () =
    assert_result
      (domain_length <> 0 && domain_length land (domain_length - 1) = 0)
      (* The computation of shard proofs further require the [domain_length] to
         be a power of two for correct FFT sizing, even though we could relax
         the constraint to a product of primes dividing the order of the group
         G1 thanks to the Prime Factorization Algorithm, as we currently do with
         the FFTs on scalar elements, if the need arises. *)
        (fun () ->
        (* [domain_length = 2 * max_polynomial_length / shard_length
                          = 2 * max_polynomial_length / (redundancy_factor * max_polynomial_length / number_of_shards)
                          = 2 * number_of_shards / redundancy_factor] *)
        Format.asprintf
          "The ratio (2 * number of shards / redundancy factor) must be a \
           power of two. Got 2 * %d / %d = %d"
          number_of_shards
          redundancy_factor
          domain_length)
  in
  assert_result
    (max_polynomial_length mod shard_length = 0)
    (fun () ->
      Format.asprintf
        "The length of a shard must divide %d. Got %d"
        max_polynomial_length
        shard_length)
