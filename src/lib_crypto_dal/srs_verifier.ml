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

let get_verifier_srs2 max_srs_size get_srs2 ~max_polynomial_length
    ~page_length_domain ~shard_length =
  let srs_g2_shards = get_srs2 shard_length in
  let srs_g2_pages = get_srs2 page_length_domain in
  let srs_g2_commitment = get_srs2 (max_srs_size - max_polynomial_length) in
  (srs_g2_shards, srs_g2_pages, srs_g2_commitment)

module Internal_for_tests = struct
  let max_srs_size = 1 lsl 16

  let max_verifier_srs_size = 1 lsl 8

  let fake_srs_seed =
    Scalar.of_string
      "20812168509434597367146703229805575690060615791308155437936410982393987532344"

  let fake_srs ?(size = max_srs_size) () =
    Srs_g1.generate_insecure size fake_srs_seed

  let get_srs1 i = G1.mul G1.one (Scalar.pow fake_srs_seed (Z.of_int i))

  let get_srs2 i = G2.mul G2.one (Scalar.pow fake_srs_seed (Z.of_int i))

  let get_verifier_srs2 = get_verifier_srs2 max_srs_size get_srs2

  let get_verifier_srs1 = fake_srs ~size:max_verifier_srs_size

  let is_in_srs2 _ = true
end
