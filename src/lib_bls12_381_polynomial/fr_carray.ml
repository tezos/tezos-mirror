(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Scalar = Bls12_381.Fr

module Elt = struct
  type t = Scalar.t

  let size = Scalar.size_in_bytes

  let zero = Scalar.zero

  let allocate () = Scalar.(copy zero)

  let eq = Scalar.eq
end

include Carray.Make (Elt)

(* Generator of the multiplicative group Fr^* *)
let generator = Scalar.of_int 7

(* Samples a primitive [n]-th root of unity. *)
let primitive_root_of_unity n =
  let n = Z.of_int n in
  let multiplicative_group_order = Z.(Scalar.order - one) in
  if not (Z.divisible multiplicative_group_order n) then
    raise
      (Invalid_argument
         (Format.sprintf
            "There do not exist %s-th roots of unity"
            (Z.to_string n)))
  else
    let exponent = Z.divexact multiplicative_group_order n in
    Scalar.pow generator exponent

(* Samples a 2^i-th root of unity, assuming that it exists *)
let primitive_root_of_unity_power_of_two ~log =
  primitive_root_of_unity (1 lsl log)

let build_array init next len =
  let xi = ref init in
  Array.init len (fun _ ->
      let i = !xi in
      xi := next !xi ;
      i)

(* TODO return carray instead of array ? *)
(* computes [| 1; x; x²; x³; ...; xᵈ⁻¹ |] *)
let powers d x = build_array Scalar.one Scalar.(mul x) d

let build_domain n = powers n (primitive_root_of_unity n)

let build_domain_power_of_two ~log =
  let g = primitive_root_of_unity_power_of_two ~log in
  powers (1 lsl log) g
