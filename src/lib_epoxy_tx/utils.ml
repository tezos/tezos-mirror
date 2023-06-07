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

open Plompiler
module Curve = Mec.Curve.Jubjub.AffineEdwards

let of_int x = Z.of_int x |> S.of_z

let to_bls_scalar s = Curve.Base.of_z (S.to_z s)

let of_bls_scalar s = S.of_z (Curve.Base.to_z s)

let point_to_affine (u, v) =
  Curve.unsafe_from_coordinates ~u:(to_bls_scalar u) ~v:(to_bls_scalar v)

let affine_to_point p =
  let u = Curve.get_u_coordinate p in
  let v = Curve.get_v_coordinate p in
  (of_bls_scalar u, of_bls_scalar v)

let random_int = Stdlib.Random.int

let random_z bound = Random.int64 (Z.to_int64 bound) |> Z.of_int64

let nb_bits_base = Z.numbits S.order

let curve_base_to_s c = S.of_z @@ Curve.Base.to_z c

let curve_base_of_s c = Curve.Base.of_z @@ S.to_z c

let curve_scalar_to_s c = S.of_z @@ Curve.Scalar.to_z c

let curve_scalar_of_s c = Curve.Scalar.of_z @@ S.to_z c

let scalar_of_bytes bs =
  assert (Bytes.length bs < S.size_in_bytes) ;
  (* The bytes are interpreted as a Z integer *)
  let z = Z.of_bits @@ Bytes.to_string bs in
  S.of_z z

let scalar_to_bytes s = S.to_z s |> Z.to_bits |> Bytes.of_string
