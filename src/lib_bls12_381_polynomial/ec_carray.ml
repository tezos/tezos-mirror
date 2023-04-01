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

module type Stubs_sig = sig
  type fr_array

  type ec_array

  (** [evaluation_ecfft_inplace points domain log log_degree] performs the ECFTT.

  requires:
  - [size p = size domain]
  - [size domain = 2^log]
  - [domain = [one; g; ..; g^{n-1}]] where [g] is a primitive
  [n]-th root of unity and [n = 2^log] (as done by {!Domain.Stubs.compute_domain}) *)
  val evaluation_ecfft_inplace :
    ec_array -> domain:fr_array -> log:int -> log_degree:int -> unit

  (** [interpolation_ecfft_inplace p domain log] performs the inverse ECFFT.

  requires:
  - [size p = size domain]
  - [size domain = 2^log]
  - [domain = [one; g; ..; g^{n-1}]] where [g] is a primitive
  [n]-th root of unity and [n = 2^log] (as done by {!Domain.Stubs.compute_domain}) *)
  val interpolation_ecfft_inplace :
    ec_array -> domain:fr_array -> log:int -> unit

  (** [add_arrays_inplace a b size] writes the element-wise sum of the input
      vectors [a] and [b] into [a].

   requires:
   - [size a = size b = size] *)
  val add_arrays_inplace : ec_array -> ec_array -> int -> unit

  (** [mul_arrays evaluations arrays (dim1, dim2)] computes the EC point multiplication 
   given the scalar multipliers [evaluations] and the points [arrays].

   requires:
   - [size evaluations = size arrays = dim1]
   - [size evaluations.(i) = size arrays.(i) = dim2] for i=0, ..., dim1-1 *)
  val mul_arrays :
    ec_array array -> fr_array array -> ec_array array -> int * int -> unit
end

module type EC_carray_sig = sig
  include Carray.Carray_sig

  type domain

  (** [evaluation_ecfft domain points] computes the ECFFT.
  [domain] can be obtained using {!Domain.build}.

  The ECFFT computes the G-linear map
  G^n -> G^n :
  (P_i)_{i=0,...,n-1} |-> (sum_{i=0}^{n-1} [domain.(i*j mod n)]P_i)_{j=0,...,n-1}.

  where [a]P denotes the elliptic curve point multiplication.

  Note:
  - size of domain must be a power of two
  - degree of polynomial must be strictly less than the size of domain *)
  val evaluation_ecfft : domain:domain -> points:t -> t

  (** [interpolation_ecfft_inplace domain points] computes the inverse ECFFT.
  [domain] can be obtained using {!Domain.build}.

  It is the inverse function of [evaluation_ecfft_inplace].

  Note:
  - size of domain must be a power of two
  - size of a polynomial must be equal to size of domain *)
  val interpolation_ecfft_inplace : domain:domain -> points:t -> unit

  (** [add_arrays_inplace a b] writes the element-wise sum of the input
      vectors [a] and [b] into [a] *)
  val add_arrays_inplace : t -> t -> unit

  type evaluations

  (** [mul_arrays evaluations arrays] computes the EC point multiplication 
   given the scalar multipliers [evaluations] and the points [arrays] *)
  val mul_arrays : evaluations:evaluations array -> arrays:t array -> t array
end

module Make
    (EC_point : Bls12_381.CURVE)
    (EC_point_array : Carray.Carray_sig with type elt = EC_point.t)
    (Stubs : Stubs_sig
               with type fr_array = Fr_carray.t
                and type ec_array = EC_point_array.t) :
  EC_carray_sig
    with type elt = EC_point.t
     and type domain = Domain.t
     and type evaluations = Evaluations.t = struct
  include EC_point_array

  type domain = Domain.t

  module Domain = Domain.Domain_unsafe

  let evaluation_ecfft ~domain ~points =
    let n_domain = Domain.length domain in
    let res = EC_point_array.init n_domain (fun _ -> EC_point.(copy zero)) in
    let degree = EC_point_array.degree points in
    let n_domain = Domain.length domain in
    if degree = -1 then res
    else
      let log = Z.log2 (Z.of_int n_domain) in
      if not (Helpers.is_power_of_two n_domain) then
        raise @@ Invalid_argument "Size of domain should be a power of 2." ;
      if not (degree < n_domain) then
        raise
        @@ Invalid_argument
             "Degree of poly should be strictly less than domain size." ;
      let log_degree = Z.log2up (Z.of_int (degree + 1)) in
      let len = EC_point_array.length points in
      EC_point_array.blit points ~src_off:0 res ~dst_off:0 ~len ;
      Stubs.evaluation_ecfft_inplace
        res
        ~domain:(Domain.to_carray domain)
        ~log
        ~log_degree ;
      res

  let interpolation_ecfft_inplace ~domain ~points =
    if EC_point_array.degree points = -1 then ()
    else
      let n = length points in
      let log = Z.log2 (Z.of_int n) in
      let n_domain = Domain.length domain in
      if not (Helpers.is_power_of_two n_domain) then
        raise @@ Invalid_argument "Size of domain should be a power of 2." ;
      let n_points = length points in
      if not (n_points = n_domain) then
        raise
        @@ Invalid_argument "Size of coefficients should be same as domain." ;
      Stubs.interpolation_ecfft_inplace
        points
        ~domain:(Domain.to_carray domain)
        ~log

  let add_arrays_inplace a b =
    let a_len = length a in
    let b_len = length b in
    if a_len <> b_len then
      raise
        (Invalid_argument
           "add_arrays_inplace: input arrays must have same length") ;
    Stubs.add_arrays_inplace a b a_len

  type evaluations = Evaluations.t

  module Evaluations_unsafe = Evaluations.Evaluations_unsafe

  let mul_arrays ~evaluations ~arrays =
    let arrays_number = Array.length arrays in
    if arrays_number <> Array.length evaluations then
      raise
        (Invalid_argument "mul_arrays_inplace: arrays must have same length") ;
    let array_size = length arrays.(0) in
    let res = Array.init arrays_number (fun _ -> allocate array_size) in
    Stubs.mul_arrays
      res
      (Array.map Evaluations_unsafe.to_carray evaluations)
      arrays
      (arrays_number, array_size) ;
    res
end

module G1 = Bls12_381.G1
module G2 = Bls12_381.G2

module G1_Elt = struct
  type t = G1.t

  (** The size in jacobian coordinates *)
  let size = G1.size_in_bytes / 2 * 3

  let zero = G1.zero

  let allocate () = G1.(copy zero)

  let eq = G1.eq
end

module G2_Elt = struct
  type t = G2.t

  (** The size in jacobian coordinates *)
  let size = G2.size_in_bytes / 2 * 3

  let zero = G2.zero

  let allocate () = G2.(copy zero)

  let eq = G2.eq
end

module G1_array_internal = Carray.Make (G1_Elt)
module G2_array_internal = Carray.Make (G2_Elt)

module Stubs_g1 = struct
  type fr_array = Fr_carray.t

  type ec_array = G1_array_internal.t

  external evaluation_ecfft_inplace :
    ec_array -> domain:fr_array -> log:int -> log_degree:int -> unit
    = "caml_bls12_381_polynomial_fft_g1_inplace_on_stubs"

  external interpolation_ecfft_inplace :
    ec_array -> domain:fr_array -> log:int -> unit
    = "caml_bls12_381_polynomial_ifft_g1_inplace_on_stubs"

  external add_arrays_inplace : ec_array -> ec_array -> int -> unit
    = "caml_bls12_381_polynomial_carray_g1_add_inplace_stubs"

  external mul_arrays :
    ec_array array -> fr_array array -> ec_array array -> int * int -> unit
    = "caml_bls12_381_polynomial_evaluations_mul_arrays_g1_stubs"
end

module Stubs_g2 = struct
  type fr_array = Fr_carray.t

  type ec_array = G2_array_internal.t

  external evaluation_ecfft_inplace :
    ec_array -> domain:fr_array -> log:int -> log_degree:int -> unit
    = "caml_bls12_381_polynomial_fft_g2_inplace_on_stubs"

  external interpolation_ecfft_inplace :
    ec_array -> domain:fr_array -> log:int -> unit
    = "caml_bls12_381_polynomial_ifft_g2_inplace_on_stubs"

  external add_arrays_inplace : ec_array -> ec_array -> int -> unit
    = "caml_bls12_381_polynomial_carray_g2_add_inplace_stubs"

  external mul_arrays :
    ec_array array -> fr_array array -> ec_array array -> int * int -> unit
    = "caml_bls12_381_polynomial_evaluations_mul_arrays_g2_stubs"
end

module G1_carray :
  EC_carray_sig
    with type elt = Bls12_381.G1.t
     and type domain = Domain.t
     and type evaluations = Evaluations.t =
  Make (G1) (G1_array_internal) (Stubs_g1)

module G2_carray :
  EC_carray_sig
    with type elt = Bls12_381.G2.t
     and type domain = Domain.t
     and type evaluations = Evaluations.t =
  Make (G2) (G2_array_internal) (Stubs_g2)
