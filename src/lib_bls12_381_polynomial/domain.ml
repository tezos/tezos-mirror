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

module Fr = Bls12_381.Fr

module Stubs = struct
  type fr = Fr.t

  type fr_array = Fr_carray.t

  (** [compute_domain res n g] computes [[one; g; ..; g^{n-1}]] for a given
  blst_fr element [g]

  requires:
  - [1 < n <= size res]

  ensures:
  - [res[i] = g^i] for [i = 0..(n-1)] *)
  external compute_domain : fr_array -> int -> fr -> unit
    = "caml_bls12_381_polynomial_polynomial_compute_domain_stubs"
    [@@noalloc]

  (** [rescale res a size_res size_a] writes the result of rescaling the evaluation
      representation of a polynomial [a] from [domain_a] of size [size_a] to
      [domain_res] of size [size_res] in [res]

   requires:
   - [size res = size_res]
   - [size a = size_a]
   - [size_res <= size_a]
   - [res] and [a] are disjoint
   - [size_res mod size_a = 0] *)
  external rescale : fr_array -> fr_array -> int -> int -> unit
    = "caml_bls12_381_polynomial_polynomial_evaluations_rescale_stubs"
    [@@noalloc]
end

module Domain_impl = struct
  type scalar = Bls12_381.Fr.t

  type t = Fr_carray.t [@@deriving repr]

  let of_carray p = p

  let to_carray p = p

  let of_array = Fr_carray.of_array

  let to_array d = Fr_carray.to_array d

  let length = Fr_carray.length

  let get = Fr_carray.get

  let create n root_of_unity =
    if n <= 1 then raise @@ Invalid_argument "create: requires n > 1" ;
    let domain = Fr_carray.allocate n in
    Stubs.compute_domain domain n root_of_unity ;
    domain

  let primitive_root_of_unity = Fr_carray.primitive_root_of_unity

  let build ?primitive_root n =
    let primitive_root =
      match primitive_root with
      | None -> Fr_carray.primitive_root_of_unity n
      | Some root -> root
    in
    create n primitive_root

  let build_power_of_two ?primitive_root log = build ?primitive_root (1 lsl log)

  let subgroup ~log d =
    let l = length d in
    let n = 1 lsl log in
    if n > l || log <= 0 then raise @@ Invalid_argument "subgroup: wrong order"
    else
      let dom = Fr_carray.allocate n in
      Stubs.rescale dom d n l ;
      dom

  let inverse d =
    let n = length d in
    Array.init n (fun i ->
        if i = 0 then Fr.(copy one) else Fr_carray.get d (n - i))

  let equal d1 d2 =
    let len = length d1 in
    len = length d2 && Fr_carray.equal d1 ~offset1:0 d2 ~offset2:0 ~len
end

module type Domain_sig = sig
  type scalar

  type t [@@deriving repr]

  (** [length p] returns the length of a given array [p] *)
  val length : t -> int

  (** [get p i] returns the [i]-th element of a given array [p] *)
  val get : t -> int -> scalar

  (** [primitive_root_of_unity n] returns a primitive [n]-th root of unity,
      provided it exists *)
  val primitive_root_of_unity : int -> scalar

  (** [build n] computes [[one; g; ..; g^{n-1}]] where [g]
      is a primitive [n]-th root of unity *)
  val build : ?primitive_root:scalar -> int -> t

  (** [build_power_of_two log] computes [[one; g; ..; g^{n-1}]] where [g]
      is a primitive [n]-th root of unity and [n = 2^log] *)
  val build_power_of_two : ?primitive_root:scalar -> int -> t

  (** [subgroup log d] returns a subgroup of [d] of order [2^log] *)
  val subgroup : log:int -> t -> t

  (** [inverse d] returns for a domain [wⁱᵢ] its inverse domain [w⁻ⁱᵢ] *)
  val inverse : t -> scalar array

  (* [equal d1 d2] returns true if [d1] is equal to [d2] *)
  val equal : t -> t -> bool
end

module type Domain_unsafe_sig = sig
  include Domain_sig

  (** [to_array d] converts a C array [d] to an OCaml array *)
  val to_array : t -> scalar array

  (** [of_array d] converts an OCaml array [d] to a C array *)
  val of_array : scalar array -> t

  (** [to_carray d] converts [d] from type {!type:t} to type {!type:Fr_carray.t}

      Note: [to_carray d] doesn't create a copy of [d] *)
  val to_carray : t -> Fr_carray.t

  (** [of_carray d] converts [d] from type {!type:Fr_carray.t} to type {!type:t}

      Note: [of_carray d] doesn't create a copy of [d] *)
  val of_carray : Fr_carray.t -> t
end

module Domain_unsafe : Domain_unsafe_sig with type scalar = Bls12_381.Fr.t =
  Domain_impl

include (
  Domain_unsafe :
    Domain_sig
      with type t = Domain_unsafe.t
       and type scalar = Domain_unsafe.scalar)
