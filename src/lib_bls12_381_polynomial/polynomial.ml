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

  (** [of_sparse res p n] converts the sparse representation of a polynomial [p] to
  the dense representation, from an OCaml array [p] of size [n] to a C array [res] of
  size [degree p + 1]

  requires:
  - degree of each coeff [d_i >= 0] and [d_i] are unique
  - the result must be initialized with zero (as done by {!Fr_carray.allocate})
  - [size res = degree p + 1]
  - [size p = n] *)
  external of_sparse : fr_array -> (fr * int) array -> int -> unit
    = "caml_bls12_381_polynomial_polynomial_of_sparse_stubs"
    [@@noalloc]

  (** [add res a b size_a size_b] writes the result of polynomial addition of [a] and [b]
  in [res]

  requires:
  - [size a = size_a]
  - [size b = size_b]
  - [size res = max (size_a, size_b)]
  - [res], [a] and [b] are either pairwise disjoint or equal *)
  external add : fr_array -> fr_array -> fr_array -> int -> int -> unit
    = "caml_bls12_381_polynomial_polynomial_add_stubs"
    [@@noalloc]

  (** [sub res a b size_a size_b] writes the result of polynomial subtraction of [b] from [a]
  in [res]

  requires:
  - [size a = size_a]
  - [size b = size_b]
  - [size res = max (size_a, size_b)]
  - [res], [a] and [b] are either pairwise disjoint or equal *)
  external sub : fr_array -> fr_array -> fr_array -> int -> int -> unit
    = "caml_bls12_381_polynomial_polynomial_sub_stubs"
    [@@noalloc]

  (** [mul res a b size_a size_b] writes the result of polynomial multiplication of [a] by [b]
  in [res]

  requires:
  - the result must be initialized with zero (as done by {!Fr_carray.allocate})
  - [size a = size_a]
  - [size b = size_b]
  - [size res = size_a + size_b - 1] *)
  external mul : fr_array -> fr_array -> fr_array -> int -> int -> unit
    = "caml_bls12_381_polynomial_polynomial_mul_stubs"
    [@@noalloc]

  (** [mul_by_scalar res b a size_a] writes the result of multiplying a polynomial [a]
  by a blst_fr element [b] in [res]

  requires:
  - [size a = size_a]
  - [size res = size_a]
  - [res] and [a] either disjoint or equal *)
  external mul_by_scalar : fr_array -> fr -> fr_array -> int -> unit
    = "caml_bls12_381_polynomial_polynomial_mul_by_scalar_stubs"
    [@@noalloc]

  (** [linear res poly_polylen_coeff nb_polys] writes the result of
      computing [λ₁·p₁(x) + λ₂·p₂(x) + … + λₖ·pₖ(x)] in [res], where
      - [poly_polylen_coeff.[i] = (pᵢ, size_p_i, λᵢ)]
      - [nb_polys] is a number of polynomials, i.e., [i = 1..nb_polys]

   requires:
   - the result must be initialized with zero (as done by {!Fr_carray.allocate})
   - [size res = max (size_p_i)]
   - [size poly_polylen_coeff = nb_polys]
   - [size p_i = size_p_i] *)
  external linear : fr_array -> (fr_array * int * fr) array -> int -> unit
    = "caml_bls12_381_polynomial_polynomial_linear_stubs"
    [@@noalloc]

  (** [linear_with_powers res c poly_polylen nb_polys] writes the result of
      computing [c⁰·p₀(x) + c¹·p₁(x) + … + cᵏ·pₖ(x)] in [res], where
      - [poly_polylen.[i] = (pᵢ, size_p_i)]
      - [nb_polys] is a number of polynomials

   requires:
   - the result must be initialized with zero (as done by {!Fr_carray.allocate})
   - [size res = max (size_p_i)]
   - [size poly_polylen = nb_polys]
   - [size p_i = size_p_i] *)
  external linear_with_powers :
    fr_array -> fr -> (fr_array * int) array -> int -> unit
    = "caml_bls12_381_polynomial_polynomial_linear_with_powers_stubs"
    [@@noalloc]

  (** [negate res p n] writes the result of negating a polynomial [p] in [res]

  requires:
  - [size p = n]
  - [size res = n]
  - [res] and [p] either disjoint or equal *)
  external negate : fr_array -> fr_array -> int -> unit
    = "caml_bls12_381_polynomial_polynomial_negate_stubs"
    [@@noalloc]

  (** [evaluate res p n x] writes the result of evaluating a polynomial [p] at [x]
  in [res]

  - requires: [size p = n] and [n > 0] *)
  external evaluate : fr -> fr_array -> int -> fr -> unit
    = "caml_bls12_381_polynomial_polynomial_evaluate_stubs"
    [@@noalloc]

  (** [division_xn res_q res_r p size_p (n, c)] writes the quotient and remainder of
      the division of a polynomial [p] by [(X^n + c)] in [res]

  requires:
  - [size p = size_p] and [size_p > n]
  - [size res_q = size_p - n]
  - [size res_r = n] *)
  external division_xn :
    fr_array -> fr_array -> fr_array -> int -> int * fr -> unit
    = "caml_bls12_381_polynomial_polynomial_division_xn_stubs"
    [@@noalloc]

  (** [mul_xn res p size_p n c] writes the result of multiplying a polynomial [p]
      by [(X^n + c)] in [res]

  requires:
  - [res] is initialized with bls-fr zeros
  - [size p = size_p]
  - [size res = size_p + n] *)
  external mul_xn : fr_array -> fr_array -> int -> int -> fr -> unit
    = "caml_bls12_381_polynomial_polynomial_mul_xn_stubs"
    [@@noalloc]

  external derivative : fr_array -> fr_array -> int -> unit
    = "caml_bls12_381_polynomial_polynomial_derivative_stubs"
    [@@noalloc]
end

module Polynomial_impl = struct
  type scalar = Fr.t

  type t = Fr_carray.t [@@deriving repr]

  let of_carray p = p

  let to_carray p = p

  let length = Fr_carray.length

  let erase p = Fr_carray.erase p (length p)

  let allocate = Fr_carray.allocate

  let copy p = Fr_carray.copy ~offset:0 ~len:(length p) p

  let copy_carray = Fr_carray.copy

  let get = Fr_carray.get

  let degree = Fr_carray.degree

  let init = Fr_carray.init

  let equal p1 p2 =
    let n1 = length p1 in
    let n2 = length p2 in
    let short_n, long_p, long_n =
      if n1 <= n2 then (n1, p2, n2) else (n2, p1, n1)
    in
    if Fr_carray.equal p1 ~offset1:0 p2 ~offset2:0 ~len:short_n then
      let rec stop_at_first_non_zero i =
        if i = long_n then true
        else if Fr.eq (get long_p i) Fr.zero then stop_at_first_non_zero (i + 1)
        else false
      in
      stop_at_first_non_zero short_n
    else false

  let to_string p =
    String.concat
      " ; "
      (List.map Fr.to_string (Array.to_list @@ Fr_carray.to_array p))

  (* ?of_sparse_coefficients *)
  let of_coefficients coefficients =
    let coefficients = Array.of_list coefficients in
    let degree =
      Array.fold_left
        (fun max_degree (_coeff, d) ->
          assert (d >= 0) ;
          max d max_degree)
        0
        coefficients
    in
    let polynomial = allocate (degree + 1) in
    Stubs.of_sparse polynomial coefficients (Array.length coefficients) ;
    polynomial

  let of_dense = Fr_carray.of_array

  let zero = of_coefficients []

  let one = of_coefficients [(Fr.one, 0)]

  let generate_biased_random_polynomial n =
    assert (n >= 0) ;
    if Random.int 10 = 0 || n = 0 then zero
    else
      let poly =
        Array.init n (fun _ ->
            if Random.bool () then Fr.random () else Fr.copy Fr.zero)
      in
      Array.set poly (n - 1) Fr.one ;
      Fr_carray.of_array poly

  let random n = List.init n (fun i -> (Fr.random (), i)) |> of_coefficients

  let to_dense_coefficients p =
    (* the polynomial could be padded with zero, so we instead of using [n] and
       wasting some space we recompute the size of the minimal representation *)
    let len = 1 + max 0 (degree p) in
    Fr_carray.to_array ~len p

  (* ensures: no coefficient in the result is zero *)
  let to_sparse_coefficients poly =
    let poly = to_dense_coefficients poly in
    let res = ref [] in
    for deg = Array.length poly - 1 downto 0 do
      let coef = poly.(deg) in
      if not (Fr.is_zero coef) then res := (Fr.copy coef, deg) :: !res
    done ;
    !res

  let add p1 p2 =
    let n1 = length p1 in
    let n2 = length p2 in
    let res_size = max n1 n2 in
    let res = allocate res_size in
    Stubs.add res p1 p2 n1 n2 ;
    res

  let add_inplace res p1 p2 =
    let n1 = length p1 in
    let n2 = length p2 in
    let n_res = length res in
    assert (n_res = max n1 n2) ;
    Stubs.add res p1 p2 n1 n2

  let sub p1 p2 =
    let n1 = length p1 in
    let n2 = length p2 in
    let max_size = max n1 n2 in
    let res = allocate max_size in
    Stubs.sub res p1 p2 n1 n2 ;
    res

  let sub_inplace res p1 p2 =
    let n1 = length p1 in
    let n2 = length p2 in
    let n_res = length res in
    assert (n_res >= max n1 n2) ;
    Stubs.sub res p1 p2 n1 n2

  let mul p1 p2 =
    let n1 = length p1 in
    let n2 = length p2 in
    let res_size = n1 + n2 - 1 in
    let res = allocate res_size in
    Stubs.mul res p1 p2 n1 n2 ;
    res

  let mul_by_scalar scalar p =
    let n = length p in
    let res = allocate n in
    Stubs.mul_by_scalar res scalar p n ;
    res

  let mul_by_scalar_inplace res scalar p =
    let n = length p in
    let n_res = length res in
    assert (n_res >= n) ;
    Stubs.mul_by_scalar res scalar p n

  let linear polys coeffs =
    let nb_polys = List.length polys in
    assert (List.compare_length_with coeffs nb_polys = 0) ;
    let res_size =
      List.fold_left (fun res_size p -> max (length p) res_size) 0 polys
    in
    if res_size = 0 then zero
    else
      let res = allocate res_size in
      let poly_polylen_coeff =
        List.map2 (fun p coeff -> (p, length p, coeff)) polys coeffs
      in
      Stubs.linear res (Array.of_list poly_polylen_coeff) nb_polys ;
      res

  let linear_with_powers polys coeff =
    let nb_polys = List.length polys in
    let polys = List.map (fun p -> (p, length p)) polys in
    let res_size =
      List.fold_left (fun res_size (_p, size) -> max size res_size) 0 polys
    in
    let res = allocate res_size in
    Stubs.linear_with_powers res coeff (Array.of_list polys) nb_polys ;
    res

  let opposite p =
    let n = length p in
    let res = allocate n in
    Stubs.negate res p n ;
    res

  let opposite_inplace p =
    let n = length p in
    Stubs.negate p p n

  let is_zero p = if degree p = -1 then true else false

  let truncate ~len p =
    if len < 0 then
      raise (Invalid_argument "truncate: expected positive length.")
    else
      (* [min_len_capacity p] returns the minimum of [len] and [degree p + 1].
         Here, [degree p + 1] is the minimal length of the {!type:Carray.t}
         representing the polynomial [p].
         When [p] is the zero polynomial its degree is -1, so we return 1 for
         one coefficient holding the value. *)
      let min_len_capacity p =
        if is_zero p then 1 else min len (degree p + 1)
      in
      Fr_carray.copy ~len:(min_len_capacity p) p

  let evaluate p scalar =
    let n = length p in
    let res = Fr.copy scalar in
    Stubs.evaluate res p n scalar ;
    res

  exception Rest_not_null of string

  let division_xn p n c =
    assert (n > 0) ;
    let poly_degree = degree p in
    let poly_size = poly_degree + 1 in
    if poly_degree = -1 || poly_degree < n then (zero, p)
    else
      let res_q = allocate (poly_size - n) in
      let res_r = allocate n in
      Stubs.division_xn res_q res_r p poly_size (n, c) ;
      let poly_q = res_q in
      let poly_r = res_r in
      (poly_q, poly_r)

  let mul_xn p n c =
    let l = length p in
    let res = allocate (l + n) in
    Stubs.mul_xn res p l n c ;
    res

  let derivative p =
    let n = length p in
    if is_zero p || n = 1 then zero
    else
      let res = allocate (n - 1) in
      Stubs.derivative res p n ;
      res

  (* for p polynomial, returns p splitted in nb_chunks parts of degree size_chunks ; the nb_chunks - 1 first parts have degree size_chunks or less (if it’s less the next parts are 0) ; the last part’s degree will contain the rest of p coefficients without any degree bound *)
  let split ~nb_chunks size_chunks p =
    let poly_degree = degree p in
    let nb_coeff_P = 1 + poly_degree in
    if poly_degree = -1 then List.init nb_chunks (fun _ -> zero)
    else
      List.init nb_chunks (fun i ->
          if (i + 1) * size_chunks <= nb_coeff_P then
            if i = nb_chunks - 1 then Fr_carray.copy ~offset:(i * size_chunks) p
            else Fr_carray.copy ~offset:(i * size_chunks) ~len:size_chunks p
          else if i * size_chunks < nb_coeff_P then
            Fr_carray.copy
              ~offset:(i * size_chunks)
              ~len:(nb_coeff_P - (i * size_chunks))
              p
          else zero)

  let blind ~nb_blinds n p =
    let blinding_factor = random nb_blinds in
    (add p (mul_xn blinding_factor n Fr.(negate one)), blinding_factor)

  let ( = ) = equal

  let ( + ) = add

  let ( - ) = sub

  let ( * ) = mul

  let constant c = of_coefficients [(c, 0)]

  let fold_left_map = Fr_carray.fold_left_map
end

module type Polynomial_sig = sig
  (**
   This library implements polynomials of Bls12_381.Fr as arrays of contiguous
   memory in C, allowing much better performances for algorithms that scan the
   polynomials.

   An array [a] of size [n] represents the polynomial $\sum_i^(n-1) a[i] X^i$
   The length of [a] is always greater or equal than the degree+1 of its
   corresponding polynomial, if greater it padded with zeros. As a consequence a
   polynomial has many representations, namely all arrays with trailing zeros.
 *)

  type scalar

  type t [@@deriving repr]

  (** [init n f] returns a fresh polynomial of length [n], with element number [i]
      initialized to the result of [f i]. *)
  val init : int -> (int -> scalar) -> t

  (** [allocate len] creates a zero polynomial of size [len] *)
  val allocate : int -> t

  (** [erase p] overwrites a polynomial [p] with a zero polynomial of
      the same size as the polynomial [p] *)
  val erase : t -> unit

  (** [generate_biased_random_polynomial n] generates a random polynomial of
       degree strictly lower than [n], the distribution is NOT uniform, it is
       biased towards sparse polynomials and particularly towards the zero
       polynomial *)
  val generate_biased_random_polynomial : int -> t

  (** [random n] generates a uniformly sampled polynomial among the set of all
      polynomials of degree strictly lower than [n] *)
  val random : int -> t

  (** [degree p] returns the degree of a polynomial [p]. Returns [-1] for the
  zero polynomial *)
  val degree : t -> int

  (** [get p i] returns the [i]-th element of a given array [p], a coefficient of [X^i]
  in [p] *)
  val get : t -> int -> scalar

  (** [to_string p] returns the string representation of a polynomial [p] *)
  val to_string : t -> string

  (** [copy p] returns a copy of a polynomial [p] *)
  val copy : t -> t

  (** [truncate ~len p] returns a new polynomial made of the first [len]
      coefficients of [p]. If [degree p + 1] is less than [len] then
      [copy p] is returned.

      @raise [Invalid_argument] if [len] is negative. *)
  val truncate : len:int -> t -> t

  (** [to_dense_coefficients p] returns the dense representation of
  a polynomial [p], i.e., it converts a C array to an OCaml array *)
  val to_dense_coefficients : t -> scalar array

  (** [of_dense p] creates a value of type [t] from the dense representation of
  a polynomial [p], i.e., it converts an OCaml array to a C array *)
  val of_dense : scalar array -> t

  (** [of_coefficients p] creates a value of type [t] from the sparse representation of
  a polynomial [p], i.e., it converts an OCaml array to a C array *)
  val of_coefficients : (scalar * int) list -> t

  (** [equal a b] checks whether a polynomial [a] is equal to a polynomial [b] *)
  val equal : t -> t -> bool

  (** [is_zero p] checks whether a polynomial [p] is the zero polynomial *)
  val is_zero : t -> bool

  (** [zero] is the zero polynomial, the neutral element for polynomial addition *)
  val zero : t

  (** [one] is the constant polynomial one, the neutral element for polynomial
  multiplication *)
  val one : t

  (** [add] computes polynomial addition *)
  val add : t -> t -> t

  (** [add_inplace res a b] computes polynomial addition of [a] and [b] and
      writes the result in [res]

  Note: [res] can be equal to either [a] or [b] *)
  val add_inplace : t -> t -> t -> unit

  (** [sub] computes polynomial subtraction *)
  val sub : t -> t -> t

  (** [sub_inplace res a b] computes polynomial subtraction of [a] and [b] and
      writes the result in [res]

  Note: [res] can be equal to either [a] or [b] *)
  val sub_inplace : t -> t -> t -> unit

  (** [mul] computes polynomial multiplication

  Note: naive quadratic algorithm, result's size is the sum of arguments' size *)
  val mul : t -> t -> t

  (** [mul_by_scalar] computes multiplication of a polynomial by a blst_fr element *)
  val mul_by_scalar : scalar -> t -> t

  (** [mul_by_scalar_inplace res s p] computes multiplication of a polynomial [p]
  by a blst_fr element [s] and stores it in [res] *)
  val mul_by_scalar_inplace : t -> scalar -> t -> unit

  (** [linear p s] computes [∑ᵢ s.(i)·p.(i)] *)
  val linear : t list -> scalar list -> t

  (** [linear_with_powers p s] computes [∑ᵢ sⁱ·p.(i)]. This function is more efficient
      than [linear] + [powers] *)
  val linear_with_powers : t list -> scalar -> t

  (** [opposite] computes polynomial negation *)
  val opposite : t -> t

  (** [opposite_inplace p] computes polynomial negation

  Note: The argument [p] is overwritten *)
  val opposite_inplace : t -> unit

  (** [evaluate p x] evaluates a polynomial [p] at [x] *)
  val evaluate : t -> scalar -> scalar

  exception Rest_not_null of string

  (** [division_xn p n c] returns the quotient and remainder of the division of
      [p] by [(X^n + c)] *)
  val division_xn : t -> int -> scalar -> t * t

  (** [mul_xn p n c] returns the product of [p] and [(X^n + c)] *)
  val mul_xn : t -> int -> scalar -> t

  (** [derivative p] returns the formal derivative of [p] *)
  val derivative : t -> t

  val split : nb_chunks:int -> int -> t -> t list

  (** [blind ~nb_blinds n p] adds to polynomial [p] a random multiple of
      polynomial [(X^n - 1)], chosen by uniformly sampling a polynomial [b]
      of degree strictly lower than [nb_blinds] and multiplying it by
      [(X^n - 1)], [b] is returned as the second argument *)
  val blind : nb_blinds:int -> int -> t -> t * t

  (** Infix operator for {!equal} *)
  val ( = ) : t -> t -> bool

  (** Infix operator for {!add} *)
  val ( + ) : t -> t -> t

  (** Infix operator for {!sub} *)
  val ( - ) : t -> t -> t

  (** Infix operator for {!mul} *)
  val ( * ) : t -> t -> t

  (** [constant s] creates a value of type [t] from a blst_fr element [s] *)
  val constant : scalar -> t

  (** [fold_left_map] is a combination of fold_left and map that threads an
    accumulator through calls to [f]. *)
  val fold_left_map : ('acc -> scalar -> 'acc * scalar) -> 'acc -> t -> 'acc * t
end

module type Polynomial_unsafe_sig = sig
  include Polynomial_sig

  (** [to_carray p] converts [p] from type {!t} to type {!Fr_carray.t}

      Note: [to_carray p] doesn't create a copy of [p] *)
  val to_carray : t -> Fr_carray.t

  (** [of_carray p] converts [p] from type {!Fr_carray.t} to type {!t}

      Note: [of_carray p] doesn't create a copy of [p] *)
  val of_carray : Fr_carray.t -> t

  (** [copy_carray ?offset ?len p] returns a polynomial made of [len] contiguous
      coefficients starting from the coefficient of index [offset].
      By default, [offset = 0] and [len = length p - offset].

      @raise [Invalid_argument] if [offset] is not in the range 0 to [(length p - 1)],
      or if [len] is not positive, or if [offset + length] is not in the range 0 to
      [(length p - 1)]. *)
  val copy_carray : ?offset:int -> ?len:int -> t -> t

  (** [length p] returns the length of the underlying {!Fr_carray.t}. *)
  val length : t -> int
end

module Polynomial_unsafe :
  Polynomial_unsafe_sig with type scalar = Bls12_381.Fr.t =
  Polynomial_impl

include (
  Polynomial_unsafe :
    Polynomial_sig
      with type scalar = Polynomial_unsafe.scalar
       and type t = Polynomial_unsafe.t)
