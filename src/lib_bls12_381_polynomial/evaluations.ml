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

  (** [add res a b size_a size_b] writes the result of polynomial addition
      of [a] and [b] using the evaluation representation in [res], where
      - [a] is evaluated on [domain_a] of size [size_a]
      - [b] is evaluated on [domain_b] of size [size_b]

   requires:
   - [size a = size_a]
   - [size b = size_b]
   - [size res = min (size_a, size_b)]
   - [res], [a] and [b] are either pairwise disjoint or equal
   - [size_b mod size_a = 0] *)
  external add : fr_array -> fr_array -> fr_array -> int -> int -> unit
    = "caml_bls12_381_polynomial_polynomial_evaluations_add_stubs"
    [@@noalloc]

  (** [mul_arrays res eval_evallen_comp_power_powlen size_res nb_evals] writes
      the result of computing [p₁(gᶜ₁·x)ᵐ₁·p₂(gᶜ₂·x)ᵐ₂·…·pₖ(gᶜₖ·x)ᵐₖ] using
      the evaluation representation in [res], where
      - [eval_evallen_comp_power_powlen.[i] = (pᵢ, size_p_i, cᵢ, mᵢ, size_bits_m_i)]
      - a polynomial [pᵢ] is evaluated on [domain_p_i] of size [size_p_i]
      - [cᵢ] is a composition_gx; it computes [pᵢ(gᶜᵢ·x)] instead of [pᵢ(x)],
      where [g] is a primitive [size_p_i]-th root of unity
      - [mᵢ] is a power in [pᵢ(x)ᵐᵢ]
      - [size_bits_m_i] is the *exact* number of bits in [mᵢ]
      - [nb_evals] is a number of evaluations, i.e., [i = 1..nb_evals]

   requires:
   - [size res = size_res]
   - [size eval_evallen_comp_power_powlen = nb_evals]
   - [size p_i = size_p_i]
   - [size_bits m_i = size_bits_m_i]
   - [size_p_i mod size_res = 0]
   - [res] and [p_i] are disjoint *)
  external mul_arrays :
    fr_array ->
    (fr_array * int * int * Bytes.t * int) array ->
    int ->
    int ->
    unit = "caml_bls12_381_polynomial_polynomial_evaluations_mul_arrays_stubs"
    [@@noalloc]

  (** [linear_arrays res eval_evallen_coeff_comp add_constant size_res nb_evals]
      writes  the result of computing
      [λ₁·p₁(gᶜ₁·x) + λ₂·p₂(gᶜ₂·x) + … + λₖ·pₖ(gᶜₖ·x) + add_constant] using
      the evaluation representation in [res], where
      - [eval_evallen_coeff_comp.[i] = (pᵢ, size_p_i, λᵢ, cᵢ)]
      - a polynomial [pᵢ] is evaluated on [domain_p_i] of size [size_p_i]
      - [cᵢ] is a composition_gx; it computes [pᵢ(gᶜᵢ·x)] instead of [pᵢ(x)],
      where [g] is a primitive [size_p_i]-th root of unity
      - [λᵢ] is a coefficient in [λᵢ·pᵢ(x)]
      - [nb_evals] is a number of evaluations, i.e., [i = 1..nb_evals]

   requires:
   - [size res = size_res]
   - [size eval_evallen_coeff_comp = nb_evals]
   - [size p_i = size_p_i]
   - [size_p_i mod size_res = 0]
   - [res] and [p_i] are disjoint *)
  external linear_arrays :
    fr_array -> (fr_array * int * fr * int) array -> fr -> int -> int -> unit
    = "caml_bls12_381_polynomial_polynomial_evaluations_linear_arrays_stubs"
    [@@noalloc]

  (** [fft_inplace p domain log log_degree] computes Fast Fourier Transform.
  It converts the coefficient representation of a polynomial [p] to
  the evaluation representation

  requires:
  - [size p = size domain]
  - [size domain = 2^log]
  - [domain = [one; g; ..; g^{n-1}]] where [g] is a primitive
  [n]-th root of unity and [n = 2^log] (as done by {!Domain.Stubs.compute_domain}) *)
  external fft_inplace :
    fr_array -> domain:fr_array -> log:int -> log_degree:int -> unit
    = "caml_bls12_381_polynomial_fft_inplace_on_stubs"

  (** [ifft_inplace p domain log] computes Inverse Fast Fourier Transform.
  It converts the evaluation representation of a polynomial [p] to
  the coefficient representation

  requires:
  - [size p = size domain]
  - [size domain = 2^log]
  - [domain = [one; g; ..; g^{n-1}]] where [g] is a primitive
  [n]-th root of unity and [n = 2^log] (as done by {!Domain.Stubs.compute_domain}) *)
  external ifft_inplace : fr_array -> domain:fr_array -> log:int -> unit
    = "caml_bls12_381_polynomial_ifft_inplace_on_stubs"
    [@@noalloc]

  (** [dft_inplace coefficients domain inverse length] computes the Fourier Transform.

  requires:
  - [size domain = size coefficients = length]
  - [length <= 2^10]
  - [length != 2^k]
  - if [inverse = true] then the inverse dft is performed
  - [domain = [one; g; ..; g^{n-1}]] where [g] is a primitive
  [n]-th root of unity (as done by {!Domain.Stubs.compute_domain}) *)
  external dft_inplace : fr_array -> fr_array -> bool -> int -> unit
    = "caml_bls12_381_polynomial_dft_stubs"
    [@@noalloc]

  (** [fft_prime_factor_algorithm_inplace coefficient (domain1, domain1_length) (domain2, domain2_length) inverse]
  computes the Fast Fourier Transform following
  {{: https://en.wikipedia.org/wiki/Prime-factor_FFT_algorithm }the Prime-factor FFT algorithm}.

  requires:
  - [size domain1 = domain1_length]
  - [size domain2 = domain2_length]
  - [size domain1] and [size domain2] to be coprime
  - if for some k [size domain1 != 2^k] then [size domain1 <= 2^10]
  - if for some k [size domain2 != 2^k] then [size domain2 <= 2^10]
  - [size coefficients = domain1_length * domain2_length]
  - if [inverse = true] then the inverse fft is performed
  - [domain = [one; g; ..; g^{n-1}]] where [g] is a primitive
  [n]-th root of unity (as done by {!Domain.Stubs.compute_domain}) *)
  external fft_prime_factor_algorithm_inplace :
    fr_array -> fr_array * int -> fr_array * int -> bool -> unit
    = "caml_bls12_381_polynomial_prime_factor_algorithm_fft_stubs"
    [@@noalloc]
end

module type Evaluations_sig = sig
  type scalar

  type polynomial

  type t [@@deriving repr]

  (** [init size f degree] initializes an evaluation of a polynomial of the given
      [degree]. *)
  val init : int -> (int -> scalar) -> degree:int -> t

  (** [of_array (d, e)] creates a value of type [t] from the evaluation
   representation of a polynomial [e] of degree [d], i.e, it converts an OCaml
   array to a C array *)
  val of_array : int * scalar array -> t

  (** [to_array] converts a C array to an OCaml array *)
  val to_array : t -> scalar array

  (** [string_of_eval e] returns the string representation of evaluation [e] *)
  val string_of_eval : t -> string

  type domain

  (** [of_domain d] converts [d] from type {!domain} to type {!t}.

  Note: [of_domain d] doesn't create a copy of [d] *)
  val of_domain : domain -> t

  (** [to_domain d] converts [d] from type {!t} to type {!domain}.

  Note: [to_domain d] doesn't create a copy of [d] *)
  val to_domain : t -> domain

  (** [zero] returns the evaluation representation of the zero polynomial *)
  val zero : t

  (** [is_zero p] checks whether a polynomial [p] is the zero polynomial *)
  val is_zero : t -> bool

  (** [degree] returns the degree of a polynomial. Returns [-1] for the zero polynomial *)
  val degree : t -> int

  (** [length e] returns the size of domain where a polynomial is evaluated, or equally,
  the size of a C array where evaluation [e] is stored *)
  val length : t -> int

  (** [create len] returns the evaluation representation of a zero polynomial of size [len] *)
  val create : int -> t

  (** [copy ?res a] returns a copy of evaluation [a]. The function writes the result in [res]
   if [res] has the correct size and allocates a new array for the result otherwise *)
  val copy : ?res:t -> t -> t

  (** [get p i] returns the [i]-th element of a given array [p] *)
  val get : t -> int -> scalar

  (** [get_inplace p i res] copies the [i]-th element of a given array [p] in res *)
  val get_inplace : t -> int -> scalar -> unit

  (** [mul_by_scalar] computes muliplication of a polynomial by a blst_fr element *)
  val mul_by_scalar : scalar -> t -> t

  (** [mul_c] computes [p₁(gᶜ₁·x)ᵐ₁·p₂(gᶜ₂·x)ᵐ₂·…·pₖ(gᶜₖ·x)ᵐₖ], where
   - [pᵢ = List.nth evaluations i]
   - [mᵢ = List.nth powers i]
   - [cᵢ = List.nth (fst composition_gx) i]
   - [n = snd composition_gx] is the order of generator, i.e., [gⁿ = 1]

  The function writes the result in [res] if [res] has the correct size (= min (size pᵢ))
  and allocates a new array for the result otherwise

  Note: [res] and [pᵢ] are disjoint *)
  val mul_c :
    ?res:t ->
    evaluations:t list ->
    ?composition_gx:int list * int ->
    ?powers:int list ->
    unit ->
    t

  (** [linear_c] computes [λ₁·p₁(gᶜ₁·x) + λ₂·p₂(gᶜ₂·x) + … + λₖ·pₖ(gᶜₖ·x) + add_constant], where
   - [pᵢ = List.nth evaluations i]
   - [λᵢ = List.nth linear_coeffs i]
   - [cᵢ = List.nth (fst composition_gx) i]
   - [n = snd composition_gx] is the order of generator, i.e., [gⁿ = 1]

  The function writes the result in [res] if [res] has the correct size (= min (size pᵢ))
  and allocates a new array for the result otherwise

  Note: [res] and [pᵢ] are disjoint *)
  val linear_c :
    ?res:t ->
    evaluations:t list ->
    ?linear_coeffs:scalar list ->
    ?composition_gx:int list * int ->
    ?add_constant:scalar ->
    unit ->
    t

  (** [linear_with_powers p s] computes [∑ᵢ sⁱ·p.(i)]. This function is more efficient
      than [linear] + [powers] for evaluations of the same size *)
  val linear_with_powers : t list -> scalar -> t

  (** [add ?res a b] computes polynomial addition of [a] and [b]. The function writes
   the result in [res] if [res] has the correct size (= min (size (a, b))) and allocates
   a new array for the result otherwise

  Note: [res] can be equal to either [a] or [b] *)
  val add : ?res:t -> t -> t -> t

  (** [equal a b] checks whether a polynomial [a] is equal to a polynomial [b]

  Note: [equal] is defined as restrictive equality, i.e., the same polynomial
  evaluated on different domains are said to be different *)
  val equal : t -> t -> bool

  (** [evaluation_fft domain p] converts the coefficient representation of
  a polynomial [p] to the evaluation representation. [domain] can be obtained
  using {!Domain.build}

  Note:
  - size of domain must be a power of two
  - degree of polynomial must be strictly less than the size of domain *)
  val evaluation_fft : domain -> polynomial -> t

  (** [interpolation_fft domain p] converts the evaluation representation of
  a polynomial [p] to the coefficient representation. [domain] can be obtained
  using {!Domain.build}

  Note:
  - size of domain must be a power of two
  - size of a polynomial must be equal to size of domain *)
  val interpolation_fft : domain -> t -> polynomial

  (* TODO DELETE *)
  val interpolation_fft2 : domain -> scalar array -> polynomial

  (** [dft domain polynomial] converts the coefficient representation of
  a polynomial [p] to the evaluation representation. [domain] can be obtained
  using {!Domain.build}.

  Computes the discrete Fourier transform in time O(n^2)
  where [n = size domain].

  requires:
  - [size domain] to divide Bls12_381.Fr.order - 1
  - [size domain != 2^k]
  - [degree polynomial < size domain] *)
  val dft : domain -> polynomial -> t

  (** [idft domain t] converts the evaluation representation of
  a polynomial [p] to the coefficient representation. [domain] can be obtained
  using {!Domain.build}.

  Computes the inverse discrete Fourier transform in time O(n^2)
  where [n = size domain].

  requires:
  - [size domain] to divide Bls12_381.Fr.order - 1
  - [size domain != 2^k]
  - [size domain = size t] *)
  val idft : domain -> t -> polynomial

  (** [evaluation_fft_prime_factor_algorithm domain1 domain2 p] converts the
  coefficient representation of a polynomial [p] to the evaluation representation.
  [domain] can be obtained using {!Domain.build}.
  See {{: https://en.wikipedia.org/wiki/Prime-factor_FFT_algorithm }the Prime-factor FFT algorithm}.

  requires:
  - [size domain1 * size domain2] to divide Bls12_381.Fr.order - 1
  - [size domain1] and [size domain2] to be coprime
  - if for some k [size domain1 != 2^k] then [size domain1 <= 2^10]
  - if for some k [size domain2 != 2^k] then [size domain2 <= 2^10]
  - [degree polynomial < size domain1 * size domain2] *)
  val evaluation_fft_prime_factor_algorithm :
    domain1:domain -> domain2:domain -> polynomial -> t

  (** [interpolation_fft_prime_factor_algorithm domain1 domain2 t] converts the
  evaluation representation of a polynomial [p] to the coefficient representation.
  [domain] can be obtained using {!Domain.build}.
  See {{: https://en.wikipedia.org/wiki/Prime-factor_FFT_algorithm }the Prime-factor FFT algorithm}.

  requires:
  - [size domain1 * size domain2] to divide Bls12_381.Fr.order - 1
  - [size domain1] and [size domain2] to be coprime
  - if for some k [size domain1 != 2^k] then [size domain1 <= 2^10]
  - if for some k [size domain2 != 2^k] then [size domain2 <= 2^10]
  - [size t = size domain1 * size domain2] *)
  val interpolation_fft_prime_factor_algorithm :
    domain1:domain -> domain2:domain -> t -> polynomial
end

module Evaluations_impl = struct
  module Domain_c = Domain.Stubs
  module Domain = Domain.Domain_unsafe
  module Polynomial_c = Polynomial.Stubs (* TODO remove *)
  module Polynomial = Polynomial.Polynomial_unsafe

  type scalar = Fr.t

  type polynomial = Polynomial.t

  (* degree & evaluations *)
  type t = int * Fr_carray.t [@@deriving repr]

  type domain = Domain.t

  let init size f ~degree = (degree, Fr_carray.init size f)

  let of_array (d, p) =
    if d < -1 then
      raise @@ Invalid_argument "make_evaluation: degree must be >= -1" ;
    if Array.length p <= d then
      raise
      @@ Invalid_argument "make_evaluation: array must be longer than degree" ;
    let res = Fr_carray.of_array p in
    (d, res)

  let to_array (_d, e) = Fr_carray.to_array e

  let to_carray (_, e) = e

  let string_of_eval (d, e) =
    Printf.sprintf "%d : [%s]" d Polynomial.(to_string (of_carray e))

  let of_domain domain =
    let d = Domain.to_carray domain in
    (1, d)

  let allocate = Fr_carray.allocate

  let to_domain (_, eval) = Domain.of_carray eval

  let zero = (-1, allocate 1)

  let degree (d, _) = d

  let length (_, e) = Fr_carray.length e

  let create n = (-1, allocate n)

  let is_zero (d, _e) =
    (* if a degree is not included in the definition of evaluations,
       use Fr_carray.Stubs.is_zero e l *)
    d = -1

  let allocate_for_res res length_result =
    match res with
    | Some (_, res) when Fr_carray.length res = length_result -> res
    | _ -> allocate length_result

  let copy ?res (d, e) =
    let len = Fr_carray.length e in
    let res = allocate_for_res res len in
    Fr_carray.blit e ~src_off:0 res ~dst_off:0 ~len ;
    (d, res)

  let get (_, eval) i = Fr_carray.get eval i

  let get_inplace (_, eval) i res = Fr_carray.get_inplace eval i res

  let mul_by_scalar lambda (d, e) =
    let len = Fr_carray.length e in
    let res = allocate len in
    Polynomial_c.mul_by_scalar res lambda e len ;
    (d, res)

  (* multiplies evaluations of all polynomials with name in poly_names,
     the resulting eval has the size of the smallest evaluation *)
  let mul_c ?res ~evaluations
      ?(composition_gx = (List.init (List.length evaluations) (fun _ -> 0), 1))
      ?(powers = List.init (List.length evaluations) (fun _ -> 1)) () =
    let domain_len = snd composition_gx in
    assert (domain_len > 0) ;
    assert (List.compare_length_with evaluations 0 > 0) ;
    assert (List.compare_lengths (fst composition_gx) evaluations = 0) ;
    assert (List.compare_lengths powers evaluations = 0) ;
    assert (List.for_all (fun power -> power > 0) powers) ;

    let length_result =
      List.fold_left min Int.max_int @@ List.map length evaluations
    in
    let res = allocate_for_res res length_result in

    if List.exists is_zero evaluations then (
      Fr_carray.erase res length_result ;
      (-1, res))
    else
      let degree_result =
        List.fold_left2
          (fun acc d pow -> acc + (d * pow))
          0
          (List.map degree evaluations)
          powers
      in
      if degree_result >= length_result then
        raise
          (Invalid_argument
             (Printf.sprintf
                "mul_evaluations : evaluation is too short (length=%d) for \
                 expected result size %d"
                length_result
                (degree_result + 1)))
      else
        let list_array =
          List.map2
            (fun (evaluation, pow) composition ->
              let pow = Z.of_int pow in
              let pow_bytes = Z.to_bits pow |> Bytes.unsafe_of_string in
              let pow_len = Z.numbits pow in
              let l = length evaluation in
              let rescale_composition = composition * l / domain_len in
              (snd evaluation, l, rescale_composition, pow_bytes, pow_len))
            (List.combine evaluations powers)
            (fst composition_gx)
        in
        let nb_evals = List.length evaluations in
        Stubs.mul_arrays res (Array.of_list list_array) length_result nb_evals ;
        (degree_result, res)

  let constant p c =
    for i = 0 to Fr_carray.length p - 1 do
      Fr_carray.set p c i
    done

  (* Adds evaluation of a1 × p1 + a2 × p2 in evaluations
     /!\ the degree may not be always accurate,
         the resulting degree may not be the max of the 2 polynomials degrees *)
  let linear_c ?res ~evaluations
      ?(linear_coeffs =
        List.init (List.length evaluations) (fun _ -> Fr.(copy one)))
      ?(composition_gx = (List.init (List.length evaluations) (fun _ -> 0), 1))
      ?(add_constant = Fr.zero) () =
    let domain_len = snd composition_gx in
    assert (domain_len > 0) ;
    assert (List.compare_length_with evaluations 0 > 0) ;
    assert (List.compare_lengths linear_coeffs evaluations = 0) ;
    assert (List.compare_lengths (fst composition_gx) evaluations = 0) ;

    let list_eval_coeff_composition =
      List.map2
        (fun (eval, coeff) composition ->
          let rescale_composition = composition * length eval / domain_len in
          (eval, coeff, rescale_composition))
        (List.combine evaluations linear_coeffs)
        (fst composition_gx)
      |> List.filter (fun (eval, _, _) -> not (is_zero eval))
    in

    if List.compare_length_with list_eval_coeff_composition 0 = 0 then (
      let length_result =
        List.fold_left min Int.max_int @@ List.map length evaluations
      in
      let res = allocate_for_res res length_result in
      constant res add_constant ;
      let degree_result = if Fr.is_zero add_constant then -1 else 0 in
      (degree_result, res))
    else
      let length_result =
        List.fold_left min Int.max_int
        @@ List.map
             (fun (eval, _, _) -> length eval)
             list_eval_coeff_composition
      in
      let degree_result =
        List.fold_left max 0
        @@ List.map
             (fun (eval, _, _) -> degree eval)
             list_eval_coeff_composition
      in
      (* TODO: check relation between length_result and degree_result? *)
      let nb_evals = List.length list_eval_coeff_composition in
      let array_eval_coeff_composition =
        List.map
          (fun (eval, linear_coeff, composition) ->
            (snd eval, length eval, linear_coeff, composition))
          list_eval_coeff_composition
        |> Array.of_list
      in
      let res = allocate_for_res res length_result in
      Stubs.linear_arrays
        res
        array_eval_coeff_composition
        add_constant
        length_result
        nb_evals ;
      (degree_result, res)

  (* Adds 2 evaluations *)
  let add ?res e1 e2 =
    let d1 = fst e1 in
    let d2 = fst e2 in
    let l1 = length e1 in
    let l2 = length e2 in
    if d1 = -1 then
      let res = allocate_for_res res l2 in
      copy ~res:(d2, res) e2
    else if d2 = -1 then
      let res = allocate_for_res res l1 in
      copy ~res:(d1, res) e1
    else
      let deg_result = max d1 d2 in
      let length_result = min l1 l2 in
      let res = allocate_for_res res length_result in
      Stubs.add res (snd e1) (snd e2) l1 l2 ;
      (deg_result, res)

  let linear_with_powers evals coeff =
    assert (List.compare_length_with evals 0 > 0) ;
    let nb_evals = List.length evals in
    let eval_lenghts = List.map length evals in
    let eval0_length = List.hd eval_lenghts in
    let is_equal_size = List.for_all (Int.equal eval0_length) eval_lenghts in
    if is_equal_size then (
      let length_result = eval0_length in
      let degree_result = List.fold_left max (-1) @@ List.map degree evals in
      if degree_result = -1 then create length_result
      else
        let res = allocate length_result in
        let evals =
          List.map (fun (_, e) -> (e, Fr_carray.length e)) evals
          |> Array.of_list
        in
        Polynomial_c.linear_with_powers res coeff evals nb_evals ;
        (degree_result, res))
    else
      let coeffs = Fr_carray.powers nb_evals coeff |> Array.to_list in
      linear_c ~evaluations:evals ~linear_coeffs:coeffs ()

  (*restrictive equality, the same polynomial evaluated
    on different domains are said to be different*)
  let equal (deg1, eval1) (deg2, eval2) =
    if deg1 <> deg2 || Fr_carray.(length eval1 <> length eval2) then false
    else Polynomial.(equal (of_carray eval1) (of_carray eval2))

  let evaluation_fft_internal : Domain.t -> polynomial -> Fr_carray.t =
   fun domain poly ->
    let degree = Polynomial.degree poly in
    let log_degree = Z.log2up (Z.of_int (degree + 1)) in
    let domain = Domain.to_carray domain in
    let n_domain = Fr_carray.length domain in
    let poly = Polynomial.to_carray poly in
    let log = Z.(log2up @@ of_int n_domain) in
    if not (Helpers.is_power_of_two n_domain) then
      raise @@ Invalid_argument "Size of domain should be a power of 2." ;
    if not (degree < n_domain) then
      raise
      @@ Invalid_argument
           "Degree of poly should be strictly less than domain size." ;
    let res = allocate n_domain in
    Fr_carray.blit poly ~src_off:0 res ~dst_off:0 ~len:(degree + 1) ;
    Stubs.fft_inplace res ~domain ~log ~log_degree ;
    res

  let evaluation_fft : domain -> polynomial -> t =
   fun domain poly ->
    let d = Polynomial.degree poly in
    let domain_length = Domain.length domain in
    if d = -1 then (d, allocate domain_length)
    else
      let res = evaluation_fft_internal domain poly in
      (d, res)

  let interpolation_fft_internal : Domain.t -> Fr_carray.t -> polynomial =
   fun domain coefficients ->
    let domain = Domain.to_carray domain in
    let n_domain = Fr_carray.length domain in
    let log = Z.(log2up @@ of_int n_domain) in
    if not (Helpers.is_power_of_two n_domain) then
      raise @@ Invalid_argument "Size of domain should be a power of 2." ;
    let n_coefficients = Fr_carray.length coefficients in
    if not (n_coefficients = n_domain) then
      raise @@ Invalid_argument "Size of coefficients should be same as domain." ;
    Stubs.ifft_inplace coefficients ~domain ~log ;
    Polynomial.of_carray coefficients

  let interpolation_fft : domain -> t -> polynomial =
   fun domain (d, evaluation) ->
    if d = -1 then Polynomial.zero
    else
      let length_res = Domain.length domain in
      let rescaled_eval = allocate length_res in
      Domain_c.rescale
        rescaled_eval
        evaluation
        length_res
        (Fr_carray.length evaluation) ;
      interpolation_fft_internal domain rescaled_eval

  let interpolation_fft2 : Domain.t -> scalar array -> polynomial =
   fun domain coefficients ->
    interpolation_fft_internal domain (Fr_carray.of_array coefficients)

  let dft domain polynomial =
    let length = Domain.length domain in
    if length > 1 lsl 10 then
      raise @@ Invalid_argument "Domain size must be <= 2^10." ;
    if Helpers.is_power_of_two length then
      raise @@ Invalid_argument "Domain size must not be a power of two" ;
    let d = Polynomial.degree polynomial in
    let polynomial = Polynomial.to_carray polynomial in
    if not (d < length) then
      raise
      @@ Invalid_argument
           "Degree of poly should be strictly less than domain size." ;
    let evaluations = allocate length in
    Fr_carray.blit polynomial ~src_off:0 evaluations ~dst_off:0 ~len:(d + 1) ;
    Stubs.dft_inplace evaluations (Domain.to_carray domain) false length ;
    (d, evaluations)

  let idft domain (_, evaluations) =
    let length = Domain.length domain in
    if length > 1 lsl 10 then
      raise @@ Invalid_argument "Domain size must be <= 2^10." ;
    if Helpers.is_power_of_two length then
      raise @@ Invalid_argument "Domain size must not be a power of two" ;
    if not (length = Fr_carray.length evaluations) then
      raise @@ Invalid_argument "Size of coefficients should be same as domain." ;
    let coefficients = Fr_carray.copy evaluations in
    Stubs.dft_inplace coefficients (Domain.to_carray domain) true length ;
    Polynomial.of_carray coefficients

  let evaluation_fft_prime_factor_algorithm ~domain1 ~domain2 polynomial =
    let domain1_length = Domain.length domain1 in
    let domain2_length = Domain.length domain2 in
    if
      (not (Helpers.is_power_of_two domain1_length))
      && domain1_length > 1 lsl 10
    then
      raise
      @@ Invalid_argument "Domain of non power of 2 length must be <= 2^10." ;
    if
      (not (Helpers.is_power_of_two domain2_length))
      && domain2_length > 1 lsl 10
    then
      raise
      @@ Invalid_argument "Domain of non power of 2 length must be <= 2^10." ;
    if not Z.(gcd (of_int domain1_length) (of_int domain2_length) = one) then
      raise @@ Invalid_argument "Size of domains must be coprime." ;
    let n_domain = domain1_length * domain2_length in
    let d = Polynomial.degree polynomial in
    let coefficients = Polynomial.to_carray polynomial in
    if not (d < n_domain) then
      raise
      @@ Invalid_argument
           "Degree of poly should be strictly less than domain size." ;
    let res = allocate n_domain in
    if d = -1 then (d, res)
    else
      let domain1 = Domain.to_carray domain1 in
      let domain2 = Domain.to_carray domain2 in
      Fr_carray.blit coefficients ~src_off:0 res ~dst_off:0 ~len:(d + 1) ;
      Stubs.fft_prime_factor_algorithm_inplace
        res
        (domain1, domain1_length)
        (domain2, domain2_length)
        false ;
      (d, res)

  let interpolation_fft_prime_factor_algorithm ~domain1 ~domain2 (d, evaluations)
      =
    let domain1_length = Domain.length domain1 in
    let domain2_length = Domain.length domain2 in
    if
      (not (Helpers.is_power_of_two domain1_length))
      && domain1_length > 1 lsl 10
    then
      raise
      @@ Invalid_argument "Domain of non power of 2 length must be <= 2^10." ;
    if
      (not (Helpers.is_power_of_two domain2_length))
      && domain2_length > 1 lsl 10
    then
      raise
      @@ Invalid_argument "Domain of non power of 2 length must be <= 2^10." ;
    if not Z.(gcd (of_int domain1_length) (of_int domain2_length) = one) then
      raise @@ Invalid_argument "Size of domains must be coprime." ;
    let n_domain = domain1_length * domain2_length in
    let n_evaluations = Fr_carray.length evaluations in
    if not (n_evaluations = n_domain) then
      raise @@ Invalid_argument "Size of coefficients should be same as domain." ;
    if d = -1 then Polynomial.zero
    else
      let domain1 = Domain.to_carray domain1 in
      let domain2 = Domain.to_carray domain2 in
      let coefficients = Fr_carray.copy evaluations in
      Stubs.fft_prime_factor_algorithm_inplace
        coefficients
        (domain1, domain1_length)
        (domain2, domain2_length)
        true ;
      Polynomial.of_carray coefficients
end

module type Evaluations_unsafe_sig = sig
  include Evaluations_sig

  (** [to_carray t] converts [t] from type {!t} to type {!Fr_carray.t}

      Note: [to_carray t] doesn't create a copy of [t] *)
  val to_carray : t -> Fr_carray.t
end

module Evaluations_unsafe :
  Evaluations_unsafe_sig
    with type scalar = Bls12_381.Fr.t
     and type domain = Domain.t
     and type polynomial = Polynomial.t =
  Evaluations_impl

include (
  Evaluations_unsafe :
    Evaluations_sig
      with type t = Evaluations_unsafe.t
       and type scalar = Evaluations_unsafe.scalar
       and type domain = Evaluations_unsafe.domain
       and type polynomial = Evaluations_unsafe.polynomial)
