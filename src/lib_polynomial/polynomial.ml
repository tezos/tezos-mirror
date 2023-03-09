(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
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

include Utils

type natural_with_infinity = Natural of int | Infinity

module type UNIVARIATE = sig
  (** The type of the polynomial coefficients. Can be a field or more generally
      a ring. For the moment, it is restricted to prime fields.
  *)
  type scalar

  (** Represents a polynomial *)
  type t

  (** Returns the polynomial [P(X) = 0] *)
  val zero : t

  (** Returns the polynomial [P(X) = 1] *)
  val one : t

  (** Returns the degree of the polynomial *)
  val degree : t -> natural_with_infinity

  val degree_int : t -> int

  (** [have_same_degree P Q] returns [true] if [P] and [Q] have the same
      degree
  *)
  val have_same_degree : t -> t -> bool

  (* (\** [shift_by_n P n] multiplies [P] by [X^n]. For instance,
   *     [P(X) = a_{0} + a_{1} X + ... + a_{m} X^m] will be transformed in
   *     [a_{0} X^{n} + a_{1} X^{n + 1} + ... a_{m} X^{n + m}].
   * *\)
   * val shift_by_n : t -> int -> t *)

  (** [get_dense_polynomial_coefficients P] returns the list of the
      coefficients of P, including the null coefficients, in decreasing order
      i.e. if P(X) = a_{0} + a_{1} X + ... + a_{n - 1} X^{n - 1}, the function
      will return [a_{n - 1}, ..., a_{0}]
  *)
  val get_dense_polynomial_coefficients : t -> scalar list

  (** [get_dense_polynomial_coefficients_with_degree P] returns the list of the
      coefficients of P with the degree as a tuple, including the null
      coefficients, in decreasing order
      i.e. if P(X) = a_{0} + a_{1} X + ... + a_{n - 1} X^{n - 1}, the function
      will return [(a_{n - 1}, n -1), ..., (a_{0}, 0)].
  *)
  val get_dense_polynomial_coefficients_with_degree : t -> (scalar * int) list

  (** [get_list_coefficients P] returns [(a_4,4), (a_2,2), (a_0,0)] if
      P = a_4 X^4 + a_2 X^2 + a_0*)
  val get_list_coefficients : t -> (scalar * int) list

  (** [evaluation P s] computes [P(s)]. Use Horner's method in O(n). *)
  val evaluation : t -> scalar -> scalar

  (** [constants s] returns the constant polynomial [P(X) = s] *)
  val constants : scalar -> t

  (** [add P Q] returns [P(X) + Q(X)] *)
  val add : t -> t -> t

  (** [sub P Q] returns [P(X) - Q(X)] *)
  val sub : t -> t -> t

  (** [mult_by_scalar s P] returns [s*P(X)] *)
  val mult_by_scalar : scalar -> t -> t

  (** [is_null P] returns [true] iff [P(X) = 0] *)
  val is_null : t -> bool

  (** [is_constant P] returns [true] iff [P(X) = s] for s scalar *)
  val is_constant : t -> bool

  (** [opposite P] returns [-P(X)] *)
  val opposite : t -> t

  (** [equal P Q] returns [true] iff [P(X) = Q(X)] on S *)
  val equal : t -> t -> bool

  (** [of_coefficients [(x_0, y_0) ; (x_1, y_1); ... ; (x_n ; y_n)]] builds the
      polynomial Σ(a_i * X^i) as a type [t].

      By default, the null coefficients will be removed as the internal
      representation of polynomials is sparsed. However, a version with null
      coefficients can be generated if required. It is not recommended to use
      this possibility as it breaks an invariant of the type [t].
  *)
  val of_coefficients : (scalar * int) list -> t

  (** [lagrange_interpolation [(x_0, y_0) ; (x_1, y_1); ... ; (x_n ; y_n)]]
      builds the unique polynomial P of degre n such that P(x_i) = y_i for i = 0...n
      using the intermediate lagrange polynomials. [lagrange_interpolation_fft] can
      be used in case of a FFT friendly scalar structure. It is supposed all x_i
      are different.
  *)
  val lagrange_interpolation : (scalar * scalar) list -> t

  (** [even_polynomial P] returns the polynomial P_even containing only the even
      coefficients of P *)
  val even_polynomial : t -> t

  (** [odd_polynomial P] returns the polynomial P_odd containing only the odd
      coefficients of P *)
  val odd_polynomial : t -> t

  (** [evaluate_fft_imperative ~domain P] evaluates P on the points given in the [domain].
      The domain should be of the form [g^{i}] where [g] is a principal root of
      unity. If the domain is of size [n], [g] must be a [n]-th principal root
      of unity.
      The degree of [P] can be smaller than the domain size. Larger polynomials
      can also be used but the implementation is not the most memory efficient
      yet and must be improved. The
      complexity is in [O(n log(m))] where [n] is the domain size and [m] the
      degree of the polynomial. When [m] is smaller than [n], the polynomial is
      padded with zeroes to reach [n] coefficients.
      The resulting list contains the evaluation points
      [P(1), P(w), ..., P(w^{n - 1})].
  *)
  val evaluation_fft : domain:scalar array -> t -> scalar list

  (** [generate_random_polynomial n] returns a random polynomial of degree [n] *)
  val generate_random_polynomial : natural_with_infinity -> t

  (** [get_highest_coefficient P] where [P(X) = a_n X^n + ... a_0] returns [a_n] *)
  val get_highest_coefficient : t -> scalar

  (** [interpolation_fft ~domain [y_{0} ; y_{1} ;
      ... y_{n}]] computes the interpolation at the points [y_{0}, ..., y_{n}]
      using FFT Cookey Tukey.
      The domain should be of the form [g^{i}] where [g] is a principal root of
      unity. If the domain is of size [n], [g] must be a [n]-th principal root
      of unity.
      The domain size must be exactly the same than the number of points. The
      complexity is [O(n log(n))] where [n] is the domain size.
  *)
  val interpolation_fft : domain:scalar array -> scalar list -> t

  (** [polynomial_multiplication P Q] computes the
      product P(X).Q(X) *)
  val polynomial_multiplication : t -> t -> t

  (** [polynomial_multiplication_fft ~domain P Q] computes the
      product [P(X).Q(X)] using FFT.
      The domain should be of the form [g^{i}] where [g] is a principal root of
      unity. If the domain is of size [n], [g] must be a [n]-th principal root
      of unity.
      The degrees of [P] and [Q] can be different. The only condition is
      [degree P + degree Q] should be smaller or equal to [n - 2] (i.e. the domain should
      be big enough to compute [n - 1] points of [P * Q]).
  *)
  val polynomial_multiplication_fft : domain:scalar array -> t -> t -> t

  val euclidian_division_opt : t -> t -> (t * t) option

  (** [extended_euclide P S] returns (GCD, U, V) the greatest common divisor of [P] and [S]
        and the Bezout's coefficient:
      [U P + V S = GCD] and [GCD] greatest coefficient is one
  *)
  val extended_euclide : t -> t -> t * t * t

  (** Infix operator for [equal] *)
  val ( = ) : t -> t -> bool

  (** Infix operator for [add] *)
  val ( + ) : t -> t -> t

  (** Infix operator for [polynomial_multiplication] *)
  val ( * ) : t -> t -> t

  (** Infix operator for [sub] *)
  val ( - ) : t -> t -> t

  val to_string : t -> string
end

module DomainEvaluation (R : Bls12_381.Ff_sig.PRIME) = struct
  type t = {size : int; generator : R.t; domain_values : R.t array}

  let generate_domain generator n =
    let rec aux previous acc i =
      if i = n then List.rev acc
      else
        let current = R.mul previous generator in
        aux current (current :: acc) (i + 1)
    in
    Array.of_list @@ aux R.one [R.one] 1

  let generate size generator =
    {size; generator; domain_values = generate_domain generator size}

  let _size d = d.size

  let _generator d = d.generator

  let domain_values d = d.domain_values
end

(* TODO: Functions should use DomainEvaluation *)
let generate_evaluation_domain (type a)
    (module Fp : Bls12_381.Ff_sig.PRIME with type t = a) size (generator : a) =
  let module D = DomainEvaluation (Fp) in
  let g = D.generate size generator in
  D.domain_values g

(* TODO: this function should be part of DomainEvaluation. However, for the
   moment, functions do not use this representation *)
let inverse_domain_values domain =
  let length_domain = Array.length domain in
  Array.init length_domain (fun i ->
      if i = 0 then domain.(i) else domain.(length_domain - i))

module MakeUnivariate (R : Bls12_381.Ff_sig.PRIME) = struct
  type scalar = R.t

  (* We encode the polynomials as a list with decreasing degree.
     Invariants to respect for the type:
     - all coefficients are non null.
     - [a_n * X^n + ... a_1 X + a0] is encoded as [a_n ; ... ; a_1 ; a_0] with [a_i]
     non zero for all [i], i.e. the monomials are given in decreasing order.
     - the zero polynomial is represented as the empty list.
  *)
  type t = (scalar * int) list

  let degree p =
    match p with
    | [] -> Infinity
    | [(e, 0)] -> if R.is_zero e then Infinity else Natural 0
    | _ as l -> Natural (snd (List.hd l))

  let degree_int p = match degree p with Infinity -> -1 | Natural n -> n

  let have_same_degree p q = degree p = degree q

  (* let shift_by_n p n =
   *   assert (n >= 1) ;
   *   List.map (fun (c, e) -> (c, e + n)) p *)

  let zero = []

  let one = [(R.one, 0)]

  let constants c = if R.eq c R.zero then [] else [(c, 0)]

  let is_null p = match p with [] -> true | _ -> false

  let is_constant p =
    match p with
    | [] -> true
    | l ->
        if List.compare_length_with l 1 > 0 then false
        else
          let _, p = List.hd l in
          if p = 0 then true else false

  let of_coefficients l =
    (* check if the powers are all positive *)
    assert (List.for_all (fun (_e, power) -> power >= 0) l) ;
    (* Remove null coefficients *)
    let l = List.filter (fun (e, _power) -> not (R.is_zero e)) l in
    (* sort by the power, higher power first *)
    let l =
      List.fast_sort
        (fun (_e1, power1) (_e2, power2) -> Int.sub power2 power1)
        l
    in
    l

  let add p1 p2 =
    let rec inner acc l1 l2 =
      match (l1, l2) with
      | [], l | l, [] -> List.rev_append acc l
      | l1, l2 ->
          let e1, p1 = List.hd l1 in
          let e2, p2 = List.hd l2 in
          if p1 = p2 && R.is_zero (R.add e1 e2) then
            inner acc (List.tl l1) (List.tl l2)
          else if p1 = p2 then
            inner ((R.add e1 e2, p1) :: acc) (List.tl l1) (List.tl l2)
          else if p1 > p2 then inner ((e1, p1) :: acc) (List.tl l1) l2
          else inner ((e2, p2) :: acc) l1 (List.tl l2)
    in
    let l = inner [] p1 p2 in
    of_coefficients l

  let mult_by_scalar a p =
    List.filter_map
      (fun (coef, power) ->
        let c = R.mul coef a in
        if R.is_zero c then None else Some (c, power))
      p

  let opposite poly = List.(rev (rev_map (fun (a, i) -> (R.negate a, i)) poly))

  let sub p1 p2 =
    let rec inner acc l1 l2 =
      match (l1, l2) with
      | [], l2 -> List.rev_append acc (opposite l2)
      | l1, [] -> List.rev_append acc l1
      | l1, l2 ->
          let e1, p1 = List.hd l1 in
          let e2, p2 = List.hd l2 in
          if p1 = p2 && R.is_zero (R.sub e1 e2) then
            inner acc (List.tl l1) (List.tl l2)
          else if p1 = p2 then
            inner ((R.sub e1 e2, p1) :: acc) (List.tl l1) (List.tl l2)
          else if p1 > p2 then inner ((e1, p1) :: acc) (List.tl l1) l2
          else inner ((R.negate e2, p2) :: acc) l1 (List.tl l2)
    in
    let l = inner [] p1 p2 in
    of_coefficients l

  let equal p1 p2 =
    if List.compare_lengths p1 p2 != 0 then false
    else List.for_all2 (fun (e1, n1) (e2, n2) -> n1 = n2 && R.eq e1 e2) p1 p2

  let get_list_coefficients p = p

  let get_dense_polynomial_coefficients polynomial =
    match polynomial with
    | [] -> [R.zero]
    | l ->
        let l = List.rev l in
        let rec to_dense acc current_i l =
          match l with
          | [] -> acc
          | (e, n) :: xs ->
              if n = current_i then to_dense (e :: acc) (current_i + 1) xs
              else to_dense (R.zero :: acc) (current_i + 1) l
        in
        to_dense [] 0 l

  let get_dense_polynomial_coefficients_with_degree polynomial =
    let n = degree_int polynomial in
    if n = -1 then [(R.zero, 0)]
    else
      let h_list = get_dense_polynomial_coefficients polynomial in
      let ffold (acc, i) a = ((a, i) :: acc, i - 1) in
      let res, _ = List.fold_left ffold ([], n) h_list in
      List.rev res

  let evaluation polynomial point =
    (* optimized_pow is used instead of Scalar.pow because Scalar.pow makes
       evaluation slower than the standard Horner algorithm when dif_degree <= 4 is
       involved.
       TODO: use memoisation
    *)
    let n = degree_int polynomial in
    let optimized_pow x = function
      | 0 -> R.one
      | 1 -> x
      | 2 -> R.square x
      | 3 -> R.(x * square x)
      | 4 -> R.(square (square x))
      | n -> R.pow x (Z.of_int n)
    in
    let aux (acc, prec_i) (a, i) =
      let dif_degree = prec_i - i in
      (R.((acc * optimized_pow point dif_degree) + a), i)
    in
    let res, last_degree = List.fold_left aux (R.zero, n) polynomial in
    R.(res * optimized_pow point last_degree)

  let assert_no_duplicate_point points =
    let points = List.map fst points in
    let points_uniq =
      List.sort_uniq (fun e1 e2 -> if R.eq e1 e2 then 0 else -1) points
    in
    assert (List.compare_lengths points points_uniq = 0)

  let intermediate_lagrange_interpolation x_i i xs =
    List.fold_left
      (fun acc (j, x_j) ->
        if i = j then acc
        else
          match acc with
          | [] -> []
          | acc ->
              let acc_1 = List.map (fun (e, p) -> (e, p + 1)) acc in
              let acc_2 = mult_by_scalar x_j (of_coefficients acc) in
              let acc = add acc_1 (opposite acc_2) in
              let scalar = R.inverse_exn R.(x_i + R.negate x_j) in
              let acc_final = mult_by_scalar scalar acc in
              acc_final)
      (constants R.one)
      xs

  let lagrange_interpolation points =
    assert_no_duplicate_point points ;
    let indexed_points = List.mapi (fun i (x_i, y_i) -> (i, x_i, y_i)) points in
    let evaluated_at = List.mapi (fun i (x_i, _) -> (i, x_i)) points in
    List.fold_left
      (fun acc (i, x_i, y_i) ->
        let l_i = intermediate_lagrange_interpolation x_i i evaluated_at in
        add acc (mult_by_scalar y_i l_i))
      []
      indexed_points

  let even_polynomial polynomial =
    match polynomial with
    | [] -> []
    | l -> List.filter (fun (_e, n) -> n mod 2 = 0) l

  let odd_polynomial polynomial =
    match polynomial with
    | [] -> []
    | l -> List.filter (fun (_e, n) -> n mod 2 = 1) l

  (* assumes that len(domain) = len(output) *)
  let evaluation_fft_in_place ~domain output =
    let n = Array.length output in
    let logn = Z.log2 (Z.of_int n) in
    let m = ref 1 in
    for _i = 0 to logn - 1 do
      let exponent = n / (2 * !m) in
      let k = ref 0 in
      while !k < n do
        for j = 0 to !m - 1 do
          let w = domain.(exponent * j) in
          (* odd *)
          let right = R.mul output.(!k + j + !m) w in
          output.(!k + j + !m) <- R.sub output.(!k + j) right ;
          output.(!k + j) <- R.add output.(!k + j) right
        done ;
        k := !k + (!m * 2)
      done ;
      m := !m * 2
    done ;
    ()

  let evaluation_fft ~domain polynomial =
    let open Utils in
    let n = degree_int polynomial + 1 in
    let d = Array.length domain in
    let logd = Z.(log2 (of_int d)) in
    if is_null polynomial then List.init d (fun _ -> R.zero)
    else
      let dense_polynomial = get_dense_polynomial_coefficients polynomial in
      let output = Array.of_list (List.rev dense_polynomial) in
      let output =
        (* if the polynomial is too small, we pad with zeroes *)
        if d > n then (
          let output = Array.append output (Array.make (d - n) R.zero) in
          reorg_coefficients d logd output ;
          output
          (* if the polynomial is larger, we evaluate on the sub polynomials *))
        else if n > d then (
          let next_power = next_power_of_two n in
          let log_next_power = Z.log2 (Z.of_int next_power) in
          let output =
            Array.append output (Array.make (next_power - n) R.zero)
          in
          let n = next_power in
          reorg_coefficients next_power log_next_power output ;
          Array.init d (fun i ->
              let poly = Array.sub output (i * (n / d)) (n / d) in
              let poly =
                List.init (n / d) (fun i ->
                    (poly.((n / d) - i - 1), (n / d) - i - 1))
              in
              let poly = of_coefficients poly in
              (* we may sum *)
              evaluation poly domain.(0)))
        else (
          reorg_coefficients d logd output ;
          output)
      in
      evaluation_fft_in_place ~domain output ;
      Array.to_list output

  let generate_random_polynomial degree =
    let rec random_non_null () =
      let r = R.random () in
      if R.is_zero r then random_non_null () else r
    in
    match degree with
    | Infinity -> []
    | Natural n when n >= 0 ->
        let coefficients = List.init n (fun _i -> R.random ()) in
        let coefficients =
          (random_non_null (), n)
          :: List.mapi (fun i c -> (c, n - i - 1)) coefficients
        in
        of_coefficients coefficients
    | _ -> failwith "The degree must be positive"

  let get_highest_coefficient polynomial =
    match polynomial with [] -> R.zero | (c, _e) :: _ -> c

  let interpolation_fft ~domain points =
    let n = Array.length domain in
    assert (List.compare_length_with points n = 0) ;
    let n_z = Z.of_int n in
    let logn = Z.log2 n_z in
    (* Points are in a list of size N. Let's define
       points = [y_0, y_1, ... y_(N - 1)]
       We build the polynomial [P(X) = y_(N - 1) X^(N - 1) + ... + y_1 X * y_0].
       The resulting value is not necessarily of type [t] because it might not
       respect the sparse representation as there might be some null
       coefficients [y_i]. However, [evaluation_fft] gets the dense
       polynomial in its body.
       If all the points are zero, mult_by_scalar will take care of keeping the
       invariant, see below.
    *)
    let inverse_domain = inverse_domain_values domain in
    let inverse_fft = Array.of_list points in
    (* We evaluate the resulting polynomial on the domain *)
    Utils.reorg_coefficients n logn inverse_fft ;
    evaluation_fft_in_place ~domain:inverse_domain inverse_fft ;
    let polynomial, _ =
      Array.fold_left
        (fun (acc, i) p -> ((p, i) :: acc, i + 1))
        ([], 0)
        inverse_fft
    in
    (* mult_by_scalar does use filter_map removing all the zero coefficients.
       Therefore, we keep the invariant consisting of representing the zero
       polynomial with an empty list
    *)
    mult_by_scalar (R.inverse_exn (R.of_z n_z)) polynomial

  let polynomial_multiplication p q =
    let mul_by_monom (scalar, int) p =
      List.map (fun (scalar_2, int_2) -> (R.mul scalar scalar_2, int + int_2)) p
    in
    List.fold_left (fun acc monom -> add acc (mul_by_monom monom q)) zero p

  let polynomial_multiplication_fft ~domain p q =
    if is_null p || is_null q then zero
    else
      (* Evaluate P on the domain -> eval_p contains N points where N is the
         domain size. The resulting list contains the points P(w_i) where w_i
         \in D
      *)
      let eval_p = evaluation_fft ~domain p in
      (* Evaluate Q on the domain -> eval_q contains N points where N is the
         domain size. The resulting list contains the points Q(w_i) where w_i
         \in D.
      *)
      let eval_q = evaluation_fft ~domain q in
      (* Contains N points, resulting of p(w_i) * q(w_i) where w_i \in D *)
      let eval_pq =
        List.(rev (rev_map2 (fun a b -> R.mul a b) eval_p eval_q))
      in
      interpolation_fft ~domain eval_pq

  let euclidian_division_opt a b =
    if is_null b then None
    else
      let deg_b = degree_int b in
      let highest_coeff_b = get_highest_coefficient b in
      let rec aux q r =
        if degree_int r < deg_b then Some (q, r)
        else
          let diff_degree = degree_int r - deg_b in
          let rescale_factor =
            R.(get_highest_coefficient r / highest_coeff_b)
          in
          let to_sub =
            polynomial_multiplication b [(rescale_factor, diff_degree)]
          in
          aux (add q [(rescale_factor, diff_degree)]) (sub r to_sub)
      in
      aux zero a

  let extended_euclide polynomial_1 polynomial_2 =
    let n_1 = degree_int polynomial_1 and n_2 = degree_int polynomial_2 in
    if n_1 = -1 && n_2 = -1 then (zero, zero, zero)
    else if n_1 = -1 then
      let rescale_factor =
        R.inverse_exn @@ get_highest_coefficient polynomial_2
      in
      ( mult_by_scalar rescale_factor polynomial_2,
        zero,
        mult_by_scalar rescale_factor one )
    else if n_2 = -1 then
      let rescale_factor =
        R.inverse_exn @@ get_highest_coefficient polynomial_1
      in

      ( mult_by_scalar rescale_factor polynomial_1,
        mult_by_scalar rescale_factor one,
        zero )
    else
      let rec aux poly_1 u_1 v_1 poly_2 u_2 v_2 =
        let q, r = euclidian_division_opt poly_1 poly_2 |> Option.get in
        if is_null r then (poly_2, u_2, v_2)
        else
          aux
            poly_2
            u_2
            v_2
            r
            (sub u_1 (polynomial_multiplication q u_2))
            (sub v_1 (polynomial_multiplication q v_2))
      in
      let gcd, u, v = aux polynomial_1 one zero polynomial_2 zero one in
      let rescale_factor = R.inverse_exn @@ get_highest_coefficient gcd in
      ( mult_by_scalar rescale_factor gcd,
        mult_by_scalar rescale_factor u,
        mult_by_scalar rescale_factor v )

  let to_string p =
    let rec inner l =
      match l with
      | [] -> "0"
      | [(e, p)] ->
          if R.is_one e && p = 1 then Printf.sprintf "X"
          else if p = 1 then Printf.sprintf "%sX" (R.to_string e)
          else if p = 0 then Printf.sprintf "%s" (R.to_string e)
          else if R.is_one e then Printf.sprintf "X^%d" p
          else Printf.sprintf "%s X^%d" (R.to_string e) p
      | (e, p) :: tail ->
          if R.is_one e && p = 1 then Printf.sprintf "X + %s" (inner tail)
          else if p = 1 then
            Printf.sprintf "%sX + %s" (R.to_string e) (inner tail)
          else if p = 0 then Printf.sprintf "%s" (R.to_string e)
          else if R.is_one e then Printf.sprintf "X^%d + %s" p (inner tail)
          else Printf.sprintf "%s X^%d + %s" (R.to_string e) p (inner tail)
    in
    inner p

  let ( = ) = equal

  let ( + ) = add

  let ( * ) = polynomial_multiplication

  let ( - ) = sub
end
