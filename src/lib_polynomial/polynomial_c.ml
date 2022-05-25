module Fr = Bls12_381.Fr

module Stubs = struct
  type fr = Fr.t

  type fr_array = Carray.Stubs.fr_array

  (** [degree p n] returns the degree of a polynomial [p]

  - requires: [size p = n] *)
  external degree : fr_array -> int -> int = "caml_polynomial_degree_stubs"
    [@@noalloc]

  (** [of_sparse res p n] converts the sparse representation of a polynomial [p] to
  the dense representation, from an OCaml array [p] of size [n] to a C array [res] of
  size [degree p + 1]

  requires:
  - degree of each coeff [d_i >= 0] and [d_i] are unique
  - the result should be initialized with zero (as done by {!allocate_fr_array})
  - [size res = degree p + 1]
  - [size p = n] *)
  external of_sparse : fr_array -> (fr * int) array -> int -> unit
    = "caml_polynomial_of_sparse_stubs"
    [@@noalloc]

  (** [add res a b size_a size_b] writes the result of polynomial addition of [a] and [b]
  in [res]

  requires:
  - [size_a >= size_b]
  - [size a = size_a]
  - [size b = size_b]
  - [size res = size_a]
  - [res], [a] and [b] are either pairwise disjoint or equal *)
  external add : fr_array -> fr_array -> fr_array -> int -> int -> unit
    = "caml_polynomial_add_stubs"
    [@@noalloc]

  (** [sub res a b size_a size_b] writes the result of polynomial subtraction of [b] from [a]
  in [res]

  requires:
  - [size a = size_a]
  - [size b = size_b]
  - [size res = max (size_a, size_b)]
  - [res], [a] and [b] are either pairwise disjoint or equal *)
  external sub : fr_array -> fr_array -> fr_array -> int -> int -> unit
    = "caml_polynomial_sub_stubs"
    [@@noalloc]

  (** [mul res a b size_a size_b] writes the result of polynomial multiplication of [a] by [b]
  in [res]

  requires:
  - the result should be initialized with zero (as done by {!allocate_fr_array})
  - [size a = size_a]
  - [size b = size_b]
  - [size res = size_a + size_b - 1] *)
  external mul : fr_array -> fr_array -> fr_array -> int -> int -> unit
    = "caml_polynomial_mul_stubs"
    [@@noalloc]

  (** [mul_by_scalar res b a size_a] writes the result of multiplying a polynomial [a]
  by a blst_fr element [b] in [res]

  requires:
  - [size a = size_a]
  - [size res = size_a]
  - [res] and [a] either disjoint or equal *)
  external mul_by_scalar : fr_array -> fr -> fr_array -> int -> unit
    = "caml_polynomial_mul_by_scalar_stubs"
    [@@noalloc]

  (** [negate res p n] writes the result of negating a polynomial [p] in [res]

  requires:
  - [size p = n]
  - [size res = n]
  - [res] and [p] either disjoint or equal *)
  external negate : fr_array -> fr_array -> int -> unit
    = "caml_polynomial_negate_stubs"
    [@@noalloc]

  (** [is_zero p n] checks whether a polynomial [p] is the zero polynomial

  - requires: [size p = n] *)
  external is_zero : fr_array -> int -> bool = "caml_polynomial_is_zero_stubs"
    [@@noalloc]

  (** [evaluate res p n x] writes the result of evaluating a polynomial [p] at [x]
  in [res]

  - requires: [size p = n] and [n > 0] *)
  external evaluate : fr -> fr_array -> int -> fr -> unit
    = "caml_polynomial_evaluate_stubs"
    [@@noalloc]

  (** [division_x_z res p n z] writes the quotient of the division of a polynomial [p]
  by [(X - z)] in [res]

  requires:
  - [size p = n] and [n > 1]
  - [size res = n - 1] *)
  external division_x_z : fr_array -> fr_array -> int -> fr -> unit
    = "caml_polynomial_division_x_z_stubs"
    [@@noalloc]

  (** [division_zs res p n s] writes the quotient of the division of a polynomial [p]
  by [(X^s - 1)] in [res]

  requires:
  - [size p = n] and [n >= 2s]
  - [size res = n - s] *)
  external division_zs : fr_array -> fr_array -> int -> int -> unit
    = "caml_polynomial_division_zs_stubs"
    [@@noalloc]

  (** [mul_zs res p n s] writes the result of multiplying a polynomial [p] by [(X^s - 1)]
  in [res]

  requires:
  - [size p = n] and [n >= s]
  - [size res = n + s] *)
  external mul_zs : fr_array -> fr_array -> int -> int -> unit
    = "caml_polynomial_mul_zs_stubs"
    [@@noalloc]

  external derivative : fr_array -> fr_array -> int -> unit
    = "caml_polynomial_derivative_stubs"
    [@@noalloc]
end

module Polynomial_impl = struct
  type scalar = Bls12_381.Fr.t

  type t = Carray.t

  let of_carray : Carray.t -> t = fun x -> x

  let to_carray : t -> Carray.t = fun x -> x

  let copy = Carray.copy

  let copy_truncated = Carray.copy_truncated

  let to_string = Carray.to_string

  let get = Carray.get

  let degree (p, n) = Stubs.degree p n

  let fail_size ~size_expected ~size_computed =
    let error_string =
      Printf.sprintf
        "size computed was %d and size expected was at least %d"
        size_computed
        size_expected
    in
    failwith error_string

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
    let polynomial = Carray.allocate (degree + 1) in
    Stubs.of_sparse polynomial coefficients (Array.length coefficients) ;
    (polynomial, degree + 1)

  let of_dense = Carray.of_array

  (* zero = [0] and not [] *)
  let zero = of_coefficients []

  let one = of_coefficients [(Fr.one, 0)]

  let generate_random_polynomial n =
    if Random.int 10 = 0 then zero
    else
      let poly =
        List.init n (fun i ->
            if Random.bool () then (Fr.random (), i) else (Fr.copy Fr.zero, i))
      in
      of_coefficients poly

  let to_dense_coefficients (polynomial, n) =
    (* the polynomial could be padded with zero, so we instead of using [n] and
       wasting some space we recompute the size of the minimal representation *)
    let size = 1 + max 0 (Stubs.degree polynomial n) in
    Carray.to_array (polynomial, size)

  (* ensures: no coefficient in the result is zero *)
  let to_sparse_coefficients poly =
    let poly = to_dense_coefficients poly in
    let res = ref [] in
    for deg = Array.length poly - 1 downto 0 do
      let coef = poly.(deg) in
      if not (Fr.is_zero coef) then res := (Fr.copy coef, deg) :: !res
    done ;
    !res

  let encoding : t Data_encoding.t =
    Data_encoding.(
      let fr_encoding = conv Fr.to_bytes Fr.of_bytes_exn bytes in
      conv to_dense_coefficients of_dense (array fr_encoding))

  let equal = Carray.equal

  let add (poly_1, size_1) (poly_2, size_2) =
    let arg_bigger, arg_smaller, size_bigger, size_smaller =
      Carray.sort_by_size (poly_1, size_1) (poly_2, size_2)
    in
    let res = Carray.allocate size_bigger in
    Stubs.add res arg_bigger arg_smaller size_bigger size_smaller ;
    (res, size_bigger)

  let add_inplace (poly_1, size_1) (poly_2, size_2) =
    let arg_bigger, arg_smaller, size_bigger, size_smaller =
      Carray.sort_by_size (poly_1, size_1) (poly_2, size_2)
    in
    Stubs.add arg_bigger arg_bigger arg_smaller size_bigger size_smaller ;
    (arg_bigger, size_bigger)

  let sub (poly_1, size_1) (poly_2, size_2) =
    let max_size = max size_1 size_2 in
    let res = Carray.allocate max_size in
    Stubs.sub res poly_1 poly_2 size_1 size_2 ;
    (res, max_size)

  let sub_inplace (poly_1, size_1) (poly_2, size_2) =
    let arg_bigger, _, size_bigger, _ =
      Carray.sort_by_size (poly_1, size_1) (poly_2, size_2)
    in
    Stubs.sub arg_bigger poly_1 poly_2 size_1 size_2 ;
    (arg_bigger, size_bigger)

  let mul (poly_1, size_1) (poly_2, size_2) =
    let res_size = size_1 + size_2 - 1 in
    let res = Carray.allocate res_size in
    Stubs.mul res poly_1 poly_2 size_1 size_2 ;
    (res, res_size)

  let mul_by_scalar scalar (poly, size) =
    let res = Carray.allocate size in
    Stubs.mul_by_scalar res scalar poly size ;
    (res, size)

  let mul_by_scalar_inplace scalar (poly, size) =
    Stubs.mul_by_scalar poly scalar poly size

  let opposite (poly, size) =
    let res = Carray.allocate size in
    Stubs.negate res poly size ;
    (res, size)

  let opposite_inplace (poly, size) = Stubs.negate poly poly size

  let is_zero (poly, size) = Stubs.is_zero poly size

  let evaluate (p, s) scalar =
    let res = Fr.copy scalar in
    Stubs.evaluate res p s scalar ;
    res

  exception Rest_not_null of string

  let division_x_z (poly, size) z =
    if is_zero (poly, size) then zero
    else if size <= 1 then fail_size ~size_expected:2 ~size_computed:size
    else
      let res = Carray.allocate (size - 1) in
      Stubs.division_x_z res poly size z ;
      (res, size - 1)

  let division_zs (poly, size) n =
    if is_zero (poly, size) then zero
    else if size < 2 * n then
      fail_size ~size_expected:(2 * n) ~size_computed:size
    else
      let res = Carray.allocate (size - n) in
      Stubs.division_zs res poly size n ;
      (res, size - n)

  let mul_zs (poly, size) n =
    if not (size >= n) then fail_size ~size_expected:n ~size_computed:size ;
    let res = Carray.allocate (size + n) in
    Stubs.mul_zs res poly size n ;
    (res, size + n)

  let derivative (poly, size) =
    if is_zero (poly, size) then zero
    else
      let res = Carray.allocate (size - 1) in
      Stubs.derivative res poly size ;
      (res, size - 1)

  (* TODO use slices? *)
  let split d n p =
    let nb_coeff_P = 1 + degree p in
    let k =
      let k = max 1 (nb_coeff_P / n) in
      (* we may need more parts if d < n + r *)
      if n + (nb_coeff_P mod n) <= d then k else k + 1
    in
    let rec take_n_first d_to_sub i acc = function
      | [] -> (acc, [])
      | (a, d) :: tl ->
          if i = n then (acc, (a, d) :: tl)
          else take_n_first d_to_sub (i + 1) ((a, d - d_to_sub) :: acc) tl
    in
    let rec parts acc tl = function
      | 1 ->
          of_coefficients (List.map (fun (a, d) -> (a, d - ((k - 1) * n))) tl)
          :: acc
      | i ->
          let pn_list, new_tl = take_n_first ((k - i) * n) 0 [] tl in

          let pn = of_coefficients pn_list in
          parts (pn :: acc) new_tl (i - 1)
    in
    let p_list =
      let t = to_dense_coefficients p in
      Array.mapi (fun i e -> (e, i)) t |> Array.to_list
    in
    List.rev (parts [] p_list k)

  let ( = ) = equal

  let ( + ) = add

  let ( - ) = sub

  let ( * ) = mul

  let constant c = of_coefficients [(c, 0)]
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

  type t

  val encoding : t Data_encoding.t

  (** [generate_random_polynomial n] generates a polynomial with a maximum size [n]

  Note: generates more often the zero polynomial and polynomials
  with zero coefficients *)
  val generate_random_polynomial : int -> t

  (** [degree p] returns the degree of a polynomial [p] *)
  val degree : t -> int

  (** [get p i] returns the [i]-th element of a given array [p], a coefficient of [X^i]
  in [p] *)
  val get : t -> int -> scalar

  (** [to_string p] returns the string representation of a polynomial [p] *)
  val to_string : t -> string

  (** [copy p] returns a copy of a polynomial [p] *)
  val copy : t -> t

  (** [copy_truncated p i] returns a copy of a polynomial [p] up to
  the monomial of degree i *)
  val copy_truncated : t -> int -> t

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

  (** [add_inplace] computes polynomial addition

  Note: The argument with the largest size is overwritten and returned *)
  val add_inplace : t -> t -> t

  (** [sub] computes polynomial subtraction *)
  val sub : t -> t -> t

  (** [sub_inplace] computes polynomial subtraction

  Note: The argument with the largest size is overwritten and returned *)
  val sub_inplace : t -> t -> t

  (** [mul] computes polynomial multiplication

  Note: naive quadratic algorithm, result's size is the sum of arguments' size *)
  val mul : t -> t -> t

  (** [mul_by_scalar] computes muliplication of a polynomial by a blst_fr element *)
  val mul_by_scalar : scalar -> t -> t

  (** [mul_by_scalar_inplace s p] computes muliplication of a polynomial [p]
  by a blst_fr element [s]

  Note: The argument [p] is overwritten and returned *)
  val mul_by_scalar_inplace : scalar -> t -> unit

  (** [opposite] computes polynomial negation *)
  val opposite : t -> t

  (** [opposite_inplace p] computes polynomial negation

  Note: The argument [p] is overwritten and returned *)
  val opposite_inplace : t -> unit

  (** [evaluate p x] evaluates a polynomial [p] at [x] *)
  val evaluate : t -> scalar -> scalar

  exception Rest_not_null of string

  (** [division_x_z p z] returns the quotient of the division of [p] by [(X - z)]

  - requires: length of p >= 2
  - Does NOT raise an error if [p] is not divisible. *)
  val division_x_z : t -> scalar -> t

  (** [division_zs p n] returns the quotient of the division of [p] by [(X^n - 1)]

  - requires: length of p >= 2*n
  - Does NOT raise an error if [p] is not divisible, it can be checked using {!mul_zs}. *)
  val division_zs : t -> int -> t

  (** [mul_zs p n] returns the product of [p] and [(X^n - 1)]

  - requires: length of p >= n *)
  val mul_zs : t -> int -> t

  (** [derivative p] returns the formal derivative of [p] *)
  val derivative : t -> t

  val split : int -> int -> t -> t list

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
end

module type Polynomial_unsafe_sig = sig
  include Polynomial_sig

  val to_carray : t -> Carray.t

  val of_carray : Carray.t -> t
end

module Polynomial_unsafe :
  Polynomial_unsafe_sig with type scalar = Bls12_381.Fr.t =
  Polynomial_impl

include (
  Polynomial_unsafe :
    Polynomial_sig
      with type scalar = Polynomial_unsafe.scalar
       and type t = Polynomial_unsafe.t)
