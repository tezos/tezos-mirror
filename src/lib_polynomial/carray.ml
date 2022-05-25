module Fr = Bls12_381.Fr

module Stubs = struct
  type fr = Fr.t

  type fr_array

  (** [allocate_fr_array n] allocates an OCaml custom block to hold a C array
  containing [n] zeros of blst_fr

  - requires: [n > 0]
  - ensures: [size res = n] and [res] is initialized with zeros of blst_fr *)
  external allocate_fr_array : int -> fr_array
    = "caml_polynomial_allocate_fr_array_stubs"

  (** [of_fr_array res p n] converts a given C array [p] of at least size [n] to
  an OCaml array [res] of size [n]

  - requires: [n <= size p]
  - ensures: [size res = n] and [res] is a prefix of [p] *)
  external of_fr_array : fr array -> fr_array -> int -> unit
    = "caml_polynomial_of_fr_array_stubs"
    [@@noalloc]

  (** [get res p i] writes [i]-th element of a given array [p] in [res]

  - requires: [0 <= i < size p]
  - ensures: [res = p[i]] *)
  external get : fr -> fr_array -> int -> unit = "caml_polynomial_get_stubs"
    [@@noalloc]

  (** [copy a b n] copies the first [n] elements from array [b] to array [a]

  - requires: [a] and [b] are disjoint; [n <= size a] and [n <= size b]
  - ensures: [a] is a prefix of [b] *)
  external copy : fr_array -> fr_array -> int -> unit
    = "caml_polynomial_copy_stubs"
    [@@noalloc]

  (** [of_dense res p n] converts an OCaml array [p] of size [n] to a C array [res] of
  size [n]

  requires:
  - [size res = n]
  - [size p = n] *)
  external of_dense : fr_array -> fr array -> int -> unit
    = "caml_polynomial_of_dense_stubs"
    [@@noalloc]

  (** [eq a b size_a size_b] checks whether a polynomial [a] is equal to a polynomial [b]

  requires:
  - [size_a >= size_b]
  - [size a = size_a]
  - [size b = size_b] *)
  external eq : fr_array -> fr_array -> int -> int -> bool
    = "caml_polynomial_eq_stubs"
    [@@noalloc]

  (** [is_zero p n] checks whether a polynomial [p] is the zero polynomial

  - requires: [size p = n] *)
  external is_zero : fr_array -> int -> bool = "caml_polynomial_is_zero_stubs"
    [@@noalloc]
end

module Carray : sig
  type scalar = Bls12_381.Fr.t

  type t = Stubs.fr_array * int

  val allocate : int -> Stubs.fr_array

  val length : t -> int

  val get : t -> int -> scalar

  val copy : t -> t

  val copy_truncated : t -> int -> t

  val to_string : t -> string

  val to_array : t -> scalar array

  val of_array : scalar array -> t

  val sort_by_size :
    Stubs.fr_array * int ->
    Stubs.fr_array * int ->
    Stubs.fr_array * Stubs.fr_array * int * int

  val equal : t -> t -> bool
end = struct
  type scalar = Bls12_381.Fr.t

  (* Invariant: size of a P is always > 0 *)
  type t = Stubs.fr_array * int

  let length (_, n) = n

  let get (p, n) i =
    if i < 0 || i >= n then raise @@ Invalid_argument "get: index out of bounds" ;
    let res = Fr.(copy zero) in
    Stubs.get res p i ;
    res

  let allocate n =
    if n < 1 then raise @@ Invalid_argument "allocate: should be greater than 1"
    else Stubs.allocate_fr_array n

  let to_array (p, n) =
    let d = Array.init n (fun _ -> Fr.(copy zero)) in
    Stubs.of_fr_array d p n ;
    d

  let of_array coeff =
    let n = Array.length coeff in
    let res = allocate n in
    Stubs.of_dense res coeff n ;
    (res, n)

  let to_string p =
    String.concat " ; " (List.map Fr.to_string (Array.to_list @@ to_array p))

  let copy (coefficients, n) =
    let n_c = allocate n in
    Stubs.copy n_c coefficients n ;
    (n_c, n)

  let copy_truncated (coefficients, n) i =
    if i < 0 || i >= n then
      raise @@ Invalid_argument "copy_truncated: index out of bounds" ;
    let n_c = allocate i in
    Stubs.copy n_c coefficients i ;
    (n_c, i)

  let sort_by_size (poly_1, size_1) (poly_2, size_2) =
    if size_1 >= size_2 then (poly_1, poly_2, size_1, size_2)
    else (poly_2, poly_1, size_2, size_1)

  let equal (poly_1, size_1) (poly_2, size_2) =
    let arg_bigger, arg_smaller, size_bigger, size_smaller =
      sort_by_size (poly_1, size_1) (poly_2, size_2)
    in
    Stubs.eq arg_bigger arg_smaller size_bigger size_smaller
end

include Carray
