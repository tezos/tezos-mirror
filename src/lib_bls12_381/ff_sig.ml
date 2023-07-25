(** Base module signature for a finite field *)
module type BASE = sig
  exception Not_in_field of Bytes.t

  type t

  (** The order of the finite field *)
  val order : Z.t

  (** Minimal number of bytes required to encode a value of the field *)
  val size_in_bytes : int

  (** [check_bytes bs] returns [true] if [bs] is a correct byte representation
      of a field element *)
  val check_bytes : Bytes.t -> bool

  (** [copy x] creates a fresh copy of [x] *)
  val copy : t -> t

  (** The neutral element for the addition *)
  val zero : t

  (** The neutral element for the multiplication *)
  val one : t

  (** [is_zero x] returns [true] if [x] is the neutral element for the addition *)
  val is_zero : t -> bool

  (** [is_one x] returns [true] if [x] is the neutral element for the
      multiplication *)
  val is_one : t -> bool

  (** {b Use carefully!}

      [random ()] returns a random element of the field. A state for the PRNG
      can be given to initialize the PRNG in the requested state. If no state is
      given, no initialisation is performed.

      To create a value of type [Random.State.t], you can use [Random.State.make
      [|42|]]. *)
  val random : ?state:Random.State.t -> unit -> t

  (** {b Use carefully!}

      [non_null_random ()] returns a non null random element of the field. A
      state for the PRNG can be given to initialize the PRNG in the requested
      state. If no state is given, no initialisation is performed.

      To create a value of type [Random.State.t], you can use [Random.State.make
      [|42|]]. *)
  val non_null_random : ?state:Random.State.t -> unit -> t

  (** [add a b] returns [a + b mod order] *)
  val add : t -> t -> t

  (** Infix operator for {!add} *)
  val ( + ) : t -> t -> t

  (** [sub a b] returns [a - b mod order] *)
  val sub : t -> t -> t

  (** [mul a b] returns [a * b mod order] *)
  val mul : t -> t -> t

  (** Infix operator for {!mul} *)
  val ( * ) : t -> t -> t

  (** [eq a b] returns [true] if [a = b mod order], else [false] *)
  val eq : t -> t -> bool

  (** Infix operator for {!eq} *)
  val ( = ) : t -> t -> bool

  (** [negate x] returns [-x mod order]. Equivalently, [negate x] returns the
      unique [y] such that [x + y mod order = 0] *)
  val negate : t -> t

  (** Infix operator for {!negate} *)
  val ( - ) : t -> t

  (** [inverse_exn x] returns [x^-1 mod order] if [x] is not [0], else raise
      [Division_by_zero]. Equivalently, [inverse_exn x] returns the unique [y]
      such that [x * y mod order = 1] *)
  val inverse_exn : t -> t

  (** [inverse_opt x] returns [x^-1 mod order] as an option if [x] is not [0],
      else returns [None]. Equivalently, [inverse_opt x] returns the unique [y]
      such that [x * y mod order = 1] *)
  val inverse_opt : t -> t option

  (** [div_exn a b] returns [a * b^-1]. Raise [Division_by_zero] if [b = zero].
      Equivalently, [div_exn] returns the unique [y] such that [b * y mod order
      = a] *)
  val div_exn : t -> t -> t

  (** [div_opt a b] returns [a * b^-1] as an option. Return [None] if [b =
      zero]. Equivalently, [div_opt] returns the unique [y] such that [b * y mod
      order = a] *)
  val div_opt : t -> t -> t option

  (** Infix operator for {!div_exn} *)
  val ( / ) : t -> t -> t

  (** [square x] returns [x^2 mod order] *)
  val square : t -> t

  (** [double x] returns [2x mod order] *)
  val double : t -> t

  (** [pow x n] returns [x^n mod order] *)
  val pow : t -> Z.t -> t

  (** Infix operator for {!pow} *)
  val ( ** ) : t -> Z.t -> t

  (** Construct a value of type [t] from the bytes representation in little
      endian of the field element. For non prime fields, the encoding starts
      with the coefficient of the constant monomial. Raise {!Not_in_field} if
      the bytes do not represent an element in the field. *)
  val of_bytes_exn : Bytes.t -> t

  (** From a predefined little endian bytes representation, construct a value of
      type [t]. The same representation than {!of_bytes_exn} is used. Return
      [None] if the bytes do not represent an element in the field. *)
  val of_bytes_opt : Bytes.t -> t option

  (** Convert the value [t] to a bytes representation. The number of bytes is
      {!size_in_bytes} and the encoding must be in little endian. For instance,
      the encoding of [1] in prime fields is always a bytes sequence of size
      {!size_in_bytes} starting with the byte [0b00000001].

      For non prime fields, the encoding starts with the coefficient of the
      constant monomial. For instance, an element [a + b * X] in [GF(p^2)] will
      be encoded as [to_bytes a || to_bytes b] where [||] is the concatenation
      of bytes *)
  val to_bytes : t -> Bytes.t
end

(** Module type for prime field of the form [GF(p)] where [p] is prime. The
    order of GF(p) is [p] *)
module type PRIME = sig
  include BASE

  (** Returns [s, q] such that [p - 1 = 2^s * q] *)
  val factor_power_of_two : int * Z.t

  (** Create a value of type [t] from a predefined string representation. It is
      not required that [to_string (of_string t) = t]. By default, decimal
      representation of the number is used, modulo the order of the field *)
  val of_string : string -> t

  (** String representation of a value of type [t]. It is not required that
      [to_string (of_string t) = t]. By default, decimal representation of the
      number is used. *)
  val to_string : t -> string

  (** [of_z x] builds an element of type [t] from the Zarith element [x]. [mod
      p] is applied if [x >= p] *)
  val of_z : Z.t -> t

  (** [to_z x] builds a Zarith element, using the decimal representation.
      Arithmetic on the result can be done using the modular functions on
      integers *)
  val to_z : t -> Z.t

  (** Returns the Legendre symbol of the parameter. Note it does not work for [p
      = 2] *)
  val legendre_symbol : t -> Z.t

  (** [is_quadratic_residue x] returns [true] if [x] is a quadratic residue i.e.
      if there exists [n] such that [n^2 mod p = x] *)
  val is_quadratic_residue : t -> bool

  (** [sqrt_opt x] returns a square root of [x] as an option if it does exist.
      If it does not exist, returns [None]. Equivalenty it returns a value [y]
      such that [y^2 mod p = x]. *)
  val sqrt_opt : t -> t option

  (** [of_int x] is equivalent to [of_z (Z.of_int x)] *)
  val of_int : int -> t
end

(** Module type for prime field with additional functions to manipulate roots of
    unity *)
module type PRIME_WITH_ROOT_OF_UNITY = sig
  include PRIME

  (** [get_nth_root_of_unity n] returns a [n]-th root of unity. Equivalently, it
      returns a value [x] such that [x^n mod p = 1] *)
  val get_nth_root_of_unity : Z.t -> t

  (** [is_nth_root_of_unity n x] returns [true] if [x] is a [n]-th root of
      unity. Equivalenty it returns [true] if [x^n mod p = 1] *)
  val is_nth_root_of_unity : Z.t -> t -> bool
end
