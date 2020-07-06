module type T = sig
  (** The type of the element in the elliptic curve *)
  type t

  (** The size of a point representation, in bytes *)
  val size : int

  module Scalar : Ff_sig.T

  (** Build an element using a bytes representation. Use carefully *)

  (** Create an empty value to store an element of the curve. DO NOT USE THIS TO
      DO COMPUTATIONS WITH, UNDEFINED BEHAVIORS MAY HAPPEN *)
  val empty : unit -> t

  (** Check if a point, represented as a byte array, is on the curve **)
  val check_bytes : Bytes.t -> bool

  (* Attempt to construct a point from a byte array *)
  val of_bytes_opt : Bytes.t -> t option

  (** UNSAFE *)
  val of_bytes : Bytes.t -> t

  (** Return a representation in bytes. Use carefully *)
  val to_bytes : t -> Bytes.t

  (** Zero of the elliptic curve *)
  val zero : t

  (** A fixed generator of the elliptic curve *)
  val one : t

  (** Return true if the given element is zero *)
  val is_zero : t -> bool

  (** Generate a random element *)
  val random : unit -> t

  (** Return the addition of two element *)
  val add : t -> t -> t

  (** Return the opposite of the element *)
  val negate : t -> t

  (** Return true if the two elements are algebraically the same *)
  val eq : t -> t -> bool

  (** Multiply an element by a scalar *)
  val mul : t -> Scalar.t -> t
end
