(** General module signature for a finite field *)
module type T = sig
  type t

  (** The size of a point representation, in bytes *)
  val size : int

  (** Check if a point, represented as a byte array, is in the field **)
  val check_bytes : Bytes.t -> bool

  (* Attempt to construct a point from a byte array *)
  val of_bytes_opt : Bytes.t -> t option

  (** Order of the field *)
  val order : Z.t

  val of_bytes : Bytes.t -> t

  val to_bytes : t -> Bytes.t

  (** Create an empty value to store an element of the field. DO NOT USE THIS TO
      DO COMPUTATIONS WITH, UNDEFINED BEHAVIORS MAY HAPPEN. USE IT AS A BUFFER *)
  val empty : unit -> t

  (* Let's use a function for the moment *)
  val zero : unit -> t

  val one : unit -> t

  val is_zero : t -> bool

  val is_one : t -> bool

  val random : unit -> t

  val add : t -> t -> t

  val mul : t -> t -> t

  val eq : t -> t -> bool

  val negate : t -> t

  (* Unsafe version of inverse *)
  val inverse : t -> t

  (* Safe version of inverse *)
  val inverse_opt : t -> t option

  val square : t -> t

  val double : t -> t

  val pow : t -> Z.t -> t
end
