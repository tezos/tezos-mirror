(** Marvellous Strategy, based on {{: https://eprint.iacr.org/2019/426 } Design
    of Symmetric-Key Primitives for Advanced Cryptographic Protocols } *)

module type PARAMETERS = sig
  (** The state size *)
  val width : int

  (** The number of rounds *)
  val rounds : int

  (** The round constants, given in decimal representation *)
  val round_constants : string array

  (** The MDS matrix, given in decimal representation *)
  val mds_matrix : string array array

  val alpha : Z.t

  val alphainv : Z.t
end

module Make (Param : PARAMETERS) (Scalar : Bls12_381.Ff_sig.PRIME) : sig
  (** The state of the strategy *)
  type state

  (** Initialize the state with the given input *)
  val init : Scalar.t array -> state

  (** Apply a complete permutation *)
  val apply : state -> unit

  (** Return the current scalar elements in the state *)
  val get : state -> Scalar.t array
end
