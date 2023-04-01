(** HADES Strategy, based on {{: https://eprint.iacr.org/2019/1107.pdf } On a
    Generalization of Substitution-Permutation Networks: The HADES Design
    Strategy} *)

module type PARAMETERS = sig
  (** The state size *)
  val width : int

  (** The total number of full rounds *)
  val full_rounds : int

  (** The number of partial rounds *)
  val partial_rounds : int

  (** The round constants, given in decimal representation

      Secure round constants can be constructed using {{:
      https://gitlab.com/dannywillems/ocaml-ec/-/tree/master/utils/poseidon-hash
      } Sage scripts provided in this repository } *)
  val round_constants : string array

  (** The linear transformation, given in decimal representation.

      Secure linear transformations can be constructed using {{:
      https://gitlab.com/dannywillems/ocaml-ec/-/tree/master/utils/poseidon-hash
      } Sage scripts provided in this repository } *)
  val linear_transformation : string array array

  (** The index of the element of the state to permute during the partial
      rounds *)
  val partial_round_idx_to_permute : int

  (** The exponent to be used in the sbox *)
  val alpha : Z.t
end

(** Build an HADES SP network based on the parameters and a scalar field *)
module Make (Param : PARAMETERS) (Scalar : Bls12_381.Ff_sig.PRIME) : sig
  (** The state of the permutation *)
  type state

  (** Initialize the state with the given input *)
  val init : Scalar.t array -> state

  (** Apply a complete permutation *)
  val apply : state -> unit

  (** Return the current scalar elements in the state *)
  val get : state -> Scalar.t array
end
