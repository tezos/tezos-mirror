(** Update the constants for the linear trick *)
val compute_updated_constants :
  (module Bls12_381.Ff_sig.PRIME with type t = 'a) ->
  int ->
  int ->
  int ->
  int ->
  'a array ->
  'a array array ->
  'a array * 'a array * 'a array * 'a array

module type PARAMETERS = sig
  val width : int

  val full_rounds : int

  val partial_rounds : int

  val batch_size : int

  val round_constants : string array

  val linear_transformation : string array array

  val partial_round_idx_to_permute : int

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
