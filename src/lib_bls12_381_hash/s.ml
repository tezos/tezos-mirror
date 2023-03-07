module type PERMUTATION = sig
  (** Parameters for a specific instance *)
  type parameters

  (** Context of the permutation *)
  type ctxt

  (** [allocate_ctxt parameters]. Allocate a context for a specific instance of
      the permutation.
  *)
  val allocate_ctxt : parameters -> ctxt

  (** Return the current state of the context *)
  val get_state : ctxt -> Bls12_381.Fr.t array

  (** Return the state size of the context *)
  val get_state_size : ctxt -> int

  (** [set_state ctxt state]. Set the context state to the given value. The
      value [state] must be of the same size than the expecting state *)
  val set_state : ctxt -> Bls12_381.Fr.t array -> unit

  (** Apply a permutation on the current state of the context *)
  val apply_permutation : ctxt -> unit
end

module type MODE = sig
  val digest :
    (module PERMUTATION with type parameters = 'p) ->
    'p ->
    Bls12_381.Fr.t array ->
    Bls12_381.Fr.t
end
