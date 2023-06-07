module type PARAMETERS = sig
  (** The state size *)
  val width : int

  (** The total number of full rounds *)
  val full_rounds : int

  (** The number of partial rounds *)
  val partial_rounds : int

  (** The round constants, given in decimal representation *)
  val round_constants : string array

  (** The MDS matrix, given in decimal representation *)
  val mds_matrix : string array array

  (** The index of the element of the state to permute during the partial rounds *)
  val partial_round_idx_to_permute : int
end

(** A HADES strategy, for a constant length construction *)
module type STRATEGY = sig
  type scalar

  (** The state of the strategy *)
  type state

  (** Initialize the state with the given input *)
  val init : ?input_length:int -> scalar array -> state

  (** Apply a permutation round *)
  val apply_perm : state -> unit

  (** Return the current scalar elements in the state *)
  val get : state -> scalar array

  (** Return the expected input length if specified *)
  val input_length : state -> int option
end

module type HASH = sig
  type scalar

  type ctxt

  (** Initialize a raw hash context *)
  val init : ?input_length:int -> unit -> ctxt

  (** [digest ctxt input] computes the hash of the given input *)
  val digest : ctxt -> scalar array -> ctxt

  (** [get ctxt] returns the resulting point after [hash] has been called *)
  val get : ctxt -> scalar
end

module Make (C : PARAMETERS) (Scalar : Bls12_381.Ff_sig.PRIME) : sig
  module Strategy : STRATEGY with type scalar = Scalar.t

  module Hash : HASH with type scalar = Scalar.t
end
