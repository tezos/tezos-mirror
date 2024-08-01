open Bls
open Utils

module type Commitment = sig
  type t [@@deriving repr]

  type prover_aux [@@deriving repr]

  type public_parameters

  type secret = Poly.t SMap.t

  (* The commitment will ignore polynomial and SRS coefficients before the
     [shift]-th coefficients ([shift] is 0 by default, which means everything
     is committed normally by default; [shift] must be positive) *)
  val commit_single : ?shift:int -> public_parameters -> Poly.t -> G1.t

  (* [all_keys] is an optional argument that should only be used for
     partial commitments. It contains all the polynomial names that
     make up the full commitment.
     For instance, if the full commitment contains polynomials "a", "b", "c" &
     "d", then all keys will contain ["a", "b", "c", "d"]
     Note that [secret] may only contain a subset of [all_keys] (for instance,
     {"a", "b"}).
  *)
  val commit :
    ?all_keys:string list -> public_parameters -> secret -> t * prover_aux

  val cardinal : t -> int

  val rename : (string -> string) -> t -> t

  val recombine : t list -> t

  val recombine_prover_aux : prover_aux list -> prover_aux

  val empty : t

  val empty_prover_aux : prover_aux

  val of_list : public_parameters -> name:string -> G1.t list -> t * prover_aux

  val to_map : t -> G1.t SMap.t
end

(* The public parameters module type for polynomial commitment
   The prover and verifier public parameters usually differ, the verifier
   parameters being usually a subset of the prover parameters *)
module type Public_parameters = sig
  type prover [@@deriving repr]

  type verifier [@@deriving repr]

  (* This type correspond to Commitment.public_parameters *)
  type commitment

  type setup_params = int

  val setup : setup_params -> Srs.t * Srs.t -> prover * verifier * Transcript.t

  (* Extracts the public parameters for committing, that may be only a subset
     of the prover public parameters. We need this function for the sake of
     interfaces unification *)
  val get_commit_parameters : prover -> commitment
end

module type Polynomial_commitment = sig
  (* polynomials to be committed *)
  type secret = Poly.t SMap.t

  (* maps evaluation point names to evaluation point values *)
  type query = Scalar.t SMap.t [@@deriving repr]

  (* maps evaluation point names to (map from polynomial names to evaluations) *)
  type answer = Scalar.t SMap.t SMap.t [@@deriving repr]

  type proof [@@deriving repr]

  module Commitment : Commitment

  module Public_parameters :
    Public_parameters with type commitment = Commitment.public_parameters

  val commit :
    ?all_keys:string list ->
    Public_parameters.prover ->
    secret ->
    Commitment.t * Commitment.prover_aux

  val evaluate : secret -> query -> answer

  val prove :
    Public_parameters.prover ->
    Transcript.t ->
    secret list ->
    Commitment.prover_aux list ->
    query list ->
    answer list ->
    proof * Transcript.t

  val verify :
    Public_parameters.verifier ->
    Transcript.t ->
    Commitment.t list ->
    query list ->
    answer list ->
    proof ->
    bool * Transcript.t
end

(** This module type is used for the proof in DAL’s cryptobox, especially for
    testing purposes *)
module type Degree_check_proof = sig
  type t [@@deriving repr]

  val zero : t

  val alter_proof : t -> t

  val encoding : t encoding
end
