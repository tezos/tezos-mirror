open Bls

module type S = sig
  module Commitment :
    Polynomial_commitment.Commitment_sig with type secret = Poly.t SMap.t

  type public_parameters = Commitment.prover_public_parameters

  type prover_aux = {poly : Poly.t; pc_prover_aux : Commitment.prover_aux}
  [@@deriving repr]

  type public = Commitment.t [@@deriving repr]

  type t = {public : public; prover_aux : prover_aux} [@@deriving repr]

  (* size is the expected length of the commitment ; it must be at least bigger
     than the length of the secret if it’s given, the secret is padded with zero
     to reach this length *)
  val commit :
    ?size:int -> ?shift:int -> public_parameters -> int -> Scalar.t array -> t
end

module Make_impl (Commitment : Polynomial_commitment.Commitment_sig) = struct
  module Commitment = Commitment

  type public_parameters = Commitment.prover_public_parameters

  type prover_aux = {poly : Poly.t; pc_prover_aux : Commitment.prover_aux}
  [@@deriving repr]

  type public = Commitment.t [@@deriving repr]

  type t = {public : public; prover_aux : prover_aux} [@@deriving repr]

  let commit ?size ?(shift = 0) pp n secret =
    let domain = Domain.build n in
    let l = Array.length secret in
    let size = Option.value ~default:l size in
    let secret =
      Array.(append secret (init (size - l) (Fun.const Scalar.zero)))
    in
    (* we add some randomness to hide the secret *)
    let secret =
      let random _ = Scalar.random () in
      let head = Array.init shift random in
      let tail = Array.init (n - size - shift) random in
      Array.concat [head; secret; tail]
    in
    let poly = Evaluations.interpolation_fft2 domain secret in
    let poly_map = SMap.singleton "com" poly in
    let public, pc_prover_aux = Commitment.commit pp poly_map in
    {public; prover_aux = {poly; pc_prover_aux}}
end

module Make : functor (Commitment : Polynomial_commitment.Commitment_sig) ->
  S with module Commitment = Commitment =
  Make_impl
