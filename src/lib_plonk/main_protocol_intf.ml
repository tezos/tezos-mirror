(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
open Kzg.Bls
module SMap = Kzg.SMap

module type S = sig
  (** Raised by the prover when the provided inputs are not a satisfying
      assignment of the circuit. *)
  exception Rest_not_null of string

  (** Raised by the prover when the provided inputs are not a satisfying
      assignment of the circuit when using Plookup. *)
  exception Entry_not_in_table of string

  module Input_commitment : Input_commitment.S

  type scalar = Scalar.t [@@deriving repr]

  (**  *)
  val scalar_encoding : scalar Data_encoding.t

  (** Before proving and verifying, circuits go through a pre-processing
      step called [setup].
      The [setup] takes as input a [circuit_map], which associates
      an identifier to a circuit and the number of statements that can be
      proved with that circuit.
      This produces a set of [public_parameters] which are bound to the
      circuits and can be reused.
  *)
  type circuit_map = (Circuit.t * int) SMap.t

  (** Set of [public_parameters] needed by the prover.
      Its size is linear in the size of the circuits.
  *)
  type prover_public_parameters [@@deriving repr]

  (** Set of [public_parameters] needed by the verifier.
      Its size is constant w.r.t. the size of the circuits.
  *)
  type verifier_public_parameters [@@deriving repr]

  val verifier_public_parameters_encoding :
    verifier_public_parameters Data_encoding.t

  (** Succinct proof for a collection of statements. *)
  type proof [@@deriving repr]

  val proof_encoding : proof Data_encoding.t

  (** Witness is the whole trace for the circuit, including
      input_commitment values first, followed by public input
      values and followed by the rest of the trace.
      This is the prover input for a single proof. *)
  type circuit_prover_input = {
    witness : scalar array;
    input_commitments : Input_commitment.t list;
  }
  [@@deriving repr]

  (** Map where each circuit identifier is bound to
      a list of [circuit_prover_input] for a list of statements. *)
  type prover_inputs = circuit_prover_input list SMap.t [@@deriving repr]

  (** The public inputs for one circuit & several statements *)
  type public_inputs [@@deriving repr]

  (** The verifier input for a circuit, represented as the actual number of
      proofs that have been proved by the prover, the public inputs & the input
      commitments *)
  type circuit_verifier_input = {
    nb_proofs : int;
    public : public_inputs;
    commitments : Input_commitment.public list list;
  }
  [@@deriving repr]

  (** The verifier inputs, represented as a map where each circuit is binded to
      the verifier inputs for this circuit. *)
  type verifier_inputs = circuit_verifier_input SMap.t [@@deriving repr]

  (** Conversion from [prover_inputs] to [verifier_inputs]. *)
  val to_verifier_inputs :
    prover_public_parameters -> prover_inputs -> verifier_inputs

  (** [input_commit ~shift pp secret] produces a commitment to the [secret]
      array and additional prover information. This commitment is designed to be
      easily involved in a PlonK proof. In particular, the values of [secret]
      will be added to the arithmetic identity in such a way that [secret.(i)]
      participates in constraint number [shift + i], where equality will be
      asserted with respect to a PlonK variable in the same constraint.
      This allows us to "load" the value of [secret.(i)] into the variable, which
      may be reused across the circuit.
      The optional argument [shift] has a default value of 0.
      The commitment is relative to a certain domain size [n], included in [pp],
      the secret will remain information-theoretically hidden as long as the
      commitment is involved in at most [n - |secret|] different proofs.
      If the optionnal argument [size] is given, the secret will be padded with
      zeros to have the length [size] (note that an error will be risen if size
      is smaller than the secret length).
   *)
  val input_commit :
    ?size:int ->
    ?shift:int ->
    prover_public_parameters ->
    scalar array ->
    Input_commitment.t

  (** [setup ~zero_knowledge circuit_map ~srs] pre-processes the [circuit_map]
      producing the public parameters.
      The SRSs of ZCash and Filecoin can be loaded from file using the
      {!Bls12_381_polynomial} library.
      Activating [zero_knowledge] adds an overhead in proving time.
  *)
  val setup :
    zero_knowledge:bool ->
    circuit_map ->
    srs:Srs.t * Srs.t ->
    prover_public_parameters * verifier_public_parameters

  (** Enrich the [prover_public_parameters] with extra application data to
      prevent replay attacks.
      The same data must be used for updating the prover and verifier
      public parameters.
  *)
  val update_prover_public_parameters :
    'a Repr.ty -> 'a -> prover_public_parameters -> prover_public_parameters

  (** Enrich the [verifier_public_parameters] with extra application data to
      prevent replay attacks.
      The same data must be used for updating the prover and verifier
      public parameters.
  *)
  val update_verifier_public_parameters :
    'a Repr.ty -> 'a -> verifier_public_parameters -> verifier_public_parameters

  (** [prove public_parameters ~inputs] produces a proof for the collection
      of statements implied by [inputs] and the circuits used for generating
      [public_parameters].

      @raises Rest_not_null
      @raises Entry_not_in_table
    *)
  val prove : prover_public_parameters -> inputs:prover_inputs -> proof

  (** [verify public_parameters ~inputs proof] checks the validity of the
      [proof] with regards to [public_parameters] and [inputs].
  *)
  val verify :
    verifier_public_parameters -> inputs:verifier_inputs -> proof -> bool

  module Internal_for_tests : sig
    (** [mutate_vi vi] returns None if vi is empty and Some vi' else.
          vi' is slightly different from vi *)
    val mutate_vi : verifier_inputs -> verifier_inputs option
  end
end
