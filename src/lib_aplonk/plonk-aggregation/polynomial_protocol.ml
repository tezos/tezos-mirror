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
open Kzg.Utils
open Plonk.Identities
module SMap = Kzg.SMap

module type S = sig
  module PC : Polynomial_commitment.S

  module Answers_commitment : Plonk.Input_commitment.S

  include Plonk.Polynomial_protocol.S with module PC := PC

  (** Auxiliary information needed by the prover for the meta-verification in
      aPlonK *)
  type prover_aux = {
    answers : Scalar.t SMap.t SMap.t list;
    batch : Scalar.t SMap.t list;
    alpha : Scalar.t;
    x : Scalar.t;
    r : Scalar.t;
    cms_answers : Answers_commitment.t SMap.t;
    t_answers : Scalar.t list;
  }

  (** Auxiliary information needed by the verifier for the meta-verification in
      aPlonK *)
  type verifier_aux = {alpha : Scalar.t; x : Scalar.t; r : Scalar.t}

  val update_transcript_with_formatted_answers :
    Transcript.t ->
    (Scalar.t SMap.t SMap.t list -> Answers_commitment.t) SMap.t ->
    Scalar.t SMap.t SMap.t list ->
    Scalar.t list * Answers_commitment.t SMap.t * Transcript.t

  val prove_super_aggregation :
    prover_public_parameters ->
    Transcript.t ->
    commit_to_answers_map:
      (Scalar.t SMap.t SMap.t list -> Answers_commitment.t) SMap.t ->
    n:int ->
    generator:Scalar.t ->
    secrets:(Poly.t SMap.t * PC.Commitment.prover_aux) list ->
    eval_points:eval_point list list ->
    evaluations:Evaluations.t SMap.t ->
    identities:prover_identities ->
    nb_of_t_chunks:int ->
    (proof * prover_aux) * Transcript.t

  val verify_super_aggregation :
    verifier_public_parameters ->
    Transcript.t ->
    n:int ->
    generator:Scalar.t ->
    commitments:PC.Commitment.t list ->
    eval_points:eval_point list list ->
    s_list:Scalar.t SMap.t list ->
    cms_answers:Answers_commitment.public SMap.t ->
    t_answers:Scalar.t list ->
    ids_batch:(Scalar.t * int) SMap.t ->
    proof ->
    (bool * verifier_aux) * Transcript.t
end

module Make_impl
    (Super_PC : Polynomial_commitment.S)
    (Answers_commitment : Plonk.Input_commitment.S) =
struct
  include Plonk.Polynomial_protocol.Make_impl (Super_PC)
  module PC = Super_PC
  module Answers_commitment = Answers_commitment

  type prover_aux = {
    answers : Scalar.t SMap.t SMap.t list;
    batch : Scalar.t SMap.t list;
    alpha : Scalar.t;
    x : Scalar.t;
    r : Scalar.t;
    cms_answers : Answers_commitment.t SMap.t;
    t_answers : Scalar.t list;
  }

  type verifier_aux = {alpha : Scalar.t; x : Scalar.t; r : Scalar.t}

  (* Expand the transcript with all answers. Circuit answers are involved
     in commiment form, whereas t_answers are included in the clear, given
     that they are exposed in the proof *)
  let update_transcript_with_answers transcript cms_answers t_answers =
    SMap.fold
      (fun circuit_name cm_answers transcript ->
        transcript
        |> Transcript.expand Repr.string circuit_name
        |> Transcript.expand Answers_commitment.public_t cm_answers)
      cms_answers
      transcript
    |> Transcript.expand (Repr.list Scalar.t) t_answers

  let verify_t ~n ~x ~alpha ~t_answers ~ids_batch =
    let sum_ids, _ =
      SMap.fold
        (fun _ (id_batch, size) (acc, acc_alpha) ->
          Scalar.
            (acc + (id_batch * acc_alpha), acc_alpha * pow alpha (Z.of_int size)))
        ids_batch
        (Scalar.zero, Scalar.one)
    in
    verify_t n x sum_ids t_answers

  let update_transcript_with_formatted_answers transcript cm_to_answers_map
      answer_list =
    let t_answers, circuit_answers =
      (List.hd answer_list, List.tl answer_list)
    in
    let t_answers = t_answers |> SMap.values |> List.concat_map SMap.values in
    let cms_answers =
      SMap.mapi
        (fun circuit_name commit ->
          List.map
            (SMap.Aggregation.select_answers_by_circuit circuit_name)
            circuit_answers
          |> commit)
        cm_to_answers_map
    in
    (* We treat t answers as Public inputs instead of through the
       input_commitment because they are not circuit-specific *)
    let transcript =
      update_transcript_with_answers
        transcript
        (SMap.map Answers_commitment.(fun a -> a.public) cms_answers)
        t_answers
    in
    (t_answers, cms_answers, transcript)

  let prove_super_aggregation pc_public_parameters transcript
      ~commit_to_answers_map ~n ~generator ~secrets ~eval_points ~evaluations
      ~identities ~nb_of_t_chunks =
    let ( (alpha, x, answer_list, cm_t),
          polys_list,
          prover_aux_list,
          query_list,
          transcript ) =
      prove_aux
        pc_public_parameters
        transcript
        n
        generator
        secrets
        eval_points
        evaluations
        identities
        nb_of_t_chunks
    in
    let t_answers, cms_answers, transcript =
      update_transcript_with_formatted_answers
        transcript
        commit_to_answers_map
        answer_list
    in
    let (pc_proof, Super_PC.{r; s_list}), transcript =
      Super_PC.prove_super_aggregation
        pc_public_parameters
        transcript
        polys_list
        prover_aux_list
        query_list
        answer_list
    in
    ( ( {cm_t; pc_proof; pc_answers = []},
        {
          answers = answer_list;
          cms_answers;
          batch = s_list;
          alpha;
          x;
          r;
          t_answers;
        } ),
      transcript )

  let verify_super_aggregation pc_public_parameters transcript ~n ~generator
      ~commitments ~eval_points ~s_list ~cms_answers ~t_answers ~ids_batch proof
      =
    let alpha, x, transcript, cmts, query_list =
      verify_aux transcript generator commitments eval_points proof
    in
    let transcript =
      update_transcript_with_answers transcript cms_answers t_answers
    in
    (* Step 2a: KZG.verify proofs for witness combinations *)
    let pc_verif, r, transcript =
      Super_PC.verify_super_aggregation
        pc_public_parameters
        transcript
        cmts
        query_list
        s_list
        proof.pc_proof
    in
    let pp_verif = verify_t ~n ~x ~alpha ~t_answers ~ids_batch in
    ((pc_verif && pp_verif, {alpha; x; r}), transcript)
end

module Make_aggregation : functor
  (PC : Polynomial_commitment.S)
  (Answers_commitment : Plonk.Input_commitment.S)
  ->
  S with module Answers_commitment = Answers_commitment with module PC = PC =
  Make_impl

module KZG_Answers_commitment = Plonk.Input_commitment.Make (Kzg.Commitment)
include Make_aggregation (Polynomial_commitment) (KZG_Answers_commitment)
