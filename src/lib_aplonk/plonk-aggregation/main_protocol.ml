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

open Plonk.Bls
open Plonk.Utils
module SMap = Plonk.SMap

module type S = sig
  module PP : Polynomial_protocol.S

  include Plonk.Main_protocol.S

  module Gates : Plonk.Custom_gates.S

  module Perm : Plonk.Permutation_gate.S with module PP := PP

  (** Returns (g, n), where n is the size of the circuit padded to the next
      power of two & g is a primitive n-th root of unity
   *)
  val get_gen_n_prover : prover_public_parameters -> scalar * int

  (** Returns (g, n), where n is the size of the circuit padded to the next
      power of two & g is a primitive n-th root of unity
   *)
  val get_gen_n_verifier : verifier_public_parameters -> scalar * int

  val filter_prv_pp_circuits :
    prover_public_parameters -> 'a SMap.t -> prover_public_parameters

  val filter_vrf_pp_circuits :
    verifier_public_parameters -> 'a SMap.t -> verifier_public_parameters

  (** Auxiliary information needed by the prover for the meta-verification in
      aPlonK *)
  type prover_aux = {
    answers : scalar SMap.t SMap.t list;
    batch : scalar SMap.t list;
    alpha : scalar;
    beta : scalar;
    gamma : scalar;
    delta : scalar;
    x : scalar;
    r : scalar;
    cms_answers : PP.Answers_commitment.t SMap.t;
    cms_pi : PP.Answers_commitment.t SMap.t;
    ids_batch : (scalar * int) SMap.t;
    t_answers : scalar list;
  }

  (** Auxiliary information needed by the verifier for the meta-verification in
      aPlonK *)
  type verifier_aux = {
    alpha : scalar;
    beta : scalar;
    gamma : scalar;
    delta : scalar;
    x : scalar;
    r : scalar;
  }

  type input_commit_info = {
    nb_max_answers : int;
    nb_max_pi : int;
    func : ?size:int -> ?shift:int -> scalar array -> PP.Answers_commitment.t;
  }

  val prove_list :
    prover_public_parameters ->
    input_commit_infos:input_commit_info SMap.t ->
    inputs:prover_inputs ->
    proof * prover_aux

  val verify_list :
    verifier_public_parameters ->
    proof
    * scalar SMap.t list
    * PP.Answers_commitment.public SMap.t
    * PP.Answers_commitment.public SMap.t
    * scalar list
    * (scalar * int) SMap.t ->
    bool * verifier_aux
end

module Make_impl (Super_PP : Polynomial_protocol.S) = struct
  include Plonk.Main_protocol.Make_impl (Super_PP)
  module Input_commitment = Input_commitment
  module PP = Super_PP

  type prover_aux = {
    answers : scalar SMap.t SMap.t list;
    batch : scalar SMap.t list;
    alpha : scalar;
    beta : scalar;
    gamma : scalar;
    delta : scalar;
    x : scalar;
    r : scalar;
    cms_answers : PP.Answers_commitment.t SMap.t;
    cms_pi : PP.Answers_commitment.t SMap.t;
    ids_batch : (scalar * int) SMap.t;
    t_answers : scalar list;
  }

  type verifier_aux = {
    alpha : scalar;
    beta : scalar;
    gamma : scalar;
    delta : scalar;
    x : scalar;
    r : scalar;
  }

  type input_commit_info = {
    nb_max_answers : int;
    nb_max_pi : int;
    func :
      ?size:int -> ?shift:int -> scalar array -> Super_PP.Answers_commitment.t;
  }

  let hash_pi (pp : prover_public_parameters) ic_infos inputs =
    (* TODO: can we commit only to the hidden pi?*)
    let pi_infos =
      SMap.mapi
        (fun circuit_name inputs_list ->
          let ic_info = SMap.find circuit_name ic_infos in
          let c = SMap.find circuit_name pp.circuits_map in
          let ic_size = List.fold_left ( + ) 0 c.input_com_sizes in
          let pi =
            List.map
              (fun s -> Array.sub s.witness ic_size c.public_input_size)
              inputs_list
          in
          (pi, ic_info.func ~size:ic_info.nb_max_pi @@ Array.concat pi))
        inputs
    in
    (SMap.map fst pi_infos, SMap.map snd pi_infos)

  let compute_ids_batch (pp : prover_public_parameters) rd alpha x
      public_inputs_map answers cms_answers =
    let n_gen = (pp.common_pp.n, Domain.get pp.common_pp.domain 1) in
    let identities =
      Verifier.build_identities
        (SMap.map
           Verifier.circuit_verifier_pp_of_circuit_prover_pp
           pp.circuits_map)
        n_gen
        rd
        public_inputs_map
    in
    let merged_answers =
      let f _key m1 m2 = Some (SMap.union_disjoint m1 m2) in
      List.fold_left (SMap.union f) SMap.empty answers
    in
    let evaluated_ids = identities x merged_answers in
    SMap.mapi
      (fun circuit_name _ ->
        let ids =
          SMap.Aggregation.filter_by_circuit_name circuit_name evaluated_ids
        in
        (Fr_generation.batch alpha (SMap.values ids), SMap.cardinal ids))
      (* we use cms_answers cause it contains the circuit_names *)
      cms_answers

  let update_transcript_with_pi transcript cms_pi =
    SMap.fold
      (fun _ cm_pi transcript ->
        PP.Answers_commitment.(Transcript.expand public_t cm_pi) transcript)
      cms_pi
      transcript

  let update_prv_pp_transcript_with_pi (pp : prover_public_parameters) cms_pi =
    {
      pp with
      transcript =
        update_transcript_with_pi
          pp.transcript
          (SMap.map PP.Answers_commitment.(fun a -> a.public) cms_pi);
    }

  let commit_to_answers_map =
    SMap.map (fun ic_info ->
        ic_info.func ~shift:ic_info.nb_max_pi ~size:ic_info.nb_max_answers)

  let prove_list (pp : prover_public_parameters) ~input_commit_infos ~inputs =
    let public_inputs_map, cms_pi = hash_pi pp input_commit_infos inputs in
    (* add the PI in the transcript *)
    let pp = update_prv_pp_transcript_with_pi pp cms_pi in
    let commit_to_answers_map = commit_to_answers_map input_commit_infos in
    let ( ( pp_proof,
            Super_PP.{answers; batch; alpha; x; r; cms_answers; t_answers} ),
          (perm_and_plook, wires_cm, rd) ) =
      Prover.prove_parameters
        ~pp_prove:(Super_PP.prove_super_aggregation ~commit_to_answers_map)
        pp
        ~inputs_map:inputs
    in
    let ids_batch =
      compute_ids_batch pp rd alpha x public_inputs_map answers cms_answers
    in
    ( {perm_and_plook; wires_cm; pp_proof},
      {
        answers;
        batch;
        alpha;
        beta = rd.beta_perm;
        gamma = rd.gamma_perm;
        delta = rd.delta;
        x;
        r;
        cms_answers;
        cms_pi;
        ids_batch;
        t_answers;
      } )

  let verify_list pp (proof, s_list, cms_answers, cms_pi, t_answers, ids_batch)
      =
    (* add the PI in the transcript *)
    let transcript = update_transcript_with_pi pp.transcript cms_pi in
    let transcript, _, rd, commitments, eval_points =
      (* Note that we don’t care about inputs here, because verify_parameters
         only cares about input_coms & identities that we don’t have here *)
      Verifier.verify_parameters
        ((pp.common_pp, pp.circuits_map), transcript)
        SMap.empty
        proof
    in
    let (kzg_verif, Super_PP.{alpha; x; r}), _transcript =
      Super_PP.verify_super_aggregation
        pp.common_pp.pp_public_parameters
        transcript
        ~n:pp.common_pp.n
        ~generator:pp.common_pp.generator
        ~commitments
        ~eval_points
        ~s_list
        ~cms_answers
        ~t_answers
        ~ids_batch
        proof.pp_proof
    in
    ( kzg_verif,
      {
        alpha;
        beta = rd.beta_perm;
        gamma = rd.gamma_perm;
        delta = rd.delta;
        x;
        r;
      } )

  let get_gen_n_prover (prover_public_params : prover_public_parameters) =
    ( Domain.get prover_public_params.common_pp.domain 1,
      prover_public_params.common_pp.n )

  let get_gen_n_verifier (verifier_public_params : verifier_public_parameters) =
    ( verifier_public_params.common_pp.generator,
      verifier_public_params.common_pp.n )
end

module Make : functor (PP : Polynomial_protocol.S) ->
  S with module PP = PP and type public_inputs = Scalar.t array list =
  Make_impl

include Make (Polynomial_protocol)
