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

open Plonk
open Kzg.Bls
open Kzg.Utils
module SMap = Kzg.SMap

module type S = sig
  include Kzg.Interfaces.Polynomial_commitment

  (** Auxiliary information needed by the prover for the meta-verification in
      aPlonK *)
  type prover_aux = {r : Scalar.t; s_list : Scalar.t SMap.t list}

  val prove_super_aggregation :
    Public_parameters.prover ->
    Transcript.t ->
    Poly.t SMap.t list ->
    Commitment.prover_aux list ->
    query list ->
    Scalar.t SMap.t SMap.t list ->
    (proof * prover_aux) * Transcript.t

  val verify_super_aggregation :
    Public_parameters.verifier ->
    Transcript.t ->
    Commitment.t list ->
    query list ->
    Scalar.t SMap.t list ->
    proof ->
    bool * Scalar.t * Transcript.t
end

module Make_impl
    (PC :
      Kzg.Interfaces.Polynomial_commitment
        with type Commitment.t = Kzg.Bls.G1.t SMap.t) =
struct
  type secret = PC.secret

  type query = PC.query [@@deriving repr]

  type answer = PC.answer [@@deriving repr]

  module Commitment = struct
    type public_parameters = {
      pc : PC.Commitment.public_parameters;
      pack : Pack.prover_public_parameters;
    }

    type secret = Poly.t SMap.t

    type t = Pack.commitment [@@deriving repr]

    (* [PC.Commitment.t] is required to be [Bls12_381.G1.t SMap.t],
       containing all the commitments that were packed *)
    type prover_aux = PC.Commitment.t * PC.Commitment.prover_aux
    [@@deriving repr]

    let commit ?all_keys (pp : public_parameters) f_map =
      (* Relevant_positions is the list of the indexes of f_map’s elements in the all_keys list.  *)
      let relevant_positions =
        match all_keys with
        | None -> List.init (SMap.cardinal f_map) Fun.id
        | Some [] -> List.init (SMap.cardinal f_map) Fun.id
        | Some ks ->
            (* Note that in order to get relevant_positions relatively to the map,
               the keys need to be sorted as they are be in the commitment smap *)
            let sorted_ks = List.sort String.compare ks in
            List.mapi (fun i x -> (i, x)) sorted_ks
            |> List.filter_map (fun (i, x) ->
                   Option.map (Fun.const i) (SMap.find_opt x f_map))
      in
      let prover_aux = PC.Commitment.commit pp.pc f_map in
      let cm_list = SMap.values (fst prover_aux) in
      let pack_cmt =
        Pack.partial_commit ~relevant_positions pp.pack (Array.of_list cm_list)
      in
      (pack_cmt, prover_aux)

    let cardinal = Pack.commitment_cardinal

    let rename _f cmt = cmt

    let commit_single ?shift pp = PC.Commitment.commit_single ?shift pp.pc

    let empty = Pack.empty_commitment

    let empty_prover_aux = (PC.Commitment.empty, PC.Commitment.empty_prover_aux)

    let recombine = function
      | [] -> empty
      | h :: t -> List.fold_left Pack.combine h t

    let recombine_prover_aux l =
      let cm = PC.Commitment.recombine (List.map fst l) in
      let p_a = PC.Commitment.recombine_prover_aux (List.map snd l) in
      (cm, p_a)

    let of_list pp ~name l =
      let pc_cm = PC.Commitment.(of_list pp.pc ~name l) in
      (Pack.commit pp.pack (Array.of_list l), pc_cm)

    let to_map _ = failwith "Not implemented & should not be used in verifier"
  end

  module Public_parameters = struct
    type prover = {
      pp_pc_prover : PC.Public_parameters.prover;
      pp_pack_prover : Pack.prover_public_parameters;
    }
    [@@deriving repr]

    type verifier = {
      pp_pc_verifier : PC.Public_parameters.verifier;
      pp_pack_verifier : Pack.verifier_public_parameters;
    }
    [@@deriving repr]

    type commitment = Commitment.public_parameters

    type setup_params = int

    let setup setup_params srs =
      let pp_pc_prover, pp_pc_verifier, transcript =
        PC.Public_parameters.setup setup_params srs
      in
      let pp_pack_prover, pp_pack_verifier =
        Pack.setup setup_params (snd srs)
      in
      let pp_prover = {pp_pc_prover; pp_pack_prover} in
      let pp_verifier = {pp_pc_verifier; pp_pack_verifier} in
      (pp_prover, pp_verifier, transcript)

    let get_commit_parameters {pp_pc_prover; pp_pack_prover} =
      Commitment.
        {
          pc = PC.Public_parameters.get_commit_parameters pp_pc_prover;
          pack = pp_pack_prover;
        }
  end

  let commit ?all_keys pp =
    Commitment.commit ?all_keys (Public_parameters.get_commit_parameters pp)

  type proof = {
    pc_proof : PC.proof;
    packed_values : Pack.packed list;
    pack_proof : Pack.proof;
  }
  [@@deriving repr]

  type prover_aux = {r : Scalar.t; s_list : Scalar.t SMap.t list}

  let batch_polys r map =
    let polys = SMap.values map in
    Poly.linear_with_powers polys r

  let batch_answers r =
    SMap.map (fun m -> Fr_generation.batch r @@ SMap.values m)

  let evaluate = PC.evaluate

  (* compute P := cmt₀ + r cmt₁ + r² cmt₂ + ... for every group of commitments
     in the list [prover_aux_list], and common randomness r (freshly sampled);
     such P values are returned as [packed_values], together with a proof
     [packed_proof] of their correctness;
     also, on input a list of evaluations [answer_list], at the requested points
     in [query_list], produce a proof of their validity: such proof is a PC
     proof (for every group) on the aggregatted commitment P with respect to the
     corresponding aggregated evaluations (we thus batch [answer_list] with [r]
     similarly) *)
  let prove_pack (pp : Public_parameters.prover) transcript f_map_list
      (prover_aux_list : Commitment.prover_aux list) query_list answer_list =
    let r, transcript = Fr_generation.random_fr transcript in
    let f_list = List.map (batch_polys r) f_map_list in
    let s_list = List.map (batch_answers r) answer_list in
    (* [cmts_list] is a list of G1.t SMap.t, containing the PC commitments to
       every polynomial (note that PC.Commitment.t = Bls12_381.G1.t SMap.t) *)
    let cmts_list =
      List.map
        (fun (cmts, _prover_aux) -> SMap.values cmts |> Array.of_list)
        prover_aux_list
    in
    (* [packed_values] has type [G1.t list] and it is the result of batching
       each map in [cmt_list] with powers of [r].
       [pack_proof] asserts that [packed_values] was correctly computed. *)
    let (packed_values, pack_proof), transcript =
      Pack.prove pp.pp_pack_prover transcript r cmts_list
    in
    (* prepare [f_list] and [s_list], the batched version of [f_map_list]
       polys and [answer_list] (using randomness [r]) by selecting a dummy
       name for them [string_of_int i] in order to call the underlying PC *)
    let f_map_list =
      List.mapi (fun i l -> SMap.singleton (string_of_int i) l) f_list
    in
    let s_map_list =
      List.mapi
        (fun i m -> SMap.map (fun s -> SMap.singleton (string_of_int i) s) m)
        s_list
    in
    let prover_aux_list = List.map snd prover_aux_list in
    (* call the underlying PC prover on the batched polynomials/evaluations
       the verifier will verify such proof using [packed_values] as the
       commitments *)
    let pc_proof, transcript =
      PC.prove
        pp.pp_pc_prover
        transcript
        f_map_list
        prover_aux_list
        query_list
        s_map_list
    in
    let proof = {pc_proof; packed_values; pack_proof} in
    let transcript = Transcript.expand proof_t proof transcript in
    ((proof, {r; s_list}), transcript)

  let prove (pp : Public_parameters.prover) transcript f_map_list
      (prover_aux_list : Commitment.prover_aux list) query_list answer_list =
    let transcript = Transcript.list_expand query_t query_list transcript in
    let transcript = Transcript.list_expand answer_t answer_list transcript in
    let (proof, _), transcript =
      prove_pack pp transcript f_map_list prover_aux_list query_list answer_list
    in
    (proof, transcript)

  let prove_super_aggregation (pp : Public_parameters.prover) transcript
      f_map_list (prover_aux_list : Commitment.prover_aux list) query_list
      answer_list =
    let transcript = Transcript.list_expand query_t query_list transcript in
    prove_pack pp transcript f_map_list prover_aux_list query_list answer_list

  let verify_pack (pp : Public_parameters.verifier) r transcript cmt_list
      query_list s_list proof =
    (* verify that the [packed_values] are correct, they will be used as
       the commitments for the PC proof of (batched) evaluations *)
    let pack_ok, transcript =
      Pack.verify
        pp.pp_pack_verifier
        transcript
        cmt_list
        r
        (proof.packed_values, proof.pack_proof)
    in
    (* batch the evaluations using [r] and prepare the query to the PC verifier
       by selecting the default dummy names [string_of_int i] names *)
    let s_map_list =
      List.mapi
        (fun i m -> SMap.map (fun s -> SMap.singleton (string_of_int i) s) m)
        s_list
    in
    let cmt_map_list =
      List.mapi
        (fun i l -> SMap.singleton (string_of_int i) l)
        proof.packed_values
    in
    (* verify that the batched evaluations are correct *)
    let pc_ok, transcript =
      PC.verify
        pp.pp_pc_verifier
        transcript
        cmt_map_list
        query_list
        s_map_list
        proof.pc_proof
    in
    (pack_ok && pc_ok, Transcript.expand proof_t proof transcript)

  let verify (pp : Public_parameters.verifier) transcript cmt_list query_list
      s_map_list proof =
    let transcript = Transcript.list_expand query_t query_list transcript in
    let transcript = Transcript.list_expand answer_t s_map_list transcript in
    let r, transcript = Fr_generation.random_fr transcript in
    let s_list = List.map (batch_answers r) s_map_list in
    verify_pack pp r transcript cmt_list query_list s_list proof

  let verify_super_aggregation (pp : Public_parameters.verifier) transcript
      cmt_list query_list s_list proof =
    let transcript = Transcript.list_expand query_t query_list transcript in
    let r, transcript = Fr_generation.random_fr transcript in
    let ok, transcript =
      verify_pack pp r transcript cmt_list query_list s_list proof
    in
    (ok, r, transcript)
end

module Make : functor
  (PC : Kzg.Interfaces.Polynomial_commitment
          with type Commitment.t = Kzg.Bls.G1.t SMap.t)
  -> S =
  Make_impl

include Make (Kzg.Polynomial_commitment)
