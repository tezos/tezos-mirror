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
module SMap = Kzg.SMap

module type Super_PC_sig = sig
  include Polynomial_commitment.PC_for_distribution_sig

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

(** Extension of the KZG_pack implementation with additional
    types and functions used in by Distributed_prover  *)
module Kzg_pack_impl = struct
  module Pack = Aggregation.Pack
  module PC = Polynomial_commitment.Kzg_impl
  module BasePC = Aggregation.Polynomial_commitment.Make_impl (PC)

  include (BasePC : module type of BasePC)

  type worker_msg = Scalar.t * string list list [@@deriving repr]

  type main_prover_msg = Poly.t list * Commitment.prover_aux list
  [@@deriving repr]

  type main_prover_state =
    Public_parameters.prover
    * Transcript.t
    * Scalar.t
    * query list
    * Scalar.t SMap.t list
    * main_prover_msg

  type partial_prover_aux = {r : Scalar.t; s_list : Scalar.t SMap.t list}

  let merge_answers : answer list -> answer =
    let open SMap in
    List.fold_left (union (fun _k m1 m2 -> Some (union_disjoint m1 m2))) empty

  let distributed_prove_worker f_map_list prover_aux_list (r, poly_keys_list) =
    let gen_powers r l =
      List.mapi (fun i x -> (x, Scalar.pow r @@ Z.of_int i)) l |> SMap.of_list
    in
    let r_powers_list = List.map (gen_powers r) poly_keys_list in
    let f_list =
      List.map2
        (fun f_map r_map ->
          let polys = SMap.bindings f_map in
          let coeffs = List.map (fun (name, _) -> SMap.find name r_map) polys in
          Poly.linear (List.map snd polys) coeffs)
        f_map_list
        r_powers_list
    in
    (f_list, prover_aux_list)

  let distributed_expand_transcript transcript query_list answer_list =
    let transcript = Transcript.list_expand query_t query_list transcript in
    Transcript.list_expand answer_t answer_list transcript

  let distributed_prove_main1 (pp : Public_parameters.prover) transcript
      query_list answer_list secret_list prover_aux_list :
      worker_msg * main_prover_state =
    let r, transcript = Fr_generation.random_fr transcript in
    let s_list = List.map (batch_answers r) answer_list in

    let get_keys map_map =
      SMap.fold
        (fun _ m acc -> SMap.union (fun _ _ x -> Some x) m acc)
        map_map
        SMap.empty
      |> SMap.bindings |> List.map fst
    in
    let poly_keys_list = List.map get_keys answer_list in
    let worker_message = (r, poly_keys_list) in
    let main_msg =
      distributed_prove_worker secret_list prover_aux_list worker_message
    in
    let state = (pp, transcript, r, query_list, s_list, main_msg) in
    (worker_message, state)

  let distributed_prove_main2
      ((pp, transcript, r, query_list, s_list, main_msg) : main_prover_state)
      worker_msg_list =
    let worker_msg_list = main_msg :: worker_msg_list in
    let f_list_list = List.map fst worker_msg_list in
    let f_list =
      Plonk.List.mapn (List.fold_left Poly.add Poly.zero) f_list_list
    in
    let prover_aux_list_list = List.map snd worker_msg_list in
    let prover_aux_list =
      Plonk.List.mapn Commitment.recombine_prover_aux prover_aux_list_list
    in
    (* [cmts_list] is a list of G1.t SMap.t, containing the PC commitments to
       every polynomial (note that PC.Commitment.t = Bls12_381.G1.t SMap.t) *)
    let cmts_list =
      List.map
        (fun (cmts, _prover_aux) ->
          List.map snd @@ SMap.bindings cmts |> Array.of_list)
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
    (proof, transcript, {s_list; r})
end
