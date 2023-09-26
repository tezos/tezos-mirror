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

(** Extension of the PC signature with additional types and functions used
    in by Distributed_prover  *)

module type PC_for_distribution_sig = sig
  module BasePC : Kzg.Interfaces.Polynomial_commitment

  include module type of BasePC

  val merge_answers : answer list -> answer

  type worker_msg [@@deriving repr]

  type main_prover_state

  type main_prover_msg [@@deriving repr]

  type partial_prover_aux

  val distributed_expand_transcript :
    Transcript.t -> query list -> answer list -> Transcript.t

  val distributed_prove_main1 :
    Public_parameters.prover ->
    Transcript.t ->
    query list ->
    answer list ->
    secret list ->
    Commitment.prover_aux list ->
    (* we assume the same message is sent to all workers *)
    worker_msg * main_prover_state

  val distributed_prove_worker :
    secret list -> Commitment.prover_aux list -> worker_msg -> main_prover_msg

  val distributed_prove_main2 :
    main_prover_state ->
    main_prover_msg list ->
    proof * Transcript.t * partial_prover_aux
end

(** Extension of the KZG implementation with additional types and functions
    used in by Distributed_prover  *)
module Kzg_impl = struct
  module BasePC = Kzg.Polynomial_commitment
  include BasePC

  type worker_msg = Scalar.t SMap.t SMap.t [@@deriving repr]

  (* batched polynomials, keyed by evaluation points *)
  type main_prover_msg = Poly.t SMap.t [@@deriving repr]

  type main_prover_state = {
    srs : Public_parameters.prover;
    transcript : Transcript.t;
    query : query;
    batched_answer : Scalar.t SMap.t;
    main_msg : main_prover_msg;
  }

  type partial_prover_aux = unit

  let merge_answers : answer list -> answer =
    let open SMap in
    List.fold_left (union (fun _k m1 m2 -> Some (union_disjoint m1 m2))) empty

  let distributed_prove_worker f_map_list _prover_aux_list coeffs =
    let f_map = group_secrets f_map_list in
    batch_polys coeffs f_map

  let distributed_expand_transcript transcript query_list answer_list =
    let transcript = Transcript.list_expand query_t query_list transcript in
    Transcript.list_expand answer_t answer_list transcript

  let distributed_prove_main1 srs transcript query_list answer_list secret_list
      prover_aux_list =
    let query = group_queries query_list in
    let answer = group_answers answer_list in
    let y_map, transcript = sample_ys transcript query in
    let batched_answer, coeffs = batch_answer y_map answer in
    let main_msg =
      distributed_prove_worker secret_list prover_aux_list coeffs
    in
    let state = {srs; transcript; query; batched_answer; main_msg} in
    (coeffs, state)

  let distributed_prove_main2 {srs; transcript; query; batched_answer; main_msg}
      batched_polys_list =
    let batched_polys_list = main_msg :: batched_polys_list in
    let batched_polys =
      SMap.map_list_to_list_map batched_polys_list
      |> SMap.map (List.fold_left Poly.add Poly.zero)
    in
    let proof = compute_Ws srs batched_polys batched_answer query in
    (proof, Transcript.expand proof_t proof transcript, ())
end
