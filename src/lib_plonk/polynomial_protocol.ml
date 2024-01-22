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
open Identities

(** A polynomial protocol allows a prover to convince a verifier of the fact
    that certain algebraic identites between polynomials (polynomials that have
    been previously committed) hold when evaluated over a set of points.
    (In our implementation such set of points must be a subgroup of roots of
    unity.)

    For example, let K be a field and let H be a subset of K.
    Let f1(X), f2(X) and f3(X) be univariate polynomials over K and let
    C1, C2 and C3 be polynomial commitments to f1, f2 and f3, respectively.
    A polynomial protocol allows a prover to argue knowledge of:
    {[
      PoK{ (f1, f2, f3) : Ci = Com(fi) ∀ i  /\  f1(x) * f2(x) = f3(x) ∀ x ∈ H }
    ]}
    This can be accomplished by evaluating polynomial commitments at a single
    point ξ (uniformly sampled from K). For that, note that the above
    polynomial identity holds for every x ∈ H iff polynomial (f1 * f2 - f3) is
    divisible by Zh, the minimal (monic) polynomial that vanishes over set H.
    Thus, the prover can commit to polynomial T := (f1 * f2 - f3) / Zh and
    evaluate polynomial commitments C1, C2, C3, T at ξ (chosen after T). Let
    c1, c2, c3, t be such evaluations.
    The verifier can then check that  t * Zh(ξ) = c1 * c2 - c3.

    A general polynomial protocol should allow for multiple identities involving
    addition, multiplication and composition of polynomials.

    See {{: https://eprint.iacr.org/2019/953.pdf }2019/953 Section 4.1} for
    more details. *)

(** Functor building an implementation of a polynomial protocol given a
    polynomial commitment scheme [PC]. *)
module Make_impl (PC : Kzg.Interfaces.Polynomial_commitment) = struct
  module PC = PC

  type prover_public_parameters = PC.Public_parameters.prover [@@deriving repr]

  type verifier_public_parameters = PC.Public_parameters.verifier
  [@@deriving repr]

  type proof = {
    cm_t : PC.Commitment.t;
    pc_proof : PC.proof;
    pc_answers : PC.answer list;
  }
  [@@deriving repr]

  (** [split_t n t nb_of_t_chunks] splits [t] polynomial in [nb_of_t_chunks]
      polynomials of at most [n] coefficients (and at most degree [n]-1),
      except the last one that may have more coeffictients depending on [t]’s
      degree
   *)
  let split_t n t nb_of_t_chunks =
    let nb_digits = String.length @@ string_of_int (nb_of_t_chunks - 1) in
    List.mapi
      (fun i t_i -> ("T_" ^ Csir.string_key_of_int ~nb_digits i, t_i))
      (Poly.split ~nb_chunks:nb_of_t_chunks n t)
    |> SMap.of_list

  (* [compute_t ~n ~alpha evaluations] returns a polynomial T splitted in chunks,
     where [T(X) = (sum_i alpha^i evaluations[i]) / (X^n - 1)] and the returned
     chunks [{ 'T_0' -> T0; 'T_1' -> T1; 'T_2' -> T2 }] are such that
     [T = T0 + X^n T1 + X^{2n} T2]. *)
  let compute_t ~n ~alpha ~nb_of_t_chunks evaluated_identities =
    let nb_ids = SMap.cardinal evaluated_identities in
    let evaluations = SMap.values evaluated_identities in
    let alphas = Fr_generation.powers nb_ids alpha |> Array.to_list in
    let s_eval = Evaluations.linear_c ~evaluations ~linear_coeffs:alphas () in
    let s_deg = Evaluations.degree s_eval in
    let domain = Domain.build_power_of_two (Z.log2up (Z.of_int (s_deg + 1))) in
    let s = Evaluations.interpolation_fft domain s_eval in
    let t, rem = Poly.division_xn s n Scalar.(negate one) in
    if Poly.is_zero rem then split_t n t nb_of_t_chunks
    else raise @@ Poly.Rest_not_null "T is not divisible by Zh"

  let eval_and_batch_ids (alpha, x) pc_answers identities =
    let answers =
      let f _key m1 m2 = Some (SMap.union_disjoint m1 m2) in
      List.fold_left (SMap.union f) SMap.empty pc_answers
    in
    identities x answers |> SMap.values |> Fr_generation.batch alpha

  let verify_t n x ids_batch t_evals =
    let t_eval = Fr_generation.batch (Scalar.pow x (Z.of_int n)) t_evals in
    let zh = Scalar.(sub (pow x (Z.of_int n)) one) in
    Scalar.(eq ids_batch (t_eval * zh))

  let setup ~setup_params ~srs = PC.Public_parameters.setup setup_params srs

  (* This function is the common code between prove & Aggregation.PP.prove *)
  let prove_aux pc_public_parameters transcript n generator secrets eval_points
      evaluations identities nb_of_t_chunks =
    let alpha, transcript = Fr_generation.random_fr transcript in
    let evaluated_ids = identities evaluations in
    let t = compute_t ~n ~alpha ~nb_of_t_chunks evaluated_ids in
    let cm_t, t_prover_aux = PC.commit pc_public_parameters t in
    let transcript = Transcript.expand PC.Commitment.t cm_t transcript in
    let x, transcript = Fr_generation.random_fr transcript in
    let prover_aux_list = t_prover_aux :: List.map snd secrets in
    let polys_list = t :: List.map fst secrets in
    let eval_points = [X] :: eval_points in
    let query_list = List.map (convert_eval_points ~generator ~x) eval_points in
    let answer_list = List.map2 PC.evaluate polys_list query_list in
    ( (alpha, x, answer_list, cm_t),
      polys_list,
      prover_aux_list,
      query_list,
      transcript )

  let prove pc_public_parameters transcript ~n ~generator ~secrets ~eval_points
      ~evaluations ~identities ~nb_of_t_chunks =
    let ( (_, _, answer_list, cm_t),
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
    let pc_proof, transcript =
      PC.prove
        pc_public_parameters
        transcript
        polys_list
        prover_aux_list
        query_list
        answer_list
    in
    ({cm_t; pc_proof; pc_answers = answer_list}, transcript)

  type pp_commit_to_t_r = Evaluations.t SMap.t [@@deriving repr]

  (* This function is the common code between verify & Aggregation.PP.verify *)
  let verify_aux transcript generator commitments eval_points proof =
    let alpha, transcript = Fr_generation.random_fr transcript in
    let transcript = Transcript.expand PC.Commitment.t proof.cm_t transcript in
    let x, transcript = Fr_generation.random_fr transcript in
    let cm_list = proof.cm_t :: commitments in
    let eval_points = [X] :: eval_points in
    let query_list = List.map (convert_eval_points ~generator ~x) eval_points in
    (alpha, x, transcript, cm_list, query_list)

  let verify pc_public_parameters transcript ~n ~generator ~commitments
      ~eval_points ~identities proof =
    let alpha, x, transcript, cmts, query_list =
      verify_aux transcript generator commitments eval_points proof
    in
    let pc_verif, transcript =
      PC.verify
        pc_public_parameters
        transcript
        cmts
        query_list
        proof.pc_answers
        proof.pc_proof
    in
    let ids_batch = eval_and_batch_ids (alpha, x) proof.pc_answers identities in
    let t_verif =
      let t_evals =
        List.hd proof.pc_answers
        |> SMap.find (string_of_eval_point X)
        |> SMap.values
      in
      verify_t n x ids_batch t_evals
    in
    (pc_verif && t_verif, transcript)
end

(**  Output signature of the functor [Polynomial_protocol.Make]. *)
module type S = sig
  (** Underlying polynomial commitment scheme on which the polynomial protocol
      is based. Input of the functor [Polynomial_protocol.Make]. *)
  module PC : Kzg.Interfaces.Polynomial_commitment

  (** The type of prover public parameters. *)
  type prover_public_parameters = PC.Public_parameters.prover [@@deriving repr]

  (** The type of verifier public parameters. *)
  type verifier_public_parameters = PC.Public_parameters.verifier
  [@@deriving repr]

  (** The type for proofs, containing a commitment to the polynomial T that
    asserts the satisfiability of the identities over the subset of interest,
    as well as a [PC] proof and a list of [PC] answers. *)
  type proof = {
    cm_t : PC.Commitment.t;
    pc_proof : PC.proof;
    pc_answers : PC.answer list;
  }
  [@@deriving repr]

  (** The polynomial commitment setup function, requires a labeled
      argument of setup parameters for the underlying [PC] and a labeled
      argument containing the path location of a set of SRS files. *)
  val setup :
    setup_params:PC.Public_parameters.setup_params ->
    srs:Srs.t * Srs.t ->
    prover_public_parameters * verifier_public_parameters * Transcript.t

  (** The prover function. Takes as input the [prover_public_parameters],
    an initial [transcript] (possibly including a context if this [prove] is
    used as a building block of a bigger protocol), the size [n] of subgroup H,
    the canonical [generator] of subgroup H, a list of [secrets] including
    polynomials that have supposedly been committed (and a verifier received
    such commitments) as well as prover auxiliary information generated
    during the committing process, a list of evaluation point lists specifying
    the evaluation points where each secret needs to be evaluated at,
    a map of the above-mentioned polynomials this time in FFT [evaluations] form,
    for efficient polynomial multiplication, and some [prover_identities] that
    are supposedly satisfied by the secret polynomials.
    Outputs a proof and an updated transcript. *)
  val prove :
    prover_public_parameters ->
    Transcript.t ->
    n:int ->
    generator:Scalar.t ->
    secrets:(Poly.t SMap.t * PC.Commitment.prover_aux) list ->
    eval_points:eval_point list list ->
    evaluations:Evaluations.t SMap.t ->
    identities:prover_identities ->
    nb_of_t_chunks:int ->
    proof * Transcript.t

  (** The verifier function. Takes as input the [verifier_public_parameters],
    an initial [transcript] (that should coincide with the initial transcript
    used by [prove]), the size [n] of subgroup H, the canonical [generator] of
    subgroup H, a list of [commitments] to the secret polynomials by the prover,
    a list of evaluation points as in [prove], some [verifier_identities], and
    a [proof].
    Outputs a [bool] value representing acceptance or rejection. *)
  val verify :
    verifier_public_parameters ->
    Transcript.t ->
    n:int ->
    generator:Scalar.t ->
    commitments:PC.Commitment.t list ->
    eval_points:eval_point list list ->
    identities:verifier_identities ->
    proof ->
    bool * Transcript.t
end

module Make : functor (PC : Kzg.Interfaces.Polynomial_commitment) ->
  S with module PC = PC =
  Make_impl

include Make (Kzg.Polynomial_commitment)
