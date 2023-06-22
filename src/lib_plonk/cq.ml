(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* This file implements Cq, a lookup protocol described in https://eprint.iacr.org/2022/1763.pdf *)

(* here we call n the size of the table, & k the size of the wire poly to check *)

open Bls

module type Cq_sig = sig
  open Bls

  exception Entry_not_in_table

  type transcript = bytes

  type prover_public_parameters

  type verifier_public_parameters

  type proof

  val setup :
    srs:Srs_g1.t * Srs_g2.t ->
    wire_size:int ->
    table:S.t array ->
    prover_public_parameters * verifier_public_parameters

  val prove :
    prover_public_parameters -> transcript -> S.t array -> proof * transcript

  val verify :
    verifier_public_parameters -> transcript -> proof -> bool * transcript
end

module Internal = struct
  open Utils

  exception Entry_not_in_table

  type transcript = bytes

  type prover_public_parameters = {
    n : int;
    domain_k : Domain.t;
    domain_2k : Domain.t;
    srs1 : Srs_g1.t;
    table : int Scalar_map.t;
    q : G1.t array;
    cms_lagrange : G1.t array;
    cms_lagrange_0 : G1.t array;
  }

  type verifier_public_parameters = {
    n : int;
    k : int;
    srs2_0 : G2.t;
    srs2_1 : G2.t;
    srs2_N_1_k_2 : G2.t;
    cm_table : G2.t;
    cm_zv : G2.t;
  }

  type proof = {
    cm_f : G1.t;
    cm_a : G1.t;
    cm_a0 : G1.t;
    cm_qa : G1.t;
    cm_qb : G1.t;
    cm_m : G1.t;
    cm_p : G1.t;
    cm_b0 : G1.t;
    a0 : Scalar.t;
    b0y : Scalar.t;
    fy : Scalar.t;
    piy : G1.t;
  }

  let kzg_0 p =
    let q, r =
      Poly.(division_xn (p - constant (evaluate p Scalar.zero)) 1 Scalar.zero)
    in
    if not (Poly.is_zero r) then failwith "Cq.kzg_0 : division error." ;
    q

  let compute_and_commit f list =
    let m, l = List.map f list |> Array.of_list |> Array.split in
    (m, pippenger1 l m)

  let setup_prover (srs1, _srs2) (n, domain) k (table_array, table_poly) =
    let domain_k = Domain.build k in
    let domain_2k = Domain.build (2 * k) in

    let table =
      (* Map that binds scalar to its first index in the table ; if there is duplication in the table the first index is kept ; converting to a map allow more efficient research in the table to build m polynomial *)
      fst
      @@ Array.fold_left
           (fun (acc, i) fi ->
             ( Scalar_map.update fi (function None -> Some i | sk -> sk) acc,
               i + 1 ))
           (Scalar_map.empty, 0)
           table_array
    in

    let lagrange =
      Array.init n (fun i ->
          Evaluations.init ~degree:(n - 1) n (fun j ->
              if j = i then Scalar.one else Scalar.zero)
          |> Evaluations.interpolation_fft domain)
    in
    let cms_lagrange = Array.map (commit1 srs1) lagrange in
    let cms_lagrange_0 =
      Array.map (fun p -> commit1 srs1 @@ kzg_0 p) lagrange
    in

    let q =
      Array.init n (fun i ->
          let q, r =
            Poly.(
              division_xn
                (lagrange.(i) * (table_poly - constant table_array.(i)))
                n
                Scalar.(negate one))
          in
          if not (Poly.is_zero r) then
            failwith "Cq.setup_prover : division error." ;
          commit1 srs1 q)
    in

    {n; domain_k; domain_2k; srs1; table; q; cms_lagrange; cms_lagrange_0}

  let setup_verifier (_srs1, srs2) n k table_poly =
    (* cm (X^n - 1) *)
    let cm_zv =
      try G2.(add (Srs_g2.get srs2 n) (negate one))
      with _ ->
        raise
          (Utils.SRS_too_short
             (Printf.sprintf
                "Cq.setup_verifier : SRS_2 of size at least (%d + 1) expected \
                 (size %d received)."
                n
                (Srs_g2.size srs2)))
    in

    let cm_table = commit2 srs2 table_poly in

    let srs2_0 = Srs_g2.get srs2 0 in
    let srs2_1 = Srs_g2.get srs2 1 in
    let srs2_N_1_k_2 = Srs_g2.get srs2 (n - 1 - (k - 2)) in

    {n; k; srs2_0; srs2_1; srs2_N_1_k_2; cm_table; cm_zv}

  let setup ~srs ~wire_size ~table =
    let len_t = Array.length table in
    let n = 1 lsl Z.(log2up (of_int len_t)) in
    (* If the table length is not a power of two we pad until n with the first element of the table *)
    let table =
      if n = Array.length table then table
      else Array.(append table (init (n - len_t) (Fun.const table.(0))))
    in
    let domain = Domain.build n in
    let table_poly = Evaluations.interpolation_fft2 domain table in
    let prv = setup_prover srs (n, domain) wire_size (table, table_poly) in
    let vrf = setup_verifier srs n wire_size table_poly in
    (prv, vrf)

  let map_of_occurences =
    (* by construction, f after convertion has no duplication *)
    Array.fold_left
      (fun acc fi ->
        Scalar_map.update
          fi
          (function None -> Some 1 | Some k -> Some (k + 1))
          acc)
      Scalar_map.empty

  let compute_m_and_t_sparse pp f =
    (* for all scalar in f, we fetch it in the table, get the index i, and then add nb_occ × cm(i-th lagrange poly) to the accumulator *)
    (* Returning a list is ok because we just iterate on ms & it is sparse *)
    let m_and_t_sparse =
      Scalar_map.fold
        (fun fi nb mt_acc ->
          let idx =
            match Scalar_map.find_opt fi pp.table with
            | None -> raise Entry_not_in_table
            | Some idx -> idx
          in
          (idx, Scalar.of_int nb, fi) :: mt_acc)
        (map_of_occurences f)
        []
    in
    let _, cm_m =
      compute_and_commit
        (fun (i, m, _) -> (m, pp.cms_lagrange.(i)))
        m_and_t_sparse
    in
    (m_and_t_sparse, cm_m)

  let compute_a pp beta m_and_t =
    let a, cm_a =
      compute_and_commit
        Scalar.(fun (i, mi, ti) -> (mi / (ti + beta), pp.cms_lagrange.(i)))
        m_and_t
    in
    (List.map2 (fun (i, _, _) a -> (i, a)) m_and_t (Array.to_list a), cm_a)

  let compute_cm_qa pp a =
    snd @@ compute_and_commit (fun (i, ai) -> (ai, pp.q.(i))) a

  let compute_b beta k domain f =
    Evaluations.init ~degree:(k - 1) k (fun i ->
        Scalar.(inverse_exn (f.(i) + beta)))
    |> Evaluations.interpolation_fft domain

  let compute_cm_qb pp beta k b f =
    let f = Evaluations.evaluation_fft pp.domain_2k f in
    let b = Evaluations.evaluation_fft pp.domain_2k b in
    let qb =
      let f_beta =
        Evaluations.linear_c ~evaluations:[f] ~add_constant:beta ()
      in
      let bf = Evaluations.mul_c ~evaluations:[b; f_beta] () in
      let bf_1 =
        Poly.(
          Evaluations.interpolation_fft pp.domain_2k bf - constant Scalar.one)
      in
      let q, r = Poly.division_xn bf_1 k Scalar.(negate one) in
      if Poly.is_zero r then q else raise Entry_not_in_table
    in
    (qb, commit1 pp.srs1 qb)

  let compute_p pp k b0 =
    Poly.mul_xn b0 (pp.n - 1 - (k - 2)) Scalar.zero |> commit1 pp.srs1

  (* as written p. 13, N × a₀ = ΣA_i for i < N ; since A is sparse, it’s fine *)
  let compute_a0 n a =
    Scalar.(List.fold_left (fun acc (_, a) -> acc + a) zero a / of_int n)

  let compute_v k (beta, gamma, eta, eta2) b0y by fy =
    let zhy = Scalar.((gamma ** Z.of_int k) + negate one) in
    let qby = Scalar.(((by * (fy + beta)) + negate one) / zhy) in
    Scalar.(b0y + (eta * fy) + (eta2 * qby))

  let compute_piy pp k (beta, gamma, eta) (b0, f, qb) (b0y, by, fy) =
    let eta2 = Scalar.(eta * eta) in
    let v = compute_v k (beta, gamma, eta, eta2) b0y by fy in
    let num =
      Poly.(b0 + mul_by_scalar eta f + mul_by_scalar eta2 qb - constant v)
    in
    let q, r = Poly.division_xn num 1 Scalar.(negate gamma) in
    if not (Poly.is_zero r) then failwith "Cq.compute_piy : division error." ;
    commit1 pp.srs1 q

  let compute_cm_a0 pp a =
    snd @@ compute_and_commit (fun (i, a) -> (a, pp.cms_lagrange_0.(i))) a

  (* On suppose que f est de degré k < n *)
  let prove pp transcript f_array =
    (* TODO padder f jusqu’à la prochaine puissance de 2 ? *)
    let k = Array.length f_array in
    let f =
      Evaluations.(interpolation_fft pp.domain_k (of_array (k - 1, f_array)))
    in
    let cm_f = commit1 pp.srs1 f in
    (* 1.1, 1.2 *)
    let m_and_t, cm_m = compute_m_and_t_sparse pp f_array in

    (* 2.1 *)
    let transcript = Transcript.list_expand G1.t [cm_f; cm_m] transcript in
    let beta, transcript = Fr_generation.random_fr transcript in
    (* 2.2, 2.3 *)
    let a, cm_a = compute_a pp beta m_and_t in
    (* 2.4 *)
    let cm_qa = compute_cm_qa pp a in
    (* 2.5 *)
    let b = compute_b beta k pp.domain_k f_array in
    (* 2.6 *)
    let b0 = kzg_0 b in
    (* 2.7 *)
    let cm_b0 = commit1 pp.srs1 b0 in
    (* 2.8, 2.9 *)
    let qb, cm_qb = compute_cm_qb pp beta k b f in
    (* 2.10 *)
    let cm_p = compute_p pp k b0 in

    (* 3.1 *)
    let transcript =
      Transcript.list_expand G1.t [cm_a; cm_qa; cm_qb; cm_b0; cm_p] transcript
    in
    let gamma, transcript = Fr_generation.random_fr transcript in
    (* 3.2 *)
    let b0y = Poly.evaluate b0 gamma in
    let by = Poly.evaluate b gamma in
    let fy = Poly.evaluate f gamma in
    (* 3.3 *)
    let a0 = compute_a0 pp.n a in

    (* 3.6.a *)
    let transcript =
      Transcript.list_expand Scalar.t [b0y; by; fy; a0] transcript
    in
    let eta, transcript = Fr_generation.random_fr transcript in
    (* 3.6.b *)
    let piy = compute_piy pp k (beta, gamma, eta) (b0, f, qb) (b0y, by, fy) in
    (* 3.7.a *)
    let cm_a0 = compute_cm_a0 pp a in

    ( {cm_f; cm_a; cm_a0; cm_qa; cm_qb; cm_m; cm_p; cm_b0; a0; b0y; fy; piy},
      transcript )

  let verify pp transcript proof =
    (* 2.1 *)
    let transcript =
      Transcript.list_expand G1.t [proof.cm_f; proof.cm_m] transcript
    in
    let beta, transcript = Fr_generation.random_fr transcript in
    (* 3.1 *)
    let transcript =
      Transcript.list_expand
        G1.t
        [proof.cm_a; proof.cm_qa; proof.cm_qb; proof.cm_b0; proof.cm_p]
        transcript
    in
    let gamma, transcript = Fr_generation.random_fr transcript in
    (* 3.4 *)
    let b0 = Scalar.(of_int pp.n * proof.a0 / of_int pp.k) in
    (* 3.5, 3.6.a *)
    let by = Scalar.((proof.b0y * gamma) + b0) in
    let transcript =
      Transcript.list_expand
        Scalar.t
        [proof.b0y; by; proof.fy; proof.a0]
        transcript
    in
    let eta, transcript = Fr_generation.random_fr transcript in

    (* 2.11 *)
    let check_a =
      Pairing.pairing_check
        G1.
          [
            (negate proof.cm_a, pp.cm_table);
            (proof.cm_qa, pp.cm_zv);
            (add proof.cm_m (negate (mul proof.cm_a beta)), pp.srs2_0);
          ]
    in

    (* 2.12 *)
    let check_b0 =
      Pairing.pairing_check
        G1.[(negate proof.cm_b0, pp.srs2_N_1_k_2); (proof.cm_p, pp.srs2_0)]
    in

    (* 3.5, 3.6.a *)
    let eta2 = Scalar.(eta * eta) in
    let v =
      let v = compute_v pp.k (beta, gamma, eta, eta2) proof.b0y by proof.fy in
      G1.(mul one v)
    in

    (* 3.6.c *)
    let c =
      G1.(add (add proof.cm_b0 (mul proof.cm_f eta)) (mul proof.cm_qb eta2))
    in
    let check_piy =
      Pairing.pairing_check
        G1.
          [
            (add (add c (negate v)) (mul proof.piy gamma), pp.srs2_0);
            (negate proof.piy, pp.srs2_1);
          ]
    in

    (* 3.6.b *)
    let check_a0 =
      Pairing.pairing_check
        G1.
          [
            (add proof.cm_a (negate (mul one proof.a0)), pp.srs2_0);
            (negate proof.cm_a0, pp.srs2_1);
          ]
    in

    (check_a && check_b0 && check_piy && check_a0, transcript)
end

include (Internal : Cq_sig)
