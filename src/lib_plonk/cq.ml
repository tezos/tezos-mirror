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

(* This file implements Cq, a lookup protocol described in
   https://eprint.iacr.org/2022/1763.pdf, in which the prover work is
   independant of the size of the table

   In the code, we call n the size of the table (N), & k the size of the wire
   to check (n).

   TODO https://gitlab.com/tezos/tezos/-/issues/6070
          - the setup is currently computed in O(N²) while it could be computed
            in O(NlogN) with Kate amortized
          - be able to select the wire’s value to lookup
          - integrate to PlonK
          - integrate to aPlonK
*)

open Kzg.Bls
open Identities
open Kzg.Utils
module Degree_check = Kzg.Degree_check.G1_proof

module type Cq_sig = sig
  exception Entry_not_in_table

  type prover_public_parameters

  type verifier_public_parameters

  type proof

  val setup :
    srs:Srs_g1.t * Srs_g2.t ->
    wire_size:int ->
    table:S.t array list ->
    prover_public_parameters * verifier_public_parameters

  val prove :
    prover_public_parameters ->
    Transcript.t ->
    S.t array SMap.t list ->
    proof * Transcript.t

  val verify :
    verifier_public_parameters -> Transcript.t -> proof -> bool * Transcript.t
end

module Internal = struct
  open Utils
  module PC = Kzg.Polynomial_commitment
  module ISet = Set.Make (Int)
  module IMap = Map.Make (Int)

  exception Entry_not_in_table

  type transcript = bytes

  type prover_public_parameters = {
    (* size of the table (= N in the paper) *)
    n : int;
    (* precomputed domain for the wires of the size of the wires *)
    domain_k : Domain.t;
    (* domain twice as large as the previous one (for poly multiplication) *)
    domain_2k : Domain.t;
    (* the table as a list of scalar_maps to be more efficient for searching in
       the prover *)
    table : ISet.t Scalar_map.t list;
    (* N kzg commitments for lagrange polynomials L_i *)
    cms_lagrange : G1.t array;
    (* N kzg commitments for (Li - Li(0))/X *)
    cms_lagrange_0 : G1.t array;
    (* N kzg commitments for Qi = (Li × (T - ti)) / X^N - 1 *)
    q : G1.t array list;
    (* prover public parameters for commitment *)
    pc : PC.Public_parameters.prover;
  }

  type verifier_public_parameters = {
    (* size of the table (= N in the paper) *)
    n : int;
    (* size of the wires (= k in the paper) *)
    k : int;
    (* first element of the SRS_2 *)
    srs2_0 : G2.t;
    (* sencond element of the SRS_2 *)
    srs2_1 : G2.t;
    (* (N-1-k+2) of the SRS_2 *)
    srs2_N_1_k_2 : G2.t;
    (* commitment of the table *)
    cm_table : G2.t list;
    (* commitment of the polynomial X^N - 1 *)
    cm_zv : G2.t;
    (* verifier public parameters for commitment *)
    pc : PC.Public_parameters.verifier;
  }

  type proof = {
    (* commitments *)
    cm_f : PC.Commitment.t;
    cm_f_agg : PC.Commitment.t;
    cm_a : PC.Commitment.t;
    cm_b0 : PC.Commitment.t;
    cm_qa : PC.Commitment.t;
    cm_m : PC.Commitment.t;
    cm_p : Degree_check.Proof.t;
    cm_b0_qb_f : PC.Commitment.t;
    (* evaluations *)
    a0 : Scalar.t list;
    b0y : Scalar.t SMap.t;
    fy : Scalar.t SMap.t;
    fy_agg : Scalar.t SMap.t;
    (* proofs *)
    pc : PC.proof;
    cm_a0 : G1.t;
  }

  (* Wires polynomial *)
  let f_name = "f"

  (* m polynomial : i-th coefficient is the number of occurences of t_i in the
     corresponding wire *)
  let m_name = "m"

  (* A polynomial : i-th coefficient is m_i/(t_i + β) *)
  let a_name = "a"

  (* Q_A polynomial : (A×(T+β) - m)/(X^N - 1) *)
  let qa_name = "qa"

  (* B₀ polynomial : (B(X) - B(0))/X, where B polynomial has i-th coefficient 1/(fi + β) *)
  let b0_name = "b0"

  (* Q_B polynomial : (B×(f+β) - 1)/(X^k - 1) (with k = n in the paper) *)
  let qb_name = "qb"

  (* P polynomial : B₀×X^(N-1-(k-2)) *)
  let p_name = "p"

  (* Wire aggregation polynomial : f₀ + αf₁ + α²f₂ + … *)
  let f_agg_name = "f_agg"

  (* This function is used to aggregate commitments for different proofs *)
  let aggregate_cm cm etas =
    Kzg.Commitment.Commit.with_affine_array_1
      (PC.Commitment.to_map cm |> SMap.values |> Array.of_list)
      etas

  (* We don’t need the generator because we don’t evaluate at gX *)
  let get_pc_query gamma =
    List.map (convert_eval_points ~generator:Scalar.zero ~x:gamma) [[X]]

  (* We use PC’s function to commit our polynomials *)
  let commit1 srs =
    PC.(Commitment.commit_single (Public_parameters.get_commit_parameters srs))

  (* [open_at_0 p] returns (p - p(0))/X *)
  let open_at_0 p =
    let q, r =
      Poly.(division_xn (p - constant (evaluate p Scalar.zero)) 1 Scalar.zero)
    in
    assert (Poly.is_zero r) ;
    q

  (* This function avoid some lines of code duplication *)
  let compute_and_commit f list =
    let m, l = List.map f list |> Array.of_list |> Array.split in
    (m, Kzg.Commitment.Commit.with_affine_array_1 l m)

  let setup_prover (n, domain) k (table_arrays, table_polys) pc =
    let domain_k = Domain.build k in
    let domain_2k = Domain.build (2 * k) in

    (* Map that binds scalar to the set of its indices in the table ;
       converting it to a map allow more efficient research in the table *)
    let table =
      List.map
        (fun t ->
          fst
          @@ Array.fold_left
               (fun (acc, i) fi ->
                 ( Scalar_map.update
                     fi
                     (function
                       | None -> Some (ISet.singleton i)
                       | Some sk -> Some (ISet.add i sk))
                     acc,
                   i + 1 ))
               (Scalar_map.empty, 0)
               t)
        table_arrays
    in

    let lagrange =
      Array.init n (fun i ->
          Evaluations.init ~degree:(n - 1) n (fun j ->
              if j = i then Scalar.one else Scalar.zero)
          |> Evaluations.interpolation_fft domain)
    in
    let cms_lagrange = Array.map (commit1 pc) lagrange in
    let cms_lagrange_0 =
      Array.map (fun p -> commit1 pc @@ open_at_0 p) lagrange
    in

    let q =
      List.map2
        (fun t_poly t_array ->
          Array.init n (fun i ->
              let q, r =
                Poly.(
                  division_xn
                    (lagrange.(i) * (t_poly - constant t_array.(i)))
                    n
                    Scalar.(negate one))
              in
              if not (Poly.is_zero r) then
                failwith "Cq.setup_prover : division error." ;
              commit1 pc q))
        table_polys
        table_arrays
    in

    {n; domain_k; domain_2k; table; q; cms_lagrange; cms_lagrange_0; pc}

  let setup_verifier (_srs1, srs2) n k table_poly pc =
    (* cm (X^n - 1) *)
    let cm_zv =
      try G2.(add (Srs_g2.get srs2 n) (negate one))
      with Invalid_argument _ ->
        raise
          (Kzg.Commitment.SRS_too_short
             (Printf.sprintf
                "Cq.setup_verifier : SRS_2 of size at least (%d + 1) expected \
                 (size %d received)."
                n
                (Srs_g2.size srs2)))
    in

    let cm_table = List.map (Kzg.Commitment.Commit.with_srs2 srs2) table_poly in

    let srs2_0 = Srs_g2.get srs2 0 in
    let srs2_1 = Srs_g2.get srs2 1 in
    let srs2_N_1_k_2 = Srs_g2.get srs2 (n - 1 - (k - 2)) in

    {n; k; srs2_0; srs2_1; srs2_N_1_k_2; cm_table; cm_zv; pc}

  let setup ~srs ~wire_size ~table =
    let len_t = Array.length (List.hd table) in
    let n = 1 lsl Z.(log2up (of_int len_t)) in
    (* If the table length is not a power of two we pad until n with the first element of the table *)
    let table =
      if n = len_t then table
      else
        List.map
          (fun t ->
            if Array.length t <> len_t then
              raise (Invalid_argument "Table columns have different lengths.") ;
            Array.(append t (init (n - len_t) (Fun.const t.(0)))))
          table
    in
    if wire_size > n then
      raise
        (Invalid_argument
           (Printf.sprintf
              "Wire (size = %d) greater than table (size = %d)."
              wire_size
              n)) ;
    let domain = Domain.build n in
    let table_polys = List.map (Evaluations.interpolation_fft2 domain) table in
    let pc_prv, pc_vrf, _ = PC.Public_parameters.setup 0 (srs, srs) in
    let prv = setup_prover (n, domain) wire_size (table, table_polys) pc_prv in
    let vrf = setup_verifier srs n wire_size table_polys pc_vrf in
    (prv, vrf)

  let compute_m_and_t_sparse pp f_arrays f_agg =
    let f_arrays = SMap.values f_arrays in
    (* Returns [(i, mi, ti)], where mi is the number of occurences of ti (aggregated version) in f ;
       returning lists is ok because we will just iterate on them & they are sparse *)
    let m_and_t_sparse =
      Array.fold_left
        (fun (m_map, i) f_agg ->
          (* Index in the table of the i-th line of f_arrays *)
          let idx, _ =
            (* For each value of the i-th line, we search it in the table and
               return its indices in the corresponding column of the table
               the index of the line will be the intersection of the indices
               for all the values of the line *)
            List.fold_left2
              (fun (acc, first) f t ->
                match Scalar_map.find_opt f.(i) t with
                | None -> raise Entry_not_in_table
                | Some idx ->
                    ((if first then idx else ISet.inter acc idx), false))
              (ISet.empty, true)
              f_arrays
              pp.table
          in
          (* If the set is empty it means that the line is not in the table ;
             if there is more than one index it means that there is duplication
             in the table, so we just keep one *)
          let idx =
            try ISet.choose idx with Not_found -> raise Entry_not_in_table
          in
          (* Note that we add f_agg here to keep track of the aggregated value
             at each index (what was called ti before) ; we will need it later
             to compute a ; we use f_agg to avoid recomputation *)
          ( IMap.update
              idx
              (function
                | None -> Some (1, f_agg) | Some (nb, _) -> Some (nb + 1, f_agg))
              m_map,
            i + 1 ))
        (IMap.empty, 0)
        f_agg
      |> fst |> IMap.to_seq |> List.of_seq
      |> List.map (fun (i, (m, t)) -> (i, Scalar.of_int m, t))
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

  let compute_cm_qa alphas pp a =
    snd
    @@ compute_and_commit
         (fun (i, ai) ->
           ( ai,
             fst
             @@ List.fold_left
                  (fun (acc, j) q ->
                    (G1.(add acc (mul q.(i) alphas.(j))), j + 1))
                  (G1.zero, 0)
                  pp.q ))
         a

  let compute_b beta k domain f =
    Evaluations.init ~degree:(k - 1) k (fun i ->
        Scalar.(inverse_exn (f.(i) + beta)))
    |> Evaluations.interpolation_fft domain

  let compute_qb pp beta k b f =
    let f = Evaluations.evaluation_fft pp.domain_2k f in
    let b = Evaluations.evaluation_fft pp.domain_2k b in
    let f_beta = Evaluations.linear_c ~evaluations:[f] ~add_constant:beta () in
    let bf = Evaluations.mul_c ~evaluations:[b; f_beta] () in
    let bf_1 =
      Poly.(Evaluations.interpolation_fft pp.domain_2k bf - constant Scalar.one)
    in
    let q, r = Poly.division_xn bf_1 k Scalar.(negate one) in
    if Poly.is_zero r then q else raise Entry_not_in_table

  let compute_p (pp : prover_public_parameters) transcript k b0 =
    Degree_check.prove_multi
      ~max_commit:(pp.n - 1)
      ~max_degree:(k - 2)
      (PC.Public_parameters.get_commit_parameters pp.pc)
      transcript
      b0

  (* as written p. 13, N × a₀ = ΣA_i for i < N ; since A is sparse, it’s fine *)
  let compute_a0 n a =
    Scalar.(List.fold_left (fun acc (_, a) -> acc + a) zero a / of_int n)

  let compute_cm_a0 pp etas a =
    (* since cm_a0 is a kzg proof, we can batch every a polynomials into one *)
    let a_agg =
      List.fold_left
        (fun (global_acc, k) a ->
          ( List.fold_left
              (fun acc (i, ai) ->
                IMap.update
                  i
                  (function
                    | None -> Some Scalar.(etas.(k) * ai)
                    | Some a -> Some Scalar.(a + (etas.(k) * ai)))
                  acc)
              global_acc
              a,
            k + 1 ))
        (IMap.empty, 0)
        a
      |> fst |> IMap.to_seq |> List.of_seq
    in
    snd @@ compute_and_commit (fun (i, a) -> (a, pp.cms_lagrange_0.(i))) a_agg

  (* produces a kzg proof for steps 3.5 & 3.6 *)
  let kzg_prove (pp : prover_public_parameters) transcript n
      ((cm_f, f_aux), (cm_f_agg, f_agg_aux)) (b0_map, f, f_agg, qb) =
    let qb_map = SMap.Aggregation.of_list ~n "" qb_name qb in
    let f_map = SMap.union_disjoint_list [f; f_agg; b0_map; qb_map] in
    let cm_b0, b0_aux = PC.commit pp.pc b0_map in
    let cm_qb, qb_aux = PC.commit pp.pc qb_map in
    (* Does this must be in lexicographic order ? *)
    let cm_map = PC.Commitment.(recombine [cm_b0; cm_f; cm_f_agg; cm_qb]) in
    let aux =
      PC.Commitment.(recombine_prover_aux [b0_aux; f_aux; f_agg_aux; qb_aux])
    in
    (* 3.1 *)
    let transcript = Transcript.expand PC.Commitment.t cm_map transcript in
    let gamma, transcript = Fr_generation.random_fr transcript in
    (* 3.2 *)
    let b0y = SMap.map (fun p -> Poly.evaluate p gamma) b0_map in
    let qby = SMap.map (fun p -> Poly.evaluate p gamma) qb_map in
    let fy = SMap.map (fun p -> Poly.evaluate p gamma) f in
    let fy_agg = SMap.map (fun p -> Poly.evaluate p gamma) f_agg in
    let query = get_pc_query gamma in
    let secret = [f_map] in
    let cm_aux = [aux] in
    let answers =
      [SMap.singleton "x" (SMap.union_disjoint_list [b0y; fy; fy_agg; qby])]
    in
    let proof, transcript =
      PC.prove pp.pc transcript secret cm_aux query answers
    in
    (b0y, fy, fy_agg, cm_map, cm_b0, proof, transcript)

  (* verify the values provided by kzg_prove *)
  let kzg_verify pp transcript proof k beta =
    (* 3.1 *)
    let transcript =
      Transcript.expand PC.Commitment.t proof.cm_b0_qb_f transcript
    in
    let gamma, transcript = Fr_generation.random_fr transcript in
    (* 3.4 *)
    let n = List.length proof.a0 in
    let b0 =
      List.mapi
        (fun i a0 ->
          ( SMap.Aggregation.add_prefix ~n ~i "" b0_name,
            Scalar.(of_int pp.n * a0 / of_int pp.k) ))
        proof.a0
    in
    (* 3.5, 3.6.a *)
    let zhy = Scalar.((gamma ** Z.of_int k) + negate one) in
    let by =
      List.map2
        (fun b0y (_, b0) -> Scalar.((b0y * gamma) + b0))
        (SMap.values proof.b0y)
        b0
    in
    let qby =
      let i = ref (-1) in
      SMap.of_list
      @@ List.map2
           (fun by fy ->
             i := !i + 1 ;
             ( SMap.Aggregation.add_prefix ~n ~i:!i "" qb_name,
               Scalar.(((by * (fy + beta)) + negate one) / zhy) ))
           by
           (SMap.values proof.fy_agg)
    in
    let cm = [proof.cm_b0_qb_f] in
    let query = get_pc_query gamma in
    let answers =
      [
        SMap.singleton
          "x"
          (SMap.union_disjoint_list [proof.b0y; proof.fy; proof.fy_agg; qby]);
      ]
    in
    PC.verify pp.pc transcript cm query answers proof.pc

  (* Checks that f_agg(γ) = f₀(γ) + αf₁(γ) + α²f₂(γ) *)
  let verify_f_agg alphas proof =
    let nb_wires = Array.length alphas in
    (* format proof.fy as [[f₀, f₁, …] ; [f₀, f₁, …] ; …] *)
    let formatted_fy =
      let (rev_formatted, last), _ =
        SMap.fold
          (fun _ f ((acc_global, acc), count) ->
            (* if count = nb_wires, it means we went accross every wires for
               this proof & the current wire is for a new proof *)
            if count = nb_wires - 1 then ((List.rev acc :: acc_global, [f]), 0)
            else ((acc_global, f :: acc), count + 1))
          proof.fy
          (([], []), -1)
      in
      List.rev (List.rev last :: rev_formatted)
    in
    (* Check equality for all proofs *)
    List.for_all2
      (fun fs f_agg ->
        let sum_fs, _ =
          List.fold_left
            (fun (acc, i) fy -> (Scalar.(acc + (fy * alphas.(i))), i + 1))
            (Scalar.zero, 0)
            fs
        in
        Scalar.eq sum_fs f_agg)
      formatted_fy
      (SMap.values proof.fy_agg)

  (* each f must be a power of two & of the same size *)
  let prove pp transcript f_map_list =
    let k = Array.length (snd @@ SMap.choose (List.hd f_map_list)) in
    (* n = nb_proofs *)
    let n = List.length f_map_list in
    (* The map of all wires polynomials *)
    let f_map =
      SMap.union_disjoint_list
      @@ List.mapi
           (fun i f_map ->
             SMap.map
               (fun f ->
                 Evaluations.(
                   interpolation_fft pp.domain_k (of_array (k - 1, f))))
               f_map
             |> SMap.Aggregation.prefix_map ~n ~i "")
           f_map_list
    in
    let cm_f, f_aux = PC.commit pp.pc f_map in
    let transcript = Transcript.expand PC.Commitment.t cm_f transcript in
    (* α will be used to aggregate wires & table’s columns *)
    let alpha, transcript = Fr_generation.random_fr transcript in
    let alphas = Fr_generation.powers (SMap.cardinal f_map) alpha in
    let f_agg_arrays_list =
      List.map
        (fun f_map ->
          Array.init k (fun i ->
              fst
              @@ SMap.fold
                   (fun _ f (acc, j) ->
                     (Scalar.(acc + (alphas.(j) * f.(i))), j + 1))
                   f_map
                   (Scalar.zero, 0)))
        f_map_list
    in
    let f_agg_list =
      List.map
        (fun f ->
          Evaluations.(interpolation_fft pp.domain_k (of_array (k - 1, f))))
        f_agg_arrays_list
    in
    let f_agg_map = SMap.Aggregation.of_list ~n "" f_agg_name f_agg_list in
    let cm_f_agg, f_agg_aux = PC.commit pp.pc f_agg_map in

    (* 1.1, 1.2 *)
    let m_and_t, cm_m =
      List.map2 (compute_m_and_t_sparse pp) f_map_list f_agg_arrays_list
      |> List.split
    in
    let cm_m, _ = PC.Commitment.of_list pp.pc ~name:m_name cm_m in

    (* 2.1 *)
    let transcript =
      Transcript.list_expand PC.Commitment.t [cm_f_agg; cm_m] transcript
    in
    let beta, transcript = Fr_generation.random_fr transcript in

    (* 2.2, 2.3 *)
    let a, cm_a = List.map (compute_a pp beta) m_and_t |> List.split in
    let cm_a, _ = PC.Commitment.of_list pp.pc ~name:a_name cm_a in
    (* 2.4 *)
    let cm_qa, _ =
      List.map (compute_cm_qa alphas pp) a
      |> PC.Commitment.of_list pp.pc ~name:qa_name
    in

    (* 2.5 *)
    let b = List.map (compute_b beta k pp.domain_k) f_agg_arrays_list in
    (* 2.6 *)
    let b0 =
      SMap.of_list
      @@ List.mapi
           (fun i b ->
             (SMap.Aggregation.add_prefix ~n ~i "" b0_name, open_at_0 b))
           b
    in
    (* 2.8, 2.9 *)
    let qb = List.map2 (compute_qb pp beta k) b (SMap.values f_agg_map) in

    let transcript =
      Transcript.list_expand PC.Commitment.t [cm_a; cm_qa] transcript
    in
    (* 3.6.b *)
    let b0y, fy, fy_agg, cm_b0_qb_f, cm_b0, pc, transcript =
      kzg_prove
        pp
        transcript
        n
        ((cm_f, f_aux), (cm_f_agg, f_agg_aux))
        (b0, f_map, f_agg_map, qb)
    in

    (* 2.10 *)
    let cm_p, transcript =
      Degree_check.prove_multi
        ~max_commit:(pp.n - 1)
        ~max_degree:(k - 2)
        (PC.Public_parameters.get_commit_parameters pp.pc)
        transcript
        cm_b0
        b0
    in

    let transcript = Transcript.expand Degree_check.Proof.t cm_p transcript in

    (* 3.3 *)
    let a0 = List.map (compute_a0 pp.n) a in

    (* 3.6.a *)
    let transcript = Transcript.list_expand Scalar.t a0 transcript in
    let eta, transcript = Fr_generation.random_fr transcript in
    (* 3.7.a *)
    let cm_a0 = compute_cm_a0 pp (Fr_generation.powers n eta) a in

    ( {
        cm_f;
        cm_f_agg;
        cm_a;
        cm_a0;
        cm_b0;
        cm_qa;
        cm_m;
        cm_p;
        a0;
        b0y;
        fy;
        fy_agg;
        pc;
        cm_b0_qb_f;
      },
      transcript )

  let verify pp transcript proof =
    let transcript = Transcript.expand PC.Commitment.t proof.cm_f transcript in
    (* α will be used to aggregate wires & table’s columns *)
    let alpha, transcript = Fr_generation.random_fr transcript in
    let alphas = Fr_generation.powers (List.length pp.cm_table) alpha in
    (* 2.1 *)
    let transcript =
      Transcript.list_expand
        PC.Commitment.t
        [proof.cm_f_agg; proof.cm_m]
        transcript
    in
    let beta, transcript = Fr_generation.random_fr transcript in
    (* 3.1 *)
    let transcript =
      Transcript.list_expand
        PC.Commitment.t
        [proof.cm_a; proof.cm_qa]
        transcript
    in

    let f_agg_verif = verify_f_agg alphas proof in

    (* 3.5, 3.6.a *)
    let kzg_verif, transcript = kzg_verify pp transcript proof pp.k beta in

    (* 2.12 *)
    (* At this point, b0 already has been added to the transcript, & it is
       added again in Degree_check *)
    let check_b0, transcript =
      Degree_check.verify_multi
        pp.srs2_N_1_k_2
        transcript
        proof.cm_b0
        proof.cm_p
    in

    let transcript =
      Transcript.expand Degree_check.Proof.t proof.cm_p transcript
    in

    (* 3.6.a *)
    let transcript = Transcript.list_expand Scalar.t proof.a0 transcript in
    let eta, transcript = Fr_generation.random_fr transcript in
    let etas =
      Fr_generation.powers
        (PC.Commitment.to_map proof.cm_a |> SMap.cardinal)
        eta
    in

    let cm_a = aggregate_cm proof.cm_a etas in
    let cm_qa = aggregate_cm proof.cm_qa etas in
    let cm_m = aggregate_cm proof.cm_m etas in

    let a0 =
      List.fold_left
        Scalar.(fun acc a -> (acc * eta) + a)
        Scalar.zero
        (List.rev proof.a0)
    in

    (* 2.11 *)
    let check_a =
      let table, _ =
        List.fold_left
          (fun (acc, i) c -> G2.(add acc (mul c alphas.(i)), i + 1))
          (G2.zero, 0)
          pp.cm_table
      in
      Pairing.pairing_check
        G1.
          [
            (negate cm_a, table);
            (cm_qa, pp.cm_zv);
            (add cm_m (negate (mul cm_a beta)), pp.srs2_0);
          ]
    in

    (* 3.6.b *)
    let check_a0 =
      Pairing.pairing_check
        G1.
          [
            (add cm_a (negate (mul one a0)), pp.srs2_0);
            (negate proof.cm_a0, pp.srs2_1);
          ]
    in

    (f_agg_verif && kzg_verif && check_a && check_b0 && check_a0, transcript)
end

include (Internal : Cq_sig)
