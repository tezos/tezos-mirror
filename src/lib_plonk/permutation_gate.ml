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

open Bls
open Utils
open Identities
module L = Plompiler.LibCircuit

module Permutation_gate_impl (PP : Polynomial_protocol.S) = struct
  module PP = PP
  module Commitment = PP.PC.Commitment

  let z_name = "Z"

  let zg_name z_name = z_name ^ "g"

  let l1 = "L1"

  let ids_label = "Perm"

  (* element preprocessed and known by both prover and verifier *)
  type public_parameters = {
    g_map_perm_PP : Poly.t SMap.t;
    cm_g_map_perm_PP : Commitment.t SMap.t;
    s_poly_map : Poly.t SMap.t;
    cm_s_poly_map : Commitment.t SMap.t;
    permutation : int array;
  }

  let srs_size ~zero_knowledge ~n = if zero_knowledge then n + 9 else n

  let one = Scalar.one

  let zero = Scalar.zero

  let mone = Scalar.negate one

  let quadratic_non_residues = Fr_generation.build_quadratic_non_residues 8

  let get_k k =
    if k < 8 then quadratic_non_residues.(k)
    else raise (Invalid_argument "Permutation.get_k : k must be lower than 8.")

  module Partition = struct
    module IntSet = Set.Make (Int)
    module IntMap = Map.Make (Int)

    type t = IntSet.t IntMap.t

    (* receives [wire_indices], a map from wire names (e.g. "a", "b", "c") to
       [int list], flattens its data into a concatenated list of indices [idxs]
       and outputs a map keyed by indices, pointing to the set of (integer)
       positions where the index appears in [idxs] *)
    let build_partition wire_indices =
      (* [add_IntMap i e map] adds [e] to the set bound to [i] in [map],
         if [i] is not bound, it binds [i] to the singleton set {e} *)
      let add_IntMap i e map =
        let set = Option.value (IntMap.find_opt i map) ~default:IntSet.empty in
        IntMap.add i (IntSet.add e set) map
      in
      let idxs = SMap.values wire_indices in
      let map, _i =
        List.fold_left
          (fun (int_map, i) wire_indices_i ->
            let new_map, j =
              Array.fold_left
                (fun (map, j) h ->
                  let new_map = add_IntMap h (i + j) map in
                  (new_map, j + 1))
                (int_map, 0)
                wire_indices_i
            in
            (new_map, i + j))
          (IntMap.empty, 0)
          idxs
      in
      map

    (* returns a permutation in the form of [int array] which splits in cycles
       that involve the indices of each group in the given [partition], e.g.
       on input partition := { 0 -> { 0; 3; 4 } ; 1 -> { 1; 2 } }
       outputs permutation [| 3 2 1 4 0 |] *)
    let partition_to_permutation partition =
      let kn =
        IntMap.fold (fun _ set sum -> sum + IntSet.cardinal set) partition 0
      in
      (* array initialisation *)
      let permutation = Array.make kn (-1) in
      let set_cycle_in_permutation _idx cycle =
        match IntSet.cardinal cycle with
        | 0 -> failwith "cycles_to_permutation_map_set : empty cycle"
        | 1 ->
            (* σ(e) = e *)
            let e = IntSet.choose cycle in
            permutation.(e) <- e
        | n ->
            let first = IntSet.min_elt cycle in
            let aux e (i, prec) =
              if i = 0 then (i + 1, e)
              else if i < n - 1 then (
                permutation.(prec) <- e ;
                (i + 1, e))
              else (
                permutation.(prec) <- e ;
                permutation.(e) <- first ;
                (i + 1, e))
            in
            ignore @@ IntSet.fold aux cycle (0, -1)
      in
      IntMap.iter set_cycle_in_permutation partition ;
      (* If cycles is a legit partition of [kn], no -1 should be left *)
      if Array.mem (-1) permutation then
        failwith "cycles is not a 'partition' of kn"
      else permutation
  end

  module Preprocessing = struct
    (* Returns the minimal (monic) polynomial L1 that satisfies
       L1(generator) = 1 and L1(h) = 0 for all h != generator in domain,
       where generator is the first non-trivial element in domain. *)
    let compute_l1 domain =
      let size_domain = Domain.length domain in
      let scalar_list =
        Array.append
          [|zero; one|]
          Array.(init (size_domain - 2) (fun _ -> zero))
      in
      Evaluations.interpolation_fft2 domain scalar_list

    (* returns [sid_0, …, sid_k] *)
    let sid_list_non_quadratic_residues size =
      if size > 8 then
        raise (Failure "sid_list_non_quadratic_residues: sid list too long")
      else List.init size (fun i -> Poly.of_coefficients [(get_k i, 1)])

    let sid_map_non_quadratic_residues_prover size =
      if size > 8 then
        raise (Failure "sid_map_non_quadratic_residues: sid map too long")
      else
        SMap.of_list
          (List.init size (fun i ->
               let k = get_k i in
               ("Si" ^ string_of_int (i + 1), Poly.of_coefficients [(k, 1)])))

    let evaluations_sid nb_sid evaluations =
      let domain_evals = Evaluations.find_evaluation evaluations "X" in
      SMap.of_list
        (List.init nb_sid (fun i ->
             let k = get_k i in
             ( "Si" ^ string_of_int (i + 1),
               Evaluations.mul_by_scalar k domain_evals )))

    let ssigma_map_non_quadratic_residues permutation domain size =
      let n = Domain.length domain in
      let ssigma_map =
        SMap.of_list
          (List.init size (fun i ->
               let offset = i * n in
               let coeff_list =
                 Array.init n (fun j ->
                     let s_ij = permutation.(offset + j) in
                     let coeff = get_k (s_ij / n) in
                     let index = s_ij mod n in
                     Scalar.mul coeff (Domain.get domain index))
               in
               ( "Ss" ^ string_of_int (i + 1),
                 Evaluations.interpolation_fft2 domain coeff_list )))
      in
      ssigma_map
  end

  module Permutation_poly = struct
    (* compute f' & g' = (f + β×Sid + γ) & (g + β×Sσ + γ) products with Z *)
    (* compute_prime computes the following
       z_name * (w_1 + beta * s_1 + gamma) * ... * (w_n + beta * s_n + gamma)
       - z_name could be either "Z" or "Zg"
       - evaluations contains "Z" but not "Zg"
       - if z_name = "Zg", we compute "Zg" as composition_gx of "Z" with 1 *)
    let compute_prime ~prefix res_evaluation tmp_evaluation tmp2_evaluation beta
        gamma evaluations wires_names s_names (z_name, this_z_name) n =
      let zg_name = zg_name z_name in
      let z_evaluation =
        Evaluations.find_evaluation evaluations (prefix z_name)
      in

      let _i, res_evaluation =
        let f_fold (i, acc_evaluation) wire_name s_name =
          let comp = if i = 0 && this_z_name = zg_name then 1 else 0 in
          let res_evaluation =
            (* tmp_evaluation <- wire_name + beta * s_name + gamma *)
            let evaluation_linear_i =
              Evaluations.linear
                ~res:tmp_evaluation
                ~evaluations
                ~poly_names:[wire_name; s_name]
                ~linear_coeffs:[one; beta]
                ~add_constant:gamma
                ()
            in
            (* tmp2_evaluation <- acc_evaluation * evaluation_linear_i *)
            let acc_evaluation_new =
              Evaluations.mul_c
                ~res:tmp2_evaluation
                ~evaluations:[evaluation_linear_i; acc_evaluation]
                ~composition_gx:([0; comp], n)
                ()
            in
            Evaluations.copy ~res:res_evaluation acc_evaluation_new
          in
          (i + 1, res_evaluation)
        in
        List.fold_left2 f_fold (0, z_evaluation) wires_names s_names
      in
      res_evaluation

    (* evaluations must contain z’s evaluation *)
    let prover_identities ~additionnal_prefix:a_pref ~prefix wires_names beta
        gamma n evaluations =
      let z_name = a_pref z_name in
      let raw_z_name = z_name in
      let zg_name = zg_name z_name in
      let z_evaluation =
        Evaluations.find_evaluation evaluations (prefix z_name)
      in
      let z_evaluation_len = Evaluations.length z_evaluation in
      let tmp_evaluation = Evaluations.create z_evaluation_len in
      let tmp2_evaluation = Evaluations.create z_evaluation_len in
      let id1_evaluation = Evaluations.create z_evaluation_len in
      let id2_evaluation = Evaluations.create z_evaluation_len in

      let wires_names = List.map prefix wires_names in

      let identity_zfg =
        let nb_wires = List.length wires_names in

        (* changes f (resp g) array to f'(resp g') array, and multiply them together
            and with z (resp zg) *)
        let f_evaluation =
          let sid_names =
            List.init nb_wires (fun i -> "Si" ^ string_of_int (i + 1))
          in
          compute_prime
            ~prefix
            tmp_evaluation
            id2_evaluation
            tmp2_evaluation
            beta
            gamma
            evaluations
            wires_names
            sid_names
            (raw_z_name, z_name)
            n
        in
        let g_evaluation =
          let ss_names =
            List.init nb_wires (fun i ->
                prefix @@ a_pref "Ss" ^ string_of_int (i + 1))
          in
          compute_prime
            ~prefix
            id2_evaluation
            id1_evaluation
            tmp2_evaluation
            beta
            gamma
            evaluations
            wires_names
            ss_names
            (raw_z_name, zg_name)
            n
        in
        Evaluations.linear_c
          ~res:id1_evaluation
          ~evaluations:[f_evaluation; g_evaluation]
          ~linear_coeffs:[one; mone]
          ()
      in
      let identity_l1_z =
        let l1_evaluation = Evaluations.find_evaluation evaluations l1 in
        let z_mone_evaluation =
          Evaluations.linear_c
            ~res:tmp_evaluation
            ~evaluations:[z_evaluation]
            ~add_constant:mone
            ()
        in

        Evaluations.mul_c
          ~res:id2_evaluation
          ~evaluations:[l1_evaluation; z_mone_evaluation]
          ()
      in
      SMap.of_list
        [
          (prefix (a_pref "Perm.a"), identity_l1_z);
          (prefix (a_pref "Perm.b"), identity_zfg);
        ]

    (* compute_Z performs the following steps in the two loops.
       ----------------------
       | f_11 f_21 ... f_k1 | -> f_prod_1 (no need to compute as Z(g) is always one)
       | f_12 f_22 ... f_k2 | -> f_prod_2 = f_12 * f_22 * ... * f_k2
       |     ...........    | -> ...
       | f_1n f_2n ... f_kn | -> f_prod_n = f_1n * f_2n * ... * f_kn
        --------------------
       1. compute f_res = [ f_prod_1; f_prod_2; ...; f_prod_n ]
       2. compute g_res = [ g_prod_1; g_prod_2; ...; g_prod_n ]
       3. compute f_over_g = [ f_prod_1 / g_prod_1; ...; f_prod_n / g_prod_n ]
       4. apply fold_mul_array to f_over_g:
          [f_over_g_1; f_over_g_1 * f_over_g_2; ..; f_over_g_1 * f_over_g_2 * .. * f_over_n ]
       5. as step 4 computes [Z(g); Z(g^2); ..; Z(g^n)], we need to do a rotate right by 1
          (i.e., composition_gx with n - 1): [Z(g^n); Z(g); Z(g^2); ..; Z(g^{n-1})] *)
    let compute_Z s domain beta gamma values =
      let size_domain = Domain.length domain in
      let scalar_array_Z =
        let values_array = Array.of_list (SMap.values values) in
        let size_res = Evaluations.length values_array.(0) in
        assert (size_res = size_domain) ;
        let g_res = Array.init size_res (fun _ -> Scalar.zero) in
        let f_prev = ref Scalar.one in
        let f_res = ref Scalar.one in
        let tmp = Scalar.(copy one) in
        (* the first element of scalar_array_Z is always one *)
        for i = 1 to size_res - 1 do
          for j = 0 to Array.length values_array - 1 do
            let value_j_i = Evaluations.get values_array.(j) i in
            let v_gamma = Scalar.add gamma value_j_i in
            let f_coeff =
              let gi = Domain.get domain i in
              Scalar.(
                mul_inplace tmp gi (get_k j) ;
                mul_inplace gi tmp beta ;
                add_inplace gi gi v_gamma ;
                gi)
            in
            let g_coeff =
              let sj = s.((j * size_domain) + i) in
              let gj = Domain.get domain (sj mod size_domain) in
              Scalar.(
                mul_inplace tmp gj (get_k (Int.div sj size_domain)) ;
                mul_inplace gj tmp beta ;
                add_inplace gj gj v_gamma ;
                gj)
            in
            if j = 0 then (
              f_res := f_coeff ;
              g_res.(i) <- g_coeff)
            else
              Scalar.(
                mul_inplace !f_res !f_res f_coeff ;
                mul_inplace g_res.(i) g_res.(i) g_coeff)
          done ;
          let f_over_g = Scalar.div_exn !f_res g_res.(i) in
          Scalar.(
            mul_inplace f_over_g f_over_g !f_prev ;
            g_res.(i) <- !f_prev ;
            f_prev := f_over_g)
        done ;

        g_res.(0) <- !f_prev ;
        g_res
      in
      Evaluations.interpolation_fft2 domain scalar_array_Z
  end

  (* max degree needed is the degree of Perm.b, which is sum of wire’s degree plus z degree *)
  let polynomials_degree ~nb_wires = nb_wires + 1

  let build_permutation wires =
    let partition = Partition.build_partition wires in
    Partition.partition_to_permutation partition

  (* d = polynomials’ max degree
     n = generator’s order
     Returns SRS of decent size, preprocessed polynomials for permutation and
     their commitments (g_map_perm, cm_g_map_perm (="L1" -> L₁, preprocessed
     polynomial for verify perm’s identity), s_poly_map, cm_s_poly_map) & those
     for PP (g_map_PP, cm_g_map_PP)
     permutation for ssigma_list computation is deducted of cycles
     Details for SRS size :
       max size needed is deg(T)+1
       v polynomials all have degree 1
       according to identities_list_perm[0], t has max degree of Z×fL×fR×fO ;
       interpolation makes polynomials of degree n-1, so Z has degree of X²×Zh =
       X²×(X^n - 1) which is n+2, and each f has degree of X×Zh so n+1
       As a consequence, deg(T)-deg(Zs) = (n+2)+3(n+1) - n = 3n+5
       (for gates’ identity verification, max degree is degree of qM×fL×fR which
       is (n-1)+(n+1)+(n+1) < 3n+5) *)
  let preprocessing ~domain ~permutation ~nb_wires () =
    Preprocessing.ssigma_map_non_quadratic_residues permutation domain nb_wires

  let common_preprocessing ~nb_wires ~domain ~evaluations =
    let sid_evals = Preprocessing.evaluations_sid nb_wires evaluations in
    let evaluations = SMap.union_disjoint evaluations sid_evals in
    let l1_map = SMap.singleton l1 @@ Preprocessing.compute_l1 domain in
    Evaluations.compute_evaluations_update_map ~evaluations l1_map

  let prover_identities ?(prefix = "") ?(circuit_name = "") ~wires_names ~beta
      ~gamma ~n () =
    let additionnal_prefix s =
      if s = z_name && prefix <> "" then prefix ^ "Perm_" ^ s else prefix ^ s
    in
    let prefix = SMap.Aggregation.add_prefix circuit_name in
    fun evaluations ->
      Permutation_poly.prover_identities
        ~additionnal_prefix
        ~prefix
        wires_names
        beta
        gamma
        n
        evaluations

  let verifier_identities ?(prefix = "") ?(circuit_name = "") ~nb_proofs
      ~generator ~n ~wires_names ~beta ~gamma ~delta () =
    let a_pref s =
      if s = z_name && prefix <> "" then prefix ^ "Perm_" ^ s else prefix ^ s
    in
    let prefix = SMap.Aggregation.add_prefix circuit_name in
    let prefix_j j =
      SMap.Aggregation.add_prefix ~n:nb_proofs ~i:j circuit_name
    in
    let z_name = a_pref z_name in
    let ss_names =
      List.init (List.length wires_names) (fun i ->
          a_pref "Ss" ^ string_of_int (i + 1))
    in
    fun x answers ->
      let get_ss i = get_answer answers X (prefix @@ List.nth ss_names i) in
      (* compute the delta-aggregated wire evaluations at x for each wire name *)
      let batched =
        let wire_j w j = get_answer answers X @@ prefix_j j w in
        List.map
          (fun w -> Fr_generation.batch delta (List.init nb_proofs (wire_j w)))
          wires_names
      in
      let z = get_answer answers X (prefix z_name) in
      let zg = get_answer answers GX (prefix z_name) in
      (* compute the first identity: (Z(x) - 1) * L1(x) *)
      let res1 =
        Scalar.(
          sub z one * Fr_generation.evaluate_l1 ~domain_size:n ~generator x)
      in
      (* compute the second identity *)
      let res2 =
        let z_factors =
          List.mapi Scalar.(fun i w -> w + (beta * get_k i * x) + gamma) batched
        in
        let zg_factors =
          List.mapi Scalar.(fun i w -> w + (beta * get_ss i) + gamma) batched
        in
        let multiply l = List.fold_left Scalar.mul (List.hd l) (List.tl l) in
        Scalar.sub
          (multiply @@ (z :: z_factors))
          (multiply @@ (zg :: zg_factors))
      in
      SMap.of_list
        [(prefix (a_pref "Perm.a"), res1); (prefix (a_pref "Perm.b"), res2)]

  let f_map_contribution ~permutation ~values ~beta ~gamma ~domain =
    SMap.singleton
      z_name
      (Permutation_poly.compute_Z permutation domain beta gamma values)

  let cs ~sum_alpha_i ~l1 ~ss1 ~ss2 ~ss3 ~ss4 ~ss5 ~beta ~gamma ~delta ~x ~z ~zg
      ~wires =
    let open L in
    let a_list, b_list, c_list, d_list, e_list =
      let rec aux (acc_a, acc_b, acc_c, acc_d, acc_e) = function
        | [] -> List.(rev acc_a, rev acc_b, rev acc_c, rev acc_d, rev acc_e)
        | [a; b; c; d; e] :: tl ->
            aux (a :: acc_a, b :: acc_b, c :: acc_c, d :: acc_d, e :: acc_e) tl
        | _ -> failwith "Unexpected wires format"
      in
      aux ([], [], [], [], []) wires
    in
    let* cs_perm_a = Num.custom ~qr:Scalar.(negate one) ~qm:Scalar.(one) z l1 in

    let* a = sum_alpha_i a_list delta in
    let* b = sum_alpha_i b_list delta in
    let* c = sum_alpha_i c_list delta in
    let* d = sum_alpha_i d_list delta in
    let* e = sum_alpha_i e_list delta in
    let* betaid1 = Num.mul ~qm:(get_k 0) beta x in
    let* betaid2 = Num.mul ~qm:(get_k 1) beta x in
    let* betaid3 = Num.mul ~qm:(get_k 2) beta x in
    let* betaid4 = Num.mul ~qm:(get_k 3) beta x in
    let* betaid5 = Num.mul ~qm:(get_k 4) beta x in
    let* betasigma1 = Num.mul beta ss1 in
    let* betasigma2 = Num.mul beta ss2 in
    let* betasigma3 = Num.mul beta ss3 in
    let* betasigma4 = Num.mul beta ss4 in
    let* betasigma5 = Num.mul beta ss5 in

    let* aid = Num.add_list (to_list [a; betaid1; gamma]) in
    let* bid = Num.add_list (to_list [b; betaid2; gamma]) in
    let* cid = Num.add_list (to_list [c; betaid3; gamma]) in
    let* did = Num.add_list (to_list [d; betaid4; gamma]) in
    let* eid = Num.add_list (to_list [e; betaid5; gamma]) in
    let* asigma = Num.add_list (to_list [a; betasigma1; gamma]) in
    let* bsigma = Num.add_list (to_list [b; betasigma2; gamma]) in
    let* csigma = Num.add_list (to_list [c; betasigma3; gamma]) in
    let* dsigma = Num.add_list (to_list [d; betasigma4; gamma]) in
    let* esigma = Num.add_list (to_list [e; betasigma5; gamma]) in

    let* left_term = Num.mul_list (to_list [aid; bid; cid; did; eid; z]) in

    let* right_term =
      Num.mul_list (to_list [asigma; bsigma; csigma; dsigma; esigma; zg])
    in
    let* cs_perm_b = Num.add ~qr:Scalar.(negate one) left_term right_term in
    ret (cs_perm_a, cs_perm_b)
end

module type S = sig
  module PP : Polynomial_protocol.S

  val srs_size : zero_knowledge:bool -> n:int -> int

  val polynomials_degree : nb_wires:int -> int

  val build_permutation : int array SMap.t -> int array

  val preprocessing :
    domain:Domain.t ->
    permutation:int array ->
    nb_wires:int ->
    unit ->
    Poly.t SMap.t

  val common_preprocessing :
    nb_wires:int ->
    domain:Domain.t ->
    evaluations:Evaluations.t SMap.t ->
    Evaluations.t SMap.t

  (* prefix is an additionnal prefix for Ss, Z and identities names ; it is
     used by Range check gate *)
  val prover_identities :
    ?prefix:string ->
    ?circuit_name:string ->
    wires_names:string list ->
    beta:Scalar.t ->
    gamma:Scalar.t ->
    n:int ->
    unit ->
    prover_identities

  (* prefix is an additionnal prefix for Ss, Z and identities names ; it is
     used by Range check gate *)
  val verifier_identities :
    ?prefix:string ->
    ?circuit_name:string ->
    nb_proofs:int ->
    generator:Scalar.t ->
    n:int ->
    wires_names:string list ->
    beta:Scalar.t ->
    gamma:Scalar.t ->
    delta:Scalar.t ->
    unit ->
    verifier_identities

  val f_map_contribution :
    permutation:int array ->
    values:Evaluations.t SMap.t ->
    beta:Poly.scalar ->
    gamma:Poly.scalar ->
    domain:Domain.t ->
    Poly.t SMap.t

  val cs :
    sum_alpha_i:(L.scalar L.repr list -> L.scalar L.repr -> L.scalar L.repr L.t) ->
    l1:L.scalar L.repr ->
    ss1:L.scalar L.repr ->
    ss2:L.scalar L.repr ->
    ss3:L.scalar L.repr ->
    ss4:L.scalar L.repr ->
    ss5:L.scalar L.repr ->
    beta:L.scalar L.repr ->
    gamma:L.scalar L.repr ->
    delta:L.scalar L.repr ->
    x:L.scalar L.repr ->
    z:L.scalar L.repr ->
    zg:L.scalar L.repr ->
    wires:L.scalar L.repr list list ->
    (L.scalar L.repr * L.scalar L.repr) L.t
end

module Permutation_gate (PP : Polynomial_protocol.S) : S with module PP = PP =
  Permutation_gate_impl (PP)
