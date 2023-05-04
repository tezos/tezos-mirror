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

(* This gate is used to do range checks on values of a wire
   We noticed an overhead of 20% in prover time when using this protocol ;
   considering N is the number of constraints, if there are κ constraints per
   range checks, denoting x the percentage of the Z polynomial used, solving
   the equation N × 1.2 = N + κ × N × x gives x = (0.2/κ).
   With κ = 3, we need the Z polynomial filled by 7%.

   Note that we don’t handle several proofs for now.

   TODO to integrate the protocol completely :
     - handle several proofs
     - integration to plompiler
     - more tests, especially for zk
     - integration to aPlonK
*)

open Bls
open Utils
open Identities

module type S = sig
  module PP : Polynomial_protocol.S

  val build_permutation :
    range_checks:int list * int -> size_domain:int -> int array

  val common_preprocessing : unit -> 'a SMap.t

  val preprocessing :
    permutation:int array ->
    range_checks:'a list * int ->
    domain:Domain.t ->
    Poly.t SMap.t

  val f_map_contribution :
    permutation:int array ->
    beta:Scalar.t ->
    gamma:Scalar.t ->
    domain:Domain.t ->
    range_checks:int list * int ->
    values:Evaluations.t SMap.t ->
    Poly.t SMap.t

  val prover_identities :
    ?circuit_name:string ->
    nb_proofs:int ->
    proof_idx:int ->
    beta:Scalar.t ->
    gamma:Scalar.t ->
    domain_size:int ->
    unit ->
    prover_identities

  val verifier_identities :
    ?circuit_name:string ->
    nb_proofs:int ->
    proof_idx:int ->
    beta:Scalar.t ->
    gamma:Scalar.t ->
    domain_size:int ->
    generator:Scalar.t ->
    unit ->
    verifier_identities
end

module Range_check_gate_impl (PP : Polynomial_protocol.S) = struct
  module PP = PP

  exception Too_many_checks of string

  let lnin1 = "Lni_plus_n_minus_1"

  let pnin1 = "Pni_plus_n_minus_1"

  let z_name = "RC_Z"

  let z_perm_name = "RC_Perm_Z"

  let ids_label = "RC_Perm"

  let wire = Plompiler.Csir.wire_name 0

  type public_parameters = Poly.t SMap.t

  let zero, one, two = Scalar.(zero, one, one + one)

  let mone, mtwo = Scalar.(negate one, negate two)

  (* This function returns the index of the first occurence of [x] in [l].
     If [l] does not contain [x], -1 is returned. *)
  let find l x =
    let rec aux i = function
      | [] -> -1
      | h :: t -> if x = h then i else aux (i + 1) t
    in
    aux 0 l

  (* Build the permutation such that nj <-> N + i_j for n = [up_bound],
     j < len([rc]), N = [size_domain], i_j = index of the j-th range check in
     [rc] *)
  let build_permutation ~range_checks:(rc, up_bound) ~size_domain =
    let get_safe l i =
      try size_domain + List.nth l (i / up_bound) with _ -> i
    in
    if rc = [] then [||]
    else
      let fst =
        Array.init size_domain (fun i ->
            (* if we are at a range check index i then the permutation goes on
               the corresponding index in the range check list ; if there is no
               more index in the range check list, or if we are not at a range
               check index, i is a fix point of the permutation
            *)
            if i mod up_bound = 0 then get_safe rc i else i)
      in
      let snd =
        Array.init size_domain (fun i ->
            (* if i is not a index of the range check list then it’s a fix point,
               else it goes on on index of the corresponding range check ;
               this piece is the mirror of the preceeding one *)
            match find rc i with -1 -> size_domain + i | j -> j * up_bound)
      in
      Array.append fst snd

  (* TODO we should be able to aggregate permutation for different range checks
     proofs as we do for wires ; for now & simplicity, we don’t handle several
     proofs in one circuit *)
  module Permutation = struct
    module Perm = Permutation_gate.Permutation_gate_impl (PP)

    let preprocessing ~permutation ~domain =
      let ss_map =
        Perm.Preprocessing.ssigma_map_non_quadratic_residues
          permutation
          domain
          2
      in
      SMap.update_keys (String.cat "RC_") ss_map

    let f_map_contribution ~permutation ~beta ~gamma ~domain ~values =
      SMap.singleton
        z_perm_name
        (Perm.Permutation_poly.compute_Z permutation domain beta gamma values)

    let prover_identities ~prefix_common ~prefix ~beta ~gamma domain_size =
      Perm.prover_identities
        ~prefix:"RC_"
        ~circuit_name:prefix_common
        ~wires_names:[prefix z_name; prefix wire]
        ~beta
        ~gamma
        ~n:domain_size
        ()

    let verifier_identities ~prefix_common ~prefix ~beta ~gamma domain_size
        generator =
      Perm.verifier_identities
        ~prefix:"RC_"
        ~circuit_name:prefix_common
        ~nb_proofs:1
        ~generator
        ~n:domain_size
        ~wires_names:[prefix z_name; prefix wire]
        ~beta
        ~gamma
        ~delta:one
        ()
  end

  module RangeChecks = struct
    let assert_not_too_many_checks k nb =
      if k < nb then
        raise
          (Too_many_checks
             (Printf.sprintf "%d checks asked, %d checks expected" nb k))

    let compute_pnin1 upper_bound domain domain_size =
      let x_w i =
        Poly.of_coefficients
          [(one, 1); (Scalar.negate (Domain.get domain i), 0)]
      in
      let k = domain_size / upper_bound in
      (* Computes product of (X-ω^(ni + n - 1)) from i = 1 to k *)
      let rec aux res = function
        | 0 -> res
        | i -> aux (Poly.mul res (x_w ((upper_bound * i) - 1))) (i - 1)
      in
      aux Poly.one k

    let preprocessing ~range_checks:(idx, upper_bound) ~domain =
      if Z.(log2up (of_int upper_bound)) <> Z.(log2 (of_int upper_bound)) then
        failwith "upper_bound must be a power of two." ;
      if idx = [] then SMap.empty
      else
        let domain_size = Domain.length domain in
        let lnin1_poly =
          Array.init domain_size (fun i ->
              if i mod upper_bound = upper_bound - 1 then one else zero)
          |> Evaluations.interpolation_fft2 domain
        in
        let pnin1_poly = compute_pnin1 upper_bound domain domain_size in
        SMap.of_list [(lnin1, lnin1_poly); (pnin1, pnin1_poly)]

    let get_checks_from_wire k check_indices wire =
      let checks = List.map (Evaluations.get wire) check_indices in
      checks @ List.(init (k - length checks) (Fun.const Scalar.zero))

    (* compute the evaluations of the Z polynomial for a scalar [x] with the bound [up] *)
    let partial_z up x =
      let x = Scalar.to_z x in
      let rec aux gwi = function
        | 1 -> gwi
        | i ->
            let q = Z.(div (List.hd gwi) (one + one)) in
            aux (q :: gwi) (i - 1)
      in
      let res = aux [x] up in
      res |> List.rev_map Scalar.of_z

    let build_z_evals domain up k check_indices values =
      let checks = get_checks_from_wire k check_indices values in
      let all_evals = List.concat_map (partial_z up) checks |> Array.of_list in
      let evals =
        Array.(
          append
            all_evals
            (init
               (Domain.length domain - length all_evals)
               (Fun.const Scalar.zero)))
      in
      Evaluations.of_array (Array.length evals - 1, evals)

    let compute_Z domain up k check_indices values =
      let evals = build_z_evals domain up k check_indices values in
      (evals, Evaluations.interpolation_fft domain evals)

    let f_map_contribution ~range_checks:(check_indices, upper_bound) ~domain
        ~values =
      let wire = SMap.find wire values in
      let nb_range_checks = List.length check_indices in
      let k = Domain.length domain / upper_bound in
      assert_not_too_many_checks k nb_range_checks ;
      let evals, z = compute_Z domain upper_bound k check_indices wire in
      (evals, SMap.of_list [(z_name, z)])

    let prover_identities ~prefix_common ~prefix n =
      let prefix_common = SMap.Aggregation.add_prefix prefix_common in
      fun evaluations ->
        let z_evaluation =
          Evaluations.find_evaluation evaluations (prefix z_name)
        in
        let z_evaluation_len = Evaluations.length z_evaluation in
        let tmp_evaluation = Evaluations.create z_evaluation_len in
        let tmp2_evaluation = Evaluations.create z_evaluation_len in
        let idrca_evaluation = Evaluations.create z_evaluation_len in
        let idrcb_evaluation = Evaluations.create z_evaluation_len in

        (* Z × (1-Z) × Lnin1 *)
        let identity_rca =
          let lnin1_evaluation =
            Evaluations.find_evaluation evaluations (prefix_common lnin1)
          in
          let one_m_z_evaluation =
            Evaluations.linear_c
              ~res:tmp_evaluation
              ~linear_coeffs:[mone]
              ~evaluations:[z_evaluation]
              ~add_constant:one
              ()
          in
          Evaluations.mul_c
            ~res:idrca_evaluation
            ~evaluations:[z_evaluation; one_m_z_evaluation; lnin1_evaluation]
            ()
        in
        (* (Z - 2Zg) × (1 - Z + 2Zg) × Pnin1 *)
        let identity_rcb =
          let pnin1_evaluation =
            Evaluations.find_evaluation evaluations (prefix_common pnin1)
          in
          let z_min_2Zg_evaluation =
            Evaluations.linear_c
              ~res:tmp_evaluation
              ~linear_coeffs:[one; mtwo]
              ~composition_gx:([0; 1], n)
              ~evaluations:[z_evaluation; z_evaluation]
              ()
          in
          let one_m_Z_p_2Zg_evaluation =
            Evaluations.linear_c
              ~res:tmp2_evaluation
              ~linear_coeffs:[mone]
              ~evaluations:[z_min_2Zg_evaluation]
              ~add_constant:one
              ()
          in
          Evaluations.mul_c
            ~res:idrcb_evaluation
            ~evaluations:
              [z_min_2Zg_evaluation; one_m_Z_p_2Zg_evaluation; pnin1_evaluation]
            ()
        in
        SMap.of_list
          [(prefix "RC.a", identity_rca); (prefix "RC.b", identity_rcb)]

    let verifier_identities ~prefix_common ~prefix =
      let prefix_common = SMap.Aggregation.add_prefix prefix_common in
      fun _x answers ->
        let z = get_answer answers X (prefix z_name) in
        let zg = get_answer answers GX (prefix z_name) in
        let lnin1 = get_answer answers X (prefix_common lnin1) in
        let pnin1 = get_answer answers X (prefix_common pnin1) in
        let identity_rca = Scalar.(z * (one + negate z) * lnin1) in
        let identity_rcb =
          Scalar.((z + (mtwo * zg)) * (one + negate z + (two * zg)) * pnin1)
        in
        SMap.of_list
          [(prefix "RC.a", identity_rca); (prefix "RC.b", identity_rcb)]
  end

  let preprocessing ~permutation ~range_checks ~domain =
    if fst range_checks = [] then SMap.empty
    else
      let rc = RangeChecks.preprocessing ~range_checks ~domain in
      let perm = Permutation.preprocessing ~permutation ~domain in
      SMap.union_disjoint rc perm

  let common_preprocessing () = SMap.empty

  let f_map_contribution ~permutation ~beta ~gamma ~domain ~range_checks
      ~(values : Evaluations.t SMap.t) =
    let values = SMap.singleton wire (SMap.find wire values) in
    let z_evals, f_rc =
      RangeChecks.f_map_contribution ~range_checks ~domain ~values
    in
    let f_perm =
      Permutation.f_map_contribution
        ~permutation
        ~beta
        ~gamma
        ~domain
        ~values:(SMap.add_unique z_name z_evals values)
    in
    SMap.union_disjoint f_rc f_perm

  let prover_identities ?(circuit_name = "") ~nb_proofs ~proof_idx ~beta ~gamma
      ~domain_size () =
    let proof_prefix =
      SMap.Aggregation.add_prefix ~n:nb_proofs ~i:proof_idx ""
    in
    let prefix s = SMap.Aggregation.add_prefix circuit_name (proof_prefix s) in
    let rc_ids =
      RangeChecks.prover_identities
        ~prefix_common:circuit_name
        ~prefix
        domain_size
    in
    let perm_ids =
      Permutation.prover_identities
        ~prefix_common:circuit_name
        ~prefix:proof_prefix
        ~beta
        ~gamma
        domain_size
    in
    Identities.merge_prover_identities [rc_ids; perm_ids]

  let verifier_identities ?(circuit_name = "") ~nb_proofs ~proof_idx ~beta
      ~gamma ~domain_size ~generator () =
    let proof_prefix =
      SMap.Aggregation.add_prefix ~n:nb_proofs ~i:proof_idx ""
    in
    let prefix s = SMap.Aggregation.add_prefix circuit_name (proof_prefix s) in
    let rc_ids =
      RangeChecks.verifier_identities ~prefix_common:circuit_name ~prefix
    in
    let perm_ids =
      Permutation.verifier_identities
        ~prefix_common:circuit_name
        ~prefix:proof_prefix
        ~beta
        ~gamma
        domain_size
        generator
    in
    Identities.merge_verifier_identities [rc_ids; perm_ids]
end

module Range_check_gate (PP : Polynomial_protocol.S) : S with module PP = PP =
  Range_check_gate_impl (PP)
