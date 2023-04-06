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
module L = Plompiler.LibCircuit

type s_repr = L.scalar L.repr

module type S = sig
  module PP : Polynomial_protocol.S

  val batched_z_name : string

  val z_names : string list

  val shared_z_names : string list

  val build_permutation :
    range_checks:int list * int -> size_domain:int -> int array

  val preprocessing :
    permutation:int array ->
    range_checks:'a list * int ->
    domain:Domain.t ->
    Poly.t SMap.t

  (* Builds the pure range check proof polynomials *)
  val f_map_contribution_1 :
    range_checks:int list * int ->
    domain:Domain.t ->
    values:Evaluations.t SMap.t ->
    Evaluations.t * Poly.t SMap.t

  (* Builds the shared permutation proof polynomials for the range check proof polynomials built with f_map_contribution_1
     [values] must contain the wire polynomial that is being range checked and its range check proof polynomial, each in aggregated version
  *)
  val f_map_contribution_2 :
    permutation:int array ->
    beta:Poly.scalar ->
    gamma:Poly.scalar ->
    domain:Domain.t ->
    values:Evaluations.t SMap.t ->
    Poly.t SMap.t

  (* Builds the pure range check identities *)
  val prover_identities_1 :
    ?circuit_prefix:(string -> string) ->
    proof_prefix:(string -> string) ->
    domain_size:int ->
    unit ->
    prover_identities

  (* Builds the permutation identities for the range check polynomials *)
  val prover_identities_2 :
    ?circuit_prefix:(string -> string) ->
    beta:Scalar.t ->
    gamma:Scalar.t ->
    domain_size:int ->
    unit ->
    prover_identities

  (* Builds the pure range check identities *)
  val verifier_identities_1 :
    ?circuit_prefix:(string -> string) ->
    proof_prefix:(string -> string) ->
    unit ->
    Scalar.t ->
    Scalar.t SMap.t SMap.t ->
    Scalar.t SMap.t

  (* Builds the permutation identities for the range check polynomials *)
  val verifier_identities_2 :
    ?circuit_prefix:(string -> string) ->
    nb_proofs:int ->
    beta:Scalar.t ->
    gamma:Scalar.t ->
    delta:Scalar.t ->
    domain_size:int ->
    generator:Scalar.t ->
    unit ->
    verifier_identities

  val cs :
    lnin1:s_repr ->
    pnin1:s_repr ->
    z_list:s_repr list ->
    zg_list:s_repr list ->
    wire_list:s_repr list ->
    sum_alpha_i:(s_repr list -> s_repr -> s_repr L.t) ->
    l1:s_repr ->
    ss_list:s_repr list ->
    beta:s_repr ->
    gamma:s_repr ->
    delta:s_repr ->
    x:s_repr ->
    z_perm:s_repr ->
    zg_perm:s_repr ->
    (string * s_repr) list L.t
end

module Range_check_gate_impl (PP : Polynomial_protocol.S) = struct
  module PP = PP

  exception Too_many_checks of string

  let lnin1 = "Lnin1"

  let pnin1 = "Pnin1"

  let z_name = "RC_Z"

  let rc_prefix = "RC_"

  let wire = Plompiler.Csir.wire_name 0

  let batched_wire = String.capitalize_ascii wire

  let batched_z_name = "RC_Z_BATCHED"

  (* Used for distribution *)
  let z_names = [z_name]

  let shared_z_names = [rc_prefix ^ "Perm_Z"]

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
     [rc]
     Note that identities and Z building must consider the polynomials in the
     order imposed by this permutation, which stands for the order Z_RC — Wire.
  *)
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

  (* We have to make sure we consider the values we give to Perm functions
     in the same order as the permutation we build ; the current permutation
     stands for the order Z_RC — Wire. *)
  let prefix_for_perm =
    SMap.Aggregation.update_key_name (fun k ->
        if k = batched_wire then "2." ^ batched_wire
        else if k = batched_z_name then "1." ^ k
        else k)

  (* TODO we should be able to aggregate permutation for different range checks
     proofs as we do for wires ; for now & simplicity, we don’t handle several
     proofs in one circuit *)
  module Permutation = struct
    module Perm = Permutation_gate.Permutation_gate (PP)

    let external_prefix = rc_prefix

    let preprocessing ~permutation ~domain =
      Perm.preprocessing ~external_prefix ~domain ~permutation ~nb_wires:2 ()

    let f_map_contribution ~permutation ~beta ~gamma ~domain
        ~values:batched_values =
      let values =
        SMap.of_list
          [
            (prefix_for_perm batched_wire, SMap.find batched_wire batched_values);
            ( prefix_for_perm batched_z_name,
              SMap.find batched_z_name batched_values );
          ]
      in
      Perm.f_map_contribution
        ~external_prefix
        ~permutation
        ~values
        ~beta
        ~gamma
        ~domain
        ()

    let prover_identities ?(circuit_prefix = Fun.id) ~beta ~gamma ~domain_size
        () evaluations =
      let evaluations = SMap.update_keys prefix_for_perm evaluations in
      Perm.prover_identities
        ~external_prefix
        ~circuit_prefix
        ~wires_names:(List.map prefix_for_perm [batched_wire; batched_z_name])
        ~beta
        ~gamma
        ~n:domain_size
        ()
        evaluations

    let verifier_identities ?(circuit_prefix = Fun.id) ~nb_proofs ~beta ~gamma
        ~delta ~domain_size ~generator () x answers =
      let answers = SMap.(map (update_keys prefix_for_perm)) answers in

      Perm.verifier_identities
        ~external_prefix
        ~circuit_prefix
        ~nb_proofs
        ~generator
        ~n:domain_size
        ~wires_names:(List.map prefix_for_perm [wire; z_name])
        ~beta
        ~gamma
        ~delta
        ()
        x
        answers

    let cs ~sum_alpha_i ~l1 ~ss_list ~beta ~gamma ~delta ~x ~z ~zg ~wires () =
      Perm.cs
        ~external_prefix
        ~sum_alpha_i
        ~l1
        ~ss_list
        ~beta
        ~gamma
        ~delta
        ~x
        ~z
        ~zg
        ~wires
        ()
  end

  module RangeChecks = struct
    let assert_not_too_many_checks k nb =
      if k < nb then
        raise
          (Too_many_checks
             (Printf.sprintf "%d checks asked, %d checks expected" nb k))

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
        let pnin1_poly =
          Evaluations.init domain_size ~degree:domain_size (fun i ->
              if i mod upper_bound = upper_bound - 1 then zero else one)
          |> Evaluations.interpolation_fft domain
        in
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

    let prover_identities ?(circuit_prefix = Fun.id) ~proof_prefix:prefix
        ~domain_size:n () evaluations =
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
          Evaluations.find_evaluation evaluations (circuit_prefix lnin1)
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
          Evaluations.find_evaluation evaluations (circuit_prefix pnin1)
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

    let verifier_identities ?(circuit_prefix = Fun.id) ~proof_prefix:prefix ()
        _x answers =
      let z = get_answer answers X (prefix z_name) in
      let zg = get_answer answers GX (prefix z_name) in
      let lnin1 = get_answer answers X (circuit_prefix lnin1) in
      let pnin1 = get_answer answers X (circuit_prefix pnin1) in
      let identity_rca = Scalar.(z * (one + negate z) * lnin1) in
      let identity_rcb =
        Scalar.((z + (mtwo * zg)) * (one + negate z + (two * zg)) * pnin1)
      in
      SMap.of_list
        [(prefix "RC.a", identity_rca); (prefix "RC.b", identity_rcb)]

    let cs ~prefix ~lnin1 ~pnin1 ~z ~zg =
      let open L in
      let* one_m_z = Num.custom ~ql:mone ~qc:one z z in
      let* id_a = Num.mul_list (to_list [z; one_m_z; lnin1]) in
      let* id_b =
        let* z_m_2zg = Num.add z ~qr:mtwo zg in
        let* one_m_z_p_2zg = Num.add one_m_z ~qr:two zg in
        Num.mul_list (to_list [z_m_2zg; one_m_z_p_2zg; pnin1])
      in
      ret [(prefix "RC.a", id_a); (prefix "RC.b", id_b)]
  end

  let preprocessing ~permutation ~range_checks ~domain =
    if fst range_checks = [] then SMap.empty
    else
      let rc = RangeChecks.preprocessing ~range_checks ~domain in
      let perm = Permutation.preprocessing ~permutation ~domain in
      SMap.union_disjoint rc perm

  let f_map_contribution_1 = RangeChecks.f_map_contribution

  let f_map_contribution_2 = Permutation.f_map_contribution

  let prover_identities_1 = RangeChecks.prover_identities

  let prover_identities_2 = Permutation.prover_identities

  let verifier_identities_1 = RangeChecks.verifier_identities

  let verifier_identities_2 = Permutation.verifier_identities

  let cs ~lnin1 ~pnin1 ~z_list ~zg_list ~wire_list ~sum_alpha_i ~l1 ~ss_list
      ~beta ~gamma ~delta ~x ~z_perm ~zg_perm =
    let open L in
    let* rc =
      let n = List.length z_list in
      let i = ref (-1) in
      (* RC’s cs for all proofs *)
      let* rc =
        map2M
          (fun z zg ->
            incr i ;
            RangeChecks.cs
              ~prefix:(SMap.Aggregation.add_prefix ~n ~i:!i "")
              ~lnin1
              ~pnin1
              ~z
              ~zg)
          z_list
          zg_list
      in
      ret (List.concat rc)
    in
    let wires = List.map2 (fun z w -> [z; w]) z_list wire_list in
    let* perm =
      Permutation.cs
        ~sum_alpha_i
        ~l1
        ~ss_list
        ~beta
        ~gamma
        ~delta
        ~x
        ~z:z_perm
        ~zg:zg_perm
        ~wires
        ()
    in
    ret (rc @ perm)
end

module Range_check_gate (PP : Polynomial_protocol.S) : S with module PP = PP =
  Range_check_gate_impl (PP)
