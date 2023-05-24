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

  val z_names : string -> string list

  val shared_z_names : string -> string list

  val build_permutations :
    size_domain:int -> range_checks:(int * int) list SMap.t -> int array SMap.t

  val preprocessing :
    range_checks:(int * int) list SMap.t ->
    permutations:int array SMap.t ->
    domain:Domain.t ->
    Poly.t SMap.t

  (* Builds the pure range check proof polynomials *)
  val f_map_contribution_1 :
    range_checks:(int * int) list SMap.t ->
    domain:Domain.t ->
    values:Evaluations.t SMap.t ->
    Evaluations.t SMap.t * Poly.t SMap.t

  (* Builds the shared permutation proof polynomials for the range check proof polynomials built with f_map_contribution_1
     [values] must contain the wire polynomial that is being range checked and its range check proof polynomial, each in aggregated version
  *)
  val f_map_contribution_2 :
    permutations:int array SMap.t ->
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
    range_checks:'a SMap.t ->
    unit ->
    prover_identities

  (* Builds the permutation identities for the range check polynomials *)
  val prover_identities_2 :
    ?circuit_prefix:(string -> string) ->
    beta:Scalar.t ->
    gamma:Scalar.t ->
    domain_size:int ->
    range_checks:'a SMap.t ->
    unit ->
    prover_identities

  (* Builds the pure range check identities *)
  val verifier_identities_1 :
    ?circuit_prefix:(string -> string) ->
    proof_prefix:(string -> string) ->
    range_checks:bool SMap.t ->
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
    range_checks:bool SMap.t ->
    unit ->
    verifier_identities

  val cs :
    rc_index:int list ->
    nb_proofs:int ->
    lnin1:s_repr list ->
    pnin1:s_repr list ->
    z_rc:s_repr list list ->
    zg_rc:s_repr list list ->
    z_perm:s_repr list ->
    zg_perm:s_repr list ->
    aggregated_wires:s_repr list ->
    sum_alpha_i:(s_repr list -> s_repr -> s_repr L.t) ->
    l1:s_repr ->
    ss_list:s_repr list list ->
    beta:s_repr ->
    gamma:s_repr ->
    delta:s_repr ->
    x:s_repr ->
    (string * s_repr) list L.t
end

module Range_check_gate_impl (PP : Polynomial_protocol.S) = struct
  module PP = PP

  exception Too_many_checks of string

  let lnin1 = "Lnin1"

  let pnin1 = "Pnin1"

  let rc_prefix = "RC_"

  let z_name wire = rc_prefix ^ wire ^ "_Z"

  let batched_wire = String.capitalize_ascii

  let batched_z_name wire = rc_prefix ^ batched_wire wire ^ "_Z"

  let suffix str wire = str ^ "_" ^ wire

  (* Used for distribution *)
  let z_names wire = [z_name wire]

  let shared_z_names wire = [rc_prefix ^ wire ^ "_Perm_Z"]

  type public_parameters = Poly.t SMap.t

  let zero, one, two = Scalar.(zero, one, one + one)

  let mone, mtwo = Scalar.(negate one, negate two)

  module Permutation = struct
    (* Build the permutation such that (n₀ + … + n_{j - 1}) <-> N + i_j for ni = upperbounds,
       j < len([rc]), N = [size_domain], i_j = index of the j-th range check in
       [range_checks]
       Note that identities and Z building must consider the polynomials in the
       order imposed by this permutation, which stands for the order Z_RC — Wire.
    *)
    let build_permutation_wire ~size_domain range_checks =
      if range_checks = [] then [||]
      else
        let fst_part =
          (* the first element is the sum of upperbounds until an index represented
             by the second element *)
          let sum_n = ref (0, 0) in
          let get_safe l j =
            try
              let i, n = List.nth l (snd !sum_n) in
              sum_n := (fst !sum_n + n, snd !sum_n + 1) ;
              size_domain + i
            with _ -> j
          in
          Array.init size_domain (fun j ->
              (* if we are at a range check index i then the permutation goes on
                 the corresponding index in the range check list ; if there is no
                 more index in the range check list, or if we are not at a range
                 check index, i is a fix point of the permutation
              *)
              if j = fst !sum_n then get_safe range_checks j else j)
        in
        let snd_part =
          (* we just keep the first argument of sum_n, the second one is now useless *)
          let sum_n = ref 0 in
          Array.init size_domain (fun j ->
              (* if i is not a index of the range check list then it’s a fix point,
                 else it goes on on index of the corresponding range check ;
                 this piece is the mirror of the preceeding one *)
              match List.assoc_opt j range_checks with
              | None -> size_domain + j
              | Some n ->
                  let res = !sum_n in
                  sum_n := res + n ;
                  res)
        in
        Array.append fst_part snd_part

    module Perm = Permutation_gate.Permutation_gate (PP)

    let external_prefix wire = rc_prefix ^ wire ^ "_"

    let prefix_for_perm =
      let batched_wire_prefix =
        String.capitalize_ascii Plompiler.Csir.wire_prefix
      in
      SMap.Aggregation.update_key_name (fun k ->
          if
            Str.(
              string_match (regexp (batched_wire_prefix ^ "\\([0-9]+\\)")) k 0)
          then "2." ^ k
          else if
            Str.(
              string_match
                (regexp (rc_prefix ^ batched_wire_prefix ^ "\\([0-9]+\\)_Z"))
                k
                0)
          then "1." ^ k
          else k)

    (* We have to make sure we consider the values we give to Perm functions
       in the same order as the permutation we build ; the current permutation
       stands for the order Z_RC — Wire. *)
    let prefix_for_perm_map = SMap.update_keys prefix_for_perm

    let preprocessing ~domain wire permutation =
      Perm.preprocessing
        ~external_prefix:(external_prefix wire)
        ~domain
        ~permutation
        ~nb_wires:2
        ()

    let f_map_contribution ~beta ~gamma ~domain ~values:batched_values wire
        permutation =
      let values =
        SMap.of_list
          [
            (batched_wire wire, SMap.find (batched_wire wire) batched_values);
            (batched_z_name wire, SMap.find (batched_z_name wire) batched_values);
          ]
        |> prefix_for_perm_map
      in
      Perm.f_map_contribution
        ~external_prefix:(external_prefix wire)
        ~permutation
        ~values
        ~beta
        ~gamma
        ~domain
        ()

    let prover_identities ?(circuit_prefix = Fun.id) ~beta ~gamma ~domain_size
        wire _ evaluations =
      let evaluations = prefix_for_perm_map evaluations in
      Perm.prover_identities
        ~external_prefix:(external_prefix wire)
        ~circuit_prefix
        ~wires_names:
          (List.map prefix_for_perm [batched_wire wire; batched_z_name wire])
        ~beta
        ~gamma
        ~n:domain_size
        ()
        evaluations

    let verifier_identities ?(circuit_prefix = Fun.id) ~nb_proofs ~beta ~gamma
        ~delta ~domain_size ~generator wire rc x answers =
      if not rc then SMap.empty
      else
        let answers = SMap.map (SMap.update_keys prefix_for_perm) answers in
        Perm.verifier_identities
          ~external_prefix:(external_prefix wire)
          ~circuit_prefix
          ~nb_proofs
          ~generator
          ~n:domain_size
          ~wires_names:(List.map prefix_for_perm [wire; z_name wire])
          ~beta
          ~gamma
          ~delta
          ()
          x
          answers

    (*
        [[SS1_a ; SS2_a] ; [SS1_b ; SS2_b]]
        [RC_perm_a ; RC_perm_b]
        [0~RC_a_Z ; 0~RC_b_Z]
       [[0~RC_a_Z ; 0~a] ; [0~RC_b_Z ; 0~b]]
    *)
    let cs_for_all_wires ~l1 ~ss_list ~beta ~gamma ~x ~z_perm ~zg_perm
        ~aggregated_wires w_list =
      let zzgwss =
        List.(combine (combine (combine z_perm zg_perm) w_list) ss_list)
      in
      L.map2M
        (fun (((z, zg), w), ss_list) aggregated_wires ->
          Perm.cs
            ~external_prefix:(rc_prefix ^ "w" ^ string_of_int w ^ "_")
            ~l1
            ~ss_list
            ~beta
            ~gamma
            ~x
            ~z
            ~zg
            ~aggregated_wires
            ())
        zzgwss
        aggregated_wires
  end

  module RangeChecks = struct
    let preprocessing ~domain wire range_checks =
      if range_checks = [] then SMap.empty
      else
        let domain_size = Domain.length domain in
        let build_poly ~at_n ~default =
          let a =
            List.concat_map
              (fun (_, n) ->
                List.init n (fun i -> if i = n - 1 then at_n else default))
              range_checks
            |> Array.of_list
          in
          if domain_size < Array.length a then
            raise
              (Too_many_checks
                 (Printf.sprintf
                    "Range checks : sum of bounds (=%d) must be less than \
                     domain size (=%d)"
                    (Array.length a)
                    domain_size)) ;
          Array.(append a (init (domain_size - length a) (Fun.const zero)))
          |> Evaluations.interpolation_fft2 domain
        in
        let lnin1_poly = build_poly ~at_n:one ~default:zero in
        let pnin1_poly = build_poly ~at_n:zero ~default:one in
        SMap.of_list
          [(suffix lnin1 wire, lnin1_poly); (suffix pnin1 wire, pnin1_poly)]

    (* compute the evaluations of the Z polynomial for a scalar [x] with the bound [up] *)
    let partial_z (x, up) =
      let x = Scalar.to_z x in
      let rec aux gwi = function
        | 1 -> gwi
        | i ->
            let q = Z.(div (List.hd gwi) (one + one)) in
            aux (q :: gwi) (i - 1)
      in
      let res = aux [x] up in
      res |> List.rev_map Scalar.of_z

    let f_map_contribution ~domain ~values wire_name range_checks =
      if range_checks = [] then (SMap.empty, SMap.empty)
      else
        let wire = SMap.find wire_name values in
        let evals =
          let to_checks =
            List.map
              (fun (idx, bound) -> (Evaluations.get wire idx, bound))
              range_checks
          in
          let all_evals =
            List.concat_map partial_z to_checks |> Array.of_list
          in
          let evals =
            Array.(
              append
                all_evals
                (init
                   (Domain.length domain - length all_evals)
                   (Fun.const zero)))
          in
          Evaluations.of_array (Array.length evals - 1, evals)
        in
        let z = Evaluations.interpolation_fft domain evals in
        ( SMap.of_list [(batched_z_name wire_name, evals)],
          SMap.of_list [(z_name wire_name, z)] )

    let prover_identities ?(circuit_prefix = Fun.id) ~proof_prefix:prefix
        ~domain_size:n wire _ evaluations =
      let z_evaluation =
        Evaluations.find_evaluation evaluations (prefix (z_name wire))
      in
      let z_evaluation_len = Evaluations.length z_evaluation in
      let tmp_evaluation = Evaluations.create z_evaluation_len in
      let tmp2_evaluation = Evaluations.create z_evaluation_len in
      let idrca_evaluation = Evaluations.create z_evaluation_len in
      let idrcb_evaluation = Evaluations.create z_evaluation_len in
      (* Z × (1-Z) × Lnin1 *)
      let identity_rca =
        let lnin1_evaluation =
          Evaluations.find_evaluation
            evaluations
            (circuit_prefix (suffix lnin1 wire))
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
          Evaluations.find_evaluation
            evaluations
            (circuit_prefix (suffix pnin1 wire))
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
        [
          (prefix (rc_prefix ^ wire) ^ ".a", identity_rca);
          (prefix (rc_prefix ^ wire) ^ ".b", identity_rcb);
        ]

    let verifier_identities ?(circuit_prefix = Fun.id) ~proof_prefix:prefix wire
        rc _x answers =
      if not rc then SMap.empty
      else
        let z = get_answer answers X (prefix (z_name wire)) in
        let zg = get_answer answers GX (prefix (z_name wire)) in
        let lnin1 = get_answer answers X (circuit_prefix (suffix lnin1 wire)) in
        let pnin1 = get_answer answers X (circuit_prefix (suffix pnin1 wire)) in
        let identity_rca = Scalar.(z * (one + negate z) * lnin1) in
        let identity_rcb =
          Scalar.((z + (mtwo * zg)) * (one + negate z + (two * zg)) * pnin1)
        in
        SMap.of_list
          [
            (prefix (rc_prefix ^ wire) ^ ".a", identity_rca);
            (prefix (rc_prefix ^ wire) ^ ".b", identity_rcb);
          ]

    let cs_unitary ~prefix ~lnin1 ~pnin1 ~z ~zg w =
      let open L in
      let* one_m_z = Num.custom ~ql:mone ~qc:one z z in
      let* id_a = Num.mul_list (to_list [z; one_m_z; lnin1]) in
      let* id_b =
        let* z_m_2zg = Num.add z ~qr:mtwo zg in
        let* one_m_z_p_2zg = Num.add one_m_z ~qr:two zg in
        Num.mul_list (to_list [z_m_2zg; one_m_z_p_2zg; pnin1])
      in
      let wire = "w" ^ string_of_int w in
      ret
        [(prefix "RC_" ^ wire ^ ".a", id_a); (prefix "RC_" ^ wire ^ ".b", id_b)]

    let cs_for_all_wires ~prefix ~lnin1 ~pnin1 ~z_list ~zg_list w_list =
      let open L in
      let lp = List.combine lnin1 pnin1 in
      let zzgw = List.(combine (combine z_list zg_list) w_list) in
      let* ids =
        map2M
          (fun (lnin1, pnin1) ((z, zg), w) ->
            cs_unitary ~prefix ~lnin1 ~pnin1 ~z ~zg w)
          lp
          zzgw
      in
      ret (List.concat ids)
  end

  let build_permutations ~size_domain ~range_checks =
    SMap.map (Permutation.build_permutation_wire ~size_domain) range_checks

  let preprocessing ~range_checks ~permutations ~domain =
    let rc = SMap.mapi (RangeChecks.preprocessing ~domain) range_checks in
    let perm = SMap.mapi (Permutation.preprocessing ~domain) permutations in
    SMap.values rc @ SMap.values perm |> SMap.union_disjoint_list

  let f_map_contribution_1 ~range_checks ~domain ~values =
    let z_evals, f_map =
      SMap.mapi (RangeChecks.f_map_contribution ~domain ~values) range_checks
      |> SMap.to_pair
    in
    ( SMap.(union_disjoint_list (values z_evals)),
      SMap.(union_disjoint_list (values f_map)) )

  let f_map_contribution_2 ~permutations ~beta ~gamma ~domain ~values =
    SMap.mapi
      (Permutation.f_map_contribution ~beta ~gamma ~domain ~values)
      permutations
    |> SMap.values |> SMap.union_disjoint_list

  let prover_identities_1 ?(circuit_prefix = Fun.id) ~proof_prefix ~domain_size
      ~range_checks () =
    SMap.mapi
      (RangeChecks.prover_identities ~circuit_prefix ~proof_prefix ~domain_size)
      range_checks
    |> SMap.values |> Identities.merge_prover_identities

  let prover_identities_2 ?(circuit_prefix = Fun.id) ~beta ~gamma ~domain_size
      ~range_checks () =
    SMap.mapi
      (Permutation.prover_identities ~circuit_prefix ~beta ~gamma ~domain_size)
      range_checks
    |> SMap.values |> Identities.merge_prover_identities

  let verifier_identities_1 ?(circuit_prefix = Fun.id) ~proof_prefix
      ~range_checks () =
    SMap.mapi
      (RangeChecks.verifier_identities ~circuit_prefix ~proof_prefix)
      range_checks
    |> SMap.values |> Identities.merge_verifier_identities

  let verifier_identities_2 ?(circuit_prefix = Fun.id) ~nb_proofs ~beta ~gamma
      ~delta ~domain_size ~generator ~range_checks () =
    SMap.mapi
      (Permutation.verifier_identities
         ~circuit_prefix
         ~nb_proofs
         ~beta
         ~gamma
         ~delta
         ~domain_size
         ~generator)
      range_checks
    |> SMap.values |> Identities.merge_verifier_identities

  (* [lni1]
     [pni1]
     z_rc = [[0~RC_a_Z ; 0~RC_b_Z] ; [1~RC_a_Z ; 1~RC_b_Z]]
     z_perm = [RC_perm_a ; RC_perm_b]
     aggregated_wires = [A, B, C, D, E] *)
  let cs ~rc_index ~nb_proofs:n ~lnin1 ~pnin1 ~z_rc ~zg_rc ~z_perm ~zg_perm
      ~aggregated_wires ~sum_alpha_i ~l1 ~ss_list ~beta ~gamma ~delta ~x =
    let open L in
    let* rc =
      let proof_idx = ref (-1) in
      map2M
        (fun z_list zg_list ->
          incr proof_idx ;
          RangeChecks.cs_for_all_wires
            ~prefix:(SMap.Aggregation.add_prefix ~n ~i:!proof_idx "")
            ~lnin1
            ~pnin1
            ~z_list
            ~zg_list
            rc_index)
        z_rc
        zg_rc
    in
    let* aggregated_z_rc =
      List.mapn (fun i -> sum_alpha_i i delta) z_rc |> mapM Fun.id
    in

    let aggregated_rc_wires =
      List.filteri (fun i _ -> List.mem i rc_index) aggregated_wires
    in

    let aggregated_wires =
      List.fold_left2
        (fun acc r w -> [r; w] :: acc)
        []
        aggregated_z_rc
        aggregated_rc_wires
      |> List.rev
    in
    let* perm =
      Permutation.cs_for_all_wires
        ~l1
        ~ss_list
        ~beta
        ~gamma
        ~x
        ~z_perm
        ~zg_perm
        ~aggregated_wires
        rc_index
    in
    ret (List.flatten (rc @ perm))
end

module Range_check_gate (PP : Polynomial_protocol.S) : S with module PP = PP =
  Range_check_gate_impl (PP)
