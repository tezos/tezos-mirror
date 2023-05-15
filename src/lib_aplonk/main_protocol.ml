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

(* This file implements the aPlonK protocol, designed to produce multiple PlonK
   proofs at the same time, which can be verified in logarithmic time.
   In addition to using a multi-polynomial commitment scheme, aPlonK delegates
   part of verification checks on scalar values to the prover, who will produce
   a PlonK proof that the checks pass correctly. *)

open Plonk.Bls
module SMap = Plonk.SMap

module Make_impl
    (Main_KZG : Plonk.Main_protocol.S
                  with type public_inputs = Scalar.t array list)
    (Main_Pack : Aggregation.Main_protocol.S
                   with type public_inputs = Scalar.t array list
                   with module PP.Answers_commitment = Main_KZG.Input_commitment)
    (PIs : Pi_parameters.S) =
struct
  module Aggreg_circuit = Circuit.V (Main_Pack)
  module L = Plompiler.LibCircuit

  exception Entry_not_in_table of string

  exception Rest_not_null of string

  module Input_commitment = Main_Pack.Input_commitment

  type scalar = Scalar.t [@@deriving repr]

  type circuit_map = Main_Pack.circuit_map

  (* Type of prover public params for meta-verification of a base circuit:
      - meta_pp           : meta-verification prover PP of this base circuit
      - meta_solver       : Plompiler solver for this meta-verification circuit
      - public_input_size : number of public inputs in this base circuit
      - input_com_sizes   : number of input commitment sizes expected by this
                            base circuit
      - nb_proofs         : maximum number of proofs that will be created on
                            this base circuit
      - nb_rc_wires       : number of range-checked wires *)
  type prover_meta_pp = {
    meta_pp : Main_KZG.prover_public_parameters;
    meta_solver : Plompiler.Solver.t;
    public_input_size : int;
    input_com_sizes : int list;
    nb_proofs : int;
    nb_rc_wires : int;
  }
  [@@deriving repr]

  (* Type of verifier public params for meta-verification of a base circuit:
      - meta_pp           : meta-verification verifier PP of this base circuit
      - public_input_size : number of public inputs in this base circuit *)
  type verifier_meta_pp = {
    meta_pp : Main_KZG.verifier_public_parameters;
    public_input_size : int;
    nb_proofs : int;
  }
  [@@deriving repr]

  (* Type of prover public parameters of aPlonK:
      - main_pp  : main_protocol (prover) public parameters of the aPlonK
                   aggregated statement
      - meta_pps : [prover_meta_pp] for meta-verification of all the supported
                   base circuits, encoded as a string map keyed by the base
                   circuit names *)
  type prover_public_parameters = {
    main_pp : Main_Pack.prover_public_parameters;
    meta_pps : prover_meta_pp SMap.t;
  }
  [@@deriving repr]

  (* Type of verifier public parameters of aPlonK:
      - main_pp  : main_protocol (verifier) public parameters of the aPlonK
                   aggregated statement
      - meta_pps : [verifier_meta_pp] for meta-verification of all the supported
                   base circuits, encoded as a string map keyed by the base
                   circuit names *)
  type verifier_public_parameters = {
    main_pp : Main_Pack.verifier_public_parameters;
    meta_pps : verifier_meta_pp SMap.t;
  }
  [@@deriving repr]

  (* Type of an aggregated aPlonK proof:
      - main_proof  : proof of the aggregated aPlonK statement
      - meta_proofs : meta-verification proofs for each base circuit,
                      encoded as a string map keyed by the base circuit names
      - batch       : COULD BE REMOVED AND RECOMPUTED FROM [batches]
      - batches     : aggregated evaluation values for each of the base circuits
                      in the catalog; its structure is as follows:
          * we have one "batch" (of type [(scalar * int) SMap.t list]) per base
            circuit, stored in a string map keyed by the base circuit names
          * each "batch" is a list of [(scalar * int) SMap.t], corresponding to
            the groups of polynomials committed during the PlonK protocol;
            currently this list includes four groups of polynomials:
               - setup-polynomials,
               - perm-and-plook polynomials
               - wire-polynomials
            in that order (although aPlonK is order-agnostic, as long as the
            order is consistent with [cms_answers]).
            IMPORTANT: t-polynomials are handeled separately, explicitly in
                       the multi-aPlonK protocol
          * each [(scalar * int) SMap.t] is a map containing the batched
            evaluation of all the polynomials in the group at the point
            represented by the map key (typically "X" and "GX"); the [int]
            indicates the number of polynomials that were batch
      - cms_answers : commitments to the PlonK polynomial evaluations
                      (a.k.a. "answers"), one commitment per base circuit,
                      presented in a string map keyed by the base circuit names;
                      each commitment includes the evaluations that were
                      "batched" in [batches]
      - cms_pi      : commitments to the public inputs of each base circuit,
                      presented in a string map keyed by the base circuit names
      - ids_batch   : a map from circuit names to pairs [scalar * int], the
                      [scalar] is the value corresponding to the batched
                      identities of that circuit, whereas the [int] indicates
                      how many identities were batched.
      - t_answers   : evaluations of polynomial [T] at [x]; this type is a list
                      of scalars given that [T] may be split into several parts.
  *)
  type proof = {
    main_proof : Main_Pack.proof;
    meta_proofs : Main_KZG.proof SMap.t;
    batch : Main_KZG.scalar SMap.t list;
    batches : (Main_KZG.scalar * int) SMap.t list SMap.t;
    cms_answers : Main_Pack.PP.Answers_commitment.public SMap.t;
    cms_pi : Main_Pack.PP.Answers_commitment.public SMap.t;
    ids_batch : (Main_KZG.scalar * int) SMap.t;
    t_answers : Main_KZG.scalar list;
  }
  [@@deriving repr]

  type circuit_prover_input = Main_Pack.circuit_prover_input = {
    witness : scalar array;
    input_commitments : Main_Pack.Input_commitment.t list;
  }
  [@@deriving repr]

  type prover_inputs = circuit_prover_input list SMap.t [@@deriving repr]

  type public_inputs = scalar list [@@deriving repr]

  type verifier_inputs =
    (public_inputs * Input_commitment.public list list) SMap.t
  [@@deriving repr]

  let to_verifier_inputs (pp : prover_public_parameters) inputs =
    let vi = Main_Pack.to_verifier_inputs pp.main_pp inputs in
    SMap.mapi
      (fun circuit_name (pi, ic) ->
        let module PI = (val PIs.get_pi_module circuit_name) in
        let pi = PI.outer_of_inner (List.map Array.to_list pi) in
        assert (List.for_all (fun i -> i = []) ic) ;
        (pi, ic))
      vi

  (* We only need to modify the main circuit PP, since it rules them all
     (meta-circuits get their state updated after the main circuit proof) *)
  let update_prover_public_parameters bytes (pp : prover_public_parameters) =
    {
      pp with
      main_pp = Main_Pack.update_prover_public_parameters bytes pp.main_pp;
    }

  (* We only need to modify the main circuit PP, since it rules them all
     (meta-circuits get their state updated after the main circuit proof) *)
  let update_verifier_public_parameters bytes (pp : verifier_public_parameters)
      =
    {
      pp with
      main_pp = Main_Pack.update_verifier_public_parameters bytes pp.main_pp;
    }

  let filter_prv_pp_circuits (pp : prover_public_parameters) inputs =
    {pp with main_pp = Main_Pack.filter_prv_pp_circuits pp.main_pp inputs}

  let filter_vrf_pp_circuits pp inputs =
    {pp with main_pp = Main_Pack.filter_vrf_pp_circuits pp.main_pp inputs}

  (* used for debug with sat *)
  let cs_global = ref SMap.empty

  (* ////////////////////////////////////////////////////// *)

  let input_commit_funcs (pp : prover_public_parameters) inputs =
    SMap.mapi
      (fun name pp ->
        let nb_proofs = List.length (SMap.find name inputs) in
        (* meta-verification circuits have exactly 2 input commitments:
           one for the PI and one for the answers (in that order) *)
        let nb_max_pi = List.hd pp.input_com_sizes in
        Main_Pack.
          {
            pi = (fun pi -> Main_KZG.input_commit ~size:nb_max_pi pp.meta_pp pi);
            answers =
              (fun answers ->
                let answers =
                  Plonk.Utils.pad_answers
                    pp.nb_proofs
                    pp.nb_rc_wires
                    nb_proofs
                    answers
                in
                Main_KZG.input_commit
                  ~shift:nb_max_pi
                  pp.meta_pp
                  (Array.of_list answers));
          })
      pp.meta_pps

  (* ////////////////////////////////////////////////////// *)

  let input_commit ?size ?shift (pp : prover_public_parameters) secret =
    ignore (size, shift, pp, secret) ;
    failwith "[input_commit] in aPlonK is not supported yet"

  let meta_setup ~zero_knowledge ~srs ~main_prover_pp ~nb_batch circuit_name
      (circuit, nb_proofs) =
    let module PI = (val PIs.get_pi_module circuit_name) in
    let cs =
      Aggreg_circuit.get_cs_verification
        main_prover_pp
        circuit
        nb_batch
        nb_proofs
        PI.(nb_outer, nb_inner)
        PI.check
    in
    (* cs_global is used for sat *)
    cs_global := SMap.add circuit_name cs.cs !cs_global ;
    (* Plompiler.Utils.dump_label_traces
       ("../../../../flamegraph/flamegraph" ^ "_" ^ Int.to_string nb_proofs)
       cs.cs; *)
    let public_input_size = cs.public_input_size in
    let input_com_sizes = cs.input_com_sizes in
    let circuit_aggreg = Plonk.Circuit.to_plonk cs in
    let agg_circuit_map =
      SMap.singleton ("meta_" ^ circuit_name) (circuit_aggreg, 1)
    in
    let prover_meta_pp, verifier_meta_pp =
      Main_KZG.setup ~zero_knowledge agg_circuit_map ~srs
    in
    let prover_meta_pp =
      {
        meta_pp = prover_meta_pp;
        meta_solver = cs.solver;
        public_input_size;
        input_com_sizes;
        nb_proofs;
        nb_rc_wires = SMap.cardinal circuit.range_checks;
      }
    in
    let verifier_meta_pp =
      {meta_pp = verifier_meta_pp; public_input_size; nb_proofs}
    in
    (prover_meta_pp, verifier_meta_pp)

  let setup ~zero_knowledge circuits_map ~srs =
    let prover_pp, verifier_pp =
      Main_Pack.setup ~zero_knowledge circuits_map ~srs
    in
    (* nb_batch gives the maximum number of batch of all circuits ; the number of batches has to be the same for all verification circuit *)
    let nb_batch =
      SMap.fold
        (fun _ (c, _) nb_batch -> max nb_batch (Aggreg_circuit.nb_batch c))
        circuits_map
        0
    in
    let meta_pps =
      SMap.mapi
        (meta_setup ~zero_knowledge ~srs ~main_prover_pp:prover_pp ~nb_batch)
        circuits_map
    in
    let prover_meta_pps = SMap.map fst meta_pps in
    let verifier_meta_pps = SMap.map snd meta_pps in
    ( ({main_pp = prover_pp; meta_pps = prover_meta_pps}
        : prover_public_parameters),
      {main_pp = verifier_pp; meta_pps = verifier_meta_pps} )

  let meta_prove ~(main_prover_aux : Main_Pack.prover_aux) ~meta_pps
      ~inner_pi_map ~transcript batches circuit_name circuit_inputs =
    let module PI = (val PIs.get_pi_module circuit_name) in
    let prover_meta_pp : prover_meta_pp = SMap.find circuit_name meta_pps in
    let batch = SMap.find circuit_name batches in
    let cm_pi = SMap.find circuit_name main_prover_aux.cms_pi in
    let cm_answers = SMap.find circuit_name main_prover_aux.cms_answers in
    let switches, compressed_switches =
      let nb_proofs = List.length circuit_inputs in
      Aggreg_circuit.compute_switches prover_meta_pp.nb_proofs nb_proofs
    in
    let inner_pi =
      SMap.find circuit_name inner_pi_map |> fst |> List.map Array.to_list
    in
    let trace =
      let outer_pi = PI.outer_of_inner inner_pi in
      Aggreg_circuit.get_witness
        prover_meta_pp.nb_proofs
        prover_meta_pp.nb_rc_wires
        main_prover_aux
        circuit_name
        prover_meta_pp.public_input_size
        prover_meta_pp.meta_solver
        (inner_pi, outer_pi)
        switches
        compressed_switches
        batch
    in
    let secret =
      Main_KZG.{witness = trace; input_commitments = [cm_pi; cm_answers]}
    in
    let cs = SMap.find circuit_name !cs_global in
    assert (Plonk.Circuit.sat cs [] trace) ;
    let inputs = SMap.singleton ("meta_" ^ circuit_name) [secret] in
    let pp_aggreg_circuit =
      Main_KZG.update_prover_public_parameters transcript prover_meta_pp.meta_pp
    in
    try Main_KZG.prove pp_aggreg_circuit ~inputs
    with Main_KZG.Rest_not_null _ ->
      raise
        (Rest_not_null
           "Main_Kzg.prove could not create a proof for the verification \
            circuit.")

  let meta_proof (pp : prover_public_parameters) inputs
      (main_proof, (prover_aux : Main_Pack.prover_aux)) =
    let open Main_Pack in
    let transcript =
      Data_encoding.Binary.to_bytes_exn proof_encoding main_proof
    in
    let inner_pi_map = to_verifier_inputs pp.main_pp inputs in
    let batches =
      Aggreg_circuit.get_batches inputs prover_aux.answers prover_aux.r
    in
    let meta_proofs =
      SMap.mapi
        (meta_prove
           ~main_prover_aux:prover_aux
           ~meta_pps:pp.meta_pps
           ~inner_pi_map
           ~transcript
           batches)
        inputs
    in
    let cms_answers =
      SMap.map
        Main_Pack.PP.Answers_commitment.(fun cm_answers -> cm_answers.public)
        prover_aux.cms_answers
    in
    let cms_pi =
      SMap.map
        Main_Pack.PP.Answers_commitment.(fun cm_pi -> cm_pi.public)
        prover_aux.cms_pi
    in
    {
      main_proof;
      meta_proofs;
      batch = prover_aux.batch;
      batches;
      cms_answers;
      cms_pi;
      t_answers = prover_aux.t_answers;
      ids_batch = prover_aux.ids_batch;
    }

  let prove (pp : prover_public_parameters) ~(inputs : prover_inputs) =
    let pp = filter_prv_pp_circuits pp inputs in
    let input_commit_funcs = input_commit_funcs pp inputs in
    let proof_base_circuits =
      try Main_Pack.prove_list pp.main_pp ~input_commit_funcs ~inputs
      with Main_Pack.Rest_not_null _ ->
        raise
          (Rest_not_null
             "Main_Pack.prove could not create a proof for the base circuit.")
    in
    meta_proof pp inputs proof_base_circuits

  let meta_verify ~transcript ~inputs ~proof alpha_et_al circuit_name pp =
    let cm_answers = SMap.find circuit_name proof.cms_answers in
    let cm_pi = SMap.find circuit_name proof.cms_pi in
    let meta_proof = SMap.find circuit_name proof.meta_proofs in
    let batch = SMap.find circuit_name proof.batches in
    let ids_batch = SMap.find circuit_name proof.ids_batch |> fst in
    let pi, input_commitments_list = SMap.find circuit_name inputs in
    let nb_proofs = List.length input_commitments_list in
    if List.exists (fun l -> l <> []) input_commitments_list then
      raise
      @@ Invalid_argument
           "input commitments in the base circuit of\n\
           \           aPlonK are not yet supported" ;
    let public_inputs =
      Aggreg_circuit.aggreg_public_inputs
        pp.public_input_size
        alpha_et_al
        batch
        ids_batch
        (Scalar.of_int nb_proofs)
        pi
    in
    let pp_aggreg_circuit =
      Main_KZG.update_verifier_public_parameters transcript pp.meta_pp
    in
    let inputs =
      SMap.singleton
        ("meta_" ^ circuit_name)
        ([public_inputs], [[cm_pi; cm_answers]])
    in
    Main_KZG.verify pp_aggreg_circuit ~inputs meta_proof

  let verify pp ~(inputs : verifier_inputs) proof =
    let pp = filter_vrf_pp_circuits pp inputs in
    let main_verif, main_verifier_aux =
      Main_Pack.verify_list
        pp.main_pp
        ( proof.main_proof,
          proof.batch,
          proof.cms_answers,
          proof.cms_pi,
          proof.t_answers,
          proof.ids_batch )
    in
    let transcript =
      Data_encoding.Binary.to_bytes_exn
        Main_Pack.proof_encoding
        proof.main_proof
    in
    let batch_ok =
      Aggreg_circuit.verify_batch
        main_verifier_aux.r
        proof.batch
        proof.batches
        proof.t_answers
    in
    let meta_proofs_ok =
      let v = main_verifier_aux in
      SMap.for_all
        (meta_verify
           ~transcript
           ~inputs
           ~proof
           (v.alpha, v.beta, v.gamma, v.beta_rc, v.gamma_rc, v.delta, v.x, v.r))
        pp.meta_pps
    in
    main_verif && batch_ok && meta_proofs_ok

  (* Encodings *)

  let scalar_encoding = Main_Pack.scalar_encoding

  let data_encoding_of_repr repr =
    Data_encoding.conv
      (Plompiler.Utils.to_bytes repr)
      (Plompiler.Utils.of_bytes repr)
      Data_encoding.bytes

  let proof_encoding = data_encoding_of_repr proof_t

  let verifier_public_parameters_encoding =
    data_encoding_of_repr verifier_public_parameters_t

  module Internal_for_tests = struct
    let mutate_vi verifier_inputs =
      let key, (public_inputs, input_commitments_list) =
        SMap.choose verifier_inputs
      in
      match public_inputs with
      | [] -> None
      | input ->
          let input = Array.of_list input in
          let i = Random.int (Array.length input) in
          input.(i) <- Scalar.random () ;
          Some
            (SMap.add
               key
               (Array.to_list input, input_commitments_list)
               verifier_inputs)
  end
end

(* Main_KZG is used on the meta-verification circuit *)
module Main_KZG = Plonk.Main_protocol
module Super_PP =
  Aggregation.Polynomial_protocol.Make_aggregation
    (Aggregation.Polynomial_commitment)
    (Main_KZG.Input_commitment)

(* Main_Pack is used for the base circuits that we are proving;
   it is built with the super aggregation *)
module Main_Pack = Aggregation.Main_protocol.Make (Super_PP)

module Make_raw
    (Main_KZG : Plonk.Main_protocol.S
                  with type public_inputs = Scalar.t array list)
    (Main_Pack : Aggregation.Main_protocol.S
                   with type public_inputs = Scalar.t array list
                   with module PP.Answers_commitment = Main_KZG.Input_commitment)
    (PIs : Pi_parameters.S) :
  Plonk.Main_protocol.S
    with module Input_commitment = Main_Pack.Input_commitment
    with type circuit_prover_input = Main_Pack.circuit_prover_input
    with type public_inputs = Scalar.t list =
  Make_impl (Main_KZG) (Main_Pack) (PIs)

module Make (PIs : Pi_parameters.S) = Make_raw (Main_KZG) (Main_Pack) (PIs)
