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
open Identities
module L = Plompiler.LibCircuit

module type S = sig
  val arith_label : string

  val qadv_label : string

  val com_label : string

  val gates_list : string list

  val nb_custom_gates : int

  val nb_input_com : int

  val get_eqs :
    string ->
    q:Scalar.t ->
    wires:Scalar.t array ->
    wires_g:Scalar.t array ->
    ?precomputed_advice:Scalar.t SMap.t ->
    unit ->
    Scalar.t list

  val get_ids : string -> string * int

  val get_cs :
    string ->
    q:L.scalar L.repr ->
    wires:L.scalar L.repr array ->
    wires_g:L.scalar L.repr array ->
    ?precomputed_advice:L.scalar L.repr SMap.t ->
    unit ->
    L.scalar L.repr list L.t

  val aggregate_blinds : gates:'a SMap.t -> int SMap.t

  val aggregate_prover_identities :
    ?circuit_prefix:(string -> string) ->
    input_coms_size:int ->
    proof_prefix:(string -> string) ->
    gates:'a SMap.t ->
    public_inputs:Scalar.t array ->
    domain:Domain.t ->
    unit ->
    prover_identities

  val aggregate_verifier_identities :
    ?circuit_prefix:(string -> string) ->
    input_com_sizes:int list ->
    proof_prefix:(string -> string) ->
    gates:'a SMap.t ->
    public_inputs:Scalar.t array ->
    generator:Scalar.t ->
    size_domain:int ->
    unit ->
    verifier_identities

  val aggregate_polynomials_degree : gates:'a SMap.t -> int

  val exists_gx_composition : gates:'a SMap.t -> bool

  val cs_pi :
    generator:Scalar.t ->
    n:Scalar.t ->
    x:L.scalar L.repr ->
    zs:L.scalar L.repr ->
    L.scalar L.repr list ->
    L.scalar L.repr L.t
end

module Aggregator = struct
  open Gates_common

  let arith_label = arith

  let com_label = com_label

  let qadv_label = qadv_label

  (* Maximum number of input commitments that can be used in a proof *)
  let nb_input_com = 3

  let gates_map =
    let open Arithmetic_gates in
    let open Boolean_gates in
    let open Hash_gates in
    let open Ecc_gates in
    let open Mod_arith_gates in
    let linear_monomials =
      let open Plompiler.Csir in
      List.init nb_wires_arch (fun i -> (linear_selector_name i, i))
    in
    SMap.of_list
      ([
         (Public.q_label, (module Public : Base_sig));
         (Constant.q_label, (module Constant));
         (Multiplication.q_label, (module Multiplication));
         (X5A.q_label, (module X5A));
         (X5C.q_label, (module X5C));
         (X2B.q_label, (module X2B));
         (AddWeierstrass.q_label, (module AddWeierstrass));
         (AddEdwards.q_label, (module AddEdwards));
         (ConditionalAddEdwards.q_label, (module ConditionalAddEdwards));
         (BoolCheck.q_label, (module BoolCheck));
         (CondSwap.q_label, (module CondSwap));
         (AnemoiDouble.q_label, (module AnemoiDouble));
         (AddMod25519.q_label, (module AddMod25519));
       ]
      @ List.map (fun (q, i) -> (q, linear_monomial i q)) linear_monomials
      @ List.map
          (fun (q, i) ->
            let q = Plompiler.Csir.add_next_wire_suffix q in
            (q, linear_monomial ~is_next:true i q))
          linear_monomials
      @ List.init nb_input_com (fun i ->
            ( "qcom" ^ string_of_int i,
              (module InputCom (struct
                let idx = i
              end) : Base_sig) )))

  let gates_list = SMap.keys gates_map

  let nb_custom_gates = SMap.cardinal gates_map

  (* Removes non-custom-gates from gates (for example q_plookup) *)
  let filter_gates gates = SMap.(filter (fun q _ -> mem q gates_map) gates)

  let find_gate q =
    match SMap.find_opt q gates_map with
    | Some gate -> gate
    | None ->
        failwith
          (Printf.sprintf "\nCustom_gates.find_gate : unknown selector %s." q)

  let get_blinds q =
    let module M = (val find_gate q : Base_sig) in
    M.blinds

  let get_prover_identities q =
    let module M = (val find_gate q : Base_sig) in
    M.prover_identities

  let get_coms q =
    let module M = (val find_gate q : Base_sig) in
    M.index_com

  let get_nb_advs q =
    let module M = (val find_gate q : Base_sig) in
    M.nb_advs

  let get_nb_buffers q =
    let module M = (val find_gate q : Base_sig) in
    M.nb_buffers

  let get_verifier_identities q =
    let module M = (val find_gate q : Base_sig) in
    M.verifier_identities

  let get_polynomials_degree q =
    let module M = (val find_gate q : Base_sig) in
    M.polynomials_degree

  let get_eqs q =
    let module M = (val find_gate q : Base_sig) in
    M.equations

  let get_ids q =
    let module M = (val find_gate q : Base_sig) in
    M.identity

  let get_cs q =
    let module M = (val find_gate q : Base_sig) in
    M.cs

  let get_gx_composition q =
    let module M = (val find_gate q : Base_sig) in
    M.gx_composition

  let aggregate_blinds ~gates =
    let f_union _key a1 a2 =
      if Array.length a1 <> Array.length a2 then
        raise (Invalid_argument "All blinds arrays must have the same size.")
      else Some Array.(init (length a1) (fun i -> max a1.(i) a2.(i)))
    in
    let blinds_array =
      SMap.(
        fold
          (fun gate _ acc_blinds -> union f_union acc_blinds (get_blinds gate))
          (filter_gates gates)
          empty)
    in
    let sum_array a = Array.fold_left ( + ) 0 a in
    SMap.map sum_array blinds_array

  let filter_evaluations ~evaluations ~prefix ~circuit_prefix gates =
    let get_eval = Evaluations.find_evaluation evaluations in
    let base =
      List.fold_left
        (fun acc x ->
          if SMap.exists (fun y _ -> String.equal x y) evaluations then
            SMap.add x (get_eval x) acc
          else acc)
        SMap.empty
        ["X"; "GX"]
    in
    List.fold_left
      (fun acc gate ->
        (* Adding wires *)
        let acc =
          SMap.fold
            (fun wire blinds acc2 ->
              let wire = prefix wire in
              if SMap.exists (fun w _ -> String.equal w wire) acc then acc2
              else
                let used = blinds.(0) = 1 || blinds.(1) = 1 in
                if used then SMap.add wire (get_eval wire) acc2 else acc2)
            (get_blinds gate)
            acc
        in
        (* Adding commitments *)
        let acc =
          match get_coms gate with
          | None -> acc
          | Some idx ->
              let com_i = prefix com_label ^ string_of_int idx in
              SMap.add com_i (get_eval com_i) acc
        in
        (* Adding advice *)
        let acc =
          List.init (get_nb_advs gate) (fun i ->
              circuit_prefix qadv_label ^ string_of_int i)
          |> List.fold_left
               (fun acc2 adv_i -> SMap.add adv_i (get_eval adv_i) acc2)
               acc
        in
        (* Adding selector *)
        let gate = circuit_prefix gate in
        if String.starts_with ~prefix:(circuit_prefix "qpub") gate then acc
        else SMap.add gate (get_eval gate) acc)
      base
      gates

  let filter_answers ~answers ~prefix ~circuit_prefix gates =
    let x, gx = (string_of_eval_point X, string_of_eval_point GX) in
    let get_x, get_gx = (get_answer answers X, get_answer answers GX) in
    let add_mapmap k' k v mm =
      SMap.(update k' (fun m -> Option.bind m (fun m -> Some (add k v m)))) mm
    in
    let add_x, add_gx = (add_mapmap x, add_mapmap gx) in
    let exists_mapmap k' k mm = SMap.exists k (SMap.find k' mm) in
    let exists_x, exists_gx = (exists_mapmap x, exists_mapmap gx) in
    let base = SMap.of_list [(x, SMap.empty); (gx, SMap.empty)] in
    List.fold_left
      (fun acc gate ->
        (* Adding wires *)
        let acc =
          SMap.fold
            (fun wire blinds acc2 ->
              let wire = prefix wire in
              let acc3 =
                if exists_x (fun w _ -> String.equal w wire) acc then acc2
                else if blinds.(0) = 1 then add_x wire (get_x wire) acc2
                else acc2
              in
              if exists_gx (fun w _ -> String.equal w wire) acc then acc3
              else if blinds.(1) = 1 then add_gx wire (get_gx wire) acc3
              else acc3)
            (get_blinds gate)
            acc
        in
        (* Adding commitments *)
        let acc =
          match get_coms gate with
          | None -> acc
          | Some idx ->
              let com_i = prefix com_label ^ string_of_int idx in
              add_x com_i (get_x com_i) acc
        in
        (* Adding advice *)
        let acc =
          List.init (get_nb_advs gate) (fun i ->
              circuit_prefix qadv_label ^ string_of_int i)
          |> List.fold_left
               (fun acc2 adv_i -> add_x adv_i (get_x adv_i) acc2)
               acc
        in
        (* Adding selector *)
        let gate = circuit_prefix gate in
        if String.starts_with ~prefix:(circuit_prefix "qpub") gate then acc
        else add_x gate (get_x gate) acc)
      base
      gates

  let aggregate_prover_identities ?(circuit_prefix = Fun.id) ~input_coms_size
      ~proof_prefix:prefix ~gates ~public_inputs ~domain () : prover_identities
      =
   fun evaluations ->
    let size_eval = Evaluations.size_evaluations evaluations in
    (* Sort and filter gates according to nb of tmp buffers *)
    let gates =
      List.sort
        (fun gate1 gate2 ->
          Int.compare (get_nb_buffers gate1) (get_nb_buffers gate2))
        (filter_gates gates |> SMap.keys)
    in
    (* Creating tmp buffers for prover identities *)
    let nb_min_buffers =
      match gates with
      | [] -> 0
      | hd_gates :: _ -> max 1 (get_nb_buffers hd_gates)
    in
    tmp_buffers :=
      Array.init nb_min_buffers (fun _ -> Evaluations.create size_eval) ;
    (* Filter evaluations *)
    let evaluations =
      filter_evaluations ~evaluations ~prefix ~circuit_prefix gates
    in
    (* Except for the arithmetic gates,
       all gates correspond to distinct identities. *)
    let init_ids =
      let arith_acc_evaluation = Evaluations.create size_eval in
      SMap.singleton (prefix @@ arith ^ ".0") arith_acc_evaluation
    in
    let union key e1 e2 =
      assert (key = prefix @@ arith ^ ".0") ;
      Some (Evaluations.add ~res:e1 e1 e2)
    in
    let public = {public_inputs; input_coms_size} in
    List.fold_left
      (fun accumulated_ids gate ->
        SMap.union
          union
          accumulated_ids
          (get_prover_identities
             gate
             ~prefix_common:circuit_prefix
             ~prefix
             ~public
             ~domain
             evaluations))
      init_ids
      gates

  let aggregate_verifier_identities ?(circuit_prefix = Fun.id) ~input_com_sizes
      ~proof_prefix:prefix ~gates ~public_inputs ~generator ~size_domain () :
      verifier_identities =
   fun x answers ->
    let gates = filter_gates gates |> SMap.keys in
    let answers = filter_answers ~answers ~prefix ~circuit_prefix gates in
    let arith_id = SMap.singleton (prefix @@ arith ^ ".0") Scalar.zero in
    let union key s1 s2 =
      assert (key = prefix @@ arith ^ ".0") ;
      Some (Scalar.add s1 s2)
    in
    let public =
      {public_inputs; input_coms_size = List.fold_left ( + ) 0 input_com_sizes}
    in
    List.fold_left
      (fun accumulated_ids gate ->
        let gate_ids =
          get_verifier_identities
            gate
            ~prefix_common:circuit_prefix
            ~prefix
            ~public
            ~generator
            ~size_domain
        in
        SMap.union union accumulated_ids (gate_ids x answers))
      arith_id
      gates

  let aggregate_polynomials_degree ~gates =
    SMap.fold
      (fun gate _ degree ->
        let map = get_polynomials_degree gate in
        SMap.fold (fun _ d acc -> max d acc) map degree)
      (filter_gates gates)
      0

  let exists_gx_composition ~gates =
    SMap.exists (fun q _ -> get_gx_composition q) (filter_gates gates)

  (* - (x^n -1)/n·(w0/(x -1) + w1/(x/g - 1) + … + wn/(x/g^n - 1)) *)
  let cs_pi ~generator ~n ~x ~zs pi_list =
    let open L in
    let open Num in
    let mone = Scalar.(negate one) in
    let n_inv = Scalar.(inverse_exn (negate n)) in
    let g_inv = Scalar.inverse_exn generator in
    match pi_list with
    | [] -> constant_scalar Scalar.zero
    | hd :: tl_pi ->
        (* negate because we want -PI(x) *)
        let* left_term = mul_by_constant n_inv zs in
        let* init_value =
          let* x_minus_one = add_constant mone x in
          div hd x_minus_one
        in
        let* sum, _g_inv_k =
          foldM
            (fun (res, g_inv_k) w_k ->
              let* x_g_inv_k_minus_one = add_constant ~ql:g_inv_k mone x in
              let* to_add = div w_k x_g_inv_k_minus_one in
              let* res = add res to_add in
              ret (res, Scalar.mul g_inv_k g_inv))
            (init_value, g_inv)
            tl_pi
        in
        mul left_term sum
end

include (Aggregator : S)
