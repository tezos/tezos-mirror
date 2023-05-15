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

(* This files contains all tools that can be used to create the verification circuit of aPlonK. *)

module SMap = Plonk.SMap
open Plonk.Bls

let nb_wires = Plompiler.Csir.nb_wires_arch

module V (Main : Aggregation.Main_protocol.S) = struct
  module Gates = Main.Gates
  module Perm = Main.Perm
  module RC = Main.RangeCheck
  module S = Plompiler.S
  open Plompiler.LibCircuit

  type scalar_input = scalar Input.input

  (* This type gathers all inputs of the verification circuit
     alpha, beta, gamma, delta, x, r are challenges generated from Fiat-Shamir
     ss_list are preprocessed permutation polynomials evaluations at x
     selectors are the list of the selectors evaluations at x with their names
     ids_batch is the batch (with alpha) of the evaluated identities at x
     wires_g is the lists of all wires evaluations at gx for each proof
     wires is the lists of all wires evaluations at x for each proof
     zg is the permutation polynomial evaluation at gx
     z is the permutation polynomial evaluation at x
     batch are the expected batched values with r of
      · ss_list selectors
      · ids_batch
      · wires_g
      · wires
      · zg_list
      · z_list
     outer_pi is the list of public inputs that are public to aPlonK’s verifier
     inner_pi is the list of hidden public inputs that are not known from aPlonK’s verifier
     rc_selectors are range checks dedicated selectors lnin1, pnin1, RC_ss1 & RC_ss2
     z_rc is the list of Range checks’ Z proof polynomials evaluations for each proof
     z_rc_perm & z_rc_perm are Range checks’ permutation polynomial evaluation for X and gX. Note that the list must be at most of size 1 (1 permutation polynomial per circuit). We use lists here in order make handling in plompiler easier.
  *)
  type circuit_inputs = {
    switches : bool Input.input list;
    compressed_switches : scalar_input;
    alpha : scalar_input;
    beta : scalar_input;
    gamma : scalar_input;
    beta_rc : scalar_input;
    gamma_rc : scalar_input;
    delta : scalar_input;
    x : scalar_input;
    r : scalar_input;
    ss_list : scalar_input list;
    selectors : (string * scalar_input) list;
    ids_batch : scalar_input;
    wires_g : scalar_input list list;
    wires : scalar_input list list;
    zg : scalar_input;
    z : scalar_input;
    batch : scalar_input list;
    inner_pi : scalar_input list list;
    outer_pi : scalar_input list;
    rc_selectors : scalar_input list;
    zg_rc : scalar_input list;
    z_rc : scalar_input list;
  }

  let nb_batch gates = if Gates.exists_gx_composition ~gates then 5 else 4

  (* This inputs is given to the verification circuit when it’s created. The value of the dummy inputs is irrelevant, only their structures & sizes matter *)
  let dummy_input range_checks gates nb_proofs nb_inner_pi nb_outer_pi =
    let switches = List.init nb_proofs (fun _ -> Input.bool true) in
    let dummy_input = Input.scalar S.one in
    let wires =
      List.init nb_proofs (fun _ -> List.init nb_wires (fun _ -> dummy_input))
    in
    let inner_pi =
      List.init nb_proofs (fun _ ->
          List.init nb_inner_pi (fun _ -> dummy_input))
    in
    let outer_pi = List.init nb_outer_pi (fun _ -> dummy_input) in
    let selectors = List.map (fun q -> (q, dummy_input)) gates in
    let ss_list =
      List.init Plompiler.Csir.nb_wires_arch @@ Fun.const dummy_input
    in
    let gates = SMap.of_list (List.map (fun g -> (g, ())) gates) in
    let batch = List.init (nb_batch gates) (fun _ -> dummy_input) in
    let wires_g = if nb_batch gates = nb_batch SMap.empty then [] else wires in
    let z_rc, rc_selectors =
      if SMap.is_empty range_checks then ([], [])
      else
        let nb_wires = SMap.cardinal range_checks in
        (* here the +nb_wires stands for RC_perm_Z *)
        ( List.init ((nb_proofs * nb_wires) + nb_wires) (Fun.const dummy_input),
          List.init (4 * nb_wires) (Fun.const dummy_input) )
    in
    {
      switches;
      compressed_switches = dummy_input;
      alpha = dummy_input;
      beta = dummy_input;
      gamma = dummy_input;
      beta_rc = dummy_input;
      gamma_rc = dummy_input;
      delta = dummy_input;
      x = dummy_input;
      r = dummy_input;
      ss_list;
      selectors;
      ids_batch = dummy_input;
      wires_g;
      wires;
      zg = dummy_input;
      z = dummy_input;
      batch;
      inner_pi;
      outer_pi;
      rc_selectors;
      zg_rc = z_rc;
      z_rc;
    }

  module Constraints = struct
    (* replace by [zero] all elements of the second list that correspond to false in the first one *)
    let switch_list = map2M (fun s l -> mapM (Num.mul s) l)

    (* Replace with zeros all the wires values when the correponding switch is false *)
    let switch switches ~wires ~wires_g ~inner_pi =
      let switches = List.map scalar_of_bool switches in
      let* wires = switch_list switches wires in
      let* wires_g =
        match wires_g with
        | [] -> ret []
        | wires_g -> switch_list switches wires_g
      in
      let* inner_pi = switch_list switches inner_pi in
      ret (wires, wires_g, inner_pi)

    (* Because we can’t deal as we want with the monad in the output of Gates functions & for gates uniformity, Arith monomials are expected to be given as scalar repr t list t. *)
    let format_arith_cs : scalar repr list t -> scalar repr t =
     fun scalar_repr_list_t ->
      let* scalar_repr_list = scalar_repr_list_t in
      match scalar_repr_list with
      | [cs] -> ret cs
      | _ -> raise (Invalid_argument "Invalid format for Arith constraint.")

    (* For a selector name and value q, given the wires values wires & wires_g, returns the evaluation of arithmetic monomial associated with the selector. *)
    let cs_of_arith_sel name q wires wires_g =
      let wires = Array.of_list wires in
      let wires_g = Array.of_list wires_g in
      format_arith_cs (Gates.get_cs name ~q ~wires ~wires_g ())

    (* For a selector name and value q, given the wires values wires & wires_g, returns the evaluation of identity given by the selector. *)
    let cs_of_custom_sel ?precomputed_advice name q wires wires_g =
      let wires = Array.of_list wires in
      let wires_g = Array.of_list wires_g in
      Gates.get_cs name ~q ~wires ~wires_g ?precomputed_advice ()

    (* Circuit that computes x^n - 1 *)
    let compute_zs x n =
      with_label ~label:"zs"
      @@
      let nb_bits = S.size_in_bytes in
      let* n_repr = constant_scalar n in
      let* n_bytes = bits_of_scalar ~nb_bits n_repr in
      let* xn = Num.pow x (of_list n_bytes) in
      Num.add_constant S.(negate one) xn

    (* Circuit that computes L1(x) := (g / n) * (x^n - 1) / (x - g) *)
    let compute_l1 x xn_minus_one n generator =
      let* den = Num.add_constant S.(negate generator) x in
      Num.div ~den_coeff:(S.div_exn n generator) xn_minus_one den

    (* Circuit that computes (x₀ + αx₁ + α²x₂ + …) for list_circuit = [x₀, x₁, x₂, …] *)
    let sum_alpha_i list_circuit alpha =
      let list_circuit = List.rev list_circuit in
      match list_circuit with
      | [] -> constant_scalar S.zero
      | init :: list_circuit ->
          foldM
            (fun acc circuit ->
              let* tmp = Num.mul acc alpha in
              Num.add tmp circuit)
            init
            list_circuit

    (* Circuit that computes (x₀ + αx₁ + α²x₂ + …) for list_circuit = [x₀, x₁, x₂, …] and ignores the coefficients when corresponding switch is false ; for instance, if switch₁ = 0 and all other switches are 1, it will compute (x₀ + αx₂ + α²x₃ + …) *)
    let sum_alpha_i_switched switches list_circuit alpha =
      let* zero = constant_scalar S.zero in
      let* alpha_min_one = Num.add ~qc:S.(negate one) alpha zero in
      let nb_proofs = List.length switches in
      let list_circuit = List.rev list_circuit in
      let switches = List.rev switches in
      match list_circuit with
      | [] -> constant_scalar S.zero
      | list_circuit ->
          let* res, _ =
            fold2M
              (fun (acc, i) coeff switch ->
                let switch = scalar_of_bool switch in
                let* coeff = Num.mul switch coeff in
                (* α = [switch × (α - 1)] + 1 *)
                let* alpha =
                  Num.custom ~qm:S.one ~qc:S.one switch alpha_min_one
                in
                let* tmp = Num.add acc coeff in
                let* res =
                  if i = nb_proofs then ret tmp else Num.mul tmp alpha
                in
                ret (res, i + 1))
              (zero, 1)
              list_circuit
              switches
          in
          ret res

    (* Circuit that recomputes T polynomial from the t_list of its parts & x^n *)
    let compute_t xn t_list = sum_alpha_i t_list xn

    let add_circuits circuit_list =
      let* circuit_list = mapM Fun.id circuit_list in
      Num.add_list (to_list circuit_list)

    (* Checks that compressed_switches is the sum of all switches and that switches has the structure [1…10…0] *)
    let check_switches compressed_switches switches =
      let switches = List.map scalar_of_bool switches in
      let* sum = Num.add_list (to_list switches) in
      let switches_pairs, _ =
        List.fold_left
          (fun (pairs, s_prev) s -> ((s_prev, s) :: pairs, s))
          ([], List.hd switches)
          (List.tl switches)
      in
      iterM
        (fun (s_prev, s) ->
          Num.assert_custom ~qm:S.one ~qr:S.(negate one) s_prev s s)
        switches_pairs
      >* equal compressed_switches sum

    (* Recomputes batches & compare them to claimed batches *)
    let check_batch r (g_list, wires, wiresg, z_list, zg_list) batch =
      let wires = List.flatten wires in
      let wiresg = List.flatten wiresg in
      let* batch_g = sum_alpha_i g_list r in
      let* batch_wires = sum_alpha_i wires r in
      let* batch_z = sum_alpha_i z_list r in
      let* batch_zg = sum_alpha_i zg_list r in
      let g_exp = List.nth batch 0 in
      let zg_exp = List.nth batch 1 in
      let z_exp = List.nth batch 2 in
      let* g = equal batch_g g_exp in
      let* zg = equal batch_zg zg_exp in
      let* z = equal batch_z z_exp in
      match wiresg with
      | [] ->
          let wires_exp = List.nth batch 3 in
          let* wires = equal batch_wires wires_exp in
          Bool.band_list [g; wires; zg; z]
      | wg ->
          let* batch_wires_g = sum_alpha_i wg r in
          let wiresg_exp = List.nth batch 3 in
          let wires_exp = List.nth batch 4 in
          let* wiresg = equal batch_wires_g wiresg_exp in
          let* wires = equal batch_wires wires_exp in
          Bool.band_list [g; wiresg; wires; zg; z]

    (* custom_ids is a nested list with 3 levels:
       - the outter one corresponds to different proofs
       - the middle level corresponds to different identities
       - the inner level corresponds to different equations of the same identity *)
    let format_custom_ids custom_ids =
      let suffix_ids l =
        List.map
          (fun (s, l) ->
            List.mapi (fun i x -> (s ^ "." ^ string_of_int i, x)) l)
          l
        |> List.flatten
      in
      let index_proofs l =
        let n = List.length l in
        List.mapi
          (fun i inner_l ->
            List.map
              (fun (s, x) -> (SMap.Aggregation.add_prefix ~n ~i "" s, x))
              inner_l)
          l
      in
      index_proofs (List.map suffix_ids custom_ids) |> List.flatten

    let format_arith_ids arith_ids =
      let arith = Plonk.Custom_gates.arith_label in
      let n = List.length arith_ids in
      List.mapi
        (fun i x -> (SMap.Aggregation.add_prefix ~n ~i "" arith, x))
        arith_ids

    (* Verifies that the linear combination of identities with alpha is equal to T×Zs
       The identities are computed from evaluations, with the functions cs of Custom_gates & Permutation_gate
    *)
    let check_identities ~switches (n, generator) x rc_wires ids_batch
        rc_selectors (q_names, selectors)
        (alpha, beta, gamma, beta_rc, gamma_rc, delta)
        (wires_g, wires, zg, z, zg_rc, z_rc) ss_list pi_list_list =
      (* We don’t care about wires_g value if it’s empty so we just take wires *)
      let wires_g = match wires_g with [] -> wires | w -> w in
      (* precompute some constant *)
      let* t = Bool.constant_bool true in
      let* zs = compute_zs x n in
      let* l1 = compute_l1 x zs n generator in
      (* split the arith selectors & other selectors in two lists *)
      let is_advice_sel s = String.starts_with ~prefix:Gates.qadv_label s in
      let arith_selectors, custom_selectors =
        List.combine q_names selectors
        |> List.partition (fun (q_name, _) ->
               if is_advice_sel q_name then false
               else
                 let id_name, nb_id = Gates.get_ids q_name in
                 if id_name = Gates.arith_label then
                   if nb_id <> 1 then
                     failwith "partition_selector : invalid Arith identity."
                   else true
                 else false)
      in
      let advice_selectors, custom_selectors =
        List.partition (fun (s, _) -> is_advice_sel s) custom_selectors
      in
      let precomputed_advice = SMap.of_list advice_selectors in
      (* Custom identities are usually composed with several identities
         Custom_ids has format
          [[[q₁.ida₁ ; q₁.idb₁] ; [q₂.ida₁ ; q₂.idb₁]] ;
           [[q₁.ida₂ ; q₁.idb₂] ; [q₂.ida₂ ; q₂.idb₂]] ;
           [[q₁.ida₃ ; q₁.idb₃] ; [q₂.ida₃ ; q₂.idb₃]]]
           for 2 selectors q₁ & q₂ with 2 identities each (ida & idb) & 3 proofs
      *)
      let* custom_ids =
        map2M
          (fun wires wires_g ->
            mapM
              (fun (name, q) ->
                let* id_values =
                  cs_of_custom_sel ~precomputed_advice name q wires wires_g
                in
                ret (name, id_values))
              custom_selectors)
          wires
          wires_g
      in
      let* arith_list =
        let monomials =
          List.map
            (fun (name, q) -> List.map2 (cs_of_arith_sel name q) wires wires_g)
            arith_selectors
        in
        let pi_list =
          List.map (Gates.cs_pi ~generator ~n ~x ~zs) pi_list_list
        in
        Plonk.List.mapn add_circuits (pi_list :: monomials) |> mapM Fun.id
      in
      (* Using switched wires is enough for adapting the perm identity to the lower number of proof *)
      let* aggregated_wires =
        Plonk.List.mapn (fun i -> sum_alpha_i i delta) wires |> mapM Fun.id
      in
      let* perm_ids =
        (Perm.cs ~l1 ~ss_list ~beta ~gamma ~x ~z ~zg) ~aggregated_wires ()
      in
      let nb_rc = List.length rc_wires in
      let* rc_ids =
        match rc_wires with
        | [] -> ret []
        | _ ->
            let nb_proofs = List.length wires in
            let lnin1 = Plonk.List.sub rc_selectors 0 nb_rc in
            let pnin1 = Plonk.List.sub rc_selectors nb_rc nb_rc in
            let ss_list =
              List.init nb_rc (fun i ->
                  Plonk.List.sub rc_selectors (nb_rc * (2 + i)) 2)
            in
            RC.cs
              ~rc_index:rc_wires
              ~nb_proofs
              ~lnin1
              ~pnin1
              ~z_list:z_rc
              ~zg_list:zg_rc
              ~aggregated_wires
              ~sum_alpha_i
              ~l1
              ~ss_list
              ~beta:beta_rc
              ~gamma:gamma_rc
              ~delta
              ~x
      in
      let identities =
        format_arith_ids arith_list
        @ format_custom_ids custom_ids
        @ perm_ids @ rc_ids
        |> List.sort (fun (s, _) (s', _) -> String.compare s s')
        |> List.map snd
      in
      (* Adapt the switches to match the identities. Note that the way we
         handle shifts here only works because the switches are of form [11…00] *)
      let id_switches =
        let for_each_proof =
          let nb_ids_per_proof =
            let nb_custom_ids =
              List.(
                hd custom_ids |> fold_left (fun acc l -> acc + length (snd l)) 0)
            in
            (* +1 for the arithmetic identity & + 2×nb_rc for the range check ids *)
            match rc_ids with
            | [] -> nb_custom_ids + 1
            | _ -> nb_custom_ids + 1 + (2 * nb_rc)
          in
          List.concat_map
            (fun b -> List.init nb_ids_per_proof (Fun.const b))
            switches
        in
        (* [true ; true] is added for the permutation identities, &
           nb_rc × [true ; true] is added for the RC permutation identities *)
        let for_whole_circuit =
          match rc_ids with
          | [] -> [t; t]
          | _ -> [t; t] @ List.init (2 * nb_rc) (Fun.const t)
        in
        for_each_proof @ for_whole_circuit
      in
      let* sum_id = sum_alpha_i_switched id_switches identities alpha in
      equal sum_id ids_batch
  end

  let verify_batch r batch batches t_answers =
    let init_sum =
      List.map (SMap.map @@ Fun.const Scalar.zero) (List.tl batch)
    in
    let init_sizes = List.map (SMap.map @@ Fun.const 0) (List.tl batch) in
    let sum_batches, _ =
      SMap.fold
        (fun _circuit_name this_batch (acc_sum, acc_sizes) ->
          let values = List.map (SMap.map fst) this_batch in
          let sizes_list = List.map (SMap.map snd) this_batch in
          let acc_sum =
            List.map2
              (fun map_sum (map_this_batch, map_sizes) ->
                SMap.mapi
                  (fun key acc_value ->
                    let size = SMap.find key map_sizes |> Z.of_int in
                    let this_value = SMap.find key map_this_batch in
                    Scalar.(acc_value + (this_value * pow r size)))
                  map_sum)
              acc_sum
              (List.combine values acc_sizes)
          in
          let acc_sizes =
            List.map2
              (fun map_acc_sizes map_sizes ->
                SMap.mapi
                  (fun key size -> size + SMap.find key map_sizes)
                  map_acc_sizes)
              acc_sizes
              sizes_list
          in
          (acc_sum, acc_sizes))
        batches
        (init_sum, init_sizes)
    in
    let t_batch =
      List.fold_left
        (fun (acc, rk) x -> Scalar.(acc + (rk * x), r * rk))
        (Scalar.zero, Scalar.one)
        t_answers
      |> fst
    in
    (* Complete the first element of [sum_batches] with [t_batch] *)
    let sum_batches =
      let hd = List.hd batch in
      assert (SMap.cardinal hd = 1) ;
      let t_key, _t_value = SMap.choose hd in
      SMap.singleton t_key t_batch :: sum_batches
    in
    let given = List.concat_map SMap.values batch in
    let computed = List.concat_map SMap.values sum_batches in
    List.for_all2 Scalar.( = ) given computed

  (* Format verification circuit public inputs *)
  let aggreg_public_inputs pi_size
      (alpha, beta, gamma, beta_rc, gamma_rc, delta, x, r) batch ids_batch
      compressed_switches outer_pi =
    let batch = List.concat_map SMap.values batch |> List.map fst in
    let public_input =
      Array.of_list
        ([alpha; beta; gamma; beta_rc; gamma_rc; delta; x; r]
        @ batch
        @ [ids_batch; compressed_switches]
        @ outer_pi)
    in
    let l = Array.length public_input in
    if l <> pi_size then
      failwith
        (Printf.sprintf
           "Public input has not expected size (expected: %d; actual: %d)."
           pi_size
           l) ;
    public_input

  let compute_switches max_nb_proofs nb_proofs =
    let switches =
      Array.init max_nb_proofs S.(fun i -> if i < nb_proofs then one else zero)
    in
    (switches, S.of_int nb_proofs)

  let pad_inputs nb_max_proofs nb_rc_wires inner_pi answers =
    let nb_proofs = List.length inner_pi in
    let padded_inner_pi =
      let to_pad = nb_max_proofs - nb_proofs in
      let nb_inner_pi = List.(length (hd inner_pi)) in
      List.flatten inner_pi
      @ List.(init (to_pad * nb_inner_pi) (Fun.const S.zero))
    in
    let padded_answers =
      Plonk.Utils.pad_answers nb_max_proofs nb_rc_wires nb_proofs answers
    in
    (padded_inner_pi, padded_answers)

  (* Returns witness of verification circuit *)
  let get_witness max_nb_proofs nb_rc_wires (p : Main.prover_aux) circuit_name
      pi_size solver (inner_pi, outer_pi) switches compressed_switches batch =
    let ids_batch = SMap.find circuit_name p.ids_batch |> fst in
    let public =
      aggreg_public_inputs
        pi_size
        (p.alpha, p.beta, p.gamma, p.beta_rc, p.gamma_rc, p.delta, p.x, p.r)
        batch
        ids_batch
        compressed_switches
        outer_pi
    in
    let circuit_answers =
      List.map
        (SMap.Aggregation.select_answers_by_circuit circuit_name)
        (* ignore the first element in p.answers corresponding to T evaluations,
           which are handled outside of the meta-verification circuit *)
        (List.tl p.answers)
    in
    let inputs =
      let inner_pi, answers =
        pad_inputs max_nb_proofs nb_rc_wires inner_pi circuit_answers
      in
      Array.(concat [of_list (inner_pi @ answers); public; switches])
    in
    try Plompiler.Solver.solve solver inputs
    with e ->
      print_string "\nSolver failure\n" ;
      raise e

  let get_batches inputs answers r =
    let batch_map r map = Plonk.Utils.Fr_generation.batch r (SMap.values map) in
    (* we map over [inputs] just because it contains the circuit_names *)
    SMap.mapi
      (fun circuit_name _ ->
        let answers =
          List.map
            (SMap.Aggregation.select_answers_by_circuit circuit_name)
            (List.tl answers)
        in
        List.map SMap.(map (fun m -> (batch_map r m, cardinal m))) answers)
      inputs

  (* generator & n are the subgroup generator & size of the subgroup of identities verification
     check_pi is a function related to PI, given here as argument in order to allow several PI handling forms
     note that wires & wires_g may also contain RC_Z polynomials
  *)
  let verification_circuit (generator, n) rc_wires check_pi
      {
        switches;
        compressed_switches;
        alpha;
        beta;
        gamma;
        beta_rc;
        gamma_rc;
        delta;
        x;
        r;
        ss_list;
        selectors;
        ids_batch;
        wires_g;
        wires;
        zg;
        z;
        batch;
        outer_pi;
        inner_pi;
        rc_selectors;
        zg_rc;
        z_rc;
      } =
    let n = S.of_int n in
    (* input selectors in the same order they are given *)
    let q_names, selectors = List.split selectors in

    let* inner_pi =
      begin_input_com (fun inner_pi -> List.map of_list (of_list inner_pi))
      |: Input.list (List.map Input.list inner_pi)
      |> end_input_com
    in
    let* rc_selectors, ss_list, selectors, zg_rc, zg, z_rc, z, wires_g, wires =
      begin_input_com
        (fun rc_selectors ss_list selectors zg_rc zg z_rc z wires_g wires ->
          ( of_list rc_selectors,
            ss_list,
            of_list selectors,
            of_list zg_rc,
            zg,
            of_list z_rc,
            z,
            List.map of_list (of_list wires_g),
            List.map of_list (of_list wires) ))
      |: Input.list rc_selectors |: Input.list ss_list |: Input.list selectors
      |: Input.list zg_rc |: zg |: Input.list z_rc |: z
      |: Input.list (List.map Input.list wires_g)
      |: Input.list (List.map Input.list wires)
      |> end_input_com
    in
    let* alpha = input ~kind:`Public alpha in
    let* beta = input ~kind:`Public beta in
    let* gamma = input ~kind:`Public gamma in
    let* beta_rc = input ~kind:`Public beta_rc in
    let* gamma_rc = input ~kind:`Public gamma_rc in
    let* delta = input ~kind:`Public delta in
    let* x = input ~kind:`Public x in
    let* r = input ~kind:`Public r in

    let* batch = mapM (input ~kind:`Public) batch in

    let* ids_batch = input ~kind:`Public ids_batch in

    let* compressed_switches = input ~kind:`Public compressed_switches in

    (* Input PI *)
    let* outer_pi = mapM (input ~kind:`Public) outer_pi in

    let* switches = mapM input switches in

    (* We use the switch to only take the n first wires evaluations (where n is the number of proofs actually performed), the rest is set to zero ; this nullifies batches and the permutation argument where proofs were not computed *)
    let* switched_wires, switched_wires_g, switched_inner_pi =
      Constraints.switch switches ~wires ~wires_g ~inner_pi
    in

    let ss_list = of_list ss_list in

    let* check_switches =
      with_label ~label:"check_switches"
      @@ Constraints.check_switches compressed_switches switches
    in

    let* check_pi =
      with_label ~label:"check_pi"
      @@ check_pi ~switches ~outer:outer_pi ~inner:inner_pi
    in

    let* check_identities =
      with_label ~label:"check_identities"
      @@ Constraints.check_identities
           ~switches
           (n, generator)
           x
           rc_wires
           ids_batch
           rc_selectors
           (q_names, selectors)
           (alpha, beta, gamma, beta_rc, gamma_rc, delta)
           (switched_wires_g, switched_wires, zg, z, zg_rc, z_rc)
           ss_list
           switched_inner_pi
    in
    let* check_batch =
      with_label ~label:"check_batch"
      @@
      let g_list = rc_selectors @ ss_list @ selectors in
      let z_list = z_rc @ [z] in
      let zg_list = zg_rc @ [zg] in
      Constraints.check_batch
        r
        (g_list, switched_wires, switched_wires_g, z_list, zg_list)
        batch
    in

    let* res =
      with_label ~label:"check_aplonk_res"
      @@ Bool.band_list
           [check_switches; check_batch; check_identities; check_pi]
    in
    Bool.assert_true res

  (* Function that creates the verification circuit as cs *)
  let get_cs_verification pp circuit nb_proofs (nb_outer_pi, nb_inner_pi)
      check_pi =
    let gen, n = Main.get_gen_n_prover pp in
    let gates = Plonk.Circuit.get_selectors circuit in
    let dummy_input =
      dummy_input circuit.range_checks gates nb_proofs nb_inner_pi nb_outer_pi
    in
    let rc_wires =
      SMap.(keys circuit.range_checks)
      |> List.map Plompiler.Csir.int_of_wire_name
    in
    Plompiler.LibCircuit.get_cs
      (verification_circuit (gen, n) rc_wires check_pi dummy_input)
end
