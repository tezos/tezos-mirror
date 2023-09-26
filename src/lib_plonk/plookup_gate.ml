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

let nb_plookup_wires = 3

module Plookup_gate_impl (PP : Polynomial_protocol.S) = struct
  module PP = PP

  exception Entry_not_in_table of string

  let q_label = "q_plookup"

  let q_table = "q_table"

  let f = "f_plookup"

  let fg = "fg_plookup"

  let z = "z_plookup"

  let t = "table"

  let h1 = "h1"

  let h2 = "h2"

  let zg = "zg_plookup"

  let tg = "tg_plookup"

  let h1g = "h1g"

  let h2g = "h2g"

  let l1 = "L1"

  let ln_p_1 = "L_n_plus_1"

  let x_m_1 = "x_minus_1"

  let x = "X"

  type public_parameters =
    (PP.prover_public_parameters * PP.verifier_public_parameters)
    * Scalar.t array list
    * Poly.t SMap.t

  let zero = Scalar.zero

  let one = Scalar.one

  let mone = Scalar.negate one

  (* alpha is a randomness used for Plookup *)
  let get_alpha = function
    | Some alpha -> alpha
    | None -> failwith "Plookup alpha is undefined"

  let gate_identity ~circuit_prefix ~prefix ~n ~generator ~wires_names ~alpha
      ~beta ~gamma : verifier_identities =
   fun x answers ->
    let q_label_name = circuit_prefix q_label in
    let q_table_name = circuit_prefix q_table in
    let t_name = circuit_prefix t in
    let z_name = prefix z in
    let f_name = prefix f in
    let h1_name = prefix h1 in
    let h2_name = prefix h2 in
    let wires_names =
      List.sub (List.map (fun x -> prefix x) wires_names) 0 nb_plookup_wires
    in
    let ( @- ) a b = Scalar.sub a b in
    let one_p_b = Scalar.(one + beta) in
    let g_one_p_b = Scalar.(gamma * one_p_b) in
    let g_one_p_b_2 = Scalar.square g_one_p_b in

    let q = get_answer answers X q_label_name in
    let z = get_answer answers X z_name in
    let f = get_answer answers X f_name in
    let t_val = get_answer answers X t_name in
    let h1 = get_answer answers X h1_name in
    let h2 = get_answer answers X h2_name in
    let zg = get_answer answers GX z_name in
    let fg = get_answer answers GX f_name in
    let tg = get_answer answers GX t_name in
    let h1g = get_answer answers GX h1_name in
    let h2g = get_answer answers GX h2_name in

    let l1, ln_p_1 =
      let n = Z.of_int n in
      let xn_1 = Scalar.(sub (pow x n) one / of_z n) in
      (Scalar.(generator * xn_1 / sub x generator), Scalar.(xn_1 / sub x one))
    in

    let z_m_1 = Scalar.(z @- one) in
    let x_m_1 = Scalar.(x @- one) in
    (* Identity: L1(x)·[Z(x) - 1] = 0 *)
    let id_a = Scalar.(l1 * z_m_1) in
    (* Identity: Ln+1(x)·[h1(x) - h2(gx)] = 0 *)
    let id_c = Scalar.(ln_p_1 * sub h1 h2g) in
    (* Identity: Ln+1(x)·[Z(x) - 1] = 0 *)
    let id_d = Scalar.(ln_p_1 * z_m_1) in
    (* Identity:
       (x - g^{n+1})·Z(x)·(1 + β)·[γ + f(x)]·[γ(1 + β) + t(x) + β·t(gx)]
       = (x - g^{n+1})·Z(g·x)·[γ(1 + β) + h1(x) + β·h1(gx)]·[γ(1 + β) + h2(x) + β·h2(gx)]
    *)
    (* Note that g^{n+1} equals 1 *)
    (* Developping the left part of the equality *)
    let l_g_a = Scalar.(g_one_p_b_2 * x_m_1 * z) in
    let l_g_b = Scalar.(g_one_p_b * x_m_1 * t_val * z) in
    let l_g_c = Scalar.(beta * g_one_p_b * x_m_1 * z * tg) in
    let l_f_a = Scalar.(g_one_p_b * one_p_b * x_m_1 * z * f) in
    let l_f_b = Scalar.(one_p_b * x_m_1 * t_val * z * f) in
    let l_f_c = Scalar.(beta * one_p_b * x_m_1 * z * f * tg) in
    (* Developping the right part of the equality *)
    let ra_a_a = Scalar.(g_one_p_b_2 * x_m_1 * zg) in
    let ra_a_b = Scalar.(g_one_p_b * x_m_1 * h2 * zg) in
    let ra_a_c = Scalar.(g_one_p_b * beta * x_m_1 * zg * h2g) in
    let ra_b_a = Scalar.(g_one_p_b * x_m_1 * h1 * zg) in
    let ra_b_b = Scalar.(x_m_1 * h1 * h2 * zg) in
    let ra_b_c = Scalar.(beta * x_m_1 * h1 * zg * h2g) in
    let ra_c_a = Scalar.(g_one_p_b * beta * x_m_1 * h1g * zg) in
    let ra_c_b = Scalar.(beta * x_m_1 * h1g * h2 * zg) in
    let ra_c_c = Scalar.(square beta * x_m_1 * h1g * zg * h2g) in
    let id_b =
      Scalar.(
        (l_g_a + l_g_b + l_g_c + l_f_a + l_f_b + l_f_c)
        @- ra_a_a + ra_a_b + ra_a_c + ra_b_a + ra_b_b + ra_b_c + ra_c_a + ra_c_b
           + ra_c_c)
    in
    let identities =
      SMap.of_list
      @@ List.map
           (fun (key, id) -> (key, Scalar.mul q id))
           [
             (prefix "Plookup.a", id_a);
             (prefix "Plookup.b", id_b);
             (prefix "Plookup.c", id_c);
             (prefix "Plookup.d", id_d);
           ]
    in
    let id_ultra =
      let q_table = get_answer answers X q_table_name in
      let wire_values =
        List.map (fun w -> get_answer answers X w) wires_names
      in
      let prod = Fr_generation.batch alpha (q_table :: wire_values) in
      Scalar.(q * (prod @- fg))
    in
    SMap.add (prefix "Plookup.ultra") id_ultra identities

  let prover_identities_aux ~circuit_prefix ~prefix ~wires_names ~alpha ~beta
      ~gamma n : prover_identities =
   fun evaluations ->
    let q = circuit_prefix q_label in
    let q_table = circuit_prefix q_table in
    let t = circuit_prefix t in
    let z = prefix z in
    let h1 = prefix h1 in
    let h2 = prefix h2 in
    let f = prefix f in
    let wires_names =
      List.sub (List.map (fun x -> prefix x) wires_names) 0 nb_plookup_wires
    in
    let fs = q_table :: wires_names in
    let z = Evaluations.find_evaluation evaluations z in
    let q = Evaluations.find_evaluation evaluations q in
    let l1 = Evaluations.find_evaluation evaluations l1 in
    let ln_p_1 = Evaluations.find_evaluation evaluations ln_p_1 in

    let eval_length = Evaluations.length q in
    let id1_evaluation = Evaluations.create eval_length in
    let id2_evaluation = Evaluations.create eval_length in
    let id3_evaluation = Evaluations.create eval_length in
    let id4_evaluation = Evaluations.create eval_length in
    let tmp_evaluation = Evaluations.create eval_length in

    let idb =
      let one_p_b = Scalar.(one + beta) in
      let g_one_p_b = Scalar.(gamma * one_p_b) in

      (* res <- γ·(1 + β) + e(x) + β·e(gx) *)
      let g_one_p_b_plus_e_plus_beta_p_eg res e =
        Evaluations.linear
          ~res
          ~evaluations
          ~poly_names:[e; e]
          ~add_constant:g_one_p_b
          ~composition_gx:([0; 1], n)
          ~linear_coeffs:[one; beta]
          ()
      in

      (* id1_evaluation <- (x - 1) *)
      let x_mone =
        Evaluations.linear
          ~res:id1_evaluation
          ~evaluations
          ~poly_names:[x]
          ~add_constant:mone
          ()
      in
      (* id2_evaluation <- (1 + β)·f(x) + γ·(1 + β) *)
      let f_expr =
        Evaluations.linear
          ~res:id2_evaluation
          ~evaluations
          ~poly_names:[f]
          ~linear_coeffs:[one_p_b]
          ~add_constant:g_one_p_b
          ()
      in
      (* id3_evaluation <- γ·(1 + β) + t(x) + β·t(gx) *)
      let t_expr = g_one_p_b_plus_e_plus_beta_p_eg id3_evaluation t in
      (* id4_evaluation <-
         z(x)·(x - 1)·((1 + β)·f(x) + γ·(1 + β))·(γ·(1 + β) + t(x) + β·t(gx)) *)
      let left_term =
        Evaluations.mul_c
          ~res:id4_evaluation
          ~evaluations:[z; x_mone; f_expr; t_expr]
          ()
      in

      (* id2_evaluation <- γ·(1 + β) + h1(x) + β·h1(gx) *)
      let h1_expr = g_one_p_b_plus_e_plus_beta_p_eg id2_evaluation h1 in
      (* id3_evaluation <- γ·(1 + β) + h2(x) + β·h2(gx) *)
      let h2_expr = g_one_p_b_plus_e_plus_beta_p_eg id3_evaluation h2 in
      (* tmp_evaluation <-
         z(gx)·(x - 1)·(γ·(1 + β) + h1(x) + β·h1(gx))·(γ·(1 + β) + h2(x) + β·h2(gx)) *)
      let right_term =
        Evaluations.mul_c
          ~res:tmp_evaluation
          ~evaluations:[z; x_mone; h1_expr; h2_expr]
          ~composition_gx:([1; 0; 0; 0], n)
          ()
      in
      let id_b =
        Evaluations.linear_c
          ~res:id1_evaluation
          ~evaluations:[left_term; right_term]
          ~linear_coeffs:[one; mone]
          ()
      in
      Evaluations.mul_c ~res:id2_evaluation ~evaluations:[q; id_b] ()
    in

    (* tmp_evaluation <- (z - 1) *)
    let z_mone =
      Evaluations.linear_c
        ~res:tmp_evaluation
        ~evaluations:[z]
        ~add_constant:mone
        ()
    in

    let ida =
      Evaluations.mul_c ~res:id1_evaluation ~evaluations:[q; l1; z_mone] ()
    in
    let idd =
      Evaluations.mul_c ~res:id4_evaluation ~evaluations:[q; ln_p_1; z_mone] ()
    in

    let idc =
      (* tmp_evaluation <- (h1(x) - h2(gx)) *)
      let h1_minus_h2g =
        Evaluations.linear
          ~res:tmp_evaluation
          ~evaluations
          ~poly_names:[h1; h2]
          ~linear_coeffs:[one; mone]
          ~composition_gx:([0; 1], n)
          ()
      in

      Evaluations.mul_c
        ~res:id3_evaluation
        ~evaluations:[q; ln_p_1; h1_minus_h2g]
        ()
    in

    let base = [ida; idb; idc; idd] in
    let ids =
      let id_agg =
        let id5_evaluation = Evaluations.create eval_length in
        (* id5_evaluation <- fs1 + alpha * fs2 + alpha^2 * fs3 + .. *)
        let s =
          let alpha_array = Fr_generation.powers (List.length fs) alpha in
          Evaluations.linear
            ~res:id5_evaluation
            ~evaluations
            ~poly_names:fs
            ~linear_coeffs:(Array.to_list alpha_array)
            ()
        in

        (* tmp_evaluation <- (s - f) *)
        let s_minus_f =
          let f = Evaluations.find_evaluation evaluations f in
          Evaluations.linear_c
            ~res:tmp_evaluation
            ~evaluations:[s; f]
            ~linear_coeffs:[one; mone]
            ~composition_gx:([0; 1], n)
            ()
        in
        Evaluations.mul_c ~res:id5_evaluation ~evaluations:[q; s_minus_f] ()
      in
      id_agg :: base
    in
    let id_names =
      let base = ["Plookup.a"; "Plookup.b"; "Plookup.c"; "Plookup.d"] in
      let base = "Plookup.ultra" :: base in
      List.map (fun id_name -> prefix id_name) base
    in
    SMap.of_list (List.combine id_names ids)

  module Plookup_poly = struct
    let ln_p_1 n domain =
      let scalar_list = Array.(append [|one|] (init (n - 1) (fun _ -> zero))) in
      Evaluations.interpolation_fft2 domain scalar_list

    (* computes an array where the i-th element is sum_j alpha_j*x_i,j
       where x_i,j is the i-th elementof the j_th array of the list*)
    let compute_aggregation array_list alpha =
      let n = Array.length (List.hd array_list) in
      let nb_wires = List.length array_list in
      let alpha_array = Fr_generation.powers nb_wires alpha in
      Array.init n (fun i ->
          let fis = List.map (fun array -> array.(i)) array_list in
          List.fold_left2
            (fun acc alpha_j fij -> Scalar.(acc + (alpha_j * fij)))
            Scalar.zero
            (Array.to_list alpha_array)
            fis)

    let compute_f_aggregation gates wires alpha n =
      let q = SMap.find q_label gates in
      let nb_wires = SMap.cardinal wires in
      let alpha_array = Fr_generation.powers nb_wires alpha in
      let array_list = SMap.values wires in
      let compute_aggregate qi fis =
        List.fold_left2
          (fun acc alpha_j fij -> Scalar.(acc + (alpha_j * qi * fij)))
          Scalar.zero
          (Array.to_list alpha_array)
          fis
      in
      (* Store previous lookup to pad with *)
      let previous_lookup =
        let index =
          List.find
            (fun i -> not (Scalar.is_zero q.(i)))
            (List.init n (fun i -> i))
        in
        let q0 = q.(index) in
        let f0s = List.map (fun array -> array.(index)) array_list in
        ref (compute_aggregate q0 f0s)
      in
      Array.init n (fun i ->
          let qi = q.(i) in
          if Scalar.is_zero qi then !previous_lookup
          else
            let fis = List.map (fun array -> array.(i)) array_list in
            let lookup = compute_aggregate qi fis in
            if not (Scalar.eq !previous_lookup lookup) then
              previous_lookup := lookup ;
            lookup)

    let sort_by f t =
      let indexes_t, _ =
        Array.fold_left
          (fun (map, i) z -> (Scalar_map.add z i map, i + 1))
          (Scalar_map.empty, 0)
          t
      in
      let my_compare a b =
        let a_index_opt = Scalar_map.find_opt a indexes_t in
        let b_index_opt = Scalar_map.find_opt b indexes_t in
        match (a_index_opt, b_index_opt) with
        | Some a_index, Some b_index -> a_index - b_index
        | _ -> raise (Entry_not_in_table "Array f is not included in array t")
      in
      Array.sort my_compare f ;
      f

    let switch t =
      let k = Array.length t in
      Array.init k (fun i -> if i = 0 then t.(k - 1) else t.(i - 1))

    let t_poly_from_tables tables alpha domain =
      let t = compute_aggregation tables alpha in
      Evaluations.interpolation_fft2 domain (switch t)

    let compute_s f t = sort_by (Array.concat [f; t]) t

    let compute_h s domain n =
      let compute_hi ~domain ~start s n =
        Evaluations.interpolation_fft2 domain (switch (Array.sub s start n))
      in
      let h1 = compute_hi ~domain ~start:0 s n in
      let h2 = compute_hi ~domain ~start:(n - 1) s n in
      (h1, h2)

    let compute_z beta gamma f t s n domain =
      let one_p_beta = Scalar.(one + beta) in
      let gamma_one_p_beta = Scalar.(gamma * one_p_beta) in
      let tmp = Scalar.(copy one) in

      let to_acc array i =
        let beta_a = Scalar.mul beta array.(Int.succ i) in
        Scalar.(
          add_inplace tmp beta_a array.(i) ;
          add_inplace beta_a tmp gamma_one_p_beta ;
          beta_a)
      in

      (* the first two elements of z_array are always one *)
      let z_array = Array.init n (fun _ -> Scalar.zero) in

      z_array.(0) <- one ;
      z_array.(1) <- one ;
      for i = 0 to n - 3 do
        let f_coeff = Scalar.(f.(i) + gamma) in
        let t_coeff = to_acc t i in
        Scalar.(
          mul_inplace tmp f_coeff one_p_beta ;
          mul_inplace f_coeff tmp t_coeff) ;

        let acc_i = to_acc s i in
        let acc_n_i = to_acc s (n - 1 + i) in
        Scalar.mul_inplace acc_i acc_i acc_n_i ;
        let z_coeff = Scalar.(f_coeff / acc_i) in
        z_array.(i + 2) <- Scalar.mul z_array.(i + 1) z_coeff
      done ;

      Evaluations.interpolation_fft2 domain z_array
  end

  let srs_size ~length_table =
    let log = Z.(log2up (of_int length_table)) in
    let length_padded = Int.shift_left 1 log in
    length_padded

  (* max degree of Plookup identities is idb’s degree, which is ~4n *)
  let polynomials_degree () = 4

  let common_preprocessing ~n:nb_records ~domain ~evaluations =
    let lnp1_map =
      SMap.singleton ln_p_1 (Plookup_poly.ln_p_1 nb_records domain)
    in
    Evaluations.compute_evaluations_update_map ~evaluations lnp1_map

  let preprocessing ~domain ~tables ~alpha () =
    let alpha = get_alpha alpha in
    SMap.singleton t (Plookup_poly.t_poly_from_tables tables alpha domain)

  let format_tables ~tables ~nb_columns ~length_not_padded ~length_padded =
    let concatenated_table =
      (* We make sure that all tables have the same number of columns as the number of wires by filling with columns of 0s.
         We also index tables. *)
      let corrected_tables =
        List.mapi
          (fun i t ->
            let nb_subtable_columns = List.length t in
            let sub_table_size = Array.length (List.hd t) in
            (* Pad table to have constant number of columns. *)
            let padding_columns =
              List.init (nb_columns - nb_subtable_columns) (fun _ ->
                  Array.make sub_table_size zero)
            in
            let full_table = t @ padding_columns in
            (* Indexing table. *)
            Array.make sub_table_size (Scalar.of_z (Z.of_int i)) :: full_table)
          tables
      in
      (* Concatenating tables. *)
      let acc_n = List.init (nb_columns + 1) (fun _ -> [||]) in
      List.fold_left
        (fun aa ll -> List.map2 (fun a l -> Array.append a l) aa ll)
        acc_n
        corrected_tables
    in
    (* Padding table. *)
    List.map
      (fun t ->
        let last = t.(length_not_padded - 1) in
        let padding = Array.make (length_padded - length_not_padded) last in
        Array.append t padding)
      concatenated_table

  let prover_identities ?(circuit_prefix = Fun.id) ~proof_prefix ~wires_names
      ~alpha ~beta ~gamma ~n () : prover_identities =
    let alpha = get_alpha alpha in
    prover_identities_aux
      ~circuit_prefix
      ~prefix:proof_prefix
      ~wires_names
      ~alpha
      ~beta
      ~gamma
      n

  let verifier_identities ?(circuit_prefix = Fun.id) ~proof_prefix ~n ~generator
      ~wires_names ~alpha ~beta ~gamma () : verifier_identities =
    let alpha = get_alpha alpha in
    gate_identity
      ~circuit_prefix
      ~prefix:proof_prefix
      ~n
      ~generator
      ~wires_names
      ~alpha
      ~beta
      ~gamma

  (* wires must be correctly padded *)
  (*TODO : do this in evaluation*)
  (*TODO : use mul z_s*)
  let f_map_contribution ~wires ~gates ~tables ~alpha ~beta ~gamma ~domain =
    let wires =
      let wires_names = List.init nb_plookup_wires Csir.wire_name in
      SMap.filter_map
        (fun k w ->
          if not (List.mem k wires_names) then None
            (* TODO : remove this conversion *)
          else Some (Evaluations.to_array w))
        wires
    in
    let size_domain = Domain.length domain in
    let alpha = get_alpha alpha in
    let t_agg = Plookup_poly.compute_aggregation tables alpha in
    (* We add the table selector to be aggregated alongside the wires. *)
    let wires_to_agg =
      let table_selector = SMap.find q_table gates in
      (* We add the prefix _ to the selector's label to make sure the selector is first in the map. *)
      SMap.add ("_" ^ q_table) table_selector wires
    in
    let final_size = size_domain - 1 in
    (* /!\ We remove here the last value of each wire, this is ok as it always corresponds to padding. *)
    let padded_f_list =
      SMap.map (fun w -> resize_array w final_size) wires_to_agg
    in
    let f_agg =
      Plookup_poly.compute_f_aggregation gates padded_f_list alpha final_size
    in
    let f_poly =
      Evaluations.interpolation_fft2 domain Array.(append [|zero|] f_agg)
    in
    let s = Plookup_poly.compute_s f_agg t_agg in
    let h1_poly, h2_poly = Plookup_poly.compute_h s domain size_domain in
    let z_poly =
      Plookup_poly.compute_z beta gamma f_agg t_agg s size_domain domain
    in
    SMap.of_list [(h1, h1_poly); (h2, h2_poly); (z, z_poly); (f, f_poly)]
end

module type S = sig
  module PP : Polynomial_protocol.S

  exception Entry_not_in_table of string

  type public_parameters =
    (PP.prover_public_parameters * PP.verifier_public_parameters)
    * Scalar.t array list
    * Poly.t SMap.t

  val srs_size : length_table:int -> int

  val polynomials_degree : unit -> int

  val format_tables :
    tables:Scalar.t array list list ->
    nb_columns:int ->
    length_not_padded:int ->
    length_padded:int ->
    Scalar.t array list

  val common_preprocessing :
    n:int ->
    domain:Domain.t ->
    evaluations:Evaluations.t SMap.t ->
    Evaluations.t SMap.t

  val preprocessing :
    domain:Domain.t ->
    tables:Scalar.t array list ->
    alpha:Scalar.t option ->
    unit ->
    Poly.t SMap.t

  val prover_identities :
    ?circuit_prefix:(string -> string) ->
    proof_prefix:(string -> string) ->
    wires_names:string list ->
    alpha:Scalar.t option ->
    beta:Scalar.t ->
    gamma:Scalar.t ->
    n:int ->
    unit ->
    prover_identities

  val verifier_identities :
    ?circuit_prefix:(string -> string) ->
    proof_prefix:(string -> string) ->
    n:int ->
    generator:Scalar.t ->
    wires_names:string list ->
    alpha:Scalar.t option ->
    beta:Scalar.t ->
    gamma:Scalar.t ->
    unit ->
    verifier_identities

  val f_map_contribution :
    wires:Evaluations.t SMap.t ->
    gates:Scalar.t array SMap.t ->
    tables:Scalar.t array list ->
    alpha:Scalar.t option ->
    beta:Scalar.t ->
    gamma:Scalar.t ->
    domain:Domain.t ->
    PP.PC.secret
end

module Plookup_gate (PP : Polynomial_protocol.S) : S with module PP = PP =
  Plookup_gate_impl (PP)
