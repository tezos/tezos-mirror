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

open Bls
open Identities
module L = Plompiler.LibCircuit
open Gates_common

(* Modular addition over a non-native modulus:
   Non Arith
   degree : 2
   nb identities : 1 + |MOD_ARITH.moduli_add|
   advice selectors : None
   equations : see lib_plompiler/gadget_mod_arith.ml *)
module Make_ModAdd (MOD_ARITH : Plompiler__Gadget_mod_arith.MOD_ARITH) :
  Base_sig = struct
  module M = MOD_ARITH (L)

  let q_label = "q_mod_add_" ^ M.label

  let ( %! ) = Z.rem

  (* In the first row:
     M.nb_limbs correspond to the first input.
     M.nb_limbs correspond to the second input.

     In the second row:
     M.nb_limbs correspond to the output.
     1 corresponds to qm variable (quotient by the main modulus).
     M.moduli_add correspond to tj (quotionts by the auxiliary moduli). *)
  let nb_used_wires =
    let used_fst_row = 2 * M.nb_limbs in
    let used_snd_row = M.nb_limbs + 1 + List.length M.moduli_add in
    let nb_used_wires = Int.max used_fst_row used_snd_row in
    assert (nb_used_wires <= Plompiler.Csir.nb_wires_arch) ;
    nb_used_wires

  (* powers of the base modulo the modulus *)
  let bs_mod_m =
    List.init M.nb_limbs (fun i -> Z.pow M.base i %! M.modulus) |> List.rev

  let (qm_shift, _), ts_bounds = M.bounds_add

  (* There are as many identities as moduli + 1, as we also have an identity
     on the native modulus *)
  let identity = (q_label, 1 + List.length M.moduli_add)

  let index_com = None

  let nb_advs = 0

  let nb_buffers = 3

  let gx_composition = true

  let polynomials_degree =
    (q_label, 2) :: List.init nb_used_wires (fun i -> (wire_name i, 2))
    |> SMap.of_list

  let get_values wires wires_g =
    let xs = List.init M.nb_limbs (fun i -> wires.(i)) in
    let ys = List.init M.nb_limbs (fun i -> wires.(M.nb_limbs + i)) in
    let zs = List.init M.nb_limbs (fun i -> wires_g.(i)) in
    let qm = wires_g.(M.nb_limbs) in
    let ts = List.mapi (fun i _ -> wires_g.(M.nb_limbs + 1 + i)) M.moduli_add in
    let t_infos =
      List.map2 (fun tj (t_shift, _) -> Some (tj, t_shift)) ts ts_bounds
    in
    (xs, ys, zs, qm, t_infos)

  let equations ~q:q_mod_add ~wires ~wires_g ?precomputed_advice:_ () =
    (* z = (x + y) mod m
       let k := nb_limbs and n := |moduli|

       PlonK wires distribution:
        row i   : x0 ... x_{k-1} y0 ... y_{k-1}
        row i+1 : z0 ... z_{k-1} qm t1 ... t_{n}
    *)
    let xs, ys, zs, qm, t_infos = get_values wires wires_g in
    let sum = List.fold_left Scalar.add Scalar.zero in
    List.map2
      (fun mj t_info ->
        (* \sum_i ((B^i mod m) mod mj) * (x_i + y_i - z_i)
           - qm * (m mod mj) - ((qm_shift * m) mod mj) = (tj + tj_shift) * mj *)
        let tj, tj_shift =
          match t_info with
          | Some (tj, tj_shift) -> (tj, tj_shift)
          | None -> (Scalar.zero, Z.zero)
        in
        let id_mj =
          let open Scalar in
          sum
            (List.map2
               (fun bi_mod_m ((xi, yi), zi) ->
                 of_z (bi_mod_m %! mj) * (xi + yi + negate zi))
               bs_mod_m
               (List.combine (List.combine xs ys) zs))
          + negate (qm * of_z (M.modulus %! mj))
          + negate (of_z Z.(qm_shift * M.modulus %! mj))
          + negate ((tj + of_z tj_shift) * of_z mj)
        in
        Scalar.(q_mod_add * id_mj))
      (Scalar.order :: M.moduli_add)
      (None :: t_infos)

  let prover_identities ~prefix_common ~prefix ~public:_ ~domain :
      prover_identities =
   fun evaluations ->
    let domain_size = Domain.length domain in
    let tmps, ids = get_buffers ~nb_buffers ~nb_ids:(snd identity) in
    let ({q; wires} : witness) =
      get_evaluations ~q_label ~prefix ~prefix_common evaluations
    in
    let q_mod_add = q in
    (* Note that in the prover we do not have wires_g, so we will need to
       compose qm & ts with gX *)
    let xs, ys, _zs, qm, t_infos = get_values wires wires in
    List.mapi
      (fun i (mj, t_info) ->
        (* id_mj :=
           \sum_i ((B^i mod m) mod mj) * (x_i + y_i - z_i)
           - qm * (m mod mj) - ((qm_shift * m) mod mj) - (tj + tj_shift) * mj *)
        let id_mj_without_sum =
          (* In the case of the native modulus, we can ignore the
             (tj + tj_shift) component *)
          let tj, tj_coeff, tj_shift, tj_comp =
            match t_info with
            | Some (tj, tj_shift) ->
                ([tj], Scalar.[negate (of_z mj)], tj_shift, [1])
            | None -> ([], [], Z.zero, [])
          in
          Evaluations.linear_c
            ~res:tmps.(0)
            ~evaluations:(qm :: tj)
            ~composition_gx:(1 :: tj_comp, domain_size)
            ~linear_coeffs:(Scalar.(negate (of_z (M.modulus %! mj))) :: tj_coeff)
            ~add_constant:
              Scalar.(
                negate (of_z Z.((qm_shift * M.modulus %! mj) + (tj_shift * mj))))
            ()
        in
        let id_mj =
          List.fold_left2
            (fun acc bi_mod_m (xi, yi) ->
              (* zi is just xi composed with gX *)
              let zi = xi in
              let xi_plus_yi_minus_zi =
                Evaluations.linear_c
                  ~res:tmps.(2)
                  ~evaluations:[xi; yi; zi]
                  ~linear_coeffs:[one; one; mone]
                  ~composition_gx:([0; 0; 1], domain_size)
                  ()
              in
              let acc =
                Evaluations.linear_c
                  ~res:tmps.(1)
                  ~evaluations:[acc; xi_plus_yi_minus_zi]
                  ~linear_coeffs:[one; Scalar.of_z @@ (bi_mod_m %! mj)]
                  ()
              in
              Evaluations.copy ~res:tmps.(0) acc)
            id_mj_without_sum
            bs_mod_m
            (List.combine xs ys)
        in
        let identity =
          Evaluations.mul_c ~res:ids.(i) ~evaluations:[q_mod_add; id_mj] ()
        in
        (prefix @@ q_label ^ "." ^ string_of_int i, identity))
      ((Scalar.order, None) :: List.combine M.moduli_add t_infos)
    |> SMap.of_list

  let verifier_identities ~prefix_common ~prefix ~public:_ ~generator:_
      ~size_domain:_ : verifier_identities =
   fun _ answers ->
    let {q; wires; wires_g} =
      get_answers ~gx:true ~q_label ~prefix ~prefix_common answers
    in
    List.mapi
      (fun i id -> (prefix @@ q_label ^ "." ^ string_of_int i, id))
      (equations ~q ~wires ~wires_g ())
    |> SMap.of_list

  let cs ~q:q_mod_add ~wires ~wires_g ?precomputed_advice:_ () =
    (* z = (x + y) mod m
       let k := nb_limbs and n := |moduli|

       PlonK wires distribution:
        row i   : x0 ... x_{k-1} y0 ... y_{k-1}
        row i+1 : z0 ... z_{k-1} qm t1 ... t_{n}
    *)
    let open L in
    let xs, ys, zs, qm, t_infos = get_values wires wires_g in
    let* zero = Num.zero in
    map2M
      (fun mj t_info ->
        (* \sum_i ((B^i mod m) mod mj) * (x_i + y_i - z_i)
           - qm * (m mod mj) - ((qm_shift * m) mod mj) = (tj + tj_shift) * mj *)
        let tj, tj_shift =
          match t_info with
          | Some (tj, tj_shift) -> (tj, tj_shift)
          | None -> (zero, Z.zero)
        in
        let* id_mj =
          let sum_terms =
            List.map2
              (fun bi_mod_m ((xi, yi), zi) ->
                let c = Scalar.of_z (bi_mod_m %! mj) in
                [(c, xi); (c, yi); (Scalar.negate c, zi)])
              bs_mod_m
              (List.combine (List.combine xs ys) zs)
            |> List.concat
          in
          let qc =
            Scalar.of_z Z.(-(qm_shift * M.modulus %! mj) - (tj_shift * mj))
          in
          let coeffs, vars =
            List.split
            @@ [
                 Scalar.(negate @@ of_z (M.modulus %! mj), qm);
                 Scalar.(negate @@ of_z mj, tj);
               ]
            @ sum_terms
          in
          Num.add_list ~qc ~coeffs (to_list vars)
        in
        Num.mul q_mod_add id_mj)
      (Scalar.order :: M.moduli_add)
      (None :: t_infos)
end

module AddMod25519 = Make_ModAdd (Plompiler.ArithMod25519)
