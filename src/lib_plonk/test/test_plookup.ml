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

module Internal = struct
  open Plonk.Plookup_gate.Plookup_gate_impl (Plonk.Polynomial_protocol)

  let test_aggregation () =
    let alpha = Scalar.random () in
    let length_list = 5 in
    let length_t = 30 in
    let length_f = 25 in
    let tables =
      List.init length_list (fun _ ->
          Array.init length_t (fun _ -> Scalar.random ()))
    in
    let k_array = Array.init length_f (fun _ -> Random.int length_t) in
    let f_list =
      List.init length_list (fun i ->
          Array.init length_f (fun j ->
              let t_i = List.nth tables i in
              t_i.(k_array.(j))))
    in
    let f_list_sorted =
      List.map2 (fun f t -> Plookup_poly.sort_by f t) f_list tables
    in
    let t = Plookup_poly.compute_aggregation tables alpha in
    let f = Plookup_poly.compute_aggregation f_list_sorted alpha in
    for i = 0 to length_f - 1 do
      assert (Array.exists (fun x -> Scalar.eq x f.(i)) t)
    done ;
    ()

  let test_z () =
    let n = 32 in
    let log = 5 in
    let domain = Domain.build_power_of_two log in
    let generator = Domain.get domain 1 in
    let t = Array.init n (fun _ -> Scalar.random ()) in
    let f =
      Array.init (n - 1) (fun i ->
          let k = if i = 0 || i = 1 || i = n - 1 then 5 else i in
          t.(k))
    in
    let beta = Scalar.random () in
    let gamma = Scalar.random () in
    let one_plus_beta = Scalar.(one + beta) in
    let gamma_one_plus_beta = Scalar.(gamma * one_plus_beta) in
    let s = Plookup_poly.compute_s f t in
    let z = Plookup_poly.compute_z beta gamma f t s n domain in
    let t_poly =
      Evaluations.interpolation_fft2 domain (Plookup_poly.switch t)
    in
    let f_poly =
      Evaluations.interpolation_fft2 domain Array.(append [|zero|] f)
    in
    let h1, h2 = Plookup_poly.compute_h s domain n in
    let eval_left x =
      let x_minus_one = Scalar.(x + negate one) in
      let f_term = Scalar.(Poly.evaluate f_poly x + gamma) in
      let t_term =
        Scalar.(
          gamma_one_plus_beta + Poly.evaluate t_poly x
          + (beta * Poly.evaluate t_poly (generator * x)))
      in
      Scalar.(x_minus_one * Poly.evaluate z x * one_plus_beta * f_term * t_term)
    in
    let eval_right x =
      let x_minus_one = Scalar.(x + negate one) in
      let h_term h =
        Scalar.(
          gamma_one_plus_beta + Poly.evaluate h x
          + (beta * Poly.evaluate h (generator * x)))
      in
      Scalar.(
        x_minus_one * Poly.evaluate z (generator * x) * h_term h1 * h_term h2)
    in
    for i = 0 to 31 do
      let eval_point = Scalar.pow generator (Z.of_int i) in
      assert (Scalar.eq (eval_left eval_point) (eval_right eval_point))
    done ;
    ()

  let test_sort () =
    let a = Array.init 10 (fun i -> Scalar.of_z (Z.of_int i)) in
    let b = Array.init 12 (fun i -> Scalar.of_z (Z.of_int (11 - i))) in
    let sorted_a = Plookup_poly.sort_by a b in
    Array.iteri
      (fun i a_i -> assert (Scalar.eq a_i (Scalar.of_z (Z.of_int (9 - i)))))
      sorted_a ;
    ()
end

let tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ("Internal.test_z", Internal.test_z);
      ("Internal.test_sort", Internal.test_sort);
      ("Internal.test_aggregation", Internal.test_aggregation);
    ]
