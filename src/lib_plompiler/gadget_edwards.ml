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

open Lang_core
open Lang_stdlib

let one = S.one

let mone = S.negate one

module MakeAffine (Curve : Mec.CurveSig.AffineEdwardsT) :
  Affine_curve_intf.EDWARDS =
functor
  (L : LIB)
  ->
  struct
    module L = L
    open L

    type point = scalar * scalar

    let scalar_order = Curve.Scalar.order

    let base_order = Curve.Base.order

    let param_d = Curve.(d |> Base.to_z) |> S.of_z

    let input_point ?(kind = `Private) (u, v) =
      Input.(pair (scalar u) (scalar v)) |> input ~kind

    let get_x_coordinate p = of_pair p |> fst

    let get_y_coordinate p = of_pair p |> snd

    let id = (S.zero, S.one)

    (* 1 constraint *)
    let is_on_curve p =
      with_label ~label:"Edwards.is_on_curve"
      @@
      let x, y = of_pair p in
      let* x2 = Num.square x in
      let* y2 = Num.square y in
      (* x_l = x^2 *)
      (* x_r = y^2 *)
      (* -1 * x^2 + 1 * y^2 - d * x^2 y^2 -  1  = 0  *)
      (*  |         |         |              |    |  *)
      (*  ql        qr        qm             qc   qo *)
      let qm = S.negate param_d in
      let* o = Num.custom ~qc:mone ~ql:mone ~qr:one ~qm x2 y2 in
      Num.is_zero o

    (* 1 constraint *)
    let assert_is_on_curve p =
      with_label ~label:"Edwards.is_on_curve"
      @@
      let x, y = of_pair p in
      let* x2 = Num.square x in
      let* y2 = Num.square y in
      let qm = S.negate param_d in
      (* The last wire is multiplied by 0 so we can put any value, we chose x here. *)
      Num.assert_custom ~qc:mone ~ql:mone ~qr:one ~qm x2 y2 x

    let from_coordinates x y =
      with_label ~label:"Edwards.from_coordinates"
      @@
      let p = pair x y in
      with_bool_check (is_on_curve p) >* ret p

    let unsafe_from_coordinates x y =
      with_label ~label:"Edwards.unsafe_from_coordinates" (pair x y |> ret)

    (* P1:(u1, v1) + P2:(u2, v2) = P3:(u3, v3)
       2 constraints
    *)
    let add p1 p2 = Ecc.edwards_add p1 p2

    let cond_add p1 p2 b = Ecc.edwards_cond_add p1 p2 b

    (* 2 * P1:(u1, v1) = P1:(u1, v1) + P1:(u1, v1) = P3:(u3, v3) as the addition is complete
       12 constraints
    *)

    let double p = add p p

    let point_or_zero point b =
      with_label ~label:"Edwards.point_or_zero"
      @@
      let p_x = get_x_coordinate point in
      let p_y = get_y_coordinate point in
      (* if b = 1, return (p_u, p_v); otherwise the zero point (0, 1) *)
      let b = scalar_of_bool b in
      let* u = Num.mul b p_x in
      let* v = Num.custom ~qr:mone ~qc:one ~qm:one p_y b in
      ret @@ pair u v

    let scalar_mul s p =
      let* one = Bool.constant true in
      with_label ~label:"Edwards.scalar_mul"
      @@
      let rev_s = List.rev (of_list s) in
      let* init = point_or_zero p (List.hd rev_s) in
      foldM
        (fun acc b ->
          let* acc = cond_add acc acc one in
          cond_add acc p b)
        init
        (List.tl rev_s)

    (* Computes \prod_i p_i^s_i with inputs:
       - ls: [[s_11; ...; s_1m]; ...; [s_n1; ...; s_nm]]
       - lp: [p1; ...; pn] *)
    let multi_scalar_mul ls lp =
      let* one = Bool.constant true in
      with_label ~label:"Edwards.multi_scalar_mul"
      @@
      (* Check we apply Shamir's trick on at least 2 points *)
      let () = assert (List.(length (of_list ls) > 1)) in
      (* Converting ls to ls' = [[s_11; ...; s_n1]; ...; [s_1m; ...; s_nm]] *)
      let ls = List.map of_list (of_list ls) |> Utils.transpose |> List.rev in
      let points = of_list lp in
      (* Check we perform scalar multiplications on lists of at least 1 bit *)
      assert (List.(length ls > 0)) ;
      (* Initializing the accumulator with the first round of Shamir's trick *)
      let heads = List.hd ls in
      let* init = point_or_zero (List.hd points) (List.hd heads) in
      let* init = fold2M cond_add init (List.tl points) (List.tl heads) in

      (* Applying Shamir's trick on the rest of the rounds *)
      foldM
        (fun acc lb ->
          let* acc = cond_add acc acc one in
          fold2M cond_add acc points (of_list lb))
        init
        List.(map to_list (tl ls))
  end
